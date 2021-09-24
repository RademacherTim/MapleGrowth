#===============================================================================
# Script to give an example of what chronology data looks like by plotting 
# individual tree responses and the site response
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!existsFunction ('readxl')) library ('readxl') # to read metadata
if (!existsFunction ('read.rwl')) library ('dplR') # to read chronology files

# read the meta data from vt003.rwl
# They are all more or less from the same part of the woods
#-------------------------------------------------------------------------------
metaData <- readxl::read_excel (
  col_names = TRUE, 
  col_types = c ('numeric','text','text','text','text','numeric','numeric',
                 'numeric','numeric','numeric','numeric','numeric','text',
                 'text','text','text'),
  path = '../data/growth/chronologyData/siteMetaData.xlsx') %>% 
  filter (!is.na (file)) %>% 
  filter (site %in% 5:6) 

# loop over files and load each of them into a tibble
#-------------------------------------------------------------------------------
for (i in 1:dim (metaData) [1]) {
  
  # use source to figure out file path
  #-------------------------------------------------------------------------------
  if (metaData$source [i] == 'ITRDB') {
    fPath <- '../data/growth/chronologyData/ITRDB_cleaned_Zhao2019/Cleaned datasets/itrdb-v713-cleaned-rwl/usa/'
  } else if (metaData$source [i] == 'NP') {
    fPath <- '../data/growth/chronologyData/NeilPedersonCollection/'
  } else if (metaData$source [i] == 'JTM') {
    fPath <- '../data/growth/chronologyData/JustinMaxwellCollection/'
  }# else if (metaData$source == 'SW') {
  #  fPath <- ''
  #}
  filename <- paste0 (fPath, metaData$file [i])
  #print (filename)
  
  # read the rwl file
  tmp <- read.rwl (fname = filename, format = 'tucson')
  
  # add year as a row
  tmp$year <- rownames (tmp)
  
  # pivot to longer format and add tree and core specifiers depending on format
  if (metaData$labelFormat [i] == 'TT-I') {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = 'rwYSTI', names_sep = '-', 
                          names_to = c ('tree','core'))
  } else if (metaData$labelFormat [i] %in% c ('SSSTTI','SSSSTTI','SSPPTTI',
                                                  'SSSPPTTI','SSSSSTTI')) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = 'rwYSTI', names_sep = c (2,3),
                          names_prefix = metaData$labelPrefix [i],
                          names_to = c ('tree','core'))
  } else if (metaData$labelFormat [i] %in% c ('SSPTTTI','TTTI')) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = 'rwYSTI', names_sep = c (3,4),
                          names_prefix = metaData$labelPrefix [i],
                          names_to = c ('tree','core'))
  }
  # delete row without ring width
  temp <- temp %>% filter (!is.na (rwYSTI))
  
  # add site name before adding it to the rwYSTI tibble
  temp <- temp %>% dplyr::mutate (site = metaData$site [i], 
                                  species = metaData$species [i],
                                  .before = tree)
  
  # initialise tibble 
  if (i == 1) {
    rwYSTI <- temp
    # or add to it
  } else {
    rwYSTI <- rbind (rwYSTI, temp)
  } 
}

# wrangle data into wide format for increment core I, trees T, and sites S
#-------------------------------------------------------------------------------
wideDataI <- rwYSTI %>% 
  pivot_wider (id_cols = c (site, tree, core), values_from = rwYSTI, 
               names_from = year, names_prefix = 'Y') 
wideDataT <- rwYSTI %>% group_by (year, site, tree) %>% 
  summarise (rwYSTI = mean (rwYSTI, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider (id_cols = c (site, tree), values_from = rwYSTI, 
               names_from = year, names_prefix = 'Y') 
wideDataSM <- rwYSTI %>% group_by (year, site) %>% 
  summarise (rwYSTI = mean (rwYSTI, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider (id_cols = c (site), values_from = rwYSTI, 
               names_from = year, names_prefix = 'Y') 
wideDataSE <- rwYSTI %>% group_by (year, site) %>% 
  summarise (rwYSTI = sd (rwYSTI, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider (id_cols = c (site), values_from = rwYSTI, 
               names_from = year, names_prefix = 'Y') 
wideData <- rwYSTI %>% group_by (year) %>% 
  summarise (rwYSTI = mean (rwYSTI, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider (values_from = rwYSTI, 
               names_from = year, names_prefix = 'Y') 
wideDataE <- rwYSTI %>% group_by (year) %>% 
  summarise (rwYSTI = sd (rwYSTI, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider (values_from = rwYSTI, 
               names_from = year, names_prefix = 'Y') 

# plot time series
#-------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = NULL, 
      xlim = c (1800, 2010), typ = 'l', ylim = c (0, 8.0), 
      axes = FALSE, xlab = 'Year of formation', ylab = 'Ring width (mm)')
axis (side = 1)
axis (side = 2, las = 1)
# add individual cores data
for (i in 1:dim (wideDataI) [1]) {
  lines (x = 1808:2005, y = wideDataI [i, 4:dim (wideDataI) [2]], lwd  = 0.3,
         col = '#66666666')
}
# add average tree data
for (t in 1:dim (wideDataT) [1]) {
  lines (x = 1808:2005, y = wideDataT [t, 3:dim (wideDataT) [2]], lwd  = 0.5,
         col = '#333333')
}
# add average site data
for (s in 1:2) {
  polygon (x = c (1808:2005, 2005:1808), 
           y = c (wideDataS [s, 2:dim (wideDataSM) [2]] + wideDataSE [s, 2:dim (wideDataS) [2]],
                  rev (wideDataS [s, 2:dim (wideDataSM) [2]] - wideDataSE [s, 2:dim (wideDataS) [2]])),
           lty = 0, col = '#10647044')
  lines (x = 1808:2005, y = wideDataS [s, 2:dim (wideDataS) [2]], lwd  = 2,
         col = '#106470')
}
# add overall mean and standard deviation
polygon (x = c (1808:2005, 2005:1808), 
         y = c (wideData + wideDataE,
                rev (wideData - wideDataE)),
         lty = 0, col = '#D6083B44')
lines (x = 1808:2005, y = wideData, lwd  = 3,
       col = '#D6083B')
