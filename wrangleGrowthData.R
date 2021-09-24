#===============================================================================
# Script to wrangle growth data from various formats (mainly Tucson format) 
# into long format
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!existsFunction ('readxl')) library ('readxl') # to read metadata
if (!existsFunction ('read.rwl')) library ('dplR') # to read chronology files

# get metadata for all chronologies with file
#-------------------------------------------------------------------------------
siteMetaData <- readxl::read_excel (
  col_names = TRUE, 
  col_types = c ('numeric','text','text','text','text','numeric','numeric',
                 'numeric','numeric','numeric','numeric','numeric','text',
                 'text','text','text'),
  path = '../data/growth/chronologyData/siteMetaData.xlsx') %>% 
  filter (!is.na (file)) %>% 
  filter (source %in% c ('ITRDB','NP','JTM')) 
# NB: Does not include BG chronologies.
# NB: Still waiting for MG, SP and SW chronologies.

# loop over files and load each of them into a tibble
#-------------------------------------------------------------------------------
for (i in 1:dim (siteMetaData) [1]) {
  
  # use source to figure out file path
  #-------------------------------------------------------------------------------
  if (siteMetaData$source [i] == 'ITRDB') {
    fPath <- '../data/growth/chronologyData/ITRDB_cleaned_Zhao2019/Cleaned datasets/itrdb-v713-cleaned-rwl/usa/'
  } else if (siteMetaData$source [i] == 'NP') {
    fPath <- '../data/growth/chronologyData/NeilPedersonCollection/'
  } else if (siteMetaData$source [i] == 'JTM') {
    fPath <- '../data/growth/chronologyData/JustinMaxwellCollection/'
  }# else if (siteMetaData$source == 'SW') {
  #  fPath <- ''
  #}
  filename <- paste0 (fPath, siteMetaData$file [i])
  #print (filename)
  
  # read the rwl file
  tmp <- read.rwl (fname = filename, format = 'tucson')
  
  # add year as a row
  tmp$year <- rownames (tmp)
  
  # pivot to longer format and add tree and core specifiers depending on format
  if (siteMetaData$labelFormat [i] == 'TT-I') {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = 'rwEYSTI', names_sep = '-', 
                          names_to = c ('tree','core'))
  } else if (siteMetaData$labelFormat [i] %in% c ('SSSTTI','SSSSTTI','SSPPTTI',
                                                  'SSSPPTTI','SSSSSTTI')) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = 'rwEYSTI', names_sep = c (2,3),
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ('tree','core'))
  } else if (siteMetaData$labelFormat [i] %in% c ('SSPTTTI','TTTI')) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = 'rwEYSTI', names_sep = c (3,4),
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ('tree','core'))
  }
  # delete row without ring width
  temp <- temp %>% filter (!is.na (rwEYSTI))
  
  # add site name before adding it to the rwYSTI tibble
  temp <- temp %>% dplyr::mutate (site = siteMetaData$site [i], 
                                  species = siteMetaData$species [i],
                                  .before = tree)
  
  # initialise tibble 
  if (i == 1) {
    rwEYSTI <- temp
  # or add to it
  } else {
    rwEYSTI <- rbind (rwEYSTI, temp)
  } 
}

# delete all data preceeding 1948 or after 2016, as there is no climate data for 
# these years
#-------------------------------------------------------------------------------
rwEYSTI <- rwEYSTI %>% filter (year >= 1948 & year <= 2016)

# make sure year  and site are a integers
#-------------------------------------------------------------------------------
rwEYSTI <- rwEYSTI %>% mutate (site = as.factor (site),
                               year = as.factor (year))

# add unique tree and core IDs for simulation to each row
#-------------------------------------------------------------------------------
rwEYSTI <- rwEYSTI %>% mutate (treeID = NA, incrementCoreID = NA)
rwEYSTI$treeID <- rwEYSTI %>% 
  group_by (site, tree) %>% 
  group_indices () %>% as.factor ()
max (as.numeric (levels (rwEYSTI$treeID)))
rwEYSTI$incrementCoreID <- rwEYSTI %>% 
  group_by (site, tree, core) %>% 
  group_indices () %>% as.factor () 
max (as.numeric (levels (rwEYSTI$incrementCoreID)))

# extract cardinal direction of the core
#-------------------------------------------------------------------------------
rwEYSTI$core [which (rwEYSTI$core %in% c ('n','N'))] <- 'North'
rwEYSTI$core [which (rwEYSTI$core %in% c ('e','E'))] <- 'East'
rwEYSTI$core [which (rwEYSTI$core %in% c ('s','S'))] <- 'South'
rwEYSTI$core [which (rwEYSTI$core %in% c ('w','W'))] <- 'West'
rwEYSTI <- rwEYSTI %>% 
  mutate (cardinalDir = ifelse (core == 'North', 'North', 
                                ifelse (core == 'East', 'East', 
                                        ifelse (core == 'South', 'South', 
                                                ifelse (core == 'West', 'West', NA)))))
# deselect cardinal direction for now and only keep incrementCoreID and treeID
#-------------------------------------------------------------------------------
rwEYSTI <- rwEYSTI %>% select (-cardinalDir, -core, -tree)

# reorder in terms of model subscripts (i.e., 'E' for species, 'Y' for year, 
# 'S' for sites, 'T' for tree, and 'I' for increment core)
#-------------------------------------------------------------------------------
rwEYSTI <- rwEYSTI %>% relocate (rwEYSTI, species, year, site, treeID, incrementCoreID)

# make histogram of ring widths
#-------------------------------------------------------------------------------
PLOT <- FALSE; if (PLOT) {
  png (file = '../fig/ringWidthsHist.png')
  par (mar = c (5, 5, 1, 5)) 
  hist (rwEYSTI$rwEYSTI, main = '', xlab = 'Ring width (mm)', las = 1, axes = FALSE,
        col = '#AAB30099', breaks = seq (0, 18, by = 0.3))
  axis (side = 1, seq (0, 18, 5))
  axis (side = 2, seq (0, 25000, 5000), las = 1)
  par (new = TRUE)
  plot (density (rwYSTI$rwYSTI), col = '#445026', lwd = 3, main = '', xlab = '', 
        ylab = '', axes = FALSE)
  axis (side = 4, seq (0, 0.4, 0.1), las = 1)
  mtext (side = 4, line = 4, text = 'Density')
  dev.off ()
}
#===============================================================================