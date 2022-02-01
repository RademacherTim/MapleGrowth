#===============================================================================
# Script to wrangle growth data from various formats (mainly Tucson format) 
# into long format
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ("%>%")) library ("tidyverse") # to generally process data
if (!existsFunction ("readxl")) library ("readxl") # to read metadata
if (!existsFunction ("read.rwl")) library ("dplR") # to read chronology files

# get metadata for all chronologies with file
#-------------------------------------------------------------------------------
siteMetaData <- readxl::read_excel (
  col_names = TRUE, 
  col_types = c ("numeric","text","text","text","text","text","text",
                 "numeric","numeric","numeric","numeric","numeric","text",
                 "logical","text","text","text"),
  path = "../data/growth/chronologyData/siteMetaData.xlsx") %>% 
  filter (!is.na (file)) %>% 
  filter (source %in% c ("ITRDB","NP","JTM","BG","SP", "SF","LD","SW","DB")) %>% # TR - Add JH eventually
  mutate (lon = as.numeric (lon),
          lat = as.numeric (lat))
# NB: Still waiting for MG chronologies.

# loop over files and load each of them into a tibble
#-------------------------------------------------------------------------------
for (i in 191:208) {# 1:dim (siteMetaData) [1]) {
  
  # use source to figure out file path
  #-------------------------------------------------------------------------------
  if (siteMetaData$source [i] == 'ITRDB') {
    fPath <- '../data/growth/chronologyData/ITRDB_cleaned_Zhao2019/Cleaned datasets/itrdb-v713-cleaned-rwl/usa/'
  } else if (siteMetaData$source [i] == 'NP') {
    fPath <- '../data/growth/chronologyData/NeilPedersonCollection/'
  } else if (siteMetaData$source [i] == 'JTM') {
    fPath <- '../data/growth/chronologyData/JustinMaxwellCollection/'
  } else if (siteMetaData$source [i] == 'BG') {
    fPath <- '../data/growth/chronologyData/ISFORT/BenoitGendreau-Berthiaume/'
  } else if (siteMetaData$source [i] == 'SW') {
    fPath <- '../data/growth/chronologyData/ScottWarnerCollection/'
  } else if (siteMetaData$source [i] == 'SP') {
    fPath <- '../data/growth/chronologyData/SergePayetteCollection/Data_ACSA_SP.xlsx'
  } else if (siteMetaData$source [i] == 'SF') {
    fPath <- '../data/growth/chronologyData/ShawnFraverCollection/'
  } else if (siteMetaData$source [i] == 'LD') {
    fPath <- '../data/growth/chronologyData/LoicDOrangevilleCollection/'
  } else if (siteMetaData$source [i] == 'DB') {
    fPath <- '../data/growth/chronologyData/DanBishopCollection/'
  }
  filename <- paste0 (fPath, siteMetaData$file [i])
  #print (filename)
  
  # read the rwl file
  if (siteMetaData$source [i] == 'SP') {
    tmp <- suppressMessages (
      readxl::read_excel (col_names = TRUE, 
                          path = fPath, # In this case this is the file name
                          sheet = siteMetaData$file [i]) [-c (1:3), ])
    names (tmp) [1] <- 'year'
  } else if (siteMetaData$source [i] != 'SP') {
    tmp <- suppressMessages (
      dplR::read.rwl (fname = filename, format = 'tucson', 
                      header = ifelse (is.na (siteMetaData$header [i]), 
                                       TRUE, 
                                       siteMetaData$header [i])))
    # add year as a seperate row
    tmp$year <- rownames (tmp)
  }
  
  
  
  # pivot to longer format and add tree and core specifiers depending on format
  if (siteMetaData$labelFormat [i] == "TT-I") {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = "rwEYSTI", names_sep = "-", 
                          names_to = c ("tree","core"))
  } else if (siteMetaData$labelFormat [i] %in% c ("SSTTI","SSSTTI","SSSSTTI",
                                                  "SSPPTTI","SSSPTTI",
                                                  "SSSPPTTI","SSSSSTTI",
                                                  "SSSXXTTI")) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = "rwEYSTI", names_sep = c (2,3),
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ("tree","core"))
    if (siteMetaData$site [i] == 97) {
      temp$core [temp$tree == "18"] <- "N2"
      temp$tree [temp$tree == "18"] <- "08"
    }
  } else if (siteMetaData$labelFormat [i] %in% c ("SSPTTTI","TTTI","SSPTTI",
                                                  "SSSTT")) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = "rwEYSTI", names_sep = c (3,4),
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ("tree","core"))
    if (siteMetaData$site [i] %in% c (23:26)) temp$core <- "1"
  } else if (siteMetaData$labelFormat [i] %in% c ("SSSSTTII","SSSS-TT","SSS-TT",
                                                  "SS-TT")) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = "rwEYSTI", names_sep = c (2,4),
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ("tree","core"))
  } else if (siteMetaData$labelFormat [i] %in% c ("SSS-TT-I")) {
    temp <- pivot_longer (tmp, cols = 2:(dim (tmp)[2]), 
                          values_to = "rwEYSTI", names_sep = "-",
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ("tree","core")) %>% 
      mutate (rwEYSTI = as.numeric (rwEYSTI) / 1000) # TR - Need to double check that precision of the file
  } else if (siteMetaData$labelFormat [i] %in% c ("XTTT","XTTTT")) { # Loic"s format
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = "rwEYSTI",
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = "tree")
  }
  # delete row without ring width
  temp <- temp %>% filter (!is.na (rwEYSTI))
  
  # add incrementCoreID for SF and LD collections, which only had one core per tree
  if (siteMetaData$source [i] %in% c ("SF","LD")) temp$core <- "1"
  
  # add site name before adding it to the rwYSTI tibble
  temp <- temp %>% dplyr::mutate (site = siteMetaData$site [i], 
                                  lat = siteMetaData$lat [i],
                                  lon = siteMetaData$lon [i],
                                  species = siteMetaData$species [i],
                                  .before = tree)
  
  # initialise tibble 
  if (i == 1) {
    tempData <- temp
  # or add to it
  } else {
    tempData <- rbind (tempData, temp)
  }

  # print important metadata
  VERBOSE <- FALSE
  if (VERBOSE) {
    print (sprintf ('Site: %i', siteMetaData$site [i]))
    print (sprintf ('Start: %s', min (temp$year)))
    print (sprintf ('End: %s', max (temp$year)))
    print (sprintf ('nTrees: %i', n_groups (temp %>% group_by (tree))))
    print (sprintf ('nCores: %i', n_groups (temp %>% group_by (tree, core))))
    print (sprintf ('File name: %s', siteMetaData$file [i]))
    print (head (temp))
    print ('')
  }
  
  # delete temporary variables 
  rm (tmp, temp)
}

# add unique tree and core IDs for simulation to each row
#-------------------------------------------------------------------------------
tempData <- tempData %>% mutate (treeID = NA, incrementCoreID = NA)
tempData$treeID <- tempData %>% 
  group_by (site, tree) %>% 
  group_indices () %>% as.factor ()
max (as.numeric (levels (tempData$treeID))) # number of trees
tempData$incrementCoreID <- tempData %>% 
  group_by (site, tree, core) %>% 
  group_indices () %>% as.factor () 
max (as.numeric (levels (tempData$incrementCoreID))) # number of increment cores
length (unique (tempData$site)) # number of sites

# extract cardinal direction of the core
#-------------------------------------------------------------------------------
tempData$core [which (tempData$core %in% c ('n','N','N1','N2','N3'))] <- 'North'
tempData$core [which (tempData$core %in% c ('e','E'))] <- 'East'
tempData$core [which (tempData$core %in% c ('s','S','S1','S2','S3'))] <- 'South'
tempData$core [which (tempData$core %in% c ('w','W'))] <- 'West'
tempData <- tempData %>% 
  mutate (cardinalDir = ifelse (core == 'North', 'North', 
                                ifelse (core == 'East', 'East', 
                                        ifelse (core == 'South', 'South', 
                                                ifelse (core == 'West', 'West', NA)))))

# delete all data preceding 1948 or after 2016, as there is no climate data for 
# these years
#-------------------------------------------------------------------------------
rwEYSTI <- tempData %>% filter (year >= 1948 & year <= 2016)

# make sure year  and site are a integers
#-------------------------------------------------------------------------------
rwEYSTI <- rwEYSTI %>% mutate (site = as.factor (site),
                               year = as.factor (year))

# deselect cardinal direction for now and only keep incrementCoreID and treeID
#-------------------------------------------------------------------------------
rwEYSTI <- rwEYSTI %>% select (-cardinalDir, -core, -tree)

# reorder in terms of model subscripts (i.e., 'E' for species, 'Y' for year, 
# 'S' for sites, 'T' for tree, and 'I' for increment core)
#-------------------------------------------------------------------------------
rwEYSTI <- rwEYSTI %>% 
  relocate (rwEYSTI, species, year, site, treeID, incrementCoreID, lat, lon)

# make histogram of ring widths
#-------------------------------------------------------------------------------
PLOT <- FALSE; if (PLOT) {
  png (file = '../fig/ringWidthsHist.png', width = 700, height = 400)
  par (mar = c (5, 5, 1, 5), mfrow = c (1, 1)) 
  hist (rwEYSTI$rwEYSTI [rwEYSTI$species == 'ACSA'], 
        main = '', xlab = 'Ring width (mm)', las = 1, axes = FALSE,
        ylim = c (0, 15000),
        col = '#f3bd4833', breaks = seq (0, 18, by = 0.3))
  hist (rwEYSTI$rwEYSTI [rwEYSTI$species == 'ACRU'], add = TRUE, axes = FALSE,
        ylim = c (0, 15000), col = '#901c3b33', breaks = seq (0, 18, by = 0.3))
  axis (side = 1, seq (0, 18, 5))
  axis (side = 2, seq (0, 15000, 5000), las = 1)
  par (new = TRUE)
  plot (density (rwEYSTI$rwEYSTI [rwEYSTI$species == 'ACSA']), 
        col = '#f3bd4866', lwd = 3, main = '', xlab = '', 
        ylab = '', axes = FALSE, ylim = c (0, 0.5))
  par (new = TRUE)
  plot (density (rwEYSTI$rwEYSTI [rwEYSTI$species == 'ACRU']), 
        col = '#901c3b66', lwd = 3, main = '', xlab = '', 
        ylab = '', axes = FALSE, ylim = c (0, 0.5))
  axis (side = 4, seq (0, 0.5, 0.1), las = 1)
  mtext (side = 4, line = 4, text = 'Density')
  dev.off ()
}

# average by tree
#-------------------------------------------------------------------------------
rwEYST <- rwEYSTI %>% group_by (species, year, site, treeID, lat, lon) %>%
  summarise (rwEYST = mean (rwEYSTI, na.rm = TRUE), .groups = 'drop') %>% 
  relocate (rwEYST, species, year, site, treeID, lat, lon)

# clean up
#-------------------------------------------------------------------------------
rm (i, fPath, filename, PLOT, VERBOSE)
#===============================================================================