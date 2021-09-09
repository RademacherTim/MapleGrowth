#===============================================================================
# Script to wrangle growth data from various formats (mainly Tucson format) 
# into long format
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
library ('tidyverse') # to generally process data
library ('readxl')    # to read metadata
library ('dplR')      # to read chronology files

# get metadata for all chronologies with file
#-------------------------------------------------------------------------------
siteMetaData <- readxl::read_excel (
  col_names = TRUE, 
  col_types = c ('numeric','text','text','text','text','numeric','numeric',
                 'numeric','numeric','numeric','numeric','numeric','text',
                 'text','text'),
  path = '../data/growth/chronologyData/siteMetaData.xlsx') %>% 
  filter (!is.na (file)) %>% 
  filter (source %in% c ('ITRDB','NP','JTM'))

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
                          values_to = 'rw', names_sep = '-', 
                          names_to = c ('tree','core'))
  } else if (siteMetaData$labelFormat [i] %in% c ('SSSTTI','SSSSTTI','SSPPTTI',
                                                  'SSSPPTTI','SSSSSTTI')) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = 'rw', names_sep = c (2,3),
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ('tree','core'))
  } else if (siteMetaData$labelFormat [i] %in% c ('SSPTTTI','TTTI')) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = 'rw', names_sep = c (3,4),
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ('tree','core'))
  }
  # delete row without ring width
  temp <- temp %>% filter (!is.na (rw))
  
  # add site name before adding it to the rwYSTI tibble
  temp <- temp %>% dplyr::mutate (site = siteMetaData$site [i], .before = tree)
  
  # initialise tibble 
  if (i == 1) {
    rwYSTI <- temp
  # or add to it
  } else {
    rwYSTI <- rbind (rwYSTI, temp)
  } 
}

# make sure year  and site are a integers
#-------------------------------------------------------------------------------
rwYSTI <- rwYSTI %>% mutate (site = as.integer (site),
                             year = as.integer (year))

# delete all data preceeding 1948 or after 2016, as there is no climate data for 
# these years
#-------------------------------------------------------------------------------
rwYSTI <- rwYSTI %>% filter (year >= 1948 & year <= 2016)
#===============================================================================