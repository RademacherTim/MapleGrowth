#===============================================================================
# Script to simplifies growth data called 'rwYSTI' read using the 
# wrangleGrowthData.R script. In particular, this script averages across trees 
# 'T' and increment cores 'I' to get site specific average chronologies 
# 'rwYS'.
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!exists ('rwYSTI')) source ('wrangleGrowthData.R')
if (!existsFunction ('%>%')) library ('tidyverse')

# group and summarise 'rwEYSTI' by tree (i.e., across increment cores)
#-------------------------------------------------------------------------------
rwEYST <- rwEYSTI %>% 
  group_by (species, year, site, treeID) %>% 
  summarise (rwEYST = mean (rwEYSTI), .groups = 'drop') 

# further group and summarise 'rwYEST' by site (i.e., across trees)
#-------------------------------------------------------------------------------
rwEYS <- rwEYST %>% 
  group_by (species, year, site) %>% 
  summarise (rwEYS = mean (rwYEST), .groups = 'drop')

#===============================================================================