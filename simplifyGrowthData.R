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

# group and summarise r'wYSTI' by tree (i.e., across increment cores)
#-------------------------------------------------------------------------------
rwYST <- rwYSTI %>% 
  group_by (species, year, site, treeID) %>% 
  summarise (rwYST = mean (rwYSTI), .groups = 'drop') 

# further group and summarise 'rwYST' by site (i.e., across trees)
#-------------------------------------------------------------------------------
rwYS <- rwYST %>% 
  group_by (species, year, site) %>% 
  summarise (rwYS = mean (rwYST), .groups = 'drop')

#===============================================================================