#===============================================================================
# Script to plot ring width data to explore the data
# - plot ring width of year Y versus ring width of the previous year Y-1
# - plot ring width versus size (cumulative ring width as proxy for size)
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!existsFunction ('as_date')) library ('lubridate') # to use as_date function
if (!existsFunction ('nc_open')) library ('ncdf4')  # to manipulate netcdf files (climate)
if (!exists ('rwEYSTI')) source ('wrangleGrowthData.R') # to load ring width data from all sites

# wrangle data 
#-------------------------------------------------------------------------------
tempData$prevRW <- NA
for (i in 1:dim (tempData) [1]) {
  # find index
  index <- which (tempData$site == tempData$site [i] & # same site
                  tempData$treeID == tempData$treeID [i] & # same tree
                  tempData$incrementCoreID == tempData$incrementCoreID [i] & # same core
                  tempData$year == as.character (as.integer (tempData$year [i])-1)) # previous year
  
  # find previous year's growth
  if (length (index) == 1) tempData$prevRW [i] <- tempData$rwEYSTI [index] 
  if (i %% 20000 == 0) print (i)
}

# plot ring width versus previous year ring width
#-------------------------------------------------------------------------------
plot (x = tempData$prevRW, y = tempData$rwEYSTI, pch = 19, col = '#91b9a466')