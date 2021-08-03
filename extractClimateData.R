#===============================================================================
# Script to extract climate data for grid cells containing sample sites of 
# growth data. The coordinates are compiled in compileSiteCoordinates.R. 
# Daily climate data were downloaded from the Princeton reanalysis data at a 
# spatial resolution of 0.25 x 0.25 degree.
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
library ('ncdf4')  # package for netcdf manipulation
library ('raster') # package for raster manipulation
source ('compileSiteCoordinates.R')

# initialise spatial resolution 
#-------------------------------------------------------------------------------
res <- 0.25

# initial possible climates for climate data 
#-------------------------------------------------------------------------------
dates <- seq (from = as_date ('1948-01-01'), 
              to   = as_date ('2016-12-31'), by = 1)

# create function to extract climate data for a specific site (s)
# maximum length of climate data is from 1948 to 2016?
#-------------------------------------------------------------------------------
extractClimate <- function (s) { # s = site index

  # create tibble for climate variables for each site 
  #-------------------------------------------------------------------------------
  climate <- tibble (
    date = dates,                   # date of the daily climate varibles
    prcp = rep (NA, length (dates)) # total daily precipiation (kg m-2 s-1)
  )
    
  # find 0.25 by 0.25 degree grid cell that contains the site s
  #-----------------------------------------------------------------------------
  lat <- ceiling (siteMetaData$lat [s] / res) * res - (res / 2)
  lon <- ceiling (siteMetaData$lon [s] / res) * res - (res / 2)

  # convert latitude and longitude to index
  #-----------------------------------------------------------------------------
  
  
  # determine start and end date
  #-----------------------------------------------------------------------------
  start <- siteMetaData$start [s]
  end   <- siteMetaData$end   [s]

  # loop over annual files to extract all relevant days
  #-----------------------------------------------------------------------------
  for (y in year (start):year (end)) {

    # concatenate file name 
    #---------------------------------------------------------------------------
    fileName <- paste0 ('../data/climate/princeton/prcp/prcp_daily_',y,'-',y,'.nc')
    
    # open annual climate data files
    #---------------------------------------------------------------------------
    nc_prcp <- ncdf4::nc_open (fileName)
    
    # extract variable 
    prcp <-

    # get climate variables for the site's grid cell
    #---------------------------------------------------------------------------

    # close annual climate data files
    #---------------------------------------------------------------------------

  } # end of annual loop

  # return climate data 
  #-------------------------------------------------------------------------------
  return (climate)
  
} # end function to extract climate data

#===============================================================================