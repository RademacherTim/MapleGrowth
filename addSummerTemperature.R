#===============================================================================
# Script to add mean summer (i.e., June, July, and August) temperature data to 
# the ring width for each tree and year
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
library ('tidyverse') # to generally process data
library ('ncdf4')  # to manipulate netcdf files (climate)
source ('wrangleGrowthData.R') # to load ring width data from all sites

# initialise spatial resolution for climate data
#-------------------------------------------------------------------------------
res <- 0.25

# directory name with climate data
#-------------------------------------------------------------------------------
dirString <- '../data/climate/princeton/'

# initial possible climates for climate data 
#-------------------------------------------------------------------------------
dates <- seq (from = as_date ('1948-01-01'), 
              to   = as_date ('2016-12-31'), by = 1)

# add elevation of climate data to siteMetaData to compare the elevation of the 
# actual stands versus the elevation to which weather is calibrated
#-------------------------------------------------------------------------------
siteMetaData <- siteMetaData %>% mutate (eleClim = NA)

# add tas column for mean summer temperature of the growing site
#-------------------------------------------------------------------------------
rwYSTI <- rwYSTI %>% mutate (tas = NA)

# loop over each site to find compute and add mean summer temperature 
#-------------------------------------------------------------------------------
for (i in 53715:dim (rwYSTI) [1]) {
  
  # get site's ID and the year of growth
  #-----------------------------------------------------------------------------
  s <- rwYSTI$site [i]
  y <- rwYSTI$year [i]
  if (s == 22) next 
  
  # find 0.25 by 0.25 degree grid cell that contains the site s
  #-----------------------------------------------------------------------------
  lat <- ceiling (siteMetaData$lat [s] / res) * res - (res / 2)
  lon <- ceiling (siteMetaData$lon [s] / res) * res - (res / 2)
  
  # convert latitude and longitude to index used in netcdf files
  #-----------------------------------------------------------------------------
  iLat  <- (60 / res) + lat / res + res * 2.0
  iLat2 <- (90 / res) + lat / res + res * 2.0 # elevation file has more gird cells
  if (lon < 0 ) { # Western hemisphere
    iLon <- (360 + lon) / res + res * 2.0
  } else { # Eastern hemisphere
    iLon <- lon / res + res * 2.0
  }

  # read grid cell elevation from climate data and add it to the metaData
  #-----------------------------------------------------------------------------
  nc_elev <- nc_open (file = '../data/climate/princeton/elevation_0.25deg.nc')
  siteMetaData$eleClim [s] <- 
    ncvar_get (nc_elev, "elev", 
               start = c (iLon, iLat2, 1, 1), 
               count = c (1, 1, 1, 1))
  nc_close (nc_elev); rm (nc_elev)
  
  # check whether year is a leap year
  #---------------------------------------------------------------------------
  daysInYear <- ifelse (y %% 4 == 0, 366, 365)
  
  # concatenate file names 
  #---------------------------------------------------------------------------
  fileNameTas  <- paste0 (dirString,'tas/tas_daily_',y,'-',y,'.nc')
  
  # open annual climate data files
  #---------------------------------------------------------------------------
  nc_tas  <- ncdf4::nc_open (fileNameTas)
  
  # extract climate variables for specific grid cell from grid file with :
  #    1440 longitudinal grid cells spanning from   0.125 to 359.875 degrees,
  #     600 latitudinal  grid cells spanning from -59.875 to  89.875 degrees,
  #     365 ou 366 temporal steps spanning from 0 to 525600 minutes 
  #---------------------------------------------------------------------------
  tas  <- ncvar_get (nc_tas,  "tas",  start = c (iLon, iLat, 1), 
                     count = c (1, 1, daysInYear)) # K
  
  # get and replace fill values
  #---------------------------------------------------------------------------
  fillValue <- ncatt_get (nc_tas, "tas", "_FillValue")
  tas  <- tas  %>% replace (tas  == fillValue$value, values = NA)
  
  # close annual climate data files
  #---------------------------------------------------------------------------
  nc_close (nc_tas)
  
  # convert units
  #---------------------------------------------------------------------------
  tas  <- tas  - 273.15                               # K          -> deg C
  
  # calculate mean summer temperature and dd it to the data
  #---------------------------------------------------------------------------
  doyStart <- ifelse (daysInYear == 365, 152, 153)
  doyEnd <- ifelse (daysInYear == 365, 243, 244)
  rwYSTI$tas [i] <- tas [doyStart:doyEnd] %>% mean ()
    
  # delete unnecessary variables
  #---------------------------------------------------------------------------
  rm (tas, nc_tas, fillValue, fileNameTas)
  
}

#===============================================================================