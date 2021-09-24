#===============================================================================
# Script to make a figure to explain the antecedant climate effects.
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!existsFunction ('as_date')) library ('lubridate') # to use as_date function
if (!existsFunction ('nc_open')) library ('ncdf4')  # to manipulate netcdf files (climate)
if (!exists ('rwYSTI')) source ('exampleChronology.R') # to load ring width data from all sites

# initialise spatial resolution for climate data
#-------------------------------------------------------------------------------
res <- 0.25

# directory name with climate data
#-------------------------------------------------------------------------------
dirString <- '/Volumes/TREE LAB001/data/climate/princeton/'

# initial possible climates for climate data 
#-------------------------------------------------------------------------------
dates <- seq (from = as_date ('1948-01-01'), 
              to   = as_date ('2016-12-31'), by = 1)

# add tas column for mean summer temperature of the growing site
#-------------------------------------------------------------------------------
d <- rwYSTI %>% mutate (tasJan0 = NA, preJan0 = NA, # current January   climate
                        tasFeb0 = NA, preFeb0 = NA, # current February  climate
                        tasMar0 = NA, preMar0 = NA, # current March     climate
                        tasApr0 = NA, preApr0 = NA, # current April     climate
                        tasMay0 = NA, preMay0 = NA, # current May       climate
                        tasJun0 = NA, preJun0 = NA, # current June      climate
                        tasJul0 = NA, preJul0 = NA, # current July      climate
                        tasAug0 = NA, preAug0 = NA, # current August    climate
                        tasSep0 = NA, preSep0 = NA) # current September climate

# loop over each site to find compute and add mean summer temperature 
#-------------------------------------------------------------------------------
time0 <- Sys.time ()

# print site number to see progress
#-----------------------------------------------------------------------------
print (s)

# get site's ID and the year of growth
#-----------------------------------------------------------------------------
startYear <- ifelse (metaData$start [1] < 1948, 1948, 
                     metaData$start [1])
endYear <-  ifelse (metaData$end [1] > 2016, 2016, 
                    metaData$end [1])


# find 0.25 by 0.25 degree grid cell that contains the site s
#-----------------------------------------------------------------------------
lat <- ceiling (metaData$lat [1] / res) * res - (res / 2)
lon <- ceiling (metaData$lon [1] / res) * res - (res / 2)

# convert latitude and longitude to index used in netcdf files
#-----------------------------------------------------------------------------
iLat  <- (60 / res) + lat / res + res * 2.0
iLat2 <- (90 / res) + lat / res + res * 2.0 # elevation file has more gird cells
if (lon < 0 ) { # Western hemisphere
  iLon <- (360 + lon) / res + res * 2.0
} else { # Eastern hemisphere
  iLon <- lon / res + res * 2.0
}

# loop over years for each site
#-----------------------------------------------------------------------------
for (y in startYear:endYear) {
  
  # check whether year is a leap year
  #---------------------------------------------------------------------------
  daysInYear <- ifelse (y %% 4 == 0, 366, 365)
  
  # concatenate file names 
  #---------------------------------------------------------------------------
  fileNameTas  <- paste0 (dirString,'tas/tas_daily_',y,'-',y,'.nc')
  fileNamePrcp <- paste0 (dirString,'prcp/prcp_daily_',y,'-',y,'.nc')
  
  # open annual climate data files
  #---------------------------------------------------------------------------
  nc_tas  <- ncdf4::nc_open (fileNameTas)
  nc_prcp <- ncdf4::nc_open (fileNamePrcp)
  
  # extract climate variables for specific grid cell from grid file with :
  #    1440 longitudinal grid cells spanning from   0.125 to 359.875 degrees,
  #     600 latitudinal  grid cells spanning from -59.875 to  89.875 degrees,
  #     365 ou 366 temporal steps spanning from 0 to 525600 minutes 
  #---------------------------------------------------------------------------
  tas  <- ncvar_get (nc_tas,  "tas",  start = c (iLon, iLat, 1), 
                     count = c (1, 1, daysInYear)) # K
  prcp <- ncvar_get (nc_prcp, "prcp", start = c (iLon, iLat, 1), 
                     count = c (1, 1, daysInYear)) # kg m-2 s-1
  
  # get and replace fill values
  #---------------------------------------------------------------------------
  fillValue <- ncatt_get (nc_tas, "tas", "_FillValue")
  tas  <- tas  %>% replace (tas  == fillValue$value, values = NA)
  prcp <- prcp %>% replace (prcp == fillValue$value, values = NA)
  
  # close annual climate data files
  #---------------------------------------------------------------------------
  nc_close (nc_tas)
  nc_close (nc_prcp)
  
  # convert units
  #---------------------------------------------------------------------------
  tas  <- tas  - 273.15                               # K          -> deg C
  prcp <- prcp * 86400                                # kg m-2 s-1 -> mm d-1
  
  # determine day of the year when month start and end
  #---------------------------------------------------------------------------
  if (daysInYear == 365) {
    startDOYs <- c (1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  } else {
    startDOYs <- c (1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336)
  }
  if (daysInYear == 365) {
    endDOYs <- c (31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
  } else {
    endDOYs <- c (31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
  }
  
  # loop over months 
  # N.B.: should eventually add the previous five years here as a loop
  #---------------------------------------------------------------------------
  for (m in 1:9) { # only include months up to current September
    
    # determine days for the start and end of the period
    #-------------------------------------------------------------------------
    doyStart <- startDOYs [m] # first day of month
    doyEnd   <- endDOYs   [m] # first day of month
    
    # add mean period air surface temperature to the data 
    #-------------------------------------------------------------------------
    d [which (d$site == s), 7 + 2 * (m - 1)] <- tas  [doyStart:doyEnd] %>% 
      mean ()
    
    # add total period precipitation to the data 
    #-------------------------------------------------------------------------
    d [which (d$site == s), 8 + 2 * (m - 1)] <- prcp [doyStart:doyEnd] %>% 
      sum ()
  }  # close loop over months
  
  # delete unnecessary variables
  #---------------------------------------------------------------------------
  rm (fillValue, tas, nc_tas, fileNameTas, prcp, nc_prcp, fileNamePrcp)
} # close loop over years

#===============================================================================
