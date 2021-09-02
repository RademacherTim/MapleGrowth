#===============================================================================
# Script to extract climate data for grid cells containing sample sites of 
# growth data. The coordinates are compiled in compileSiteCoordinates.R. 
# Daily climate data were downloaded from the Princeton reanalysis data 
# (available at http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/) at a 
# spatial resolution of 0.25 x 0.25 degree.
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
library ('ncdf4')  # package for netcdf manipulation
library ('raster') # package for raster manipulation
library ('tidyquant') # package to summarise daily to weekly data
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

  # determine number of climate variables 
  #     1. mean daily relative humidity       (%)
  #     2. total daily precipiation           (mm d-1)
  #     3. mean daily air surface temperature (deg C)
  #     4. mean daily maximum temperature     (deg C)
  #     5. mean daily minimum temperature     (deg C)
  #     6. mean daily vapour pressure deficit (kPa)
  #     7. mean daily wind speed              (m s-1)
  #-----------------------------------------------------------------------------
  n <- 7
  
  # create tibble for climate variables for each site in long format
  #-----------------------------------------------------------------------------
  climate <- tibble (
    site = s,              # site index from the siteMetaData
    date = rep (dates, n), # date of the daily climate variables
    var  = c (rep ('rh',   length (dates)),
              rep ('prcp', length (dates)),
              rep ('tas',  length (dates)),
              rep ('tmax', length (dates)),
              rep ('tmin', length (dates)),
              rep ('vpd',  length (dates)),
              rep ('wind', length (dates))),
    value = NA
  )
    
  # find 0.25 by 0.25 degree grid cell that contains the site s
  #-----------------------------------------------------------------------------
  lat <- ceiling (siteMetaData$lat [s] / res) * res - (res / 2)
  lon <- ceiling (siteMetaData$lon [s] / res) * res - (res / 2)

  # convert latitude and longitude to index
  #-----------------------------------------------------------------------------
  iLat  <- (60 / res) + lat / res + res * 2.0
  iLat2 <- (90 / res) + lat / res + res * 2.0 # elevation file has more gird cells
  if (lon < 0 ) { # Western hemisphere
    iLon <- (360 + lon) / res + res * 2.0
  } else { # Eastern hemisphere
    iLon <- lon / res + res * 2.0
  }
  
  # determine start and end date
  #-----------------------------------------------------------------------------
  start <- siteMetaData$start [s]
  end   <- siteMetaData$end   [s]
  
  # check that there is overlap between climate data and growth data
  #-----------------------------------------------------------------------------
  if (start < 1948) {
    if (end < 1948) {
      return ('No common period for climate and growth data.')
    } else {
      start <- 1948 
    }
  } 
  if (end > 2016) end <- 2016
  siteMetaData$startClim [s] <- start
  siteMetaData$endClim   [s] <- end
  
  # read grid cell elevation from climate data and add it to the metaData
  #-----------------------------------------------------------------------------
  nc_elev <- nc_open (file = '../data/climate/princeton/elevation_0.25deg.nc')
  #la <- ncvar_get (nc_elev, "latitude")
  #lo <- ncvar_get (nc_elev, "longitude")
  siteMetaData$eleClim [s] <- 
    ncvar_get (nc_elev, "elev", 
               start = c (iLon, iLat2, 1, 1), 
               count = c (1, 1, 1, 1))
  nc_close (nc_elev); rm (nc_elev)
   
  
  # loop over annual files to extract all relevant days
  #-----------------------------------------------------------------------------
  for (y in start:end) {

    # check whether year is a leap year
    #---------------------------------------------------------------------------
    daysInYear <- ifelse (y %% 4 == 0, 366, 365)
    
    # concatenate file names 
    #---------------------------------------------------------------------------
    dirString <- '../data/climate/princeton/'
    fileNamePrcp <- paste0 (dirString,'prcp/prcp_daily_',y,'-',y,'.nc')
    fileNamePres <- paste0 (dirString,'pres/pres_daily_',y,'-',y,'.nc')
    fileNameShum <- paste0 (dirString,'shum/shum_daily_',y,'-',y,'.nc')
    fileNameTas  <- paste0 (dirString,'tas/tas_daily_',y,'-',y,'.nc')
    #fileNameTmax <- paste0 (dirString,'tmax/tmax_daily_',y,'-',y,'.nc')
    #fileNameTmin <- paste0 (dirString,'tmin/tmin_daily_',y,'-',y,'.nc')
    #fileNameWind <- paste0 (dirString,'wind/wind_daily_',y,'-',y,'.nc')
    
    # open annual climate data files
    #---------------------------------------------------------------------------
    nc_prcp <- ncdf4::nc_open (fileNamePrcp)
    nc_pres <- ncdf4::nc_open (fileNamePres)
    nc_shum <- ncdf4::nc_open (fileNameShum)
    nc_tas  <- ncdf4::nc_open (fileNameTas)
    #nc_tmax <- ncdf4::nc_open (fileNameTmax)
    #nc_tmin <- ncdf4::nc_open (fileNameTmin)
    #nc_wind <- ncdf4::nc_open (fileNameWind)
    
    # extract climate variables for specific grid cell from grid file with :
    #    1440 longitudinal grid cells spanning from   0.125 to 359.875 degrees,
    #     600 latitudinal  grid cells spanning from -59.875 to  89.875 degrees,
    #     365 ou 366 temporal steps spanning from 0 to 525600 minutes 
    #---------------------------------------------------------------------------
    prcp <- ncvar_get (nc_prcp, "prcp", start = c (iLon, iLat, 1), 
                       count = c (1, 1, daysInYear)) # kg m-2 s-1
    pres <- ncvar_get (nc_pres, "pres", start = c (iLon, iLat, 1), 
                       count = c (1, 1, daysInYear)) # Pa
    shum <- ncvar_get (nc_shum, "shum", start = c (iLon, iLat, 1), 
                       count = c (1, 1, daysInYear)) # kg kg-1
    tas  <- ncvar_get (nc_tas,  "tas",  start = c (iLon, iLat, 1), 
                       count = c (1, 1, daysInYear)) # K
    #tmax <- ncvar_get (nc_tmax, "tmax", start = c (iLon, iLat, 1), 
    #                   count = c (1, 1, daysInYear)) # K
    #tmin <- ncvar_get (nc_tmin, "tmin", start = c (iLon, iLat, 1), 
    #                   count = c (1, 1, daysInYear)) # K
    #wind <- ncvar_get (nc_wind, "wind", start = c (iLon, iLat, 1), 
    #                   count = c (1, 1, daysInYear)) # m s-1
     
    # get and replace fill values
    #---------------------------------------------------------------------------
    fillValue <- ncatt_get (nc_prcp, "prcp", "_FillValue")
    prcp <- prcp %>% replace (prcp == fillValue$value, values = NA)
    pres <- pres %>% replace (pres == fillValue$value, values = NA)
    shum <- shum %>% replace (shum == fillValue$value, values = NA)
    tas  <- tas  %>% replace (tas  == fillValue$value, values = NA)
    #tmax  <- tmax  %>% replace (tmax == fillValue$value, values = NA)
    #tmin  <- tmin  %>% replace (tmin == fillValue$value, values = NA)
    #wind  <- wind  %>% replace (wind == fillValue$value, values = NA)
    
    # close annual climate data files
    #---------------------------------------------------------------------------
    nc_close (nc_prcp); nc_close (nc_pres); nc_close (nc_shum)
    nc_close (nc_tas)#; nc_close (nc_tmax); nc_close (nc_tmin)
    #nc_close (nc_wind)
    
    # convert units
    #---------------------------------------------------------------------------
    prcp <- prcp * 86400                                # kg m-2 s-1 -> mm d-1
    tas  <- tas  - 273.15                               # K          -> deg C
    #tmax <- tmax - 273.15                               # K          -> deg C 
    #tmin <- tmin - 273.15                               # K          -> deg C 
    # Magnus formula for water
    #es <- 610.94 * exp ((17.625 * tas) / (tas + 243.04))              # Pa
    # Improved formula after Huang 2018
    es <- (exp (34.494 - (4924.99 / (tas + 237.1)))) / (tas + 105)^1.57 # Pa
    ea <-  (shum * pres) / (0.622 + 0.378 * shum)       # Pa
    rh <- 100 * ea / es                                 # %
    vpd <- (es - ea)  / 1e3                             # kPa
    
    # add the variables to the climate tibble
    #---------------------------------------------------------------------------
    climate$value [year (climate$date) == y & climate$var == 'rh']   <- rh
    climate$value [year (climate$date) == y & climate$var == 'prcp'] <- prcp
    climate$value [year (climate$date) == y & climate$var == 'tas']  <- tas
    #climate$tmax [year (climate$date) == y & climate$var == 'tmax'] <- tmax
    #climate$tmin [year (climate$date) == y & climate$var == 'tmin'] <- tmin
    climate$value [year (climate$date) == y & climate$var == 'vpd']  <- vpd
    #climate$wind [year (climate$date) == y & climate$var == 'wind'] <- wind
    
    # delete unnecessary variables
    #---------------------------------------------------------------------------
    rm (ea, es, rh, shum, prcp, pres, tas, vpd, #tmax, tmin, wind, 
        nc_prcp, nc_pres, nc_shum, nc_tas, #nc_tmax, nc_tmin, nc_wind, 
        fillValue,
        fileNamePrcp, fileNamePres, fileNameShum, fileNameTas, 
        #fileNameTmax, fileNameTmin, fileNameWind
    )
    
  } # end of annual loop

  # return climate data 
  #-------------------------------------------------------------------------------
  return (climate)
  
} # end function to extract climate data


# create function to summarise weekly climatologies
#-------------------------------------------------------------------------------
getWeeklySummaries <- function (clm) {
  
  # derive weekly climate variables in long format
  #-----------------------------------------------------------------------------
  # year
  # week
  # mid-date of the climate week (i.e. 4th day of a normal week)
  #  rh   = NA, # weekly mean relative humidity                (%)
  #  prcp = NA, # weekly total precipitation                   (mm w-1)
  #  tas  = NA, # weekly mean air temperature at the surface   (deg C)
  #  tmax = NA, # weekly maximum temperature of daily maximums (deg C)
  #  tmin = NA, # weekly minimum temperature daily minimums    (deg C)
  #  vpd  = NA, # weekly mean vapour pressure deficit          (kPa)
  #  wind = NA  # weekly mean wind speed                       (m s-1)
  #)
  
  # summarise individual climate variables
  #-----------------------------------------------------------------------------
  weeklyClimate <- clm %>%  
    mutate (year = lubridate::year (date), 
            week = lubridate::week (date)) %>% 
    group_by (year, week, var) %>% 
    summarise (date = mean (date,  na.rm = TRUE),
               mean = mean (value, na.rm = TRUE),
               sum  = sum  (value, na.rm = TRUE), 
               .groups = 'drop')
   
  # deselect all variables that are not useful
  #-----------------------------------------------------------------------------
  weeklyClimate <- weeklyClimate %>% 
    filter (var %in% c ('prcp','rh','tas','vpd')) %>%
    mutate (value = ifelse (var == 'prcp', sum, mean)) %>%
    dplyr::select (year, week, var, date, value)
    
  # return climate summarise by week
  #-----------------------------------------------------------------------------
  return (weeklyClimate)
}

# create function to summarise monthly climatologies
#-------------------------------------------------------------------------------
getMonthlySummaries <- function (clm) {
  
  # derive monthly climate variables in long format
  #-----------------------------------------------------------------------------
  # year
  # month
  #  rh   = NA, # monthly mean relative humidity                (%)
  #  prcp = NA, # monthly total precipitation                   (mm w-1)
  #  tas  = NA, # monthly mean air temperature at the surface   (deg C)
  #  tmax = NA, # monthly maximum temperature of daily maximums (deg C)
  #  tmin = NA, # monthly minimum temperature daily minimums    (deg C)
  #  vpd  = NA, # monthly mean vapour pressure deficit          (kPa)
  #  wind = NA  # monthly mean wind speed                       (m s-1)
  #)
  
  # summarise individual climate variables
  #-----------------------------------------------------------------------------
  monthlyClimate <- clm %>%  
    mutate (year  = lubridate::year  (date), 
            month = lubridate::month (date)) %>% 
    group_by (year, month, var) %>% 
    summarise (date = mean (date,  na.rm = TRUE),
               mean = mean (value, na.rm = TRUE),
               sum  = sum  (value, na.rm = TRUE), 
               .groups = 'drop')
  
  # deselect all variables that are not useful
  #-----------------------------------------------------------------------------
  monthlyClimate <- monthlyClimate %>% 
    filter (var %in% c ('prcp','rh','tas','vpd')) %>%
    mutate (value = ifelse (var == 'prcp', sum, mean)) %>%
    dplyr::select (year, month, var, value)
  
  # return climate summarise by month
  #-----------------------------------------------------------------------------
  return (monthlyClimate)
}

#===============================================================================