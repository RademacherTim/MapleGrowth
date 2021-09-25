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
  tasTmp  <- ncvar_get (nc_tas,  "tas",  start = c (iLon, iLat, 1), 
                     count = c (1, 1, daysInYear)) # K
  prcpTmp <- ncvar_get (nc_prcp, "prcp", start = c (iLon, iLat, 1), 
                     count = c (1, 1, daysInYear)) # kg m-2 s-1
  
  # get and replace fill values
  #---------------------------------------------------------------------------
  fillValue <- ncatt_get (nc_tas, "tas", "_FillValue")
  tasTmp  <- tasTmp  %>% replace (tasTmp  == fillValue$value, values = NA)
  prcpTmp <- prcpTmp %>% replace (prcpTmp == fillValue$value, values = NA)
  
  # close annual climate data files
  #---------------------------------------------------------------------------
  nc_close (nc_tas)
  nc_close (nc_prcp)
  
  # convert units
  #---------------------------------------------------------------------------
  tasTmp  <- tasTmp  - 273.15                               # K          -> deg C
  prcpTmp <- prcpTmp * 86400                                # kg m-2 s-1 -> mm d-1
  
  # concatenate the climate variables
  #-----------------------------------------------------------------------------
  if (y == startYear) {
    tas  <- tasTmp
    prcp <- prcpTmp
    doy  <- 1:daysInYear
    year <- rep (y, daysInYear)
  } else {
    tas  <- c (tas, tasTmp)
    prcp <- c (prcp, prcpTmp)
    doy  <- c (doy, 1:daysInYear)
    year <- c (year, rep (y, daysInYear))
  }
  
} # close loop over years

# put all data back into one tibble
#-------------------------------------------------------------------------------
clmDaily <- tibble (tas = tas, prcp = prcp, doy = doy, year = year) %>% 
  mutate (date = as_date (doy, origin = paste0 (year,"-01-01")),
          month = month (date))
clmMonthly <- clmDaily %>% group_by (year, month) %>%
  summarise (tas = mean (tas),
             prcp = sum (prcp), .groups = 'drop') %>%
  mutate (date = as_date (paste (year,month,'15', sep = '-')))
clmAnnually <- clmDaily %>% group_by (year) %>%
  summarise (muTas = mean (tas),
             sdTas = sd (tas, na.rm = TRUE),
             muPrcp = sum (prcp),
             sdPrcp = sd (prcp, na.rm = TRUE), .groups = 'drop') %>%
  mutate (date = as_date (paste (year,'07-01', sep = '-')))

# plot the temperature and precipitation for entire record
#-------------------------------------------------------------------------------
par (mar = c (3, 5, 1, 5))
plot (x = clmDaily$date, y = clmDaily$tas, typ = 'l', xlab = '', lwd = 0.1, 
      ylim = c (-35, 70), 
      ylab = expression (paste ('Temperature (',degree,'C)', sep = '')), 
      axes = FALSE)
axis (side = 1, at = c (as_date ('1950-01-01'), as_date ('1960-01-01'), 
                        as_date ('1970-01-01'), as_date ('1980-01-01'),
                        as_date ('1990-01-01'), as_date ('2000-01-01')), 
      labels = seq (1950, 2000, by = 10))
axis (side = 2, las = 1, at = seq (-30, 30, by = 10))
lines (x = clmMonthly$date, y = clmMonthly$tas, col = '#66666666')
polygon (x = c (clmAnnually$date, rev (clmAnnually$date)),
         y = c (clmAnnually$muTas + clmAnnually$sdTas, 
                rev (clmAnnually$muTas - clmAnnually$sdTas)),
         lty = 0, col = '#901C3B33')
lines (x = clmAnnually$date, y = clmAnnually$muTas, col = '#901C3B', lwd = 2)
par (new = TRUE)
plot (x = clmAnnually$date, y = clmAnnually$muPrcp, xlab = '', pch = 19,
      ylab = '', ylim = c (0, 1500),
      col = '#003E74aa', axes = FALSE)
lines (x = clmAnnually$date, y = clmAnnually$muPrcp, col = '#003E74aa', lwd = 3)
axis (side = 4, at = seq (0, 1500, 500), las = 1)
mtext (side = 4, line = 3, text = 'Precipitation (mm)')

# plot the temperature and precipitation for 1968 to 1972
#-------------------------------------------------------------------------------
par (mar = c (3, 5, 1, 5))
plot (x = clmDaily$date, y = clmDaily$tas, typ = 'l', xlab = '', lwd = 0.1, 
      xlim = c (as_date ('1969-01-01'), as_date ('1974-12-31')), ylim = c (-50, 30),
      ylab = expression (paste ('Temperature (',degree,'C)', sep = '')), axes = FALSE)
axis (side = 1, at = c (as_date ('1969-01-01'), 
                        as_date ('1970-01-01'), as_date ('1971-01-01'),
                        as_date ('1972-01-01'), as_date ('1973-01-01'),
                        as_date ('1974-01-01'), as_date ('1975-01-01')), 
      labels = seq (1969, 1975, 1))
axis (side = 2, las = 1, at = seq (-30, 30, by = 10))
lines (x = clmMonthly$date, y = clmMonthly$tas, col = '#901C3B', lwd = 2)
points (x = clmMonthly$date, y = clmMonthly$tas, col = '#901C3B', pch = 21, 
        bg = 'white', lwd = 2)
par (new = TRUE)
plot (x = clmMonthly$date, y = clmMonthly$prcp, xlab = '', pch = 19,
      ylab = '', xlim = c (as_date ('1969-01-01'), as_date ('1974-12-31')), 
      ylim = c (0, 520),
      col = '#003E74', axes = FALSE)
lines (x = clmMonthly$date, y = clmMonthly$prcp, col = '#003E74aa', lwd = 3)
axis (side = 4, at = seq (0, 500, 100), las = 1)
mtext (side = 4, line = 3, text = 'Precipitation (mm)')


#===============================================================================
