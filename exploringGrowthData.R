#===============================================================================
# Script to plot ring width data to explore the data
# - plot ring width of year Y versus ring width of the previous year Y-1
# - plot ring width versus size (cumulative ring width as proxy for size)
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!exists ('rwEYSTI')) source ('wrangleGrowthData.R') # to load ring width data from all sites

# wrangle data (NB.: This takes pretty long)
#-------------------------------------------------------------------------------
tempData$prevRW <- NA; tempData$cumRW <- NA
for (i in 1:dim (tempData) [1]) {
  # find indices for previous years growth and cumulative growth
  iPrev <- which (#tempData$site == tempData$site [i] & # same site
                  tempData$treeID == tempData$treeID [i] & # same tree
                  tempData$incrementCoreID == tempData$incrementCoreID [i] & # same core
                  tempData$year == as.character (as.integer (tempData$year [i])-1)) # previous year
  iCum <- which (#tempData$site == tempData$site [i] & # same site
                 tempData$treeID == tempData$treeID [i] & # same tree
                 tempData$incrementCoreID == tempData$incrementCoreID [i] & # same core
                 tempData$year < as.character (as.integer (tempData$year [i]))) # all previous years
  # NB.: We exclude the current year growth, so the cumulative RW is approximate 
  #      dbh at the beginning of the year.
  
  # find previous year's growth
  if (length (iPrev) == 1) tempData$prevRW [i] <- tempData$rwEYSTI [iPrev] 
  if (length (iCum)  >= 1) {
    tempData$cumRW  [i] <- sum (tempData$rwEYSTI [iCum], na.rm = TRUE) 
  } else {
    tempData$cumRW  [i] <- 0
  }
  if (i %% 20000 == 0) print (i)
}

# plot ring width versus previous year ring width
#-------------------------------------------------------------------------------
max (tempData$rwEYSTI [tempData$species == 'ACSA'], na.rm = TRUE)
max (tempData$rwEYSTI [tempData$species == 'ACRU'], na.rm = TRUE)
summary (lm (prevRW [tempData$species == 'ACSA'] ~ 
               rwEYSTI [tempData$species == 'ACSA'], data = tempData))
summary (lm (prevRW [tempData$species == 'ACRU'] ~ 
               rwEYSTI [tempData$species == 'ACRU'], data = tempData))
png (file = '../fig/RWvsRW-1.png', width = 980, height = 490) 
  par (mar = c (5, 5, 1, 1), mfrow = c (1, 2))
  plot (x = tempData$prevRW [tempData$species == 'ACSA'], 
        y = tempData$rwEYSTI [tempData$species == 'ACSA'], 
        pch = 19, col = '#f3bd4811',
        xlab = expression (paste (rw['y-1,s,t,i'],' (mm)', sep = '')), 
        ylab = expression (paste (rw['y,s,t,i'],' (mm)', sep = '')),
        xlim = c (0, 18), ylim = c (0, 18), axes = FALSE)
  axis (side = 1, at = seq (0, 15, by = 5))
  axis (side = 2, at = seq (0, 15, by = 5), las = 1)
  abline (a = 0 , b = 1, col = '#666666', lty = 3)
  abline (lm (prevRW [tempData$species == 'ACSA'] ~ 
                rwEYSTI [tempData$species == 'ACSA'], data = tempData), 
          col = '#f38d48', lwd = 2)
  plot (x = tempData$prevRW [tempData$species == 'ACRU'], 
        y = tempData$rwEYSTI [tempData$species == 'ACRU'], 
        pch = 19, col = '#901c3b11',
        xlab = expression (paste (rw['y-1,s,t,i'],' (mm)', sep = '')), 
        ylab = expression (paste (rw['y,s,t,i'],' (mm)', sep = '')),
        xlim = c (0, 18), ylim = c (0, 18), axes = FALSE)
  axis (side = 1, at = seq (0, 15, by = 5))
  axis (side = 2, at = seq (0, 15, by = 5), las = 1)
  abline (a = 0 , b = 1, col = '#666666', lty = 3)
  abline (lm (prevRW [tempData$species == 'ACRU'] ~ 
                rwEYSTI [tempData$species == 'ACRU'], data = tempData), 
          col = '#EB99A9', lwd = 2)
dev.off ()

# plot ring width versus over size (approximated dbh = cumulative ring width)
#-------------------------------------------------------------------------------
max (tempData$cumRW [tempData$species == 'ACSA'], na.rm = TRUE) / 10
max (tempData$cumRW [tempData$species == 'ACRU'], na.rm = TRUE) / 10
png (file = '../fig/RWvsRWcum.png', width = 980, height = 490)
  par (mar = c (5, 5, 1, 1), mfrow = c (1, 2))
  plot (x = tempData$cumRW [tempData$species == 'ACSA'] / 10, 
        y = tempData$rwEYSTI [tempData$species == 'ACSA'], 
        pch = 19, col = '#f3bd4811',
        xlab = expression (paste (rw['cum,s,t,i'],' (cm)', sep = '')), 
        ylab = expression (paste (rw['y,s,t,i'],' (mm)', sep = '')),
        xlim = c (0, 42), ylim = c (0, 18), axes = FALSE)
  axis (side = 1, at = seq (0, 40, by = 10))
  axis (side = 2, at = seq (0, 15, by = 5), las = 1)
  par (mar = c (5, 5, 1, 1))
  plot (x = tempData$cumRW [tempData$species == 'ACRU'] / 10, 
        y = tempData$rwEYSTI [tempData$species == 'ACRU'], 
        pch = 19, col = '#901c3b11',
        xlab = expression (paste (rw['cum,s,t,i'],' (cm)', sep = '')), 
        ylab = expression (paste (rw['y,s,t,i'],' (mm)', sep = '')),
        xlim = c (0, 42), ylim = c (0, 18), axes = FALSE)
  axis (side = 1, at = seq (0, 40, by = 10))
  axis (side = 2, at = seq (0, 15, by = 5), las = 1)
dev.off ()

