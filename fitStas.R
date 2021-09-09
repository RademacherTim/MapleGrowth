#===============================================================================
# Script to fit a simple linear Bayesian model to ring width from mean summer 
# temperatures (addSummerTemperature.R) using the rwYSTI data created in 
# wrangleGrowthData.R
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
source ('addSummerTemperature.R') # to load ring width data and summer temperatures from all sites
library ('rethinking') # to fit bayesian models qith quap
library ('extraDistr') # to use half-normal distribution

# standardise tas
#-------------------------------------------------------------------------------
data <- d %>% mutate (tasJan0s = (tasJan0 - mean (tasJan0, na.rm = TRUE)) / 
                                  sd (tasJan0, na.rm = TRUE),
                      preJan0s = preJan0 / max (preJan0, na.rm = TRUE))

# this is a hack to deal with NA, which needs to be changed eventually
data <- data [complete.cases (data)]

# fit model using quap from rethinking package
#-------------------------------------------------------------------------------
mRW <- quap (
  alist (
      rw ~ dhnorm (sigma),
      sigma <- a0 + 
               b1  * tasJan0 + b2  * preJan0 + b3  * tasFeb0 + b4  * preFeb0,# +
               #b5  * tasMar0 + b6  * preMar0 + b7  * tasApr0 + b8  * preApr0 + 
               #b9  * tasMay0 + b10 * preMay0 + b11 * tasJun0 + b12 * preJun0 + 
               #b13 * tasJul0 + b14 * preJul0 + b15 * tasAug0 + b16 * preAug0 + 
               #b17 * tasSep0 + b18 * preSep0, 
      c (tasJan0, tasFeb0) ~ dnorm (0, 3),
      c (preJan0, preFeb0) ~ dnorm (0, 3),
      a0 ~ dnorm (1, 0.5),
      c (b1, b2, b3, b4#, 
         #b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18
         ) ~ dnorm (0, 5)
  ), data = data, start = list (a = 0.8, b1 = 0, b2 = 0, b3 = 0, b4 = 0)
)
precis (mRW)
posterior <- extract.samples (mSTas)
#===============================================================================