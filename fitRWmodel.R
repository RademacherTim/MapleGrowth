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

# standardise climate variables
#-------------------------------------------------------------------------------
data <- d %>% mutate (tasJan0s = (tasJan0 - mean (tasJan0, na.rm = TRUE)) / 
                                  sd (tasJan0, na.rm = TRUE),
                      tasFeb0s = (tasFeb0 - mean (tasFeb0, na.rm = TRUE)) / 
                                  sd (tasFeb0, na.rm = TRUE),
                      preFeb0s = preFeb0 / max (preFeb0, na.rm = TRUE))

# select one species only
#-------------------------------------------------------------------------------
data <- data %>% filter (species == 'ACSA')

# fit model using quap from rethinking package
#-------------------------------------------------------------------------------
mRW <- quap (
  alist (
    rwYSTI ~ dnorm (rwYST, sigmaI),
    rwYST  ~ dnorm (rwYS,  sigmaT), # TR - Need to look up again how to add measurement error
    rwYS   ~ dhnorm (theta),
    theta <- a0 + 
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
       ) ~ dnorm (0, 5),
    c (sigmaI, sigmaT) ~ dexp (1)
  ), data = data, #start = list (a = 0.8, b1 = 0, b2 = 0, b3 = 0, b4 = 0)
)
precis (mRW)
posterior <- extract.samples (mSTas)
#===============================================================================