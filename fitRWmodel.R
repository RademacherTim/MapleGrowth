#===============================================================================
# Script to fit a simple linear Bayesian model to ring width from mean summer 
# temperatures (addSummerTemperature.R) using the rwYSTI data created in 
# wrangleGrowthData.R
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!exists ('d')) source ('addClimate.R') # to load ring width data and summer temperatures from all sites
if (!existsFunction ('ulam')) library ('rethinking') # to fit bayesian models qith ulam
if (!existsFunction ('stan')) library ('rstan') # to run stan through R
if (!existsFunction ('tibble')) library ('tidyverse') # to use tidyverse

# standardise climate variables
#-------------------------------------------------------------------------------
data <- d %>% mutate (tasJan0s = (tasJan0 - mean (tasJan0, na.rm = TRUE)) / 
                                  sd (tasJan0, na.rm = TRUE),
                      tasFeb0s = (tasFeb0 - mean (tasFeb0, na.rm = TRUE)) / 
                                  sd (tasFeb0, na.rm = TRUE),
                      preFeb0s = preFeb0 / max (preFeb0, na.rm = TRUE))

# let's start with sugar maple only 
# N.B.: Integrate species as a variable in the model eventually
#-------------------------------------------------------------------------------
data <- data %>% dplyr::filter (species == 'ACSA') %>%
  dplyr::filter (year == 1992, site == 5)

# reduce data to list of relevant variables only
#-------------------------------------------------------------------------------
#data <- data %>% dplyr::select (rw, year, site, treeID, incrementCoreID) %>%#, tasJan0s, tasFeb0s)
data <- data %>% dplyr::select (rw, treeID, incrementCoreID) %>%
    rename (rwYSTI = rw)

data <- list (rwYSTI = data$rwYSTI / max (data$rwYSTI), 
              inc = data$incrementCoreID, 
              t = data$treeID, 
              #y = data$year - 1947,
              #s = data$site,
              N = dim (data) [1])

# fit Bayesian model using stan
#-------------------------------------------------------------------------------
mRW0 <- ulam (
  alist (
    rwYSTI ~ normal (rwYST, sigmaI),
    rwYST <- aT [t],
    aT [t] ~ normal (0, theta),
    theta ~ normal (0.5, 0.3),
    sigmaI <- aI [inc],
    aI [inc] ~ dexp (1)
  ), data = data, cmdstan = TRUE, chains = 1, 
)

precis (mRW0)
mRW0 <- stan (
  file = 'mRW0.stan', # Stan program
  data = data, # named list of data with arc in radians from -pi to pi
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)
precis (mRW0)
postRW0 <- extract.samples (mRW0)
postRW0$rw <- rnorm (1e4, mean = 0, sd = postRW0$theta)
hist (postRW0$rw, xlim = c (0, 10))
#===============================================================================