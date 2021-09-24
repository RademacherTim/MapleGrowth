#===============================================================================
# Script to fit a simple linear Bayesian model to ring width from mean summer 
# temperatures (addSummerTemperature.R) using the rwYSTI data created in 
# wrangleGrowthData.R
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!exists ('d')) source ('addClimate.R') # to load ring width data and summer temperatures from all sites
if (!existsFunction ('ulam')) library ('rethinking') # to fit bayesian models with ulam
if (!existsFunction ('stan')) library ('rstan') # to run stan through R
if (!existsFunction ('tibble')) library ('tidyverse') # to use tidyverse
if (!existsFunction ('brm')) library ('brms') # to actually fit the model

# Options to optimise stan on this machine
#-------------------------------------------------------------------------------
rstan_options (auto_write = TRUE)
options (mc.cores = parallel::detectCores ())

# plot ring width data to show tits raw distribution
#-------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
hist (d$rwYS [d$species == 'ACSA'], 
      xlab = expression (paste (rw['y,s'],' (mm)', sep = '')),
      main = '', col = '#f3bd4833', xlim = c (0, 10.5), breaks = seq (0, 10, by = 0.5))
hist (d$rwYS [d$species == 'ACRU'], 
      xlab = expression (paste (rw['y,s'],' (mm))', sep = '')),
      main = '', col = '#901c3b33', add = TRUE, breaks = seq (0, 10, by = 0.5))

# transform ring width
#-------------------------------------------------------------------------------
muRWprior <- mean (log (d$rwYS))
sigmaRWprior <- sd (log (d$rwYS))
data <- d %>% mutate (rwYS = log (rwYS + 1))

# plot ring width data to show that transformation made is close to normal
#-------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
hist (data$rwYS [data$species == 'ACSA'], 
      xlab = expression (paste ('log (',rw['y,s'],' + 1)', sep = '')),
      main = '', col = '#f3bd4833', xlim = c (0, 2.5))
hist (data$rwYS [data$species == 'ACRU'], 
      xlab = expression (paste ('log (',rw['y,s'],' + 1)', sep = '')),
      main = '', col = '#901c3b33', add = TRUE)

# let's start with sugar maple only 
# N.B.: Integrate species as a variable in the model eventually
#-------------------------------------------------------------------------------
data <- data %>% dplyr::filter (species == 'ACSA')

# Some exploration to chose reasonable priors
#-------------------------------------------------------------------------------


# reduce data to list of relevant variables only
#-------------------------------------------------------------------------------
dat <- list (rw = data$rwYS, 
             #inc = data$incrementCoreID, 
             #t = data$treeID, 
             y = as.integer (data$year - 1947),
             s = as.integer (as.factor (data$site)), # re-indexing because original indices included exclusive red maple sites causing gaps
             #tasJan0 = data$tasJan0,
             #tasFeb0 = data$tasFeb0,
             #tasMar0 = data$tasMar0,
             #tasApr0 = data$tasApr0,
             tasMay0 = data$tasMay0,
             tasJun0 = data$tasJun0,
             tasJul0 = data$tasJul0,
             tasAug0 = data$tasAug0,
             #tasSep0 = data$tasSep0,
             #preJan0 = data$preJan0,
             #preFeb0 = data$preFeb0,
             #preMar0 = data$preMar0,
             #preApr0 = data$preApr0,
             preMay0 = data$preMay0,
             preJun0 = data$preJun0,
             preJul0 = data$preJul0,
             preAug0 = data$preAug0,
             #preSep0 = data$preSep0,
             N = dim (data) [1],                                        # number of ring width samples 
             Y = as.integer (max(data$year - 1947)),                    # number of years 
             S = as.integer (max (as.integer (as.factor (data$site))))) # number of sites

# fit Bayesian model using stan
#-------------------------------------------------------------------------------
mRW0 <- stan (
  file = 'mRW0.stan', # Stan program
  data = dat, # named list of data with arc in radians from -pi to pi
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)
precis (mRW0, depth = 2)

#===============================================================================