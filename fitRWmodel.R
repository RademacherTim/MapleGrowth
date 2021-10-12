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
if (!existsFunction ('data_grid')) library ('modelr') # to plot posterior 
if (!existsFunction ('add_predictive_draws')) library ('tidybayes') # to plot posterior 

# Options to optimise stan on this machine
#-------------------------------------------------------------------------------
rstan_options (auto_write = TRUE)
options (mc.cores = parallel::detectCores ())

# plot ring width data to show tits raw distribution
#-------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
hist (d$rwEYSTI [d$species == 'ACSA'], 
      xlab = expression (paste (rw['e,y,s,t,i'],' (mm)', sep = '')),
      main = '', col = '#f3bd4833', xlim = c (0, 17.5), breaks = seq (0, 18, by = 0.5))
hist (d$rwEYSTI [d$species == 'ACRU'], 
      col = '#901c3b33', add = TRUE, breaks = seq (0, 18, by = 0.5))

# transform ring width, so that I can use the log for transformation
#-------------------------------------------------------------------------------
data <- d %>% mutate (rwEYSTI = rwEYSTI + 1.2)

# plot ring width data to show that transformation made is close to normal
#-------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
hist (log (data$rwEYSTI [data$species == 'ACSA']), 
      xlab = expression (paste ('log (',rw['y,s'],' + 1.5)', sep = '')),
      main = '', col = '#f3bd4833', xlim = c (0, 3.5), breaks = seq (0, 3.5, by = 0.1))
hist (log (data$rwEYSTI [data$species == 'ACRU']), 
      col = '#901c3b33', add = TRUE, breaks = seq (0, 3.5, by = 0.1))

# let's start with sugar maple only, because red maple does not have enough 
# samples to represent entire distribution
# N.B.: Integrate species as a variable in the model eventually
#-------------------------------------------------------------------------------
data <- data %>% dplyr::filter (species == 'ACSA') %>% select (-species)

# select only sites with coordinates, thus climate data for now
#-------------------------------------------------------------------------------
data <- data %>% dplyr::filter (site != 130)

# average by tree for now
#-------------------------------------------------------------------------------
data <- data %>% group_by (year, site, treeID, tasJan0, preJan0, tasFeb0, preFeb0, 
                           tasMar0, preMar0, tasApr0, preApr0, tasMay0, preMay0, 
                           tasJun0, preJun0, tasJul0, preJul0, tasAug0, preAug0, 
                           tasSep0, preSep0) %>% 
  summarise (rwYST = mean (rwEYSTI), .groups = 'drop') %>% relocate (rwYST, .before = 1)

# average climate data to annual mean temperature and annual total precipitation
#-------------------------------------------------------------------------------
data <- data %>% 
  group_by (rwYST, year, site, treeID) %>%
  summarise (tas0 = mean (tasJan0, tasFeb0, tasMar0, tasApr0, tasMay0, tasJun0, 
                          tasJul0, tasAug0, tasSep0),
             pre0 = sum (preJan0, preFeb0, preMar0, preApr0, preMay0, preJun0, 
                         preJul0, preAug0, preSep0),
             .groups = 'drop')

# fit a NULL model using brms
#-------------------------------------------------------------------------------
formula0 <- bf (rwYST ~ (1 | site) + (1 | treeID) +
                  tas0 + pre0 +
                  ar (time = year, gr = treeID, p = 1)) # not sure the group for the auto-correlative term is correct
time0 <- Sys.time ()
mod0 <- brm (family = 'gaussian',
             formula = formula0,
             data = data)
time1 <- Sys.time ()
time1 - time0
summary (mod0)
conditional_effects (mod0)
postMod0 <- as_draws (mod0)
posterior::summarize_draws(postMod0)[2, ]

# fit a model including between-tree (within-site varibility) using brms
#-------------------------------------------------------------------------------
formula1 <- bf (rwYST ~ (1 | site) +
                  tas0 + pre0 +
                  ar (time = year, gr = treeID, p = 1),
                sigma ~ treeID)
time0 <- Sys.time ()
mod1 <- brm (family = 'gaussian',
             formula = formula1,
             data = data)
time1 <- Sys.time ()
time1 - time0
summary (mod1)
conditional_effects (mod1)
postMod0 <- posterior_samples (mod1)

muTas0 <- apply ()
plot ()

# simulate some data to make sure the model works as intended
#-------------------------------------------------------------------------------

# start with global temperature, precipitation, and temporal autocorrelation 
# coefficients as well as background growth rate (global intercept; a0)
#-------------------------------------------------------------------------------
bT <- 0.25
bP <- 0.15
bAR <- 0.3
a0 <- 0.3
aS <- runif (52, min = 0.2, max = 0.4)

# generate data.frame for simulated data
#-------------------------------------------------------------------------------
simData <- data %>% mutate (rwYS = NA,
                            year = as.numeric (year) + 1947,
                            tas0 = (tas0 - mean (tas0)) / sd (tas0),
                            pre0 = pre0 / max (pre0))

# simulate global ring width
#-------------------------------------------------------------------------------
for (i in 1:dim (simData) [1]) {
  mu <- a0 + aS [simData$site [i]] + 
    bT * simData$tas0 [i] + bP * simData$pre0 [i] + 
    ifelse (simData$year [i] == 1948, rnorm (1, 0.5, 0.1),
            bAR * simData$rwYS [which (simData$site == simData$site [i] &
                                       simData$year == simData$year [i] - 1)]) + 
    rnorm (n = 1, mean = 0, sd = 0.2)
  if (mu < 0) mu <- 0
  simData$rwYS [i] <- mu #rnorm (n = 1, mean = mu, sd = rexp (1, 0.1))
  print (c (i, simData$site [i], simData$year [i], mu))
}
hist (simData$rwYS)

# fit a model using brms
#-------------------------------------------------------------------------------
modSim0 <- brm (formula = rwYS ~ (1 | site) + 
               tas0 + pre0 +
               ar (time = year, gr = site, p = 1),
             data = simData,
             family = 'gaussian')
summary (modSim0)


#===============================================================================
