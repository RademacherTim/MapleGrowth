#===============================================================================
# Script to fit HGAM to ring width data
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!exists ('d')) source ('addClimate.R') # to load ring width data and summer temperatures from all sites
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!existsFunction ('gam')) library ('mgcv') # to fit HGAM

# let's start with sugar maple only, because red maple does not have enough 
# samples to represent entire distribution
# N.B.: Integrate species as a variable in the model eventually
#-------------------------------------------------------------------------------
data <- d %>% dplyr::filter (species == 'ACSA') %>% select (-species)

# average by tree for now
#-------------------------------------------------------------------------------
data <- data %>% group_by (year, site, lat, lon, treeID, tasJan0, preJan0, 
                           tasFeb0, preFeb0, tasMar0, preMar0, tasApr0, preApr0, 
                           tasMay0, preMay0, tasJun0, preJun0, tasJul0, preJul0, 
                           tasAug0, preAug0, tasSep0, preSep0) %>% 
  summarise (rwYST = mean (rwEYSTI), .groups = 'drop') %>% relocate (rwYST, .before = 1)

# start with model with a model of site- and tree-specific growth factors 
# (random effects) and a temporal autocorrelation of the residuals with a lag of 
# one year
#-------------------------------------------------------------------------------
time0 <- Sys.time ()
modNull <- gam (log (rwYST + 1) ~ s (site, bs = 're') + 
                  s (treeID, bs = 're'),
                correlation = corAR1 (form = ~ year | treeID),
                data = data, 
                method = 'REML', 
                family = 'gaussian')
time1 <- Sys.time ()
time1 - time0 
summary (modNull)
check.gam (modNull)

# model that includes latitude as a proxy for photoperiod on top of site- and 
# tree-specific growth factors (random effects) and a temporal autocorrelation 
# of the residuals with a lag of one year.
#-------------------------------------------------------------------------------
modNull <- gam (log (rwEYSTI) ~ s (site, bs = 're') + 
                  s (treeID, bs = 're') + 
                  s (lat),
                correlation = corAR1 (form = ~ year | treeID),
                data = data, 
                method = 'REML', 
                family = 'gaussian')
check.gam (modNull)


#===============================================================================