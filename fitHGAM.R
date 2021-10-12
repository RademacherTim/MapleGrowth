#===============================================================================
# Script to fit HGAM to ring width data
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!exists ('d')) source ('addClimate.R') # to load ring width data and summer temperatures from all sites
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!existsFunction ('gam')) library ('mgcv') # to fit HGAM

# transform ring width, so that I can use the log for transformation
#-------------------------------------------------------------------------------
data <- d %>% mutate (rwEYSTI = rwEYSTI + 1.2)

# let's start with sugar maple only, because red maple does not have enough 
# samples to represent entire distribution
# N.B.: Integrate species as a variable in the model eventually
#-------------------------------------------------------------------------------
data <- data %>% dplyr::filter (species == 'ACSA') %>% select (-species)

# start with model with a model of site specific growth as a 
#-------------------------------------------------------------------------------
modGlobal <- gam (log (rwEYSTI + 1) ~ s (site, bs = 're'),
                  correlation = corAR1 (form = ~ year | site),
                  data = rwEYSTI, 
                  method = 'REML', 
                  family = 'gaussian')
#===============================================================================