#===============================================================================
# Script to fit HGAM to ring width data
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!existsFunction ('gam')) library ('mgcv') # to fit HGAM

# start with model with just global parameters of summer temperature 
#-------------------------------------------------------------------------------

#===============================================================================