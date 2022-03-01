#===============================================================================
# Script to add  climate data to the ring width for each growth measurement.
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!existsFunction ('as_date')) library ('lubridate') # to use as_date function
if (!existsFunction ('nc_open')) library ('ncdf4')  # to manipulate netcdf files (climate)
if (!exists ('rwEYSTI')) source ('wrangleGrowthData.R') # to load ring width data from all sites
if (!existsFunction ('thornthwaite')) library ('SPEI') # to calculate the potential evapotranspiration
if (!existsFunction ('pdsi')) library ('scPDSI') # to calculate the scPDSI

# initialise spatial resolution for climate data
#-------------------------------------------------------------------------------
res <- 0.25

# directory name with climate data
#-------------------------------------------------------------------------------
dirString <- '/Volumes/Untitled/TREE LAB 001/data/climate/princeton/'

# initial possible climates for climate data 
#-------------------------------------------------------------------------------
dates <- seq (from = as_date ('1948-01-01'), 
              to   = as_date ('2016-12-31'), by = 1)

# add elevation of climate data to siteMetaData to compare the elevation of the 
# actual stands versus the elevation to which weather is calibrated
#-------------------------------------------------------------------------------
#siteMetaData <- siteMetaData %>% mutate (eleClim = NA)

# add tas column for mean summer temperature of the growing site
#-------------------------------------------------------------------------------
d <- rwEYSTI %>% mutate (tasJan0 = NA, preJan0 = NA, # current January   climate
                         tasFeb0 = NA, preFeb0 = NA, # current February  climate
                         tasMar0 = NA, preMar0 = NA, # current March     climate
                         tasApr0 = NA, preApr0 = NA, # current April     climate
                         tasMay0 = NA, preMay0 = NA, # current May       climate
                         tasJun0 = NA, preJun0 = NA, # current June      climate
                         tasJul0 = NA, preJul0 = NA, # current July      climate
                         tasAug0 = NA, preAug0 = NA, # current August    climate
                         tasSep0 = NA, preSep0 = NA) # current September climate
 
# also create files with site climate data
#-------------------------------------------------------------------------------
siteTempData <- siteMetaData %>% mutate ('Jan-1948' = NA, 'Feb-1948' = NA, 'Mar-1948' = NA, 'Apr-1948' = NA, 'May-1948' = NA, 'Jun-1948' = NA, 'Jul-1948' = NA, 'Aug-1948' = NA, 'Sep-1948' = NA, 'Oct-1948' = NA, 'Nov-1948' = NA, 'Dec-1948' = NA,
                                         'Jan-1949' = NA, 'Feb-1949' = NA, 'Mar-1949' = NA, 'Apr-1949' = NA, 'May-1949' = NA, 'Jun-1949' = NA, 'Jul-1949' = NA, 'Aug-1949' = NA, 'Sep-1949' = NA, 'Oct-1949' = NA, 'Nov-1949' = NA, 'Dec-1949' = NA,
                                         'Jan-1950' = NA, 'Feb-1950' = NA, 'Mar-1950' = NA, 'Apr-1950' = NA, 'May-1950' = NA, 'Jun-1950' = NA, 'Jul-1950' = NA, 'Aug-1950' = NA, 'Sep-1950' = NA, 'Oct-1950' = NA, 'Nov-1950' = NA, 'Dec-1950' = NA,
                                         'Jan-1951' = NA, 'Feb-1951' = NA, 'Mar-1951' = NA, 'Apr-1951' = NA, 'May-1951' = NA, 'Jun-1951' = NA, 'Jul-1951' = NA, 'Aug-1951' = NA, 'Sep-1951' = NA, 'Oct-1951' = NA, 'Nov-1951' = NA, 'Dec-1951' = NA,
                                         'Jan-1952' = NA, 'Feb-1952' = NA, 'Mar-1952' = NA, 'Apr-1952' = NA, 'May-1952' = NA, 'Jun-1952' = NA, 'Jul-1952' = NA, 'Aug-1952' = NA, 'Sep-1952' = NA, 'Oct-1952' = NA, 'Nov-1952' = NA, 'Dec-1952' = NA,
                                         'Jan-1953' = NA, 'Feb-1953' = NA, 'Mar-1953' = NA, 'Apr-1953' = NA, 'May-1953' = NA, 'Jun-1953' = NA, 'Jul-1953' = NA, 'Aug-1953' = NA, 'Sep-1953' = NA, 'Oct-1953' = NA, 'Nov-1953' = NA, 'Dec-1953' = NA,
                                         'Jan-1954' = NA, 'Feb-1954' = NA, 'Mar-1954' = NA, 'Apr-1954' = NA, 'May-1954' = NA, 'Jun-1954' = NA, 'Jul-1954' = NA, 'Aug-1954' = NA, 'Sep-1954' = NA, 'Oct-1954' = NA, 'Nov-1954' = NA, 'Dec-1954' = NA,
                                         'Jan-1955' = NA, 'Feb-1955' = NA, 'Mar-1955' = NA, 'Apr-1955' = NA, 'May-1955' = NA, 'Jun-1955' = NA, 'Jul-1955' = NA, 'Aug-1955' = NA, 'Sep-1955' = NA, 'Oct-1955' = NA, 'Nov-1955' = NA, 'Dec-1955' = NA,
                                         'Jan-1956' = NA, 'Feb-1956' = NA, 'Mar-1956' = NA, 'Apr-1956' = NA, 'May-1956' = NA, 'Jun-1956' = NA, 'Jul-1956' = NA, 'Aug-1956' = NA, 'Sep-1956' = NA, 'Oct-1956' = NA, 'Nov-1956' = NA, 'Dec-1956' = NA,
                                         'Jan-1957' = NA, 'Feb-1957' = NA, 'Mar-1957' = NA, 'Apr-1957' = NA, 'May-1957' = NA, 'Jun-1957' = NA, 'Jul-1957' = NA, 'Aug-1957' = NA, 'Sep-1957' = NA, 'Oct-1957' = NA, 'Nov-1957' = NA, 'Dec-1957' = NA,
                                         'Jan-1958' = NA, 'Feb-1958' = NA, 'Mar-1958' = NA, 'Apr-1958' = NA, 'May-1958' = NA, 'Jun-1958' = NA, 'Jul-1958' = NA, 'Aug-1958' = NA, 'Sep-1958' = NA, 'Oct-1958' = NA, 'Nov-1958' = NA, 'Dec-1958' = NA,
                                         'Jan-1959' = NA, 'Feb-1959' = NA, 'Mar-1959' = NA, 'Apr-1959' = NA, 'May-1959' = NA, 'Jun-1959' = NA, 'Jul-1959' = NA, 'Aug-1959' = NA, 'Sep-1959' = NA, 'Oct-1959' = NA, 'Nov-1959' = NA, 'Dec-1959' = NA,
                                         'Jan-1960' = NA, 'Feb-1960' = NA, 'Mar-1960' = NA, 'Apr-1960' = NA, 'May-1960' = NA, 'Jun-1960' = NA, 'Jul-1960' = NA, 'Aug-1960' = NA, 'Sep-1960' = NA, 'Oct-1960' = NA, 'Nov-1960' = NA, 'Dec-1960' = NA,
                                         'Jan-1961' = NA, 'Feb-1961' = NA, 'Mar-1961' = NA, 'Apr-1961' = NA, 'May-1961' = NA, 'Jun-1961' = NA, 'Jul-1961' = NA, 'Aug-1961' = NA, 'Sep-1961' = NA, 'Oct-1961' = NA, 'Nov-1961' = NA, 'Dec-1961' = NA,
                                         'Jan-1962' = NA, 'Feb-1962' = NA, 'Mar-1962' = NA, 'Apr-1962' = NA, 'May-1962' = NA, 'Jun-1962' = NA, 'Jul-1962' = NA, 'Aug-1962' = NA, 'Sep-1962' = NA, 'Oct-1962' = NA, 'Nov-1962' = NA, 'Dec-1962' = NA,
                                         'Jan-1963' = NA, 'Feb-1963' = NA, 'Mar-1963' = NA, 'Apr-1963' = NA, 'May-1963' = NA, 'Jun-1963' = NA, 'Jul-1963' = NA, 'Aug-1963' = NA, 'Sep-1963' = NA, 'Oct-1963' = NA, 'Nov-1963' = NA, 'Dec-1963' = NA,
                                         'Jan-1964' = NA, 'Feb-1964' = NA, 'Mar-1964' = NA, 'Apr-1964' = NA, 'May-1964' = NA, 'Jun-1964' = NA, 'Jul-1964' = NA, 'Aug-1964' = NA, 'Sep-1964' = NA, 'Oct-1964' = NA, 'Nov-1964' = NA, 'Dec-1964' = NA,
                                         'Jan-1965' = NA, 'Feb-1965' = NA, 'Mar-1965' = NA, 'Apr-1965' = NA, 'May-1965' = NA, 'Jun-1965' = NA, 'Jul-1965' = NA, 'Aug-1965' = NA, 'Sep-1965' = NA, 'Oct-1965' = NA, 'Nov-1965' = NA, 'Dec-1965' = NA,
                                         'Jan-1966' = NA, 'Feb-1966' = NA, 'Mar-1966' = NA, 'Apr-1966' = NA, 'May-1966' = NA, 'Jun-1966' = NA, 'Jul-1966' = NA, 'Aug-1966' = NA, 'Sep-1966' = NA, 'Oct-1966' = NA, 'Nov-1966' = NA, 'Dec-1966' = NA,
                                         'Jan-1967' = NA, 'Feb-1967' = NA, 'Mar-1967' = NA, 'Apr-1967' = NA, 'May-1967' = NA, 'Jun-1967' = NA, 'Jul-1967' = NA, 'Aug-1967' = NA, 'Sep-1967' = NA, 'Oct-1967' = NA, 'Nov-1967' = NA, 'Dec-1967' = NA,
                                         'Jan-1968' = NA, 'Feb-1968' = NA, 'Mar-1968' = NA, 'Apr-1968' = NA, 'May-1968' = NA, 'Jun-1968' = NA, 'Jul-1968' = NA, 'Aug-1968' = NA, 'Sep-1968' = NA, 'Oct-1968' = NA, 'Nov-1968' = NA, 'Dec-1968' = NA,
                                         'Jan-1969' = NA, 'Feb-1969' = NA, 'Mar-1969' = NA, 'Apr-1969' = NA, 'May-1969' = NA, 'Jun-1969' = NA, 'Jul-1969' = NA, 'Aug-1969' = NA, 'Sep-1969' = NA, 'Oct-1969' = NA, 'Nov-1969' = NA, 'Dec-1969' = NA,
                                         'Jan-1970' = NA, 'Feb-1970' = NA, 'Mar-1970' = NA, 'Apr-1970' = NA, 'May-1970' = NA, 'Jun-1970' = NA, 'Jul-1970' = NA, 'Aug-1970' = NA, 'Sep-1970' = NA, 'Oct-1970' = NA, 'Nov-1970' = NA, 'Dec-1970' = NA,
                                         'Jan-1971' = NA, 'Feb-1971' = NA, 'Mar-1971' = NA, 'Apr-1971' = NA, 'May-1971' = NA, 'Jun-1971' = NA, 'Jul-1971' = NA, 'Aug-1971' = NA, 'Sep-1971' = NA, 'Oct-1971' = NA, 'Nov-1971' = NA, 'Dec-1971' = NA,
                                         'Jan-1972' = NA, 'Feb-1972' = NA, 'Mar-1972' = NA, 'Apr-1972' = NA, 'May-1972' = NA, 'Jun-1972' = NA, 'Jul-1972' = NA, 'Aug-1972' = NA, 'Sep-1972' = NA, 'Oct-1972' = NA, 'Nov-1972' = NA, 'Dec-1972' = NA,
                                         'Jan-1973' = NA, 'Feb-1973' = NA, 'Mar-1973' = NA, 'Apr-1973' = NA, 'May-1973' = NA, 'Jun-1973' = NA, 'Jul-1973' = NA, 'Aug-1973' = NA, 'Sep-1973' = NA, 'Oct-1973' = NA, 'Nov-1973' = NA, 'Dec-1973' = NA,
                                         'Jan-1974' = NA, 'Feb-1974' = NA, 'Mar-1974' = NA, 'Apr-1974' = NA, 'May-1974' = NA, 'Jun-1974' = NA, 'Jul-1974' = NA, 'Aug-1974' = NA, 'Sep-1974' = NA, 'Oct-1974' = NA, 'Nov-1974' = NA, 'Dec-1974' = NA,
                                         'Jan-1975' = NA, 'Feb-1975' = NA, 'Mar-1975' = NA, 'Apr-1975' = NA, 'May-1975' = NA, 'Jun-1975' = NA, 'Jul-1975' = NA, 'Aug-1975' = NA, 'Sep-1975' = NA, 'Oct-1975' = NA, 'Nov-1975' = NA, 'Dec-1975' = NA,
                                         'Jan-1976' = NA, 'Feb-1976' = NA, 'Mar-1976' = NA, 'Apr-1976' = NA, 'May-1976' = NA, 'Jun-1976' = NA, 'Jul-1976' = NA, 'Aug-1976' = NA, 'Sep-1976' = NA, 'Oct-1976' = NA, 'Nov-1976' = NA, 'Dec-1976' = NA,
                                         'Jan-1977' = NA, 'Feb-1977' = NA, 'Mar-1977' = NA, 'Apr-1977' = NA, 'May-1977' = NA, 'Jun-1977' = NA, 'Jul-1977' = NA, 'Aug-1977' = NA, 'Sep-1977' = NA, 'Oct-1977' = NA, 'Nov-1977' = NA, 'Dec-1977' = NA,
                                         'Jan-1978' = NA, 'Feb-1978' = NA, 'Mar-1978' = NA, 'Apr-1978' = NA, 'May-1978' = NA, 'Jun-1978' = NA, 'Jul-1978' = NA, 'Aug-1978' = NA, 'Sep-1978' = NA, 'Oct-1978' = NA, 'Nov-1978' = NA, 'Dec-1978' = NA,
                                         'Jan-1979' = NA, 'Feb-1979' = NA, 'Mar-1979' = NA, 'Apr-1979' = NA, 'May-1979' = NA, 'Jun-1979' = NA, 'Jul-1979' = NA, 'Aug-1979' = NA, 'Sep-1979' = NA, 'Oct-1979' = NA, 'Nov-1979' = NA, 'Dec-1979' = NA,
                                         'Jan-1980' = NA, 'Feb-1980' = NA, 'Mar-1980' = NA, 'Apr-1980' = NA, 'May-1980' = NA, 'Jun-1980' = NA, 'Jul-1980' = NA, 'Aug-1980' = NA, 'Sep-1980' = NA, 'Oct-1980' = NA, 'Nov-1980' = NA, 'Dec-1980' = NA,
                                         'Jan-1981' = NA, 'Feb-1981' = NA, 'Mar-1981' = NA, 'Apr-1981' = NA, 'May-1981' = NA, 'Jun-1981' = NA, 'Jul-1981' = NA, 'Aug-1981' = NA, 'Sep-1981' = NA, 'Oct-1981' = NA, 'Nov-1981' = NA, 'Dec-1981' = NA,
                                         'Jan-1982' = NA, 'Feb-1982' = NA, 'Mar-1982' = NA, 'Apr-1982' = NA, 'May-1982' = NA, 'Jun-1982' = NA, 'Jul-1982' = NA, 'Aug-1982' = NA, 'Sep-1982' = NA, 'Oct-1982' = NA, 'Nov-1982' = NA, 'Dec-1982' = NA,
                                         'Jan-1983' = NA, 'Feb-1983' = NA, 'Mar-1983' = NA, 'Apr-1983' = NA, 'May-1983' = NA, 'Jun-1983' = NA, 'Jul-1983' = NA, 'Aug-1983' = NA, 'Sep-1983' = NA, 'Oct-1983' = NA, 'Nov-1983' = NA, 'Dec-1983' = NA,
                                         'Jan-1984' = NA, 'Feb-1984' = NA, 'Mar-1984' = NA, 'Apr-1984' = NA, 'May-1984' = NA, 'Jun-1984' = NA, 'Jul-1984' = NA, 'Aug-1984' = NA, 'Sep-1984' = NA, 'Oct-1984' = NA, 'Nov-1984' = NA, 'Dec-1984' = NA,
                                         'Jan-1985' = NA, 'Feb-1985' = NA, 'Mar-1985' = NA, 'Apr-1985' = NA, 'May-1985' = NA, 'Jun-1985' = NA, 'Jul-1985' = NA, 'Aug-1985' = NA, 'Sep-1985' = NA, 'Oct-1985' = NA, 'Nov-1985' = NA, 'Dec-1985' = NA,
                                         'Jan-1986' = NA, 'Feb-1986' = NA, 'Mar-1986' = NA, 'Apr-1986' = NA, 'May-1986' = NA, 'Jun-1986' = NA, 'Jul-1986' = NA, 'Aug-1986' = NA, 'Sep-1986' = NA, 'Oct-1986' = NA, 'Nov-1986' = NA, 'Dec-1986' = NA,
                                         'Jan-1987' = NA, 'Feb-1987' = NA, 'Mar-1987' = NA, 'Apr-1987' = NA, 'May-1987' = NA, 'Jun-1987' = NA, 'Jul-1987' = NA, 'Aug-1987' = NA, 'Sep-1987' = NA, 'Oct-1987' = NA, 'Nov-1987' = NA, 'Dec-1987' = NA,
                                         'Jan-1988' = NA, 'Feb-1988' = NA, 'Mar-1988' = NA, 'Apr-1988' = NA, 'May-1988' = NA, 'Jun-1988' = NA, 'Jul-1988' = NA, 'Aug-1988' = NA, 'Sep-1988' = NA, 'Oct-1988' = NA, 'Nov-1988' = NA, 'Dec-1988' = NA,
                                         'Jan-1989' = NA, 'Feb-1989' = NA, 'Mar-1989' = NA, 'Apr-1989' = NA, 'May-1989' = NA, 'Jun-1989' = NA, 'Jul-1989' = NA, 'Aug-1989' = NA, 'Sep-1989' = NA, 'Oct-1989' = NA, 'Nov-1989' = NA, 'Dec-1989' = NA,
                                         'Jan-1990' = NA, 'Feb-1990' = NA, 'Mar-1990' = NA, 'Apr-1990' = NA, 'May-1990' = NA, 'Jun-1990' = NA, 'Jul-1990' = NA, 'Aug-1990' = NA, 'Sep-1990' = NA, 'Oct-1990' = NA, 'Nov-1990' = NA, 'Dec-1990' = NA,
                                         'Jan-1991' = NA, 'Feb-1991' = NA, 'Mar-1991' = NA, 'Apr-1991' = NA, 'May-1991' = NA, 'Jun-1991' = NA, 'Jul-1991' = NA, 'Aug-1991' = NA, 'Sep-1991' = NA, 'Oct-1991' = NA, 'Nov-1991' = NA, 'Dec-1991' = NA,
                                         'Jan-1992' = NA, 'Feb-1992' = NA, 'Mar-1992' = NA, 'Apr-1992' = NA, 'May-1992' = NA, 'Jun-1992' = NA, 'Jul-1992' = NA, 'Aug-1992' = NA, 'Sep-1992' = NA, 'Oct-1992' = NA, 'Nov-1992' = NA, 'Dec-1992' = NA,
                                         'Jan-1993' = NA, 'Feb-1993' = NA, 'Mar-1993' = NA, 'Apr-1993' = NA, 'May-1993' = NA, 'Jun-1993' = NA, 'Jul-1993' = NA, 'Aug-1993' = NA, 'Sep-1993' = NA, 'Oct-1993' = NA, 'Nov-1993' = NA, 'Dec-1993' = NA,
                                         'Jan-1994' = NA, 'Feb-1994' = NA, 'Mar-1994' = NA, 'Apr-1994' = NA, 'May-1994' = NA, 'Jun-1994' = NA, 'Jul-1994' = NA, 'Aug-1994' = NA, 'Sep-1994' = NA, 'Oct-1994' = NA, 'Nov-1994' = NA, 'Dec-1994' = NA,
                                         'Jan-1995' = NA, 'Feb-1995' = NA, 'Mar-1995' = NA, 'Apr-1995' = NA, 'May-1995' = NA, 'Jun-1995' = NA, 'Jul-1995' = NA, 'Aug-1995' = NA, 'Sep-1995' = NA, 'Oct-1995' = NA, 'Nov-1995' = NA, 'Dec-1995' = NA,
                                         'Jan-1996' = NA, 'Feb-1996' = NA, 'Mar-1996' = NA, 'Apr-1996' = NA, 'May-1996' = NA, 'Jun-1996' = NA, 'Jul-1996' = NA, 'Aug-1996' = NA, 'Sep-1996' = NA, 'Oct-1996' = NA, 'Nov-1996' = NA, 'Dec-1996' = NA,
                                         'Jan-1997' = NA, 'Feb-1997' = NA, 'Mar-1997' = NA, 'Apr-1997' = NA, 'May-1997' = NA, 'Jun-1997' = NA, 'Jul-1997' = NA, 'Aug-1997' = NA, 'Sep-1997' = NA, 'Oct-1997' = NA, 'Nov-1997' = NA, 'Dec-1997' = NA,
                                         'Jan-1998' = NA, 'Feb-1998' = NA, 'Mar-1998' = NA, 'Apr-1998' = NA, 'May-1998' = NA, 'Jun-1998' = NA, 'Jul-1998' = NA, 'Aug-1998' = NA, 'Sep-1998' = NA, 'Oct-1998' = NA, 'Nov-1998' = NA, 'Dec-1998' = NA,
                                         'Jan-1999' = NA, 'Feb-1999' = NA, 'Mar-1999' = NA, 'Apr-1999' = NA, 'May-1999' = NA, 'Jun-1999' = NA, 'Jul-1999' = NA, 'Aug-1999' = NA, 'Sep-1999' = NA, 'Oct-1999' = NA, 'Nov-1999' = NA, 'Dec-1999' = NA,
                                         'Jan-2000' = NA, 'Feb-2000' = NA, 'Mar-2000' = NA, 'Apr-2000' = NA, 'May-2000' = NA, 'Jun-2000' = NA, 'Jul-2000' = NA, 'Aug-2000' = NA, 'Sep-2000' = NA, 'Oct-2000' = NA, 'Nov-2000' = NA, 'Dec-2000' = NA,
                                         'Jan-2001' = NA, 'Feb-2001' = NA, 'Mar-2001' = NA, 'Apr-2001' = NA, 'May-2001' = NA, 'Jun-2001' = NA, 'Jul-2001' = NA, 'Aug-2001' = NA, 'Sep-2001' = NA, 'Oct-2001' = NA, 'Nov-2001' = NA, 'Dec-2001' = NA,
                                         'Jan-2002' = NA, 'Feb-2002' = NA, 'Mar-2002' = NA, 'Apr-2002' = NA, 'May-2002' = NA, 'Jun-2002' = NA, 'Jul-2002' = NA, 'Aug-2002' = NA, 'Sep-2002' = NA, 'Oct-2002' = NA, 'Nov-2002' = NA, 'Dec-2002' = NA,
                                         'Jan-2003' = NA, 'Feb-2003' = NA, 'Mar-2003' = NA, 'Apr-2003' = NA, 'May-2003' = NA, 'Jun-2003' = NA, 'Jul-2003' = NA, 'Aug-2003' = NA, 'Sep-2003' = NA, 'Oct-2003' = NA, 'Nov-2003' = NA, 'Dec-2003' = NA,
                                         'Jan-2004' = NA, 'Feb-2004' = NA, 'Mar-2004' = NA, 'Apr-2004' = NA, 'May-2004' = NA, 'Jun-2004' = NA, 'Jul-2004' = NA, 'Aug-2004' = NA, 'Sep-2004' = NA, 'Oct-2004' = NA, 'Nov-2004' = NA, 'Dec-2004' = NA,
                                         'Jan-2005' = NA, 'Feb-2005' = NA, 'Mar-2005' = NA, 'Apr-2005' = NA, 'May-2005' = NA, 'Jun-2005' = NA, 'Jul-2005' = NA, 'Aug-2005' = NA, 'Sep-2005' = NA, 'Oct-2005' = NA, 'Nov-2005' = NA, 'Dec-2005' = NA,
                                         'Jan-2006' = NA, 'Feb-2006' = NA, 'Mar-2006' = NA, 'Apr-2006' = NA, 'May-2006' = NA, 'Jun-2006' = NA, 'Jul-2006' = NA, 'Aug-2006' = NA, 'Sep-2006' = NA, 'Oct-2006' = NA, 'Nov-2006' = NA, 'Dec-2006' = NA,
                                         'Jan-2007' = NA, 'Feb-2007' = NA, 'Mar-2007' = NA, 'Apr-2007' = NA, 'May-2007' = NA, 'Jun-2007' = NA, 'Jul-2007' = NA, 'Aug-2007' = NA, 'Sep-2007' = NA, 'Oct-2007' = NA, 'Nov-2007' = NA, 'Dec-2007' = NA,
                                         'Jan-2008' = NA, 'Feb-2008' = NA, 'Mar-2008' = NA, 'Apr-2008' = NA, 'May-2008' = NA, 'Jun-2008' = NA, 'Jul-2008' = NA, 'Aug-2008' = NA, 'Sep-2008' = NA, 'Oct-2008' = NA, 'Nov-2008' = NA, 'Dec-2008' = NA,
                                         'Jan-2009' = NA, 'Feb-2009' = NA, 'Mar-2009' = NA, 'Apr-2009' = NA, 'May-2009' = NA, 'Jun-2009' = NA, 'Jul-2009' = NA, 'Aug-2009' = NA, 'Sep-2009' = NA, 'Oct-2009' = NA, 'Nov-2009' = NA, 'Dec-2009' = NA,
                                         'Jan-2010' = NA, 'Feb-2010' = NA, 'Mar-2010' = NA, 'Apr-2010' = NA, 'May-2010' = NA, 'Jun-2010' = NA, 'Jul-2010' = NA, 'Aug-2010' = NA, 'Sep-2010' = NA, 'Oct-2010' = NA, 'Nov-2010' = NA, 'Dec-2010' = NA,
                                         'Jan-2011' = NA, 'Feb-2011' = NA, 'Mar-2011' = NA, 'Apr-2011' = NA, 'May-2011' = NA, 'Jun-2011' = NA, 'Jul-2011' = NA, 'Aug-2011' = NA, 'Sep-2011' = NA, 'Oct-2011' = NA, 'Nov-2011' = NA, 'Dec-2011' = NA,
                                         'Jan-2012' = NA, 'Feb-2012' = NA, 'Mar-2012' = NA, 'Apr-2012' = NA, 'May-2012' = NA, 'Jun-2012' = NA, 'Jul-2012' = NA, 'Aug-2012' = NA, 'Sep-2012' = NA, 'Oct-2012' = NA, 'Nov-2012' = NA, 'Dec-2012' = NA,
                                         'Jan-2013' = NA, 'Feb-2013' = NA, 'Mar-2013' = NA, 'Apr-2013' = NA, 'May-2013' = NA, 'Jun-2013' = NA, 'Jul-2013' = NA, 'Aug-2013' = NA, 'Sep-2013' = NA, 'Oct-2013' = NA, 'Nov-2013' = NA, 'Dec-2013' = NA,
                                         'Jan-2014' = NA, 'Feb-2014' = NA, 'Mar-2014' = NA, 'Apr-2014' = NA, 'May-2014' = NA, 'Jun-2014' = NA, 'Jul-2014' = NA, 'Aug-2014' = NA, 'Sep-2014' = NA, 'Oct-2014' = NA, 'Nov-2014' = NA, 'Dec-2014' = NA,
                                         'Jan-2015' = NA, 'Feb-2015' = NA, 'Mar-2015' = NA, 'Apr-2015' = NA, 'May-2015' = NA, 'Jun-2015' = NA, 'Jul-2015' = NA, 'Aug-2015' = NA, 'Sep-2015' = NA, 'Oct-2015' = NA, 'Nov-2015' = NA, 'Dec-2015' = NA,
                                         'Jan-2016' = NA, 'Feb-2016' = NA, 'Mar-2016' = NA, 'Apr-2016' = NA, 'May-2016' = NA, 'Jun-2016' = NA, 'Jul-2016' = NA, 'Aug-2016' = NA, 'Sep-2016' = NA, 'Oct-2016' = NA, 'Nov-2016' = NA, 'Dec-2016' = NA)
#-------------------------------------------------------------------------------
sitePrecData <- siteMetaData %>% mutate ('Jan-1948' = NA, 'Feb-1948' = NA, 'Mar-1948' = NA, 'Apr-1948' = NA, 'May-1948' = NA, 'Jun-1948' = NA, 'Jul-1948' = NA, 'Aug-1948' = NA, 'Sep-1948' = NA, 'Oct-1948' = NA, 'Nov-1948' = NA, 'Dec-1948' = NA,
                                         'Jan-1949' = NA, 'Feb-1949' = NA, 'Mar-1949' = NA, 'Apr-1949' = NA, 'May-1949' = NA, 'Jun-1949' = NA, 'Jul-1949' = NA, 'Aug-1949' = NA, 'Sep-1949' = NA, 'Oct-1949' = NA, 'Nov-1949' = NA, 'Dec-1949' = NA,
                                         'Jan-1950' = NA, 'Feb-1950' = NA, 'Mar-1950' = NA, 'Apr-1950' = NA, 'May-1950' = NA, 'Jun-1950' = NA, 'Jul-1950' = NA, 'Aug-1950' = NA, 'Sep-1950' = NA, 'Oct-1950' = NA, 'Nov-1950' = NA, 'Dec-1950' = NA,
                                         'Jan-1951' = NA, 'Feb-1951' = NA, 'Mar-1951' = NA, 'Apr-1951' = NA, 'May-1951' = NA, 'Jun-1951' = NA, 'Jul-1951' = NA, 'Aug-1951' = NA, 'Sep-1951' = NA, 'Oct-1951' = NA, 'Nov-1951' = NA, 'Dec-1951' = NA,
                                         'Jan-1952' = NA, 'Feb-1952' = NA, 'Mar-1952' = NA, 'Apr-1952' = NA, 'May-1952' = NA, 'Jun-1952' = NA, 'Jul-1952' = NA, 'Aug-1952' = NA, 'Sep-1952' = NA, 'Oct-1952' = NA, 'Nov-1952' = NA, 'Dec-1952' = NA,
                                         'Jan-1953' = NA, 'Feb-1953' = NA, 'Mar-1953' = NA, 'Apr-1953' = NA, 'May-1953' = NA, 'Jun-1953' = NA, 'Jul-1953' = NA, 'Aug-1953' = NA, 'Sep-1953' = NA, 'Oct-1953' = NA, 'Nov-1953' = NA, 'Dec-1953' = NA,
                                         'Jan-1954' = NA, 'Feb-1954' = NA, 'Mar-1954' = NA, 'Apr-1954' = NA, 'May-1954' = NA, 'Jun-1954' = NA, 'Jul-1954' = NA, 'Aug-1954' = NA, 'Sep-1954' = NA, 'Oct-1954' = NA, 'Nov-1954' = NA, 'Dec-1954' = NA,
                                         'Jan-1955' = NA, 'Feb-1955' = NA, 'Mar-1955' = NA, 'Apr-1955' = NA, 'May-1955' = NA, 'Jun-1955' = NA, 'Jul-1955' = NA, 'Aug-1955' = NA, 'Sep-1955' = NA, 'Oct-1955' = NA, 'Nov-1955' = NA, 'Dec-1955' = NA,
                                         'Jan-1956' = NA, 'Feb-1956' = NA, 'Mar-1956' = NA, 'Apr-1956' = NA, 'May-1956' = NA, 'Jun-1956' = NA, 'Jul-1956' = NA, 'Aug-1956' = NA, 'Sep-1956' = NA, 'Oct-1956' = NA, 'Nov-1956' = NA, 'Dec-1956' = NA,
                                         'Jan-1957' = NA, 'Feb-1957' = NA, 'Mar-1957' = NA, 'Apr-1957' = NA, 'May-1957' = NA, 'Jun-1957' = NA, 'Jul-1957' = NA, 'Aug-1957' = NA, 'Sep-1957' = NA, 'Oct-1957' = NA, 'Nov-1957' = NA, 'Dec-1957' = NA,
                                         'Jan-1958' = NA, 'Feb-1958' = NA, 'Mar-1958' = NA, 'Apr-1958' = NA, 'May-1958' = NA, 'Jun-1958' = NA, 'Jul-1958' = NA, 'Aug-1958' = NA, 'Sep-1958' = NA, 'Oct-1958' = NA, 'Nov-1958' = NA, 'Dec-1958' = NA,
                                         'Jan-1959' = NA, 'Feb-1959' = NA, 'Mar-1959' = NA, 'Apr-1959' = NA, 'May-1959' = NA, 'Jun-1959' = NA, 'Jul-1959' = NA, 'Aug-1959' = NA, 'Sep-1959' = NA, 'Oct-1959' = NA, 'Nov-1959' = NA, 'Dec-1959' = NA,
                                         'Jan-1960' = NA, 'Feb-1960' = NA, 'Mar-1960' = NA, 'Apr-1960' = NA, 'May-1960' = NA, 'Jun-1960' = NA, 'Jul-1960' = NA, 'Aug-1960' = NA, 'Sep-1960' = NA, 'Oct-1960' = NA, 'Nov-1960' = NA, 'Dec-1960' = NA,
                                         'Jan-1961' = NA, 'Feb-1961' = NA, 'Mar-1961' = NA, 'Apr-1961' = NA, 'May-1961' = NA, 'Jun-1961' = NA, 'Jul-1961' = NA, 'Aug-1961' = NA, 'Sep-1961' = NA, 'Oct-1961' = NA, 'Nov-1961' = NA, 'Dec-1961' = NA,
                                         'Jan-1962' = NA, 'Feb-1962' = NA, 'Mar-1962' = NA, 'Apr-1962' = NA, 'May-1962' = NA, 'Jun-1962' = NA, 'Jul-1962' = NA, 'Aug-1962' = NA, 'Sep-1962' = NA, 'Oct-1962' = NA, 'Nov-1962' = NA, 'Dec-1962' = NA,
                                         'Jan-1963' = NA, 'Feb-1963' = NA, 'Mar-1963' = NA, 'Apr-1963' = NA, 'May-1963' = NA, 'Jun-1963' = NA, 'Jul-1963' = NA, 'Aug-1963' = NA, 'Sep-1963' = NA, 'Oct-1963' = NA, 'Nov-1963' = NA, 'Dec-1963' = NA,
                                         'Jan-1964' = NA, 'Feb-1964' = NA, 'Mar-1964' = NA, 'Apr-1964' = NA, 'May-1964' = NA, 'Jun-1964' = NA, 'Jul-1964' = NA, 'Aug-1964' = NA, 'Sep-1964' = NA, 'Oct-1964' = NA, 'Nov-1964' = NA, 'Dec-1964' = NA,
                                         'Jan-1965' = NA, 'Feb-1965' = NA, 'Mar-1965' = NA, 'Apr-1965' = NA, 'May-1965' = NA, 'Jun-1965' = NA, 'Jul-1965' = NA, 'Aug-1965' = NA, 'Sep-1965' = NA, 'Oct-1965' = NA, 'Nov-1965' = NA, 'Dec-1965' = NA,
                                         'Jan-1966' = NA, 'Feb-1966' = NA, 'Mar-1966' = NA, 'Apr-1966' = NA, 'May-1966' = NA, 'Jun-1966' = NA, 'Jul-1966' = NA, 'Aug-1966' = NA, 'Sep-1966' = NA, 'Oct-1966' = NA, 'Nov-1966' = NA, 'Dec-1966' = NA,
                                         'Jan-1967' = NA, 'Feb-1967' = NA, 'Mar-1967' = NA, 'Apr-1967' = NA, 'May-1967' = NA, 'Jun-1967' = NA, 'Jul-1967' = NA, 'Aug-1967' = NA, 'Sep-1967' = NA, 'Oct-1967' = NA, 'Nov-1967' = NA, 'Dec-1967' = NA,
                                         'Jan-1968' = NA, 'Feb-1968' = NA, 'Mar-1968' = NA, 'Apr-1968' = NA, 'May-1968' = NA, 'Jun-1968' = NA, 'Jul-1968' = NA, 'Aug-1968' = NA, 'Sep-1968' = NA, 'Oct-1968' = NA, 'Nov-1968' = NA, 'Dec-1968' = NA,
                                         'Jan-1969' = NA, 'Feb-1969' = NA, 'Mar-1969' = NA, 'Apr-1969' = NA, 'May-1969' = NA, 'Jun-1969' = NA, 'Jul-1969' = NA, 'Aug-1969' = NA, 'Sep-1969' = NA, 'Oct-1969' = NA, 'Nov-1969' = NA, 'Dec-1969' = NA,
                                         'Jan-1970' = NA, 'Feb-1970' = NA, 'Mar-1970' = NA, 'Apr-1970' = NA, 'May-1970' = NA, 'Jun-1970' = NA, 'Jul-1970' = NA, 'Aug-1970' = NA, 'Sep-1970' = NA, 'Oct-1970' = NA, 'Nov-1970' = NA, 'Dec-1970' = NA,
                                         'Jan-1971' = NA, 'Feb-1971' = NA, 'Mar-1971' = NA, 'Apr-1971' = NA, 'May-1971' = NA, 'Jun-1971' = NA, 'Jul-1971' = NA, 'Aug-1971' = NA, 'Sep-1971' = NA, 'Oct-1971' = NA, 'Nov-1971' = NA, 'Dec-1971' = NA,
                                         'Jan-1972' = NA, 'Feb-1972' = NA, 'Mar-1972' = NA, 'Apr-1972' = NA, 'May-1972' = NA, 'Jun-1972' = NA, 'Jul-1972' = NA, 'Aug-1972' = NA, 'Sep-1972' = NA, 'Oct-1972' = NA, 'Nov-1972' = NA, 'Dec-1972' = NA,
                                         'Jan-1973' = NA, 'Feb-1973' = NA, 'Mar-1973' = NA, 'Apr-1973' = NA, 'May-1973' = NA, 'Jun-1973' = NA, 'Jul-1973' = NA, 'Aug-1973' = NA, 'Sep-1973' = NA, 'Oct-1973' = NA, 'Nov-1973' = NA, 'Dec-1973' = NA,
                                         'Jan-1974' = NA, 'Feb-1974' = NA, 'Mar-1974' = NA, 'Apr-1974' = NA, 'May-1974' = NA, 'Jun-1974' = NA, 'Jul-1974' = NA, 'Aug-1974' = NA, 'Sep-1974' = NA, 'Oct-1974' = NA, 'Nov-1974' = NA, 'Dec-1974' = NA,
                                         'Jan-1975' = NA, 'Feb-1975' = NA, 'Mar-1975' = NA, 'Apr-1975' = NA, 'May-1975' = NA, 'Jun-1975' = NA, 'Jul-1975' = NA, 'Aug-1975' = NA, 'Sep-1975' = NA, 'Oct-1975' = NA, 'Nov-1975' = NA, 'Dec-1975' = NA,
                                         'Jan-1976' = NA, 'Feb-1976' = NA, 'Mar-1976' = NA, 'Apr-1976' = NA, 'May-1976' = NA, 'Jun-1976' = NA, 'Jul-1976' = NA, 'Aug-1976' = NA, 'Sep-1976' = NA, 'Oct-1976' = NA, 'Nov-1976' = NA, 'Dec-1976' = NA,
                                         'Jan-1977' = NA, 'Feb-1977' = NA, 'Mar-1977' = NA, 'Apr-1977' = NA, 'May-1977' = NA, 'Jun-1977' = NA, 'Jul-1977' = NA, 'Aug-1977' = NA, 'Sep-1977' = NA, 'Oct-1977' = NA, 'Nov-1977' = NA, 'Dec-1977' = NA,
                                         'Jan-1978' = NA, 'Feb-1978' = NA, 'Mar-1978' = NA, 'Apr-1978' = NA, 'May-1978' = NA, 'Jun-1978' = NA, 'Jul-1978' = NA, 'Aug-1978' = NA, 'Sep-1978' = NA, 'Oct-1978' = NA, 'Nov-1978' = NA, 'Dec-1978' = NA,
                                         'Jan-1979' = NA, 'Feb-1979' = NA, 'Mar-1979' = NA, 'Apr-1979' = NA, 'May-1979' = NA, 'Jun-1979' = NA, 'Jul-1979' = NA, 'Aug-1979' = NA, 'Sep-1979' = NA, 'Oct-1979' = NA, 'Nov-1979' = NA, 'Dec-1979' = NA,
                                         'Jan-1980' = NA, 'Feb-1980' = NA, 'Mar-1980' = NA, 'Apr-1980' = NA, 'May-1980' = NA, 'Jun-1980' = NA, 'Jul-1980' = NA, 'Aug-1980' = NA, 'Sep-1980' = NA, 'Oct-1980' = NA, 'Nov-1980' = NA, 'Dec-1980' = NA,
                                         'Jan-1981' = NA, 'Feb-1981' = NA, 'Mar-1981' = NA, 'Apr-1981' = NA, 'May-1981' = NA, 'Jun-1981' = NA, 'Jul-1981' = NA, 'Aug-1981' = NA, 'Sep-1981' = NA, 'Oct-1981' = NA, 'Nov-1981' = NA, 'Dec-1981' = NA,
                                         'Jan-1982' = NA, 'Feb-1982' = NA, 'Mar-1982' = NA, 'Apr-1982' = NA, 'May-1982' = NA, 'Jun-1982' = NA, 'Jul-1982' = NA, 'Aug-1982' = NA, 'Sep-1982' = NA, 'Oct-1982' = NA, 'Nov-1982' = NA, 'Dec-1982' = NA,
                                         'Jan-1983' = NA, 'Feb-1983' = NA, 'Mar-1983' = NA, 'Apr-1983' = NA, 'May-1983' = NA, 'Jun-1983' = NA, 'Jul-1983' = NA, 'Aug-1983' = NA, 'Sep-1983' = NA, 'Oct-1983' = NA, 'Nov-1983' = NA, 'Dec-1983' = NA,
                                         'Jan-1984' = NA, 'Feb-1984' = NA, 'Mar-1984' = NA, 'Apr-1984' = NA, 'May-1984' = NA, 'Jun-1984' = NA, 'Jul-1984' = NA, 'Aug-1984' = NA, 'Sep-1984' = NA, 'Oct-1984' = NA, 'Nov-1984' = NA, 'Dec-1984' = NA,
                                         'Jan-1985' = NA, 'Feb-1985' = NA, 'Mar-1985' = NA, 'Apr-1985' = NA, 'May-1985' = NA, 'Jun-1985' = NA, 'Jul-1985' = NA, 'Aug-1985' = NA, 'Sep-1985' = NA, 'Oct-1985' = NA, 'Nov-1985' = NA, 'Dec-1985' = NA,
                                         'Jan-1986' = NA, 'Feb-1986' = NA, 'Mar-1986' = NA, 'Apr-1986' = NA, 'May-1986' = NA, 'Jun-1986' = NA, 'Jul-1986' = NA, 'Aug-1986' = NA, 'Sep-1986' = NA, 'Oct-1986' = NA, 'Nov-1986' = NA, 'Dec-1986' = NA,
                                         'Jan-1987' = NA, 'Feb-1987' = NA, 'Mar-1987' = NA, 'Apr-1987' = NA, 'May-1987' = NA, 'Jun-1987' = NA, 'Jul-1987' = NA, 'Aug-1987' = NA, 'Sep-1987' = NA, 'Oct-1987' = NA, 'Nov-1987' = NA, 'Dec-1987' = NA,
                                         'Jan-1988' = NA, 'Feb-1988' = NA, 'Mar-1988' = NA, 'Apr-1988' = NA, 'May-1988' = NA, 'Jun-1988' = NA, 'Jul-1988' = NA, 'Aug-1988' = NA, 'Sep-1988' = NA, 'Oct-1988' = NA, 'Nov-1988' = NA, 'Dec-1988' = NA,
                                         'Jan-1989' = NA, 'Feb-1989' = NA, 'Mar-1989' = NA, 'Apr-1989' = NA, 'May-1989' = NA, 'Jun-1989' = NA, 'Jul-1989' = NA, 'Aug-1989' = NA, 'Sep-1989' = NA, 'Oct-1989' = NA, 'Nov-1989' = NA, 'Dec-1989' = NA,
                                         'Jan-1990' = NA, 'Feb-1990' = NA, 'Mar-1990' = NA, 'Apr-1990' = NA, 'May-1990' = NA, 'Jun-1990' = NA, 'Jul-1990' = NA, 'Aug-1990' = NA, 'Sep-1990' = NA, 'Oct-1990' = NA, 'Nov-1990' = NA, 'Dec-1990' = NA,
                                         'Jan-1991' = NA, 'Feb-1991' = NA, 'Mar-1991' = NA, 'Apr-1991' = NA, 'May-1991' = NA, 'Jun-1991' = NA, 'Jul-1991' = NA, 'Aug-1991' = NA, 'Sep-1991' = NA, 'Oct-1991' = NA, 'Nov-1991' = NA, 'Dec-1991' = NA,
                                         'Jan-1992' = NA, 'Feb-1992' = NA, 'Mar-1992' = NA, 'Apr-1992' = NA, 'May-1992' = NA, 'Jun-1992' = NA, 'Jul-1992' = NA, 'Aug-1992' = NA, 'Sep-1992' = NA, 'Oct-1992' = NA, 'Nov-1992' = NA, 'Dec-1992' = NA,
                                         'Jan-1993' = NA, 'Feb-1993' = NA, 'Mar-1993' = NA, 'Apr-1993' = NA, 'May-1993' = NA, 'Jun-1993' = NA, 'Jul-1993' = NA, 'Aug-1993' = NA, 'Sep-1993' = NA, 'Oct-1993' = NA, 'Nov-1993' = NA, 'Dec-1993' = NA,
                                         'Jan-1994' = NA, 'Feb-1994' = NA, 'Mar-1994' = NA, 'Apr-1994' = NA, 'May-1994' = NA, 'Jun-1994' = NA, 'Jul-1994' = NA, 'Aug-1994' = NA, 'Sep-1994' = NA, 'Oct-1994' = NA, 'Nov-1994' = NA, 'Dec-1994' = NA,
                                         'Jan-1995' = NA, 'Feb-1995' = NA, 'Mar-1995' = NA, 'Apr-1995' = NA, 'May-1995' = NA, 'Jun-1995' = NA, 'Jul-1995' = NA, 'Aug-1995' = NA, 'Sep-1995' = NA, 'Oct-1995' = NA, 'Nov-1995' = NA, 'Dec-1995' = NA,
                                         'Jan-1996' = NA, 'Feb-1996' = NA, 'Mar-1996' = NA, 'Apr-1996' = NA, 'May-1996' = NA, 'Jun-1996' = NA, 'Jul-1996' = NA, 'Aug-1996' = NA, 'Sep-1996' = NA, 'Oct-1996' = NA, 'Nov-1996' = NA, 'Dec-1996' = NA,
                                         'Jan-1997' = NA, 'Feb-1997' = NA, 'Mar-1997' = NA, 'Apr-1997' = NA, 'May-1997' = NA, 'Jun-1997' = NA, 'Jul-1997' = NA, 'Aug-1997' = NA, 'Sep-1997' = NA, 'Oct-1997' = NA, 'Nov-1997' = NA, 'Dec-1997' = NA,
                                         'Jan-1998' = NA, 'Feb-1998' = NA, 'Mar-1998' = NA, 'Apr-1998' = NA, 'May-1998' = NA, 'Jun-1998' = NA, 'Jul-1998' = NA, 'Aug-1998' = NA, 'Sep-1998' = NA, 'Oct-1998' = NA, 'Nov-1998' = NA, 'Dec-1998' = NA,
                                         'Jan-1999' = NA, 'Feb-1999' = NA, 'Mar-1999' = NA, 'Apr-1999' = NA, 'May-1999' = NA, 'Jun-1999' = NA, 'Jul-1999' = NA, 'Aug-1999' = NA, 'Sep-1999' = NA, 'Oct-1999' = NA, 'Nov-1999' = NA, 'Dec-1999' = NA,
                                         'Jan-2000' = NA, 'Feb-2000' = NA, 'Mar-2000' = NA, 'Apr-2000' = NA, 'May-2000' = NA, 'Jun-2000' = NA, 'Jul-2000' = NA, 'Aug-2000' = NA, 'Sep-2000' = NA, 'Oct-2000' = NA, 'Nov-2000' = NA, 'Dec-2000' = NA,
                                         'Jan-2001' = NA, 'Feb-2001' = NA, 'Mar-2001' = NA, 'Apr-2001' = NA, 'May-2001' = NA, 'Jun-2001' = NA, 'Jul-2001' = NA, 'Aug-2001' = NA, 'Sep-2001' = NA, 'Oct-2001' = NA, 'Nov-2001' = NA, 'Dec-2001' = NA,
                                         'Jan-2002' = NA, 'Feb-2002' = NA, 'Mar-2002' = NA, 'Apr-2002' = NA, 'May-2002' = NA, 'Jun-2002' = NA, 'Jul-2002' = NA, 'Aug-2002' = NA, 'Sep-2002' = NA, 'Oct-2002' = NA, 'Nov-2002' = NA, 'Dec-2002' = NA,
                                         'Jan-2003' = NA, 'Feb-2003' = NA, 'Mar-2003' = NA, 'Apr-2003' = NA, 'May-2003' = NA, 'Jun-2003' = NA, 'Jul-2003' = NA, 'Aug-2003' = NA, 'Sep-2003' = NA, 'Oct-2003' = NA, 'Nov-2003' = NA, 'Dec-2003' = NA,
                                         'Jan-2004' = NA, 'Feb-2004' = NA, 'Mar-2004' = NA, 'Apr-2004' = NA, 'May-2004' = NA, 'Jun-2004' = NA, 'Jul-2004' = NA, 'Aug-2004' = NA, 'Sep-2004' = NA, 'Oct-2004' = NA, 'Nov-2004' = NA, 'Dec-2004' = NA,
                                         'Jan-2005' = NA, 'Feb-2005' = NA, 'Mar-2005' = NA, 'Apr-2005' = NA, 'May-2005' = NA, 'Jun-2005' = NA, 'Jul-2005' = NA, 'Aug-2005' = NA, 'Sep-2005' = NA, 'Oct-2005' = NA, 'Nov-2005' = NA, 'Dec-2005' = NA,
                                         'Jan-2006' = NA, 'Feb-2006' = NA, 'Mar-2006' = NA, 'Apr-2006' = NA, 'May-2006' = NA, 'Jun-2006' = NA, 'Jul-2006' = NA, 'Aug-2006' = NA, 'Sep-2006' = NA, 'Oct-2006' = NA, 'Nov-2006' = NA, 'Dec-2006' = NA,
                                         'Jan-2007' = NA, 'Feb-2007' = NA, 'Mar-2007' = NA, 'Apr-2007' = NA, 'May-2007' = NA, 'Jun-2007' = NA, 'Jul-2007' = NA, 'Aug-2007' = NA, 'Sep-2007' = NA, 'Oct-2007' = NA, 'Nov-2007' = NA, 'Dec-2007' = NA,
                                         'Jan-2008' = NA, 'Feb-2008' = NA, 'Mar-2008' = NA, 'Apr-2008' = NA, 'May-2008' = NA, 'Jun-2008' = NA, 'Jul-2008' = NA, 'Aug-2008' = NA, 'Sep-2008' = NA, 'Oct-2008' = NA, 'Nov-2008' = NA, 'Dec-2008' = NA,
                                         'Jan-2009' = NA, 'Feb-2009' = NA, 'Mar-2009' = NA, 'Apr-2009' = NA, 'May-2009' = NA, 'Jun-2009' = NA, 'Jul-2009' = NA, 'Aug-2009' = NA, 'Sep-2009' = NA, 'Oct-2009' = NA, 'Nov-2009' = NA, 'Dec-2009' = NA,
                                         'Jan-2010' = NA, 'Feb-2010' = NA, 'Mar-2010' = NA, 'Apr-2010' = NA, 'May-2010' = NA, 'Jun-2010' = NA, 'Jul-2010' = NA, 'Aug-2010' = NA, 'Sep-2010' = NA, 'Oct-2010' = NA, 'Nov-2010' = NA, 'Dec-2010' = NA,
                                         'Jan-2011' = NA, 'Feb-2011' = NA, 'Mar-2011' = NA, 'Apr-2011' = NA, 'May-2011' = NA, 'Jun-2011' = NA, 'Jul-2011' = NA, 'Aug-2011' = NA, 'Sep-2011' = NA, 'Oct-2011' = NA, 'Nov-2011' = NA, 'Dec-2011' = NA,
                                         'Jan-2012' = NA, 'Feb-2012' = NA, 'Mar-2012' = NA, 'Apr-2012' = NA, 'May-2012' = NA, 'Jun-2012' = NA, 'Jul-2012' = NA, 'Aug-2012' = NA, 'Sep-2012' = NA, 'Oct-2012' = NA, 'Nov-2012' = NA, 'Dec-2012' = NA,
                                         'Jan-2013' = NA, 'Feb-2013' = NA, 'Mar-2013' = NA, 'Apr-2013' = NA, 'May-2013' = NA, 'Jun-2013' = NA, 'Jul-2013' = NA, 'Aug-2013' = NA, 'Sep-2013' = NA, 'Oct-2013' = NA, 'Nov-2013' = NA, 'Dec-2013' = NA,
                                         'Jan-2014' = NA, 'Feb-2014' = NA, 'Mar-2014' = NA, 'Apr-2014' = NA, 'May-2014' = NA, 'Jun-2014' = NA, 'Jul-2014' = NA, 'Aug-2014' = NA, 'Sep-2014' = NA, 'Oct-2014' = NA, 'Nov-2014' = NA, 'Dec-2014' = NA,
                                         'Jan-2015' = NA, 'Feb-2015' = NA, 'Mar-2015' = NA, 'Apr-2015' = NA, 'May-2015' = NA, 'Jun-2015' = NA, 'Jul-2015' = NA, 'Aug-2015' = NA, 'Sep-2015' = NA, 'Oct-2015' = NA, 'Nov-2015' = NA, 'Dec-2015' = NA,
                                         'Jan-2016' = NA, 'Feb-2016' = NA, 'Mar-2016' = NA, 'Apr-2016' = NA, 'May-2016' = NA, 'Jun-2016' = NA, 'Jul-2016' = NA, 'Aug-2016' = NA, 'Sep-2016' = NA, 'Oct-2016' = NA, 'Nov-2016' = NA, 'Dec-2016' = NA)
#-------------------------------------------------------------------------------

# loop over each site to find compute and add mean summer temperature 
#-------------------------------------------------------------------------------
time0 <- Sys.time ()
for (s in 1:dim (siteMetaData) [1]) {
  
  # print site number to see progress
  #-----------------------------------------------------------------------------
  print (s)
  
  # get site's ID and the year of growth
  #-----------------------------------------------------------------------------
  startYear <- ifelse (siteMetaData$start [s] < 1948, 1948, 
                       siteMetaData$start [s])
  endYear <-  2016 #ifelse (siteMetaData$end [s] > 2016, 2016, 
                   #    siteMetaData$end [s])
                   # Just use 2016 as the final year. 
  
  
  # find 0.25 by 0.25 degree grid cell that contains the site s
  #-----------------------------------------------------------------------------
  lat <- ceiling (siteMetaData$lat [s] / res) * res - (res / 2)
  lon <- floor (siteMetaData$lon [s] / res) * res + (res / 2)
  
  # convert latitude and longitude to index used in netcdf files
  #-----------------------------------------------------------------------------
  iLat  <- (60 / res) + lat / res + res * 2.0
  iLat2 <- (90 / res) + lat / res + res * 2.0 # elevation file has more gird cells
  if (lon < 0 ) { # Western hemisphere
    iLon <- (360 + lon) / res + res * 2.0
  } else { # Eastern hemisphere
    iLon <- lon / res + res * 2.0
  }

  # read grid cell elevation from climate data and add it to the metaData
  #-----------------------------------------------------------------------------
  # nc_elev <- nc_open (file = paste0 (dirString,'elevation_0.25deg.nc'))
  # siteMetaData$eleClim [s] <- 
  #   ncvar_get (nc_elev, "elev", 
  #              start = c (iLon, iLat2, 1, 1), 
  #              count = c (1, 1, 1, 1))
  # nc_close (nc_elev); rm (nc_elev)
  
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
    tas  <- ncvar_get (nc_tas,  "tas",  start = c (iLon, iLat, 1), 
                       count = c (1, 1, daysInYear)) # K
    prcp <- ncvar_get (nc_prcp, "prcp", start = c (iLon, iLat, 1), 
                       count = c (1, 1, daysInYear)) # kg m-2 s-1
  
    # get and replace fill values
    #---------------------------------------------------------------------------
    fillValue <- ncatt_get (nc_tas, "tas", "_FillValue")
    tas  <- tas  %>% replace (tas  == fillValue$value, values = NA)
    prcp <- prcp %>% replace (prcp == fillValue$value, values = NA)
  
    # close annual climate data files
    #---------------------------------------------------------------------------
    nc_close (nc_tas)
    nc_close (nc_prcp)
  
    # convert units
    #---------------------------------------------------------------------------
    tas  <- tas  - 273.15                               # K          -> deg C
    prcp <- prcp * 86400                                # kg m-2 s-1 -> mm d-1
  
    # determine day of the year when month start and end
    #---------------------------------------------------------------------------
    if (daysInYear == 365) {
      startDOYs <- c (1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    } else {
      startDOYs <- c (1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336)
    }
    if (daysInYear == 365) {
      endDOYs <- c (31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
    } else {
      endDOYs <- c (31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
    }
                       
    # loop over months 
    # N.B.: should eventually add the previous five years here as a loop
    #---------------------------------------------------------------------------
    for (m in 1:12) {

      # determine days for the start and end of the period
      #-------------------------------------------------------------------------
      doyStart <- startDOYs [m] # first day of month
      doyEnd   <- endDOYs   [m] # first day of month
      
      # add mean period air surface temperature to the data 
      #-------------------------------------------------------------------------
      d [which (d$site == siteMetaData$site [s] & d$year == y), 9 + 2 * (m - 1)] <- 
        tas  [doyStart:doyEnd] %>% 
        mean ()
      
      # add temperature data to site climate data 
      #-------------------------------------------------------------------------
      siteTempData [s, ((y - 1948) * 12 + 17 + m)] <- tas  [doyStart:doyEnd] %>% 
        mean ()
      
      # add total period precipitation to the data 
      #-------------------------------------------------------------------------
      d [which (d$site == siteMetaData$site [s] & d$year == y), 10 + 2 * (m - 1)] <- 
        prcp [doyStart:doyEnd] %>% 
        sum ()
      
      # add precipitation data to site climate data 
      #-------------------------------------------------------------------------
      sitePrecData [s, ((y - 1948) * 12 + 17 + m)] <- prcp [doyStart:doyEnd] %>% 
        sum ()
      
    }  # close loop over months
    
    # delete unnecessary variables
    #---------------------------------------------------------------------------
    rm (fillValue, tas, nc_tas, fileNameTas, prcp, nc_prcp, fileNamePrcp)
  } # close loop over years
  
} # close loop over sites
time1 <- Sys.time ()
time1 - time0 
# currently takes about 3.4 hours for 149 sites with files on external 
# hard drive. Access to external hard drive slows this substantially.

# calculate monthly self-calibrating Palmer Drought Severity Index 
#-------------------------------------------------------------------------------
for (s in 1:dim (siteMetaData) [1]) {
  
  # estimate potential evapotransporation [mm]
  #-----------------------------------------------------------------------------
  PET <- thornthwaite (Tave = t (siteTempData [s, 18:845]), 
                       lat = siteMetaData$lat [s],
                       na.rm = TRUE)
  PET <- as.numeric (PET)
  
  # calculate self-calibrating Palmer Drought Severity Index
  #-----------------------------------------------------------------------------
  scPDSI <- pdsi (P = unlist (sitePrecData [s, 18:845]),
                  PE = PET,
                  sc = TRUE)
  
  # append PDSI data
  #-----------------------------------------------------------------------------
  if (s == 1) {
    sitePDSIData <- as_tibble (t (scPDSI$X))
  } else {
    sitePDSIData <- add_row (sitePDSIData, as_tibble (t (scPDSI$X)))
  }
}

# change names of monthly scPDSI data for consistency with temperature and 
# precipitation data
#-------------------------------------------------------------------------------
sitePDSIData <- sitePDSIData %>% 
  dplyr:: rename ('Jan-1948' =   V1, 'Feb-1948' =   V2, 'Mar-1948' =   V3, 'Apr-1948' =   V4, 'May-1948' =   V5, 'Jun-1948' =   V6, 'Jul-1948' =   V7, 'Aug-1948' =   V8, 'Sep-1948' =   V9, 'Oct-1948' =  V10, 'Nov-1948' =  V11, 'Dec-1948' =  V12,
                  'Jan-1949' =  V13, 'Feb-1949' =  V14, 'Mar-1949' =  V15, 'Apr-1949' =  V16, 'May-1949' =  V17, 'Jun-1949' =  V18, 'Jul-1949' =  V19, 'Aug-1949' =  V20, 'Sep-1949' =  V21, 'Oct-1949' =  V22, 'Nov-1949' =  V23, 'Dec-1949' =  V24,
                  'Jan-1950' =  V25, 'Feb-1950' =  V26, 'Mar-1950' =  V27, 'Apr-1950' =  V28, 'May-1950' =  V29, 'Jun-1950' =  V30, 'Jul-1950' =  V31, 'Aug-1950' =  V32, 'Sep-1950' =  V33, 'Oct-1950' =  V34, 'Nov-1950' =  V35, 'Dec-1950' =  V36,
                  'Jan-1951' =  V37, 'Feb-1951' =  V38, 'Mar-1951' =  V39, 'Apr-1951' =  V40, 'May-1951' =  V41, 'Jun-1951' =  V42, 'Jul-1951' =  V43, 'Aug-1951' =  V44, 'Sep-1951' =  V45, 'Oct-1951' =  V46, 'Nov-1951' =  V47, 'Dec-1951' =  V48,
                  'Jan-1952' =  V49, 'Feb-1952' =  V50, 'Mar-1952' =  V51, 'Apr-1952' =  V52, 'May-1952' =  V53, 'Jun-1952' =  V54, 'Jul-1952' =  V55, 'Aug-1952' =  V56, 'Sep-1952' =  V57, 'Oct-1952' =  V58, 'Nov-1952' =  V59, 'Dec-1952' =  V60,
                  'Jan-1953' =  V61, 'Feb-1953' =  V62, 'Mar-1953' =  V63, 'Apr-1953' =  V64, 'May-1953' =  V65, 'Jun-1953' =  V66, 'Jul-1953' =  V67, 'Aug-1953' =  V68, 'Sep-1953' =  V69, 'Oct-1953' =  V70, 'Nov-1953' =  V71, 'Dec-1953' =  V72,
                  'Jan-1954' =  V73, 'Feb-1954' =  V74, 'Mar-1954' =  V75, 'Apr-1954' =  V76, 'May-1954' =  V77, 'Jun-1954' =  V78, 'Jul-1954' =  V79, 'Aug-1954' =  V80, 'Sep-1954' =  V81, 'Oct-1954' =  V82, 'Nov-1954' =  V83, 'Dec-1954' =  V84,
                  'Jan-1955' =  V85, 'Feb-1955' =  V86, 'Mar-1955' =  V87, 'Apr-1955' =  V88, 'May-1955' =  V89, 'Jun-1955' =  V90, 'Jul-1955' =  V91, 'Aug-1955' =  V92, 'Sep-1955' =  V93, 'Oct-1955' =  V94, 'Nov-1955' =  V95, 'Dec-1955' =  V96,
                  'Jan-1956' =  V97, 'Feb-1956' =  V98, 'Mar-1956' =  V99, 'Apr-1956' = V100, 'May-1956' = V101, 'Jun-1956' = V102, 'Jul-1956' = V103, 'Aug-1956' = V104, 'Sep-1956' = V105, 'Oct-1956' = V106, 'Nov-1956' = V107, 'Dec-1956' = V108,
                  'Jan-1957' = V109, 'Feb-1957' = V110, 'Mar-1957' = V111, 'Apr-1957' = V112, 'May-1957' = V113, 'Jun-1957' = V114, 'Jul-1957' = V115, 'Aug-1957' = V116, 'Sep-1957' = V117, 'Oct-1957' = V118, 'Nov-1957' = V119, 'Dec-1957' = V120,
                  'Jan-1958' = V121, 'Feb-1958' = V122, 'Mar-1958' = V123, 'Apr-1958' = V124, 'May-1958' = V125, 'Jun-1958' = V126, 'Jul-1958' = V127, 'Aug-1958' = V128, 'Sep-1958' = V129, 'Oct-1958' = V130, 'Nov-1958' = V131, 'Dec-1958' = V132,
                  'Jan-1959' = V133, 'Feb-1959' = V134, 'Mar-1959' = V135, 'Apr-1959' = V136, 'May-1959' = V137, 'Jun-1959' = V138, 'Jul-1959' = V139, 'Aug-1959' = V140, 'Sep-1959' = V141, 'Oct-1959' = V142, 'Nov-1959' = V143, 'Dec-1959' = V144,
                  'Jan-1960' = V145, 'Feb-1960' = V146, 'Mar-1960' = V147, 'Apr-1960' = V148, 'May-1960' = V149, 'Jun-1960' = V150, 'Jul-1960' = V151, 'Aug-1960' = V152, 'Sep-1960' = V153, 'Oct-1960' = V154, 'Nov-1960' = V155, 'Dec-1960' = V156,
                  'Jan-1961' = V157, 'Feb-1961' = V158, 'Mar-1961' = V159, 'Apr-1961' = V160, 'May-1961' = V161, 'Jun-1961' = V162, 'Jul-1961' = V163, 'Aug-1961' = V164, 'Sep-1961' = V165, 'Oct-1961' = V166, 'Nov-1961' = V167, 'Dec-1961' = V168,
                  'Jan-1962' = V169, 'Feb-1962' = V170, 'Mar-1962' = V171, 'Apr-1962' = V172, 'May-1962' = V173, 'Jun-1962' = V174, 'Jul-1962' = V175, 'Aug-1962' = V176, 'Sep-1962' = V177, 'Oct-1962' = V178, 'Nov-1962' = V179, 'Dec-1962' = V180,
                  'Jan-1963' = V181, 'Feb-1963' = V182, 'Mar-1963' = V183, 'Apr-1963' = V184, 'May-1963' = V185, 'Jun-1963' = V186, 'Jul-1963' = V187, 'Aug-1963' = V188, 'Sep-1963' = V189, 'Oct-1963' = V190, 'Nov-1963' = V191, 'Dec-1963' = V192,
                  'Jan-1964' = V193, 'Feb-1964' = V194, 'Mar-1964' = V195, 'Apr-1964' = V196, 'May-1964' = V197, 'Jun-1964' = V198, 'Jul-1964' = V199, 'Aug-1964' = V200, 'Sep-1964' = V201, 'Oct-1964' = V202, 'Nov-1964' = V203, 'Dec-1964' = V204,
                  'Jan-1965' = V205, 'Feb-1965' = V206, 'Mar-1965' = V207, 'Apr-1965' = V208, 'May-1965' = V209, 'Jun-1965' = V210, 'Jul-1965' = V211, 'Aug-1965' = V212, 'Sep-1965' = V213, 'Oct-1965' = V214, 'Nov-1965' = V215, 'Dec-1965' = V216,
                  'Jan-1966' = V217, 'Feb-1966' = V218, 'Mar-1966' = V219, 'Apr-1966' = V220, 'May-1966' = V221, 'Jun-1966' = V222, 'Jul-1966' = V223, 'Aug-1966' = V224, 'Sep-1966' = V225, 'Oct-1966' = V226, 'Nov-1966' = V227, 'Dec-1966' = V228,
                  'Jan-1967' = V229, 'Feb-1967' = V230, 'Mar-1967' = V231, 'Apr-1967' = V232, 'May-1967' = V233, 'Jun-1967' = V234, 'Jul-1967' = V235, 'Aug-1967' = V236, 'Sep-1967' = V237, 'Oct-1967' = V238, 'Nov-1967' = V239, 'Dec-1967' = V240,
                  'Jan-1968' = V241, 'Feb-1968' = V242, 'Mar-1968' = V243, 'Apr-1968' = V244, 'May-1968' = V245, 'Jun-1968' = V246, 'Jul-1968' = V247, 'Aug-1968' = V248, 'Sep-1968' = V249, 'Oct-1968' = V250, 'Nov-1968' = V251, 'Dec-1968' = V252,
                  'Jan-1969' = V253, 'Feb-1969' = V254, 'Mar-1969' = V255, 'Apr-1969' = V256, 'May-1969' = V257, 'Jun-1969' = V258, 'Jul-1969' = V259, 'Aug-1969' = V260, 'Sep-1969' = V261, 'Oct-1969' = V262, 'Nov-1969' = V263, 'Dec-1969' = V264,
                  'Jan-1970' = V265, 'Feb-1970' = V266, 'Mar-1970' = V267, 'Apr-1970' = V268, 'May-1970' = V269, 'Jun-1970' = V270, 'Jul-1970' = V271, 'Aug-1970' = V272, 'Sep-1970' = V273, 'Oct-1970' = V274, 'Nov-1970' = V275, 'Dec-1970' = V276,
                  'Jan-1971' = V277, 'Feb-1971' = V278, 'Mar-1971' = V279, 'Apr-1971' = V280, 'May-1971' = V281, 'Jun-1971' = V282, 'Jul-1971' = V283, 'Aug-1971' = V284, 'Sep-1971' = V285, 'Oct-1971' = V286, 'Nov-1971' = V287, 'Dec-1971' = V288,
                  'Jan-1972' = V289, 'Feb-1972' = V290, 'Mar-1972' = V291, 'Apr-1972' = V292, 'May-1972' = V293, 'Jun-1972' = V294, 'Jul-1972' = V295, 'Aug-1972' = V296, 'Sep-1972' = V297, 'Oct-1972' = V298, 'Nov-1972' = V299, 'Dec-1972' = V300,
                  'Jan-1973' = V301, 'Feb-1973' = V302, 'Mar-1973' = V303, 'Apr-1973' = V304, 'May-1973' = V305, 'Jun-1973' = V306, 'Jul-1973' = V307, 'Aug-1973' = V308, 'Sep-1973' = V309, 'Oct-1973' = V310, 'Nov-1973' = V311, 'Dec-1973' = V312,
                  'Jan-1974' = V313, 'Feb-1974' = V314, 'Mar-1974' = V315, 'Apr-1974' = V316, 'May-1974' = V317, 'Jun-1974' = V318, 'Jul-1974' = V319, 'Aug-1974' = V320, 'Sep-1974' = V321, 'Oct-1974' = V322, 'Nov-1974' = V323, 'Dec-1974' = V324,
                  'Jan-1975' = V325, 'Feb-1975' = V326, 'Mar-1975' = V327, 'Apr-1975' = V328, 'May-1975' = V329, 'Jun-1975' = V330, 'Jul-1975' = V331, 'Aug-1975' = V332, 'Sep-1975' = V333, 'Oct-1975' = V334, 'Nov-1975' = V335, 'Dec-1975' = V336,
                  'Jan-1976' = V337, 'Feb-1976' = V338, 'Mar-1976' = V339, 'Apr-1976' = V340, 'May-1976' = V341, 'Jun-1976' = V342, 'Jul-1976' = V343, 'Aug-1976' = V344, 'Sep-1976' = V345, 'Oct-1976' = V346, 'Nov-1976' = V347, 'Dec-1976' = V348,
                  'Jan-1977' = V349, 'Feb-1977' = V350, 'Mar-1977' = V351, 'Apr-1977' = V352, 'May-1977' = V353, 'Jun-1977' = V354, 'Jul-1977' = V355, 'Aug-1977' = V356, 'Sep-1977' = V357, 'Oct-1977' = V358, 'Nov-1977' = V359, 'Dec-1977' = V360,
                  'Jan-1978' = V361, 'Feb-1978' = V362, 'Mar-1978' = V363, 'Apr-1978' = V364, 'May-1978' = V365, 'Jun-1978' = V366, 'Jul-1978' = V367, 'Aug-1978' = V368, 'Sep-1978' = V369, 'Oct-1978' = V370, 'Nov-1978' = V371, 'Dec-1978' = V372,
                  'Jan-1979' = V373, 'Feb-1979' = V374, 'Mar-1979' = V375, 'Apr-1979' = V376, 'May-1979' = V377, 'Jun-1979' = V378, 'Jul-1979' = V379, 'Aug-1979' = V380, 'Sep-1979' = V381, 'Oct-1979' = V382, 'Nov-1979' = V383, 'Dec-1979' = V384,
                  'Jan-1980' = V385, 'Feb-1980' = V386, 'Mar-1980' = V387, 'Apr-1980' = V388, 'May-1980' = V389, 'Jun-1980' = V390, 'Jul-1980' = V391, 'Aug-1980' = V392, 'Sep-1980' = V393, 'Oct-1980' = V394, 'Nov-1980' = V395, 'Dec-1980' = V396,
                  'Jan-1981' = V397, 'Feb-1981' = V398, 'Mar-1981' = V399, 'Apr-1981' = V400, 'May-1981' = V401, 'Jun-1981' = V402, 'Jul-1981' = V403, 'Aug-1981' = V404, 'Sep-1981' = V405, 'Oct-1981' = V406, 'Nov-1981' = V407, 'Dec-1981' = V408,
                  'Jan-1982' = V409, 'Feb-1982' = V410, 'Mar-1982' = V411, 'Apr-1982' = V412, 'May-1982' = V413, 'Jun-1982' = V414, 'Jul-1982' = V415, 'Aug-1982' = V416, 'Sep-1982' = V417, 'Oct-1982' = V418, 'Nov-1982' = V419, 'Dec-1982' = V420,
                  'Jan-1983' = V421, 'Feb-1983' = V422, 'Mar-1983' = V423, 'Apr-1983' = V424, 'May-1983' = V425, 'Jun-1983' = V426, 'Jul-1983' = V427, 'Aug-1983' = V428, 'Sep-1983' = V429, 'Oct-1983' = V430, 'Nov-1983' = V431, 'Dec-1983' = V432,
                  'Jan-1984' = V433, 'Feb-1984' = V434, 'Mar-1984' = V435, 'Apr-1984' = V436, 'May-1984' = V437, 'Jun-1984' = V438, 'Jul-1984' = V439, 'Aug-1984' = V440, 'Sep-1984' = V441, 'Oct-1984' = V442, 'Nov-1984' = V443, 'Dec-1984' = V444,
                  'Jan-1985' = V445, 'Feb-1985' = V446, 'Mar-1985' = V447, 'Apr-1985' = V448, 'May-1985' = V449, 'Jun-1985' = V450, 'Jul-1985' = V451, 'Aug-1985' = V452, 'Sep-1985' = V453, 'Oct-1985' = V454, 'Nov-1985' = V455, 'Dec-1985' = V456,
                  'Jan-1986' = V457, 'Feb-1986' = V458, 'Mar-1986' = V459, 'Apr-1986' = V460, 'May-1986' = V461, 'Jun-1986' = V462, 'Jul-1986' = V463, 'Aug-1986' = V464, 'Sep-1986' = V465, 'Oct-1986' = V466, 'Nov-1986' = V467, 'Dec-1986' = V468,
                  'Jan-1987' = V469, 'Feb-1987' = V470, 'Mar-1987' = V471, 'Apr-1987' = V472, 'May-1987' = V473, 'Jun-1987' = V474, 'Jul-1987' = V475, 'Aug-1987' = V476, 'Sep-1987' = V477, 'Oct-1987' = V478, 'Nov-1987' = V479, 'Dec-1987' = V480,
                  'Jan-1988' = V481, 'Feb-1988' = V482, 'Mar-1988' = V483, 'Apr-1988' = V484, 'May-1988' = V485, 'Jun-1988' = V486, 'Jul-1988' = V487, 'Aug-1988' = V488, 'Sep-1988' = V489, 'Oct-1988' = V490, 'Nov-1988' = V491, 'Dec-1988' = V492,
                  'Jan-1989' = V493, 'Feb-1989' = V494, 'Mar-1989' = V495, 'Apr-1989' = V496, 'May-1989' = V497, 'Jun-1989' = V498, 'Jul-1989' = V499, 'Aug-1989' = V500, 'Sep-1989' = V501, 'Oct-1989' = V502, 'Nov-1989' = V503, 'Dec-1989' = V504,
                  'Jan-1990' = V505, 'Feb-1990' = V506, 'Mar-1990' = V507, 'Apr-1990' = V508, 'May-1990' = V509, 'Jun-1990' = V510, 'Jul-1990' = V511, 'Aug-1990' = V512, 'Sep-1990' = V513, 'Oct-1990' = V514, 'Nov-1990' = V515, 'Dec-1990' = V516,
                  'Jan-1991' = V517, 'Feb-1991' = V518, 'Mar-1991' = V519, 'Apr-1991' = V520, 'May-1991' = V521, 'Jun-1991' = V522, 'Jul-1991' = V523, 'Aug-1991' = V524, 'Sep-1991' = V525, 'Oct-1991' = V526, 'Nov-1991' = V527, 'Dec-1991' = V528,
                  'Jan-1992' = V529, 'Feb-1992' = V530, 'Mar-1992' = V531, 'Apr-1992' = V532, 'May-1992' = V533, 'Jun-1992' = V534, 'Jul-1992' = V535, 'Aug-1992' = V536, 'Sep-1992' = V537, 'Oct-1992' = V538, 'Nov-1992' = V539, 'Dec-1992' = V540,
                  'Jan-1993' = V541, 'Feb-1993' = V542, 'Mar-1993' = V543, 'Apr-1993' = V544, 'May-1993' = V545, 'Jun-1993' = V546, 'Jul-1993' = V547, 'Aug-1993' = V548, 'Sep-1993' = V549, 'Oct-1993' = V550, 'Nov-1993' = V551, 'Dec-1993' = V552,
                  'Jan-1994' = V553, 'Feb-1994' = V554, 'Mar-1994' = V555, 'Apr-1994' = V556, 'May-1994' = V557, 'Jun-1994' = V558, 'Jul-1994' = V559, 'Aug-1994' = V560, 'Sep-1994' = V561, 'Oct-1994' = V562, 'Nov-1994' = V563, 'Dec-1994' = V564,
                  'Jan-1995' = V565, 'Feb-1995' = V566, 'Mar-1995' = V567, 'Apr-1995' = V568, 'May-1995' = V569, 'Jun-1995' = V570, 'Jul-1995' = V571, 'Aug-1995' = V572, 'Sep-1995' = V573, 'Oct-1995' = V574, 'Nov-1995' = V575, 'Dec-1995' = V576,
                  'Jan-1996' = V577, 'Feb-1996' = V578, 'Mar-1996' = V579, 'Apr-1996' = V580, 'May-1996' = V581, 'Jun-1996' = V582, 'Jul-1996' = V583, 'Aug-1996' = V584, 'Sep-1996' = V585, 'Oct-1996' = V586, 'Nov-1996' = V587, 'Dec-1996' = V588,
                  'Jan-1997' = V589, 'Feb-1997' = V590, 'Mar-1997' = V591, 'Apr-1997' = V592, 'May-1997' = V593, 'Jun-1997' = V594, 'Jul-1997' = V595, 'Aug-1997' = V596, 'Sep-1997' = V597, 'Oct-1997' = V598, 'Nov-1997' = V599, 'Dec-1997' = V600,
                  'Jan-1998' = V601, 'Feb-1998' = V602, 'Mar-1998' = V603, 'Apr-1998' = V604, 'May-1998' = V605, 'Jun-1998' = V606, 'Jul-1998' = V607, 'Aug-1998' = V608, 'Sep-1998' = V609, 'Oct-1998' = V610, 'Nov-1998' = V611, 'Dec-1998' = V612,
                  'Jan-1999' = V613, 'Feb-1999' = V614, 'Mar-1999' = V615, 'Apr-1999' = V616, 'May-1999' = V617, 'Jun-1999' = V618, 'Jul-1999' = V619, 'Aug-1999' = V620, 'Sep-1999' = V621, 'Oct-1999' = V622, 'Nov-1999' = V623, 'Dec-1999' = V624,
                  'Jan-2000' = V625, 'Feb-2000' = V626, 'Mar-2000' = V627, 'Apr-2000' = V628, 'May-2000' = V629, 'Jun-2000' = V630, 'Jul-2000' = V631, 'Aug-2000' = V632, 'Sep-2000' = V633, 'Oct-2000' = V634, 'Nov-2000' = V635, 'Dec-2000' = V636,
                  'Jan-2001' = V637, 'Feb-2001' = V638, 'Mar-2001' = V639, 'Apr-2001' = V640, 'May-2001' = V641, 'Jun-2001' = V642, 'Jul-2001' = V643, 'Aug-2001' = V644, 'Sep-2001' = V645, 'Oct-2001' = V646, 'Nov-2001' = V647, 'Dec-2001' = V648,
                  'Jan-2002' = V649, 'Feb-2002' = V650, 'Mar-2002' = V651, 'Apr-2002' = V652, 'May-2002' = V653, 'Jun-2002' = V654, 'Jul-2002' = V655, 'Aug-2002' = V656, 'Sep-2002' = V657, 'Oct-2002' = V658, 'Nov-2002' = V659, 'Dec-2002' = V660,
                  'Jan-2003' = V661, 'Feb-2003' = V662, 'Mar-2003' = V663, 'Apr-2003' = V664, 'May-2003' = V665, 'Jun-2003' = V666, 'Jul-2003' = V667, 'Aug-2003' = V668, 'Sep-2003' = V669, 'Oct-2003' = V670, 'Nov-2003' = V671, 'Dec-2003' = V672,
                  'Jan-2004' = V673, 'Feb-2004' = V674, 'Mar-2004' = V675, 'Apr-2004' = V676, 'May-2004' = V677, 'Jun-2004' = V678, 'Jul-2004' = V679, 'Aug-2004' = V680, 'Sep-2004' = V681, 'Oct-2004' = V682, 'Nov-2004' = V683, 'Dec-2004' = V684,
                  'Jan-2005' = V685, 'Feb-2005' = V686, 'Mar-2005' = V687, 'Apr-2005' = V688, 'May-2005' = V689, 'Jun-2005' = V690, 'Jul-2005' = V691, 'Aug-2005' = V692, 'Sep-2005' = V693, 'Oct-2005' = V694, 'Nov-2005' = V695, 'Dec-2005' = V696,
                  'Jan-2006' = V697, 'Feb-2006' = V698, 'Mar-2006' = V699, 'Apr-2006' = V700, 'May-2006' = V701, 'Jun-2006' = V702, 'Jul-2006' = V703, 'Aug-2006' = V704, 'Sep-2006' = V705, 'Oct-2006' = V706, 'Nov-2006' = V707, 'Dec-2006' = V708,
                  'Jan-2007' = V709, 'Feb-2007' = V710, 'Mar-2007' = V711, 'Apr-2007' = V712, 'May-2007' = V713, 'Jun-2007' = V714, 'Jul-2007' = V715, 'Aug-2007' = V716, 'Sep-2007' = V717, 'Oct-2007' = V718, 'Nov-2007' = V719, 'Dec-2007' = V720,
                  'Jan-2008' = V721, 'Feb-2008' = V722, 'Mar-2008' = V723, 'Apr-2008' = V724, 'May-2008' = V725, 'Jun-2008' = V726, 'Jul-2008' = V727, 'Aug-2008' = V728, 'Sep-2008' = V729, 'Oct-2008' = V730, 'Nov-2008' = V731, 'Dec-2008' = V732,
                  'Jan-2009' = V733, 'Feb-2009' = V734, 'Mar-2009' = V735, 'Apr-2009' = V736, 'May-2009' = V737, 'Jun-2009' = V738, 'Jul-2009' = V739, 'Aug-2009' = V740, 'Sep-2009' = V741, 'Oct-2009' = V742, 'Nov-2009' = V743, 'Dec-2009' = V744,
                  'Jan-2010' = V745, 'Feb-2010' = V746, 'Mar-2010' = V747, 'Apr-2010' = V748, 'May-2010' = V749, 'Jun-2010' = V750, 'Jul-2010' = V751, 'Aug-2010' = V752, 'Sep-2010' = V753, 'Oct-2010' = V754, 'Nov-2010' = V755, 'Dec-2010' = V756,
                  'Jan-2011' = V757, 'Feb-2011' = V758, 'Mar-2011' = V759, 'Apr-2011' = V760, 'May-2011' = V761, 'Jun-2011' = V762, 'Jul-2011' = V763, 'Aug-2011' = V764, 'Sep-2011' = V765, 'Oct-2011' = V766, 'Nov-2011' = V767, 'Dec-2011' = V768,
                  'Jan-2012' = V769, 'Feb-2012' = V770, 'Mar-2012' = V771, 'Apr-2012' = V772, 'May-2012' = V773, 'Jun-2012' = V774, 'Jul-2012' = V775, 'Aug-2012' = V776, 'Sep-2012' = V777, 'Oct-2012' = V778, 'Nov-2012' = V779, 'Dec-2012' = V780,
                  'Jan-2013' = V781, 'Feb-2013' = V782, 'Mar-2013' = V783, 'Apr-2013' = V784, 'May-2013' = V785, 'Jun-2013' = V786, 'Jul-2013' = V787, 'Aug-2013' = V788, 'Sep-2013' = V789, 'Oct-2013' = V790, 'Nov-2013' = V791, 'Dec-2013' = V792,
                  'Jan-2014' = V793, 'Feb-2014' = V794, 'Mar-2014' = V795, 'Apr-2014' = V796, 'May-2014' = V797, 'Jun-2014' = V798, 'Jul-2014' = V799, 'Aug-2014' = V800, 'Sep-2014' = V801, 'Oct-2014' = V802, 'Nov-2014' = V803, 'Dec-2014' = V804,
                  'Jan-2015' = V805, 'Feb-2015' = V806, 'Mar-2015' = V807, 'Apr-2015' = V808, 'May-2015' = V809, 'Jun-2015' = V810, 'Jul-2015' = V811, 'Aug-2015' = V812, 'Sep-2015' = V813, 'Oct-2015' = V814, 'Nov-2015' = V815, 'Dec-2015' = V816,
                  'Jan-2016' = V817, 'Feb-2016' = V818, 'Mar-2016' = V819, 'Apr-2016' = V820, 'May-2016' = V821, 'Jun-2016' = V822, 'Jul-2016' = V823, 'Aug-2016' = V824, 'Sep-2016' = V825, 'Oct-2016' = V826, 'Nov-2016' = V827, 'Dec-2016' = V828)

# add siteMetaData to sitePDSIData to have same format as siteTempData and sitePrecData
sitePDSIData <- add_column (siteMetaData, sitePDSIData)

# make histogram of mean January temperatures
#-------------------------------------------------------------------------------
PLOT <- FALSE; if (PLOT) {
  png (file = "../fig/janTemperaturesHist.png")
  par (mar = c (5, 5, 1, 5))
  hist (d$tasJul0, 
        xlab = expression (paste ('Mean january temperatures (',degree,'C)', 
                                  sep = '')), 
        main = '', col = '#EB99A999', xlim = c (-20, 10), ylim = c (0, 40000), 
        las = 1, lty = 1, lwd = 1, ylab = '', axes = FALSE)
  axis (side = 1, seq (-20, 10, 5))
  axis (side = 2, at = seq (0, 35000, 5000), las = 1)
  mtext (side = 2, text = "Frequency", line = 3)
  mtext (side = 4, text = "Density", line = 3)
  rhoTas <- density (d$tasJan0, na.rm = TRUE, bw = 0.9)
  par (new = TRUE)
  plot (rhoTas, ylim = c (0, 0.12), axes = FALSE, col = '#901C3B', lwd = 3, 
        main = "", xlab = "", ylab = "")
  axis (side = 4, las = 1)
  dev.off ()
}
#===============================================================================
