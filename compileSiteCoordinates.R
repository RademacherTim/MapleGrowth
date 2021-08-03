#===============================================================================
# Script to read locations for maple (i.e., red maple and sugar maple) growth 
# data such as chronologies from the International Tree-Ring Database (ITRDB), 
# as cleaned by Zhao et al. (2019)
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
library ('tidyverse')
library ('ggplot2')
library ('ggmap')
library ('maps')
library ('mapdata')
library ('maptools')
library ('broom')
library ('sf')
library ('rnaturalearth')
library ('rnaturalearthdata')
library ('lubridate')

# read ITRDB coordinates
#-------------------------------------------------------------------------------
ITRDBcoordinates <- read_csv (
  file = '../data/growth/chronologyData/ITRDB_cleaned_Zhao2019/coordinate.csv',
  col_types = cols ())

# filter out any rows that are not maple species
#-------------------------------------------------------------------------------
ITRDBcoordinates <- ITRDBcoordinates %>% 
  filter (substr (speciesCode, 1, 2) == 'AC')

# compile list of all coordinates
#-------------------------------------------------------------------------------
siteMetaData <- tibble (
  source = 'ITRDB',             # source of growth data 
  code = ITRDBcoordinates$code, # code for sample site
  file = ITRDBcoordinates$file, # file with growth data
  species = ITRDBcoordinates$speciesCode, # maple species at the site
  lat = ITRDBcoordinates$latitude, # site latitude (decimal degrees)
  lon = ITRDBcoordinates$longitude, # site longitude (decimal degrees)
  ele = ITRDBcoordinates$elevation, # site elevation (m)
  start = as_date (paste0 (as.character (ITRDBcoordinates$start.date),'-01-01'), format = '%Y-%m-%d'), # start date of growth data
  end = as_date (paste0 (as.character (ITRDBcoordinates$end.date),'-12-31')) # end date of growth data
)
# does it make sense to use first of Jan and 31st of Dec as dates here? 
# Need to think about this, as growing season is never the entire year.

# add other coordinates for multiple data types
#-------------------------------------------------------------------------------
# Need to add inventory data from:
#     - FIA
#     - Canadian National Forest Inventory
#     - Québec inventory
#     - HF dendrometer data
#     - HF chronologies
#     - ACERnet data
#     - HF microcores
# get shapefile for red and sugar maple distributions from US Forest service
#-------------------------------------------------------------------------------
disACRU <- st_read  ('../data/distribution/little1991/ACRU/litt316av.shp',
                     stringsAsFactors = FALSE)
disACSH <- st_read  ('../data/distribution/little1991/ACSH/litt318av.shp',
                     stringsAsFactors = FALSE)

# set the coordinate system to Albers equal area projection with US Forest 
# Service parameters from https://www.fs.fed.us/nrs/atlas/littlefia/albers_prj.txt
#-------------------------------------------------------------------------------
st_crs (disACRU) <- 
  '+proj=aea +lat_1=38.0 +lat_2=42.0 +lat_0=40.0 +lon_0=-82.0 +x_0=0 +y_0=0'
st_crs (disACSH) <- 
  '+proj=aea +lat_1=38.0 +lat_2=42.0 +lat_0=40.0 +lon_0=-82.0 +x_0=0 +y_0=0'

# convert coordinate system to WGS84
#-------------------------------------------------------------------------------
disACRU_ll <- disACRU %>% 
  st_transform (crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
disACSH_ll <- disACSH %>% 
  st_transform (crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

# get map data for USA and Canada
#-------------------------------------------------------------------------------
usa <- map_data ("usa")
canada <- map_data ("worldHires", "Canada")

# make a map of the study region (including Eastern USA, Atlantic Canada, 
# Québec, and Ontario)
#-------------------------------------------------------------------------------
NAmap <- ggplot () + 
  geom_polygon (data = usa, 
                aes (x = long, y = lat, group = group), 
                fill = "white", 
                color = "#333333") +  
  geom_polygon (data = canada, aes (x = long, y = lat, group = group), 
                fill = "white", color = "#333333") +
  geom_sf (data = disACRU_ll, fill = '#901c3b33', size = 0) +
  geom_sf (data = disACSH_ll, fill = '#f3bd4833', size = 0) +
  xlab ("Longitude") + ylab ("Latitude") +
  ggtitle('Samples sites and types across North America', 
          subtitle = 'for growth of maple species') +
  geom_point (data = siteMetaData, 
              aes (x = lon, y = lat, fill = species), 
             fill = c ('#901c3bcc', rep ('#f3bd48cc', 5)), 
             color = "#444444", shape = 21, size = 2.5) + 
  coord_sf (xlim = c (-98, -50),  ylim = c (26, 55)) +
  theme_minimal (12) #+ theme (legend.position = 'right')
NAmap

#===============================================================================