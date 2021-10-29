#===============================================================================
# Script to read locations for maple (i.e., red maple and sugar maple) growth 
# data such as chronologies:
#     - ITRDB, from Zhao et al. (2019) (ITRDB)    - TRUE
#     - Neil Pederson                  (NP)       - TRUE
#     - Justin Timothy Maxwell         (JTM)      - TRUE
#     - Benoit Gendreau-Berthiaume     (BG)       - TRUE
#     - Scott Warner                   (SW)       - FALSE
#     - David A. Orwig                 (DAO)      - TRUE
#     - Tim Rademacher                 (TR)       - FALSE
#     - Serge Payette                  (SP)       - TRUE
#     - Martin Girardin                (MG)       - FALSE
#     - Brett Huggett                  (BH)       - FALSE
#     - Loic D'Orangeville             (LO)       - FALSE
#     - Chistina Stinson               (CS)       - FALSE
#     = Shawn Fraver                   (SF)       - TRUE
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse')
if (!existsFunction ('ggplot')) library ('ggplot2')
#library ('ggmap')
if (!existsFunction ('map_data')) library ('mapdata') # needed for Canadian borders
if (!existsFunction ('st_read')) library ('sf')
#library ('rnaturalearth')
#library ('rnaturalearthdata')
if (!existsFunction ('readxl')) library ('readxl')
library ('tiff')

# get coordinates from all chronologies, but only colour the ones that are 
# already added
#-------------------------------------------------------------------------------
siteMetaData <- readxl::read_excel (col_names = TRUE, 
  col_types = c ('numeric','text','text','text','text','text','text',
                 'numeric','numeric','numeric','numeric','numeric','text',
                 'logical','text','text','text'),
  path = '../data/growth/chronologyData/siteMetaData.xlsx')
siteMetaData <- siteMetaData %>% 
  mutate (colour = ifelse (species == 'ACRU', '#901c3bcc','#f3bd48cc'),
          colour = ifelse (source %in% c ('SW', 'TR', 'MG', 'BH', 'LO', 
                                          'CS'), '#eeeeeecc', colour),
          lon = as.numeric (lon),
          lat = as.numeric (lat))

# sum number of cores and trees sampled
#-------------------------------------------------------------------------------
nSamples <- siteMetaData %>% select (nTrees, nCores) %>% 
  summarise (nTrees = sum (nTrees, na.rm = TRUE),
             nCores = sum (nCores, na.rm = TRUE)) 

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
disACRU <- sf::st_read  ('../data/distribution/little1991/ACRU/litt316av.shp',
                         stringsAsFactors = FALSE, quiet = TRUE)
disACSH <- sf::st_read  ('../data/distribution/little1991/ACSH/litt318av.shp',
                         stringsAsFactors = FALSE, quiet = TRUE)

# set the coordinate system to Albers equal area projection with US Forest 
# Service parameters from https://www.fs.fed.us/nrs/atlas/littlefia/albers_prj.txt
#-------------------------------------------------------------------------------
USFS_CRS <- 
  '+proj=aea +lat_1=38.0 +lat_2=42.0 +lat_0=40.0 +lon_0=-82.0 +x_0=0 +y_0=0'
sf::st_crs (disACRU) <- USFS_CRS
sf::st_crs (disACSH) <- USFS_CRS

# convert coordinate system to WGS84
#-------------------------------------------------------------------------------
disACRU_ll <- disACRU %>% 
  sf::st_transform (crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
  #st_transform (crs = '+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45')
disACSH_ll <- disACSH %>% 
  sf::st_transform (crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
  #st_transform (crs = '+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45')

# Load map biomass map from Beaudoin et al. (2014)
#-------------------------------------------------------------------------------
#disACRU_be <- readTIFF ('../data/distribution/beaudoin2014/Beaudoin_etal_2014_Acer/NFI_MODIS250m_kNN_Species_Acer_Rub_v0.tif')
#disACRU_be [which (disACRU_be < -1e6)] <- NA
  
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
  #coord_map ("lambert", lat0 = 33, lat1 = 45) +
  geom_sf (data = disACRU_ll, fill = '#901c3b33', size = 0) +
  geom_sf (data = disACSH_ll, fill = '#f3bd4833', size = 0) +
  xlab ("Longitude") + ylab ("Latitude") +
  ggtitle('Samples sites across North America', 
          subtitle = paste0 ('for growth of ', nSamples$nTrees,
                             ' maple trees from ',nSamples$nCores,' cores at ',
                             max (siteMetaData$site),' sites')) +
  geom_point (data = siteMetaData, 
              aes (x = lon, y = lat, fill = colour), 
             fill = siteMetaData [['colour']], 
             color = "#444444", shape = 21, size = 2.5) + 
  coord_sf (xlim = c (-98, -50),  ylim = c (26, 55)) +
  theme_minimal (12) #+ theme (legend.position = 'right')
NAmap

#===============================================================================