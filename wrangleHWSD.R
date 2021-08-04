#===============================================================================
# Script extracting only the necessary data from Harmonized World Soil Database
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
library ('raster')
library ('rgdal')
library ('RSQLite')

# get soil and topographic data from the Harmonized World Soil Database 
#-------------------------------------------------------------------------------
hwsd <- raster ("../data/soils/HWSD_RASTER/hwsd.bil")

# add projection information (i.e., Plate Carrée using WGS84 datum according to 
# documentation) to HWSD
#-------------------------------------------------------------------------------
proj4string (hwsd) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# crop to data from Eastern North America
#-------------------------------------------------------------------------------
hwsdENA <- crop (hwsd, extent (c (-110, -50, 25, 60)))

plot (hwsdENA, col = bpy.colors (length (unique (hwsdENA))))

# write an csv file for Eastern North America only
write_csv (hwsd, file = "../data/soils/HWSD_RASTER/HWSD_EasternNorthAmerica.csv")
#===============================================================================