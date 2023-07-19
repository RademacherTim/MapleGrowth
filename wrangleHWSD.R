#===============================================================================
# Script extracting only the necessary data from Harmonized World Soil Database
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
library ('ncdf4')

# get soil and topographic data from the Harmonized World Soil Database 
#-------------------------------------------------------------------------------
hwsd <- ncdf4::nc_open ("../data/soils/HWSD_1247/data/S_C.nc4")

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