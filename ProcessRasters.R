library(raster)
library(rdgdal)
install.packages("rgdal")

# Set working directory
setwd("E:/Msc/Dissertation/Code/Data")

# Read in  raster
slope <-raster("E:/Msc/Dissertation/Code/Data/Generated/humberstone.asc")
plot(slope)
plot()
elevation <-raster("Input/DTM/Dales_Nidderdale_Moorland_line_DTM_5m.tif")
plot(elevation)
crs(elevation)

crop_raster_by_aoi <- function (area_aoi, file_to_clip) {
  # read in AOI
  aoi <- readOGR(dsn = "Input/Site_AOIs", layer = "Humberstone_AOI")
  # Crop slope data by extent of sample area AOI
  # THis just returns the square covering the AOI (missing spaces are due to moorland line)
  clipped <- crop(file_to_clip, extent(aoi), snap = 'out')
  #As a final step, you need to identify those pixels of your elevation raster that lie within the borders of the given state polygons. Use the 'mask' function for that.
  #This doesn't actually remove these values, they are still in the raster but just masked
  clipped <- mask(clipped, aoi)
  return (clipped)
  }

#Humberstone
elevation_clip <- crop_raster_by_aoi("Humberstone_AOI", elevation)
slope_clip <- crop_raster_by_aoi("Stake_Moss_AOI", slope)

# Check plotting
plot(elevation_clip)

# Change projection
slope_clip <- projectRaster(slope_clip, crs = "+proj=longlat +datum=WGS84 +no_defs")
elevation_clip <- projectRaster(elevation_clip, crs = "+proj=longlat +datum=WGS84 +no_defs")

# Save to file
writeRaster(slope_clip, "Generated/Humberstone_slope.tif")
writeRaster(elevation_clip, "Generated/Humberstone_elevation.tif")

# Convert to dataframe
slope <- raster("Generated/Humberstone_slope.tif")
elevation <- raster ("Generated/Humberstone_elevation.tif")
plot(elevation)
datframe_slope <-as.data.frame(slope, xy = TRUE)
datframe_elevation <-as.data.frame(elevation, xy = TRUE)
#Join them together on the x/y coordinate columns. 
datframe_slope_elevation <- merge(datframe_elevation, datframe_slope)

#Rename columns
colnames(datframe_slope_elevation)[c(1,2,3,4)] <- c('coords.x1', 'coords.x2','elevation', 'Slope_5m')

# Keep only the values with no NA in either
# This is necessary due to the way the rasters for slope and elevation are generated
# i.e. using mask which seems to only hide unneeded raster cells when they are plotted but
# not to delete them from the underlying dataframe
datframe_slope_elevation <-  datframe_slope_elevation[complete.cases(datframe_slope_elevation), ]
#Rename columns
colnames(datframe_slope_elevation)[c(1,2,3,4)] <- c('coords.x1', 'coords.x2','elevation', 'Slope_5m')

# Save
write.csv (datframe_slope_elevation, file = "Generated/humberstone.csv", row.names = FALSE)


# Convert slope and elevation rasters to dataframe
datframe_slope <-as.data.frame(slope_clip, xy = TRUE)



datframe_slope_elevation <-  datframe_slope_elevation[complete.cases(datframe_slope_elevation), ]

# Make stacks
humberstone_stack <- stack(humberstone_slope, humberstone_elevation)
names(humberstone_stack)[1] <- 'Slope_5m'
names(humberstone_stack)[2] <- 'elevation'