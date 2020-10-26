# Set up processing environment
library(raster)
library(rdgdal)

# Set working directory
setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis/")

#######################################################################################
# Create functions for defning locations at 5m intervals in study area.
#######################################################################################
# Set working directory
setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis")

RasterToDF_cropped <- function (aoi_fp, raster_fp) {
  'Clips a raster file to the extent of an AOI
  And converts to a dataframe.
  @param aoi_fp: 
  @param raster_fp: 
  @returns df:' 
  # Read in the raster file and a shape file containing the outline of the AOI
  raster <- raster (raster_fp)
  aoi <- readOGR(dsn = "Data/Input/Site_AOIs", layer = aoi_fp)
  # Crop slope data by extent of sample area AOI.
  # This crops to the square surrounding the AOI, the area outwith the AOI is returned as NA values
  clipped <- crop(raster, extent(aoi), snap = 'out')
  # Mask function hides values which are not actually within the AO (however, values are still present)
  clipped <- mask(clipped, aoi)
  # Convert the raster to a dataframe
  df<-as.data.frame(clipped, xy = TRUE)
  # Completely remove the NA values from the dataframe
  df <-  df[complete.cases(df), ]
  return (df)
}

create_aoi_df <- function (aoi_fp, DTM_filepath, Slope_filepath) {
  # Clip slope and elevation rasters to the AOI and convert to datafames
  elevation_clip_df <- RasterToDF_cropped(aoi_fp, DTM_filepath)
  slope_clip_df <- RasterToDF_cropped(aoi_fp, Slope_filepath)
  
  # Join these together on the x/y coordinate columns and rename columns 
  aoi_df <- merge(elevation_clip_df, slope_clip_df)
  colnames(aoi_df)[c(1,2,3,4)] <- c('coords.x1', 'coords.x2','elevation', 'Slope_5m')
  return (aoi_df)}

#######################################################################################
# Create functions for defning locations at 5m intervals in study area.
#######################################################################################
### Something about using centre coordinates of raster cell
aoi_df <- create_aoi_df ("Humberstone_AOI",
                                "Data/Input/DTM/Dales_Nidderdale_Moorland_line_DTM_5m.tif",
                                "Data/Input/DTM/Dales_Nidderdale_Moorland_line_slope_5m.tif")
# Save
write.csv (aoi_df, file = "Data/Generated/UnmeasuredLocations/humberstone_aoi_df.csv", row.names = FALSE)

