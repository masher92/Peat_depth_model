################################################################################
# Set up processing environment
################################################################################
library(rgdal)
library(geoR)
library(dplyr)
library(geosphere)
library(leaflet)
library (rgeos)
library(gdistance)

# Set working directory
setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis")
# Define parameters
aoi <- "Humberstone"
covar_names <- c("elevation", "Slope_5m") # Names of variables to be used in model 

################################################################################
# Define function
################################################################################
join_topographic_data <- function (pd_samples_shp, covars, covar_names) {
"
Description
----------
     Joins a shapefile containing locations with peat depth measurements, with values from rasters (slope and elevation)
Parameters
----------
    pd_samples_shp: Spatial Points Data Frame (spdf)
        An spdf contaiing locations with peat depth measurements
    covars: list
        A list containing rasters from which to extract the values (slope and elevation) 
    covar_names: list
        A list containing strings of the names of the rasters (slope and elevation)
Returns
-------
    shp: Spatial Points Data Frame (spdf)
       A spdf containing x/y coordinates, peat depth values and all the values from the rasters of interest
"    
  # Create dataframe
  df <- data.frame(pd_samples_shp)
  # Extract the values from the rasters which match the points in the pd_samples dataset
  for (covar in covars) {
    my.pts <- as.data.frame(extract(covar, pd_samples_shp))
    df <- cbind(df, my.pts)
  } 
  # Rename columns
  colnames(df)[c(1:3, 5:ncol(df))] <- c('depth', 'longitude', 'latitude', covar_names)
  
  # Convert back to shapefile
  shp <- SpatialPointsDataFrame(coords = df[c(2,3)], data = df[c(1,5:ncol(df))],
                                proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs
                                                  +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
  return(shp)
}


################################################################################
# Read in required data
################################################################################
peat_depth_samples <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples", layer = paste(aoi, "PeatDepthPoints_cleaned", sep = ''))
dtm <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_DTM_5m.tif")
slope <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")

################################################################################
# Process data
################################################################################
# Remove any NA depth values
sample <- peat_depth_samples[!is.na(peat_depth_samples@data$Depth),]

# Join to covariates
sample <- join_topographic_data(sample, c(dtm, slope), covar_names)

###### Check plotting of sample
aoi_trimmed_wgs84 <- spTransform(aoi_trimmed, CRS("+init=epsg:4326"))
sample_wgs84 <- spTransform(sample, CRS("+init=epsg:4326"))

# Plot
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>%  
  addTiles() %>%
  addPolygons(data = aoi_trimmed_wgs84, fillOpacity = 0, weight = 3) %>%
  addCircles(data = sample_wgs84, radius = 5, weight = 1, fillOpacity = 1, opacity=1, color = 'green', fillColor = 'green')  %>% 
  addScaleBar(position = c("topright", "bottomright", "bottomleft",
                           "topleft"), options = scaleBarOptions())


################################################################################
# Save sample
################################################################################
writeOGR(sample, dsn = "Data/Generated/CleanedPeatDepthSamples_withCovariates" , layer = "Humberstone_CleanedPD_withCovariates", driver="ESRI Shapefile")





