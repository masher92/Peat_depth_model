###
# Define locations to be included in synthetic sample
# Assigned depth values programtically to these locations
# Save to file

##################################################
# Set-up processing environment
##################################################
library(raster)
library(rgdal)
library(leaflet)
library(geoR)
library (gstat)
library(rgeos)
library(geosphere)
library(spcosa)

setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis/")

# Source files containing functions
source("Code/Peat_depth_model-master/4.CreateSyntheticSamples/AssignDepth_functions.R")

##################################################
# Define parameters to be used in synthetic sample creation  
##################################################
# Size of grid spacing (in metres)
grid_spacing <- 250

# For short range subsets:
# The number of samples to include in each cluster
n_per_cluster <- 1
# The distance of the buffer in metres from the point around which the cluster is based within which
# the extra samples should be located
buffer_size = 30

##################################################
# Read in required data
##################################################
# Raster layers containing slope and elevation data
# These are used to define the slope and elevation of the synthtic sample locations
dtm <- raster("Data/Input/DTM/Dales_Nidderdale_Moorland_line_DTM_5m.tif")
slope <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")

# Dataframe containing locations at 5m intervals with slope and elevation values (generated from above raster layers
# in "DefineStudyArea/DefinePtsWithinRegion_withCovariateValues.R")
# This is used in defining locations of extra points to add in synthetic samples with short distance subset
aoi_5mIntervals_pts <- read.csv("Data/Generated/UnmeasuredLocations/humberstone_aoi_df.csv")
# Convert this to a spatial points dataframe
aoi_5mIntervals_pts_spdf <- SpatialPointsDataFrame(coords = aoi_5mIntervals_pts[c(1:2)], data = aoi_5mIntervals_pts[c(3:4)],proj4string =  CRS("+init=epsg:27700"))

# Spatial Polygons Dataframe containing the outline of the study area (trimmed to Moorland Line)
aoi_trimmed <- readOGR(dsn = "Data/Generated/StudyAreaTrimmedToMoorlandLine", layer = "humberstone_aoi_trimmed")

# Spatial points dataframe containing locations of measured peat depth samples and their depth, slope and elevation values 
pd_sample_pts_with_covars <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples_withCovariates", layer = "Humberstone_CleanedPD_withCovariates")

##################################################
# Create synthetic samples:
# 1. Regular grid
# 2. Regular grid with short range subset
# 3. Spatial coverage sample
# 4. Spatial coverage sample with short range subset
##################################################

####################################################################################################
# 1. Regular grid - Create a gridded sample within the boundaries of the study area 
# First a grid is created over the bounding box of the study area, and later trimmed to the actual outline
# of the study area
####################################################################################################
# Make a grid with the spacing between points specified at the top
# This returns dataframe containing lat/long values which are corners of grid
gridded_sample_locations <- makegrid(aoi_trimmed, cellsize = grid_spacing)

# Convert the lat/long points dataframe to a spatial points dataframe
gridded_sample_locations <- SpatialPoints(gridded_sample_locations, proj4string = CRS(proj4string(aoi_trimmed)))

# This grid covers the whole bounding box around AOI_trimmed, need to clip it to extent
gridded_sample_locations <- gridded_sample_locations[aoi_trimmed, ]

# Extract the slope and elevation values from the cell in which the coordinates of the grid
# sample points are found.
# Convert this back to a dataframe
gridded_sample_locations_df <- as.data.frame(gridded_sample_locations)

# This leaves a dataframe of points containing the vertices of the grid, and their slope/elevation values
for (covar in c(dtm, slope)) {
  my.pts <- as.data.frame(extract(covar, gridded_sample_locations))
  gridded_sample_locations_df <- cbind(gridded_sample_locations_df, my.pts)} 

# Rename columns
colnames(gridded_sample_locations_df) <- c('longitude', 'latitude', 'elevation', 'Slope_5m')

### Assign depth values
gridded_sample_df <- assign_depths(pd_sample_pts_with_covars,gridded_sample_locations_df)

# Save to file
write.csv(gridded_sample_df, paste("Data/Generated/SyntheticSamples/Grid/",grid_spacing, "m_spacing.csv", sep = ''),
          row.names = FALSE)

####################################################################################################
# 2. Regular grid, with short distance subset - Create a gridded sample within the boundaries of the study area 
# and then add some points clustered at shorter distances around some of the points.
####################################################################################################
# Convert back to a spatial points dataframe for use in gBuffer
gridded_sample_spdf <- SpatialPointsDataFrame(coords = gridded_sample_locations_df[c(1:2)],
                                               data = gridded_sample_locations_df[c(3:4)],
                                               proj4string =  CRS(proj4string( CRS("+init=epsg:27700"))))

# Define how many points to include as short distance subset; here it is 10% of the total
short_range_subset_size <- NROW(gridded_sample_locations_df)*0.1
# Define the number of points which will be the centre point of the cluster
n_points_to_base_clusters <- short_range_subset_size/n_per_cluster
# Randomly select some of these points to add 
points_to_add_to <- gridded_sample_spdf[sample(1:nrow(gridded_sample_spdf),n_points_to_base_clusters),]

# Create a dataframe to store all of the extra points to add.
short_range_subset <- data.frame()

# Loop through the points around which the extra samples will be located
# For each of these points, create a buffer of "buffer_size"m arond it.
# Search the dataframe containing all the points within AOI at 5m intervals for which we have slope and elevation values
# to define all points from this within the buffer
# Randomy select "n_per_cluster" from the points within this buffer
# Add this to a dataframe storing all the extra points
for (point in 1:length(points_to_add_to)){
  # Print the coordinates
  print(points_to_add_to[point,]@coords)
  # Create a buffer around the point
  shp_buff <- gBuffer(points_to_add_to[point,], width = buffer_size, byid=TRUE, quadsegs=10)
  # Find the points from the AOI which are within the buffer
  pts_inside_buff <- aoi_5mIntervals_pts_spdf[!is.na(over(geometry(aoi_5mIntervals_pts_spdf),geometry(shp_buff))),]
  # Randomly sample X points from within the buffer
  pts_to_add <- pts_inside_buff[sample(1:length(pts_inside_buff),n_per_cluster),]
  # Convert to a dataframe
  pts_to_add_df <- as.data.frame(pts_to_add)
  # Add to dataframe storing all the extra samples
  short_range_subset <- rbind(short_range_subset, pts_to_add_df)
} 

# Rename columns
colnames(short_range_subset)[c(3:4)] <- c('longitude', 'latitude')

# Remove the same number of points from grid as were added in short distance subset
# This is so the number of points in the whole sample is comparable to the straight
# regular grid sample (;however, doing this doesn't necessarily make sense)
# NB: should maybe ensure this doesn't remove any of the points around which the clusters were based
indices_toremove <- sample(1:nrow(gridded_sample_spdf), short_range_subset_size)
gridded_sample_spdf <- gridded_sample_spdf[-indices_toremove,]

# Join the gridded sampe (with the points removed) with the extra short distance subset
gridded_sample <- as.data.frame(gridded_sample_spdf)[c(1:4)]
gridded_sample_with_sr_subset <- rbind(short_range_subset, gridded_sample)

# Convert to spdf
gridded_sample_with_sr_subset <- SpatialPointsDataFrame(coords = gridded_sample_with_sr_subset[c(3,4)], 
                                                        data = gridded_sample_with_sr_subset[c(1,2)],
                                                        proj4string =  CRS("+init=epsg:27700"))

###### Check plotting of sample
aoi_trimmed_wgs84 <- spTransform(aoi_trimmed, CRS("+init=epsg:4326"))
gridded_sample_with_sr_subset_wgs84 <- spTransform(gridded_sample_with_sr_subset, CRS("+init=epsg:4326"))

# Plot
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>%  
  addTiles() %>%
  addPolygons(data = aoi_trimmed_wgs84, fillOpacity = 0, weight = 3) %>%
  addCircles(data = gridded_sample_with_sr_subset, radius = 5, weight = 1, fillOpacity = 1, opacity=1, color = 'green', fillColor = 'green')  %>% 
  addScaleBar(position = c("topright", "bottomright", "bottomleft",
                           "topleft"), options = scaleBarOptions())

# Convert back to df
gridded_sample_sr_locations_df <- as.data.frame(all_samples_spdf)


########## Assign depth values
gridded_sample_sr_df <- assign_depths(pd_sample_pts_with_covars,gridded_sample_sr_locations_df)

##################################################
# 3. Spatial coverage sample
##################################################
myStratification <- stratify(aoi_trimmed, nStrata = 767, priorPoints = NULL, maxIterations = 1000, nTry = 1,
                             equalArea = FALSE, verbose = getOption("verbose"))
mySamplingPattern <- spsample(myStratification, n = 1)

# Plot sampling pattern
plot(myStratification, mySamplingPattern)

# Convert to dataframe
spcosa_sample_locations_df <- as.data.frame(mySamplingPattern@sample)

########## Assign depth values
spcosa_sample_df <- assign_depths(pd_sample_pts_with_covars,spcosa_sample_locations_df)

##################################################
# 4. Spatial coverage sample, with short distance subset
##################################################
myStratification <- stratify(aoi_trimmed, nStrata = 767, priorPoints = NULL, maxIterations = 1000, nTry = 1,
                             equalArea = FALSE, verbose = getOption("verbose"))
mySamplingPattern <- spsample(myStratification, n = 1)
# Plot sampling pattern
plot(myStratification, mySamplingPattern)

# Convert to dataframe
spcosa_sample_sr_locations_df <- as.data.frame(mySamplingPattern@sample)

########## Assign depth values
spcosa_sample_sr_df <- assign_depths(pd_sample_pts_with_covars,spcosa_sample_sr_locations_df)
