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

setwd("E:/Msc/Dissertation/Code/Peat_depth_model")

# Source files containing functions
source("Functions/assign_depths.R")
source("Functions/test_sm.R")
source("Functions/cross_validate.R")
source("Functions/analyse_results.R")

##################################################
# Define how the avilable sample numbers will be used
##################################################
# Size of grid spacing
grid_spacing <- 150
n_per_cluster <- 1
buffer_size = 30

##################################################
# Read in data
##################################################
# Shapefile with extent of Moorland line
ml <- readOGR(dsn = "Data/Input/Moorland_Line_and_Project_Area", layer = "180208_Dales_and_Nidderdale_Moorland_OSGB")
# Shapefile containing extent of the AOI
aoi <- readOGR(dsn = "Data/Input/Site_AOIs", layer = 'Humberstone_AOI')
# Tif files containing slope and elevation data.
dtm <- raster("Data/Input/DTM/Dales_Nidderdale_Moorland_line_DTM_5m.tif")
slope <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")

# Read in dataframe with all the locations across the study region (at 5m intervals)
aoi_df <- read.csv("Data/Generated/UnmeasuredLocations/humberstone_aoi_df.csv")
# Convert to shapefile
aoi_spdf <- SpatialPointsDataFrame(coords = aoi_df[c(1:2)], data = aoi_df[c(3:4)],proj4string =  CRS("+init=epsg:27700"))

##################################################
# Determine the boundaries within which to construct the synthetic dataset 
##################################################
# Trim AOI to within the Moorland line (to ensure exclusion of areas which aren't peatland)
ml_buff <- gBuffer(ml, byid=TRUE, width=0)
aoi_buff <- gBuffer(aoi, byid=TRUE, width=0)
aoi_trimmed <- crop(ml_buff, aoi_buff)

##################################################
# Create gridded samples
##################################################
# Multiplying the n_samples value by 2.45 is found to result in a grid with approximately the right number samples
#grid <- makegrid(aoi_trimmed, n = n_gridded_samples*2.45)
grid <- makegrid(aoi_trimmed, cellsize = grid_spacing)

# Convert to spdf
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(aoi_trimmed)))
# Grid covers the whole bounding box around AOI_trimmed, need to clip it to extent
grid <- grid[aoi_trimmed, ]

# Convert to dataframe
gridded_samples <- as.data.frame(grid)

# Join sample locations to slope and elevation data
# Extract the values from the rasters which match the points in the pd_samples dataset
for (covar in c(dtm, slope)) {
  my.pts <- as.data.frame(extract(covar, grid))
  gridded_samples <- cbind(gridded_samples, my.pts)
} 
#gridded_samples <- na.omit(gridded_samples)

# Rename columns
colnames(gridded_samples) <- c('longitude', 'latitude', 'elevation', 'Slope_5m')

# Convert projection to spdf for use in gBuffer
gridded_samples_spdf <- SpatialPointsDataFrame(coords = gridded_samples[c(1:2)], data = gridded_samples[c(3:4)],proj4string =  CRS("+init=epsg:27700"))
#gridded_samples_spdf <- spTransform(gridded_samples_spdf, CRS("+init=epsg:4326"))

##################################################
# Create extra short range samples
##################################################


# Subset data to just the points which are more likely to contain deeper peat
#likely_lads <- gridded_samples_spdf[gridded_samples_spdf$elevation > 375 & gridded_samples_spdf$Slope_5m < 5,] 

#m Decide how manye extra points needed
extra_points_n <- NROW(gridded_samples)*0.1
n_points_to_base_clusters <- extra_points_n/n_per_cluster

# Randomly select some of these points to add to
#points_to_add_to <- likely_lads[sample(1:nrow(likely_lads),n_points_to_base_clusters),]
points_to_add_to <- gridded_samples_spdf[sample(1:nrow(gridded_samples_spdf),n_points_to_base_clusters),]

# Create a dataframe to store all of the extra points to add.
extra_samples <- data.frame()

# Loop through the points around which the extra samples will be located
for (point in 1:length(points_to_add_to)){
  # Print the coordinates
  print(points_to_add_to[point,]@coords)
  # Create a buffer around the point
  shp_buff <- gBuffer(points_to_add_to[point,], width = buffer_size, byid=TRUE, quadsegs=10)
  # Find the points from the AOI which are within the buffer
  pts_inside_buff <- aoi_spdf[!is.na(over(geometry(aoi_spdf),geometry(shp_buff))),]
  # Randomly sample X points from within the buffer
  pts_to_add <- pts_inside_buff[sample(1:length(pts_inside_buff),n_per_cluster),]
  # Convert to a dataframe
  pts_to_add_df <- as.data.frame(pts_to_add)
  # Add to dataframe storing all the extra samples
  extra_samples <- rbind(extra_samples, pts_to_add_df)
} 

# rename columns
colnames(extra_samples)[c(3:4)] <- c('longitude', 'latitude')

##################################################
# Remove points from grid
##################################################
indices <- sample(1:nrow(gridded_samples_spdf), extra_points_n)
gridded_samples_spdf <- gridded_samples_spdf[-indices,]

##################################################
# Join gridded samples and short range sample
##################################################
gridded_samples <- as.data.frame(gridded_samples_spdf)[c(1:4)]

# Join together
all_samples <- rbind(extra_samples, gridded_samples)

# Convert to spdf
all_samples_spdf <- SpatialPointsDataFrame(coords = all_samples[c(3,4)], data = all_samples[c(1,2)],proj4string =  CRS("+init=epsg:27700"))
all_samples_spdf <- spTransform(all_samples_spdf, CRS("+init=epsg:4326"))

# Plot
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>%  
  addTiles() %>%
  #addPolygons(data = aoi_trimmed_wgs84, fillOpacity = 0, weight = 3) %>%
  addCircles(data = all_samples_spdf, radius = 5, weight = 1, fillOpacity = 1, opacity=1, color = 'green', fillColor = 'green')  %>% 
  addScaleBar(position = c("topright", "bottomright", "bottomleft",
                           "topleft"), options = scaleBarOptions())


# Convert back to df
all_samples <- as.data.frame(all_samples_spdf)

#########################################################################
# Assign depths using spatial model
#########################################################################
#sample_with_depths_sm <- assign_depths_using_sm ('Data/Generated/MeasuredSamples/HumberstoneMeasuredSampleDataset.RData', all_samples)
#sample_with_depths <- assign_depths_from_distrib('Data/Generated/MeasuredSamples/HumberstoneMeasuredSampleDataset.RData', all_samples)
sample_with_depths <- assign_depths_from_distrib_newmethod('E:/Msc/Dissertation/Code/Peat_depth_model/Data/Generated/MeasuredSamples/HumberstoneMeasuredSampleDataset.RData', all_samples)

#distances<- sample_with_depths[[2]]
#distances_df <- as.data.frame(distances)
#hist(distances, breaks =100)
sample_with_depths <- sample_with_depths[[1]]
sample_with_depths$sqrtdepth <- sqrt(sample_with_depths$depth)

#########################################################################
# Cross-validate
#########################################################################
results <- cross_validate (sample_with_depths, c('elevation', 'Slope_5m'))
# Create dataframe summarising the bias, RMSE, coverage and interval width
summary_results <- create_results ('Humberstone', results, sample_with_depths)
# Compare the predicted values with the observed values
predicted_vs_observed <- create_predicted_vs_observed (results, sample_with_depths)
cor(predicted_vs_observed$real_values, predicted_vs_observed$LM.mean)
cor(predicted_vs_observed$real_values, predicted_vs_observed$SM.mean)

plot ( predicted_vs_observed$real_values,predicted_vs_observed$LM.mean, main = paste('Linear model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$LM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

plot ( predicted_vs_observed$real_values,predicted_vs_observed$SM.mean, main = paste('Spatial model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$SM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

#########################################################################
# Fit a spatial model using the synthetic sample.
# Use this to predict depths for the measured samples dataset.
#########################################################################
predictions <- fit_spatial_model_and_predict (sample_with_depths,'E:/Msc/Dissertation/Code/Peat_depth_model/Data/Generated/MeasuredSamples/HumberstoneMeasuredSampleDataset.RData' )

#########################################################################
# Plot results
#########################################################################
ggplot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  xlab('Observed depth (cm)')+
  ylab('Predicted depth (cm)')+
  geom_abline(intercept = 0) +
  ylim(0, 400) + xlim(0,400) +
  ggtitle(paste('CC = ' , round(cor(predictions$depth, predictions$prediction),2)))+
  geom_point(data = predictions, size=1,  color = '#56B4E9', aes(x=depth, y=prediction)) 


cor(predictions$sqrtdepth, predictions$sqrt_prediction)

#########################################################################
# Save
#########################################################################
results_ls <- list(results, sample_with_depths)
output_folder_fp <- "E:/Msc/Dissertation/Code/Peat_depth_model/Data/Generated/GridTestingPlusShortRange/"
if (dir.exists(output_folder_fp) == FALSE) {
  dir.create(output_folder_fp)}
filename <- "200mGrid_30mB_3pc_likelylads" 
save(results_ls, file = paste(output_folder_fp, '/', filename, '.RData', sep = '' ))


#load("E:/Msc/Dissertation/Code/Peat_depth_model/Data/Generated/GridTesting/Humberstone/200mGrid.RData")
#results <- results_ls[[1]]
#sample_with_depths <- results_ls[[2]]


extra <- sample_with_depths[30,]
extra$longitude <- extra$longitude + 0.000002
extra$latitude <- extra$latitude + 0.000002
sample_with_depths <- rbind(sample_with_depths, extra)
