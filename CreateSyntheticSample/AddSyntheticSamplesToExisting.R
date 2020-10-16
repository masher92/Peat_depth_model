library(raster)
library(rgdal)
library(leaflet)
library(geoR)
library(rgeos)
library(geosphere)
library(geoR)


setwd("E:/Msc/Dissertation/Code/Peat_depth_model")

# Source files containing functions
source("Functions/assign_depths.R")
source("Functions/test_sm.R")
source("Functions/cross_validate.R")
source("Functions/analyse_results.R")

load('Data/Generated/MeasuredSamples/HumberstoneMeasuredSampleDataset.RData')
measured_sample <- sample
#measured_sample <- spTransform(measured_sample, CRS("+init=epsg:27700"))
measured_sample_df <- as.data.frame(measured_sample)

# Read in dataframe with all the locations across the study region (at 5m intervals)
aoi_df <- read.csv("Data/Generated/UnmeasuredLocations/humberstone_aoi_df.csv")
# Convert to shapefile
aoi_spdf <- SpatialPointsDataFrame(coords = aoi_df[c(1:2)], data = aoi_df[c(3:4)],proj4string =  CRS("+init=epsg:27700"))

# Define parameters
n_extra_points <- nrow(measured_sample_df) * 0.1
size_of_cluster <- 5
n_points_to_base_clusters <- round(n_extra_points/size_of_cluster,0)
buffer_size = 50

##################################################
# Create extra short range samples
##################################################
# Subset data to just the points which are more likely to contain deeper peat
# Randomly select some of these points to add to
#likely_lads <- measured_sample[measured_sample$elevation > 400 & measured_sample$Slope_5m < 3,] 
#points_to_add_to <- likely_lads[sample(1:nrow(likely_lads),n_points_to_base_clusters),]

points_to_add_to <- measured_sample[sample(1:nrow(measured_sample),n_points_to_base_clusters),]
points_to_add_to <- spTransform(points_to_add_to, CRS("+init=epsg:27700"))

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
  pts_to_add <- pts_inside_buff[sample(1:length(pts_inside_buff),5),]
  # Convert to a dataframe
  pts_to_add_df <- as.data.frame(pts_to_add)
  # Add to dataframe storing all the extra samples
  extra_samples <- rbind(extra_samples, pts_to_add_df)
} 

# rename columns
colnames(extra_samples)[c(3:4)] <- c('longitude', 'latitude')

# COnvert projection
extra_samples_spdf <- SpatialPointsDataFrame(coords = extra_samples[c(3,4)], data = extra_samples[c(1,2)],proj4string =  CRS("+init=epsg:27700"))
extra_samples_spdf <- spTransform(extra_samples_spdf, CRS("+init=epsg:4326"))
extra_samples <- as.data.frame(extra_samples_spdf)

#########################################################################
# Assign depths 
#########################################################################
extra_samples_with_depths <- assign_depths_from_distrib_newmethod('E:/Msc/Dissertation/Code/Peat_depth_model/Data/Generated/MeasuredSamples/HumberstoneMeasuredSampleDataset.RData', extra_samples)

#distances<- sample_with_depths[[2]]
#distances_df <- as.data.frame(distances)
#hist(distances, breaks =100)
extra_samples_with_depths <- extra_samples_with_depths[[1]]
extra_samples_with_depths$sqrtdepth <- sqrt(extra_samples_with_depths$depth)

##################################################
# Join original samples and short range sample
##################################################
# Join together
all_samples <- rbind(extra_samples_with_depths[c(8,1:4)], measured_sample_df)

# Convert to spdf
all_samples_spdf <- all_samples

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

all_samples$sqrtdepth <- sqrt(all_samples$depth)

#########################################################################
# Cross-validate
#########################################################################
results <- cross_validate (all_samples, c('elevation', 'Slope_5m'))
# Create dataframe summarising the bias, RMSE, coverage and interval width
summary_results <- create_results ('Humberstone', results, all_samples)
# Compare the predicted values with the observed values
predicted_vs_observed <- create_predicted_vs_observed (results, all_samples)
cor(predicted_vs_observed$real_values_sqrt, predicted_vs_observed$LM.mean_sqrt)
cor(predicted_vs_observed$real_values_sqrt, predicted_vs_observed$SM.mean_sqrt)

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
# Cant do this stage as it would be testing on the same dataset as was used to fit the model. Overfitting.

leaflet() %>% 
  addTiles() %>%
  addCircles(data = sample_with_depths, radius = 5, weight = 1, fillOpacity = 1, opacity=1, color = 'green', fillColor = 'green')  %>% 
  addScaleBar(position = c("bottomleft"), options = scaleBarOptions())


