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
grid_spacing <- 250

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
gridded_samples_spdf <- spTransform(gridded_samples_spdf, CRS("+init=epsg:4326"))

all_samples <- as.data.frame(gridded_samples_spdf)

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

aoi_trimmed <- spTransform(aoi_trimmed, CRS("+init=epsg:4326"))
leaflet() %>% 
  addTiles() %>%
  addPolygons (data= aoi_trimmed, fillOpacity =0) %>%
  addCircles(data = sample_with_depths)  %>% 
  addScaleBar(position = c("bottomleft"), options = scaleBarOptions())

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
output_folder_fp <- "E:/Msc/Dissertation/Code/Peat_depth_model/Data/Generated/GridTesting/Humberstone"
if (dir.exists(output_folder_fp) == FALSE) {
  dir.create(output_folder_fp)}
filename <- "100mGrid" 
save(results_ls, file = paste(output_folder_fp, '/', filename, '.RData', sep = '' ))





load("E:/Msc/Dissertation/Code/Peat_depth_model/Data/Generated/GridTesting/Humberstone/200mGrid.RData")
results <- results_ls[[1]]
sample_with_depths <- results_ls[[2]]
