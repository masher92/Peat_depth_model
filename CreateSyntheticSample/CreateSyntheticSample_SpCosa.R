##################################################
# Set-up processing environment
##################################################
library(raster)
library(rgdal)
library(leaflet)
library(geoR)
library (gstat)
library(spcosa)
library(rgeos)
library(geosphere)

setwd("E:/Msc/Dissertation/Code/Peat_depth_model")

source("Functions/assign_depths.R")
source("Functions/test_sm.R")
source("Functions/cross_validate.R")
source("Functions/analyse_results.R")

##################################################
# Define parameters
##################################################
# Name of the study region ('Humberstone', 'Melbecks', 'Newhouse' 'Stakemoss')
aoi_name <- 'Humberstone'
# Factor = 'sample_distance' or 'sample_number'
deciding_factor <- 'sample_number'
# Distance required between points on an equally spaced grid
samples_distance <- 100
# Or the number of samples required. 
n_samples <- 641

##################################################
# Read in data
##################################################
# Shapefile with extent of Moorland line
ml <- readOGR(dsn = "Data/Input/Moorland_Line_and_Project_Area", layer = "180208_Dales_and_Nidderdale_Moorland_OSGB")
# Shapefile containing extent of the AOI
aoi <- readOGR(dsn = "Data/Input/Site_AOIs", layer = paste(aoi_name, '_AOI', sep =''))
# Tif files containing slope and elevation data.
dtm <- raster("Data/Input/DTM/Dales_Nidderdale_Moorland_line_DTM_5m.tif")
slope <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")

##################################################
# Determine the boundaries within which to construct the synthetic dataset 
##################################################
# Trim AOI to within the Moorland line (to ensure exclusion of areas which aren't peatland)
ml_buff <- gBuffer(ml, byid=TRUE, width=0)
aoi_buff <- gBuffer(aoi, byid=TRUE, width=0)
aoi_trimmed <- crop(ml_buff, aoi_buff)
aoi_trimmed_wgs84<- spTransform(aoi_trimmed, CRS("+init=epsg:4326"))


myStratification <- stratify(aoi_trimmed_wgs84, nStrata = 767, priorPoints = NULL, maxIterations = 1000, nTry = 1,
                             equalArea = FALSE, verbose = getOption("verbose"))
mySamplingPattern <- spsample(myStratification, n = 1)
# plot sampling pattern
plot(myStratification, mySamplingPattern)

# TO dataframe
synthetic_dataset <- as.data.frame(mySamplingPattern@sample)
# TO SPDF
synthetic_dataset_spdf <- SpatialPointsDataFrame(coords = synthetic_dataset[c(1:2)], data = synthetic_dataset[c(1,2)],proj4string =  CRS("+init=epsg:4326"))
synthetic_dataset_spdf <- spTransform(synthetic_dataset_spdf, CRS("+init=epsg:27700"))

##################################################
# Join to slope and elevation data
##################################################
# Extract the values from the rasters which match the points in the pd_samples dataset
for (covar in c(dtm, slope)) {
  my.pts <- as.data.frame(extract(covar, synthetic_dataset_spdf))
  synthetic_dataset <- cbind(synthetic_dataset, my.pts)
} 
synthetic_dataset <- na.omit(synthetic_dataset)
# Rename columns
colnames(synthetic_dataset) <- c('longitude', 'latitude', 'elevation', 'Slope_5m')

# COnvert projection back to WGS84
#synthetic_dataset <- SpatialPointsDataFrame(coords = synthetic_dataset[c(1:2)], data = synthetic_dataset[c(3:4)],proj4string =  CRS("+init=epsg:27700"))
#synthetic_dataset <- spTransform(synthetic_dataset, CRS("+init=epsg:4326"))
#synthetic_dataset <- as.data.frame(synthetic_dataset)

all_samples <- synthetic_dataset

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
output_folder_fp <- paste("E:/Msc/Dissertation/Code/Peat_depth_model/Data/Generated/SyntheticSamples/", aoi_name, sep = '')
if (dir.exists(output_folder_fp) == FALSE) {
  dir.create(output_folder_fp)}
filename <- paste(aoi_name, 'SyntheticSampleGridded', samples_distance , 'm.csv', sep ='_')
write.csv(synthetic_dataset, paste(output_folder_fp, filename, sep ='/'), row.names =  FALSE)




filename = 'test.csv'



