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
library(plyr)

setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis/")

# Source files containing functions
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/clean_pd.R")
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/cross_validate.R")
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/check_models.R")
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/analyse_results.R")

##################################################
# Parameters defining number samples and how they will be used
##################################################
grid_spacing = 157

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
# These are boundaries within which to construct the synthetic dataset 
aoi_trimmed <- readOGR(dsn = "Data/Generated/StudyAreaTrimmedToMoorlandLine", layer = "humberstone_aoi_trimmed")

# Spatial points dataframe containing locations of measured peat depth samples and their depth, slope and elevation values 
pd_sample_pts_with_covars <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples_withCovariates", layer = "Humberstone_CleanedPD_withCovariates")

##################################################
# Create gridded samples
# 
##################################################
# Coordinates of a larger area around the AOI
coords = matrix(c(-1.896798, 54.061784,
                  -1.788280, 54.066396,
                  -1.779337,54.023056, 
                  -1.889167, 54.014181), 
                ncol = 2, byrow = TRUE)

# Convert to spatialpolygon
bbox = Polygon(coords)
bbox = SpatialPolygons(list(Polygons(list(bbox), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Convert projection - needs to be in BNG for metres
bbox <- spTransform(bbox, CRS("+init=epsg:27700"))

# Convert to grid
grid <- spsample(bbox, cellsize = grid_spacing, type = "regular")

# Define the range of coordinates possible
long_range <- seq(0,grid_spacing, by =5)
lat_range <-  seq(0,grid_spacing, by =5)

# Create dataframe to store results
all_positions <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Long", "Lat"))

# Create a dataframe containing amounts to shift x, y coordiantes
# Each should be unique
for (i in (1:80)){
  position <- data.frame(sample(long_range,1), sample(lat_range,1))
  if (nrow(match_df(all_positions, position))==0){
    all_positions <- rbind(all_positions, position)}}

# Trim to be just 50 long
all_positions <- all_positions[c(1:50),]

# Create dataframe to store results
overall_results <- data.frame(Metric = c('bias-LM', 'RMSE-LM', 'Coverage-LM', 'IW-LM', 'CC-LM', 'bias-SM', 'RMSE-SM', 'Coverage-SM', 'IW-SM', 'CC-SM', 'Length'))

## Process
for (i in 1:25){
  print(paste("Model run number:", i))
  grid_copy <- grid
  # Shift the grid by the amount needed
  grid_copy@coords[,1] <- grid_copy@coords[,1] + all_positions[i,1]
  grid_copy@coords[,2] <- grid_copy@coords[,2] + all_positions[i,2]
 # tryCatch(
  #  {
      crs(grid_copy) <- crs(aoi_trimmed)
      grid_copy <- grid_copy[aoi_trimmed, ]
      
      # Grid covers the whole bounding box around AOI_trimmed, need to clip it to extent
      grid_copy <- grid_copy[aoi_trimmed, ]
      
      # Convert to dataframe
      gridded_samples <- as.data.frame(grid_copy)
      
      # Join sample locations to slope and elevation data
      # Extract the values from the rasters which match the points in the pd_samples dataset
      for (covar in c(dtm, slope)) {
        my.pts <- as.data.frame(extract(covar, grid_copy))
        gridded_samples <- cbind(gridded_samples, my.pts)
      } 
      gridded_samples <- na.omit(gridded_samples)
      
      # Rename columns
      colnames(gridded_samples) <- c('longitude', 'latitude', 'elevation', 'Slope_5m')
      
      # Convert projection to spdf for use in gBuffer
      gridded_samples_spdf <- SpatialPointsDataFrame(coords = gridded_samples[c(1:2)], data = gridded_samples[c(3:4)],proj4string =  CRS("+init=epsg:27700"))
      
      gridded_samples_spdf <- spTransform(gridded_samples_spdf, CRS("+init=epsg:4326"))
      
      # COnvert to dataframe
      all_samples <- as.data.frame(gridded_samples_spdf)
      
      #########################################################################
      # Assign depths using spatial model
      #########################################################################
      sample_with_depths <- assign_depths_from_distrib_newmethod(sample, all_samples)
      
      sample_with_depths <- sample_with_depths[[1]]
      sample_with_depths$sqrtdepth <- sqrt(sample_with_depths$depth)
      
      # If its divisible by ten add a slightly permutated row
      if (nrow(sample_with_depths)%%10 == 0){
        random_number <- sample(1:nrow(sample_with_depths), 1) 
        extra_row <- sample_with_depths[96,]
        extra_row$longitude =  extra_row$longitude + 0.0003
        sample_with_depths <- rbind(sample_with_depths, extra_row)
      }
      print ('Cross validating')
      #########################################################################
      # Cross-validates
      #########################################################################
      results <- cross_validate (sample_with_depths, c('elevation', 'Slope_5m'))
      options(scipen=999)
      summary_results <- create_results ('Humberstone', results, sample_with_depths)
      
      # Compare the predicted values with the observed values
      predicted_vs_observed <- create_predicted_vs_observed (results, sample_with_depths)
      
      # Add correlation coefficient to the results
      cc_results <- data.frame(results_metric = 'CC', Humberstone.sm = cor(predicted_vs_observed$real_values, predicted_vs_observed$SM.mean),
                               Humberstone.lm = cor(predicted_vs_observed$real_values, predicted_vs_observed$LM.mean))
      summary_results <- rbind (summary_results, cc_results )
      
      
      # Reshape the results, so each column is a model run
      lm_results <- summary_results[c(1,3)]
      lm_results <- data.frame(results_metric = c('bias-LM', 'RMSE-LM', 'Coverage-LM', 'IW-LM', 'CC-LM'),summary_results[c(3)] )
      colnames(lm_results) <- c('Metric', 'Value')
      sm_results <- summary_results[c(1,2)]
      sm_results <- data.frame(results_metric = c('bias-SM', 'RMSE-SM', 'Coverage-SM', 'IW-SM', 'CC-SM'),summary_results[c(2)] )
      colnames(sm_results) <- c('Metric', 'Value')
      results_reshaped <- rbind(lm_results, sm_results)
      
      # Add a column with the number of samples
      results_reshaped <- rbind(results_reshaped, data.frame(Metric = 'Number Samples', Value = nrow(sample_with_depths)))
      
      overall_results <- cbind (overall_results, results_reshaped[2]) 
      
      #filename = paste(as.character(grid_spacing), "grid_", as.character(i), '.csv', sep ='')
      # setwd("C:/Users/gy17m2a/Desktop/Dissertation/Code/Peat_depth_model")
      #  write.csv(results_reshaped, file = filename, row.names =F)
      
   #   'no error'
   # },
  #  error = function(e){
  #    'error'
  #  }
    
 # )
  print("Done") 
}

#setwd("C:/Users/gy17m2a/Desktop/Dissertation/Code/Peat_depth_model")
#write.csv(overall_results, file = "Shuffled_grid/150grid.50runs.csv", row.names = F)
