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
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/cross_validate.R")
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/check_models.R")
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/analyse_results.R")

# Source files containing functions
source("Code/Peat_depth_model-master/5.CreateSyntheticSamples/AssignDepth_functions.R")

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
original_grid <- create_regular_grid(grid_spacing = 157)
grid_shifts <- shift_grid(grid, grid_spacing = 157, n_shifts = 50)

# Create dataframe to store results
overall_results <- data.frame(Metric = c('bias-LM', 'RMSE-LM', 'Coverage-LM', 'IW-LM', 'CC-LM', 'bias-SM', 'RMSE-SM', 'Coverage-SM', 'IW-SM', 'CC-SM', 'Length'))

## Process
for (i in 1:50){
  print(paste("Model run number:", i))
  # Create a copy of the origal grid
  shifted_grid <- original_grid
  # Shift both the x and y coordinate of the shifted grid by the amount 
  # in the ith row of the grid_shifts dataframe
  shifted_grid@coords[,1] <- shifted_grid@coords[,1] + grid_shifts[i,1]
  shifted_grid@coords[,2] <- shifted_grid@coords[,2] + grid_shifts[i,2]
  
  # Set the crs of shifted grid to be the same as the study area (it should be anyway)
  crs(shifted_grid) <- crs(aoi_trimmed)
  # Trim the grid to the outline of the study area (before this it covers bounding box of area)
  shifted_grid <- shifted_grid[aoi_trimmed, ]
  
  # Convert to dataframe
  shifted_grid_df <- as.data.frame(shifted_grid)
  
  # Join sample locations to slope and elevation data
  # Extract the values from the rasters which match the points in the pd_samples dataset
  for (covar in c(dtm, slope)) {
    my.pts <- as.data.frame(extract(covar, shifted_grid))
    shifted_grid_df <- cbind(shifted_grid_df, my.pts)
  } 
  # NB: Not sure why any values are getting NA?
  shifted_grid_df <- na.omit(shifted_grid_df)
  
  # Rename columns
  colnames(shifted_grid_df) <- c('longitude', 'latitude', 'elevation', 'Slope_5m')
  
  # Convert projection to spdf for use in gBuffer
  shifted_grid_spdf <- SpatialPointsDataFrame(coords = shifted_grid_df[c(1:2)], data = shifted_grid_df[c(3:4)],proj4string =  CRS("+init=epsg:27700"))
  shifted_grid_spdf <- spTransform(shifted_grid_spdf, CRS("+init=epsg:4326"))
  
  # COnvert to dataframe
  shifted_grid_df <- as.data.frame(shifted_grid_spdf)
  
  #########################################################################
  # Assign depths
  #########################################################################
  shifted_grid_df <- assign_depths(pd_sample_pts_with_covars,shifted_grid_df)
  # Add a square root of depth
  shifted_grid_df$sqrtdepth <- sqrt(shifted_grid_df$depth)
  
  ################################################################################
  # Check model performance using the whole dataset
  ################################################################################
  #lm_test <- check_lm(shifted_grid_df, c("elevation", "Slope_5m"))
  #sm_test <- check_sm(shifted_grid_df, c("elevation", "Slope_5m"))
  
  #########################################################################
  # Cross-validate model with shifted grid iteration
  #########################################################################
  # Can't remember reason for this
  # If its divisible by ten add a slightly permutated row
  # if (nrow(shifted_grid_df)%%10 == 0){
  #   random_number <- sample(1:nrow(shifted_grid_df), 1) 
  #   extra_row <- shifted_grid_df[96,]
  #   extra_row$longitude =  extra_row$longitude + 0.0003
  #   shifted_grid_df <- rbind(shifted_grid_df, extra_row)}
  
  print ('Cross validating')
  results <- cross_validate (shifted_grid_df, c('elevation', 'Slope_5m'))
  
  ################################################################################
  # Analyse results
  ################################################################################
  options(scipen=999) # stops use of scientific notations
  
  # Create dataframe summarising the bias, RMSE, coverage and interval width
  summary_results <- create_results (aoi, results, shifted_grid_df)
  # Compare the predicted values with the observed values
  predicted_vs_observed <- create_predicted_vs_observed (results, shifted_grid_df)
  
  ################################################################################
  # Analyse results
  ################################################################################
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

    
 # )
  print("Done") 
}

#setwd("C:/Users/gy17m2a/Desktop/Dissertation/Code/Peat_depth_model")
#write.csv(overall_results, file = "Shuffled_grid/150grid.50runs.csv", row.names = F)
