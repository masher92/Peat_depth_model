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
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/clean_pd.R")
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/cross_validate.R")
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/check_models.R")
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/analyse_results.R")

##################################################
# Parameters defining number samples and how they will be used
##################################################
# Size of grid spacing
n_samples <- 766
#grid_spacing <- 150
#n_per_cluster <- 1
#buffer_size = 30

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
#
##################################################
overall_results <- data.frame(Metric = c('bias-LM', 'RMSE-LM', 'Coverage-LM', 'IW-LM', 'CC-LM', 'bias-SM', 'RMSE-SM', 'Coverage-SM', 'IW-SM', 'CC-SM'))

for (i in (1:2)){
  print(i)
  ##################################################
  # Create SpCosa sample
  ##################################################
  myStratification <- stratify(aoi_trimmed, nStrata = n_samples, priorPoints = NULL, maxIterations = 1000, nTry = 1,
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
  sample_with_depths <- assign_depths_from_distrib_newmethod(sample, all_samples)
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
tryCatch(
    {results <- cross_validate (sample_with_depths, c('elevation', 'Slope_5m'))
    options(scipen=999)
    # Create dataframe summarising the bias, RMSE, coverage and interval width
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
    colnames(lm_results) <- c('Metric', i)
    sm_results <- summary_results[c(1,2)]
    sm_results <- data.frame(results_metric = c('bias-SM', 'RMSE-SM', 'Coverage-SM', 'IW-SM', 'CC-SM'),summary_results[c(2)] )
    colnames(sm_results) <- c('Metric', i)
    results_reshaped <- rbind(lm_results, sm_results)
    
    # save to overall resutls table
    overall_results <- cbind (overall_results, results_reshaped[2])   
      'no error'
    },
    error = function(e){
      'error'
    }
)}

setwd("E:/Msc/Dissertation/Code/Peat_depth_model")
write.csv(overall_results, file = "SPCosa/766,spcosa.8runs.csv", row.names =F)


  

