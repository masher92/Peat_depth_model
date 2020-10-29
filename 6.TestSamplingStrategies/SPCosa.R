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
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/cross_validate.R")
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/check_models.R")
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/analyse_results.R")
source("Code/Peat_depth_model-master/6.TestSamplingStrategies/CreateSyntheticSamples_functions.R")

##################################################
# Parameters defining number samples and how they will be used
##################################################
# Area in which samples will be constructed
aoi = 'Humberstone'

# Size of grid spacing
grid_spacing = 100
# Projection to use in the CV
proj_for_cv = 'wgs84'

# Number of samples to be equivalent to the number of samples in a regular grid
if (grid_spacing == 100) { 
  n_samples <- 766  # this equates to the number of samples in a regular grid with 10m spacing
} else if (grid_spacing == 150) {
  n_samples = 500
} 

##################################################
# Read in required data
##################################################
# Raster layers containing slope and elevation data
# These are used to define the slope and elevation of the synthtic sample locations
dtm <- raster("Data/Input/DTM/Dales_Nidderdale_Moorland_line_DTM_5m.tif")
slope <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")

# Spatial Polygons Dataframe containing the outline of the study area (trimmed to Moorland Line)
# These are boundaries within which to construct the synthetic dataset 
aoi_trimmed <- readOGR(dsn = "Data/Generated/StudyAreaTrimmedToMoorlandLine", layer = "humberstone_aoi_trimmed")

# Spatial points dataframe containing locations of measured peat depth samples and their depth, slope and elevation values 
pd_sample_pts_with_covars <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples_withCovariates", layer = "Humberstone_CleanedPD_withCovariates")

##################################################
# Iterate through 50 different versions of a spatial coverage sample
# and record the results
##################################################
overall_results <- data.frame(Metric = c('bias-LM', 'RMSE-LM', 'Coverage-LM', 'IW-LM', 'CC-LM', 'bias-SM', 'RMSE-SM', 'Coverage-SM', 'IW-SM', 'CC-SM'))

# Iterate through 50 variations of the spatial coverage sample; each time the stratify function
# is run the results are different (I think)
for (i in (1:50)){
  print(paste("Spatial coverage sample number:", i))
  
  ##################################################
  # Create SPCosa sample
  ##################################################
  # Split the study area into compact strata - AOI_trimmed must be provided in WGS84
  myStratification <- stratify(spTransform(aoi_trimmed, CRS("+init=epsg:4326")), nStrata = n_samples, priorPoints = NULL, maxIterations = 1000, nTry = 1,
                               equalArea = FALSE, verbose = getOption("verbose"))
  # Select one sample point in each of the strata
  mySamplingPattern <- spsample(myStratification, n = 1)

  # Convert the output of spcosa to dataframe/spatial points dataframe
  spcosa_df <- as.data.frame(mySamplingPattern@sample)
  spcosa_spdf <- SpatialPointsDataFrame(coords = spcosa_df[c(1:2)], data = spcosa_df[c(1,2)],proj4string =  CRS("+init=epsg:4326"))

  #########################################################################
  # Extract slope and elevation values of synthetic sample locations
  # and join to coordinates to create spatial points dataframe
  #########################################################################
  # Convert projection to match slope and elevation
  spcosa_spdf <- spTransform(spcosa_spdf, CRS("+init=epsg:27700"))
  # Find slope and elevation value of points
  slopes <- data.frame(extract(slope, spcosa_spdf))
  elevations <- data.frame(extract(dtm, spcosa_spdf))
  # Join slope, elevation values with coords to create spdf
  spcosa_spdf <- SpatialPointsDataFrame(coords =  data.frame(longitude = spcosa_spdf@coords[,1], latitude = spcosa_spdf@coords[,2]),
                                              data =  data.frame("Slope_5m" =slopes[,1], "elevation" = elevations[,1]), 
                                              proj4string =  CRS("+init=epsg:27700"))
  # Keep only values that don't have NA for slope and elevation
  # Points get NA because they are outside the Moorland Line - ideally these points wouldn't be incldued inititally 
  spcosa_spdf <- spcosa_spdf[!is.na(spcosa_spdf$Slope_5m),]
  
  ##################################################
  # Check plotting of sample
  ##################################################
  # Plot
  leaflet() %>% 
    addProviderTiles(providers$OpenStreetMap) %>%  
    addTiles() %>%
    addPolygons(data = spTransform(aoi_trimmed, CRS("+init=epsg:4326")), fillOpacity = 0, weight = 3) %>%
    addCircles(data = spTransform(spcosa_spdf, CRS("+init=epsg:4326")), radius = 5, weight = 1, fillOpacity = 1, opacity=1, color = 'black', fillColor = 'black')  %>% 
    addScaleBar(position = c("topright", "bottomright", "bottomleft",
                             "topleft"), options = scaleBarOptions())
  
  #########################################################################
  # Assign depths
  # This also converts spdf to a dataframe, and to WGS84 projection
  #########################################################################
  # Assign depths - must be a dataframe, with coordinates in wgs84 for disthaversine function  
  # A dataframe is also returned 
  spcosa_df <- assign_depths(pd_sample_pts_with_covars,
                             as.data.frame(spTransform(spcosa_spdf, CRS("+init=epsg:4326"))))
  # Add a square root of depth
  spcosa_df$sqrtdepth <- sqrt(spcosa_df$depth)
  
  ################################################################################
  # Check model performance using the whole dataset
  ################################################################################
  #lm_test <- check_lm(spcosa_df, c("elevation", "Slope_5m"))
  #sm_test <- check_sm(spcosa_df, c("elevation", "Slope_5m"))
  
  #########################################################################
  # Cross-validate model with this iteration of spatial coverge dataset
  #########################################################################
  if (proj_for_cv == 'bng') { 
    spcosa_spdf =  SpatialPointsDataFrame(coords = spcosa_df[c(1:2)], data = spcosa_df[c(3:9)],proj4string =  CRS("+init=epsg:4326"))
    spcosa_spdf = spTransform(spcosa_spdf, CRS("+init=epsg:27700"))
    spcosa_df = as.data.frame(spcosa_spdf)  
  } 
  
  print ('Cross validating')
  results <- cross_validate (spcosa_df, c('elevation', 'Slope_5m'))
  
  ################################################################################
  #Analyse results
  ################################################################################
  options(scipen=999) # stops use of scientific notations
  
  # Compare the predicted values with the observed values
  predicted_vs_observed <- create_predicted_vs_observed (results, spcosa_df)
  
  # Create dataframe summarising the bias, RMSE, coverage and interval width
  summary_results <- create_results (aoi, results, spcosa_df)
  # Add correlation coefficient to the results
  cc_results <- data.frame(results_metric = 'CC', Humberstone.sm = cor(predicted_vs_observed$real_values, predicted_vs_observed$SM.mean),
                           Humberstone.lm = cor(predicted_vs_observed$real_values, predicted_vs_observed$LM.mean))
  summary_results <- rbind (summary_results, cc_results )
  
  # Check plots
  plot ( predicted_vs_observed$real_values,predicted_vs_observed$LM.mean, main = paste('Linear model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$LM.mean),2), sep = ''),
         xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
  plot ( predicted_vs_observed$real_values,predicted_vs_observed$SM.mean, main = paste('Spatial model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$SM.mean),2), sep = ''),
         xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
  
  ################################################################################
  # Analyse results
  ################################################################################
  # Reshape the results, so each column is a model run
  lm_results <- summary_results[c(1,3)]
  lm_results <- data.frame(results_metric = c('bias-LM', 'RMSE-LM', 'Coverage-LM', 'IW-LM', 'CC-LM'),summary_results[c(3)] )
  colnames(lm_results) <- c('Metric', 'Value')
  sm_results <- summary_results[c(1,2)]
  sm_results <- data.frame(results_metric = c('bias-SM', 'RMSE-SM', 'Coverage-SM', 'IW-SM', 'CC-SM'),summary_results[c(2)] )
  colnames(sm_results) <- c('Metric', 'Value')
  results_reshaped <- rbind(lm_results, sm_results)
  
  # Add a column with the number of samples
  results_reshaped <- rbind(results_reshaped, data.frame(Metric = 'Number Samples', Value = nrow(spcosa_df)))
  
  # Add to the overall tablewhich will store results from each permutation 
  overall_results <- cbind (overall_results, results_reshaped[2])
  
  ################################################################################
  # Save results for this iteration?
  ################################################################################
  #filename = paste(as.character(grid_spacing), "grid_", as.character(i), '.csv', sep ='')
  #write.csv(results_reshaped, file = filename, row.names =F)
  
  print(paste("Completed grid shift iteration:", i))
}

################################################################################
# Save overall results for regular grid
################################################################################
filename = paste("Data/Generated/CrossValidation/SyntheticData/SPCosa_50shuffles/", grid_spacing, "mgrid.50runs.csv", sep = '')
write.csv(overall_results, file = filename, row.names = F)

  


  

