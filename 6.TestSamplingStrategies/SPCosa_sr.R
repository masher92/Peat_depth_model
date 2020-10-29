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
library(spcosa)

#setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis/")

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

# Number of samples to be equivalent to the number of samples in a regular grid
if (grid_spacing == 100) { 
  n_samples <- 766  # this equates to the number of samples in a regular grid with 10m spacing
} else if (grid_spacing == 150) {
  n_samples = 500
} 

# Projection to use in the CV
proj_for_cv = 'wgs84'

# Parameters related to short range cluster
n_per_cluster <- 3
buffer_size = 20

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
aoi_5mIntervals_pts_spdf <- SpatialPointsDataFrame(coords = aoi_5mIntervals_pts[c(3:4)], data = aoi_5mIntervals_pts[c(1,2)],proj4string =  CRS("+init=epsg:4326"))
# Convert projection to BNG
aoi_5mIntervals_pts_spdf <- spTransform(aoi_5mIntervals_pts_spdf, CRS("+init=epsg:27700"))

# Spatial Polygons Dataframe containing the outline of the study area (trimmed to Moorland Line)
# These are boundaries within which to construct the synthetic dataset 
aoi_trimmed <- readOGR(dsn = "Data/Generated/StudyAreaTrimmedToMoorlandLine", layer = "humberstone_aoi_trimmed")

# Spatial points dataframe containing locations of measured peat depth samples and their depth, slope and elevation values 
pd_sample_pts_with_covars <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples_withCovariates", layer = "Humberstone_CleanedPD_withCovariates")

##################################################
# Iterate through 50 different versions of a spatial coverage sample, with short range subset,
# and record the results
##################################################
# Create dataframe to store results
overall_results <- data.frame(Metric = c('bias-LM', 'RMSE-LM', 'Coverage-LM', 'IW-LM', 'CC-LM', 'bias-SM', 'RMSE-SM', 'Coverage-SM', 'IW-SM', 'CC-SM', 'Length'))

# Iterate through 50 variations of the spatial coverage sample, with short distance subset
# Each time the stratify function is run the results are different (I think)
for (i in 1:50){
  print(paste("Model run number:", i))
  
  ##################################################
  # Create SpCosa sample
  ##################################################
  # Split the study area into compact strata
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
  
  ########################################################################################
  # Add short range subset 
  ########################################################################################
  # Define how many points to include as short distance subset; here it is 10% of the total
  short_range_subset_size <- NROW(spcosa_spdf)*0.1
  # Define the number of points which will be the centre point of the cluster
  n_points_to_base_clusters <- short_range_subset_size/n_per_cluster
  # Randomly select some of these points to add 
  points_to_add_to <- spcosa_spdf[sample(1:nrow(spcosa_spdf),n_points_to_base_clusters),]
  
  # Create an empty spdf to store all of the extra points to add.
  short_range_subset = SpatialPointsDataFrame(coords = data.frame(longitude = 0, latitude = 0),
                                              data = data.frame(Slope_5m = 0, elevation = 0),
                                              proj4string =  CRS("+init=epsg:27700"))[-1,]
  
  # Loop through the points around which the extra samples will be located
  # For each of these points, create a buffer of "buffer_size"m around it.
  # Search the dataframe containing all the points within AOI at 5m intervals for which we have slope and elevation values
  # to define all points from this within the buffer
  # Randomly select "n_per_cluster" from the points within this buffer
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
    # Add to dataframe storing all the extra samples
    short_range_subset <- rbind(short_range_subset, pts_to_add)
  } 
  
  # Remove the same number of points from grid as were added in short distance subset
  # This is so the number of points in the whole sample is comparable to the straight
  # regular grid sample (;however, doing this doesn't necessarily make sense)
  # NB: should maybe ensure this doesn't remove any of the points around which the clusters were based
  indices_toremove <- sample(1:nrow(spcosa_spdf), short_range_subset_size)
  spcosa_spdf <- spcosa_spdf[-indices_toremove,]
  
  # Join the spatial coverage sample (with the points removed) with the extra short distance subset
  spcosa_sr_spdf <- rbind(short_range_subset, spcosa_spdf)
  
  #########################################################################
  # Check plotting of sample
  #########################################################################
  leaflet() %>% 
    addProviderTiles(providers$OpenStreetMap) %>%  
    addTiles() %>%
    addPolygons(data = spTransform(aoi_trimmed, CRS("+init=epsg:4326")), fillOpacity = 0, weight = 3) %>%
    addCircles(data = spTransform(spcosa_sr_spdf, CRS("+init=epsg:4326")), radius = 5, weight = 1, fillOpacity = 1, opacity=1, color = 'black', fillColor = 'black')  %>% 
    addScaleBar(position = c("topright", "bottomright", "bottomleft",
                             "topleft"), options = scaleBarOptions())
  
  #########################################################################
  # Assign depths
  # This also converts spdf to a dataframe, and to WGS84 projection
  #########################################################################
  # Assign depths - must be a dataframe, with coordinates in wgs84 for disthaversine function  
  # A dataframe is also returned 
  spcosa_df <- assign_depths(pd_sample_pts_with_covars,
                             as.data.frame(spTransform(spcosa_sr_spdf, CRS("+init=epsg:4326"))))
  # Add a square root of depth
  spcosa_df$sqrtdepth <- sqrt(spcosa_df$depth)
  
  ################################################################################   
  # Check model performance using the whole dataset
  ################################################################################
  #lm_test <- check_lm(spcosa_sr_df, c("elevation", "Slope_5m"))
  #sm_test <- check_sm(spcosa_sr_df, c("elevation", "Slope_5m"))
  
  #########################################################################
  # Cross-validate model with shifted grid iteration
  #########################################################################
  print ('Cross validating')
  results <- cross_validate (spcosa_sr_df, c('elevation', 'Slope_5m'))
  
  ################################################################################
  # Analyse results
  ################################################################################
  options(scipen=999) # stops use of scientific notations
  
  # Compare the predicted values with the observed values
  predicted_vs_observed <- create_predicted_vs_observed (results, spcosa_sr_df)
  
  # Create dataframe summarising the bias, RMSE, coverage and interval width
  summary_results <- create_results (aoi, results, spcosa_sr_df)
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
  results_reshaped <- rbind(results_reshaped, data.frame(Metric = 'Number Samples', Value = nrow(spcosa_sr_df)))
  
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
filename = paste("Data/Generated/CrossValidation/SyntheticData/SPCosa_sr_50shuffles/", grid_spacing, ,'_',  "mgrid.50runs.csv", sep = '')
write.csv(overall_results, file = filename, row.names = F)

