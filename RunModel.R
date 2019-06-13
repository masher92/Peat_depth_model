################################################################################
# Set up processing environment
library(rgdal)
library(raster)
library(geoR)
library(dplyr)
library(geosphere)
library(leaflet)
library (rgeos)
library(gdistance)

setwd("E:/Msc/Dissertation/Code/Peat_depth_model")

# Source files containing functions
source("Functions/clean_pd.R")
source("Functions/cross_validate.R")
source("Functions/check_models.R")
source("Functions/analyse_results.R")

################################################################################
# Define model parameters
aoi <- "Melbecks"
projection <- 'wgs84'  # 'bng' or 'wgs84'
duplication <- 'drop' # 'keep' or 'drop'
covar_names <- c("elevation", "Slope_5m") # Names of variables to be used in model 

# Check metrics available
#dir("Data/Input/Metrics/Metrics_40")

################################################################################
# Read in required data
peat_depth_samples <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples", layer = paste(aoi, "PeatDepthPoints_cleaned", sep = ''))
dtm <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_DTM_5m.tif")
slope <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")
#w_hhhh = raster("Data/Input/Metrics/Metrics_40/w_hhhh_40.0.asc")

# Filepath to folder in which to save outputs
output_fp <- paste("Data/Generated/CrossValidationResults/", aoi, sep = '')
model_run_description <- paste(aoi, projection, duplication, sep = '_')

################################################################################
# Process data
# Remove any NA depth valus
#peat_depth_samples <- clean_peat_depth_samples (peat_depth_samples) # Remove any NA depth values
peat_depth_samples <- peat_depth_samples[!is.na(peat_depth_samples@data$Depth),]

# Join to covariates
peat_depth_samples_with_covars <- join_topographic_data(peat_depth_samples, c(dtm, slope), covar_names)
# Check for duplicated x,y locations - If duplication is set to drop then  delete duplicated locations
if (duplication == 'drop'){
  print ("duplicates removed")
  peat_depth_samples_with_covars = find_duplication(peat_depth_samples_with_covars)
  # Check for the presence of points which are <1m together.
  # Delete one of each of these pairs of points.
  peat_depth_samples_with_covars = find_nearestNeighours (peat_depth_samples_with_covars, projection)} else
  {print ("duplicates kept")}

# Convert projection system to that specified at start of file.
peat_depth_samples_with_covars <- convert_projection(peat_depth_samples_with_covars, projection)
print (crs(peat_depth_samples_with_covars))

# Convert the spatial points dataframe to a dataframe for modelling
sample_df <- data.frame(peat_depth_samples_with_covars)

# Add transformations of depth
sample_df$sqrtdepth <- sqrt(sample_df$depth)
sample_df$logdepth <- log(sample_df$depth)

################################################################################
# Check model performance using the whole dataset
lm_test <- check_lm(sample_df, covar_names)
sm_test <- check_sm(sample_df, covar_names)

################################################################################
# Perform cross validation
results <- cross_validate (sample_df, covar_names)

# Store the results alongside the sample_df used to produce them
results_ls <- list(results, sample_df)

################################################################################
# Analyse results
options(scipen=999) # stops use of scientific notations

# Create dataframe summarising the bias, RMSE, coverage and interval width
summary_results <- create_results (aoi, results, sample_df)
# Compare the predicted values with the observed values
predicted_vs_observed <- create_predicted_vs_observed (results, sample_df)

################################################################################
# Create directory to save results in if it doesn't exist already
if (dir.exists(output_fp) == FALSE) {
  dir.create(output_fp)}

# Save results
save(results_ls, file = paste(output_fp, '/', model_run_description, '.R', sep = '' ))
write.csv(summary_results, file = paste(output_fp, '/', model_run_description, '_summary_results.csv', sep = '' ), row.names = F)

# Create and save plots
# Linear model
jpeg(paste(output_fp, '/', model_run_description, '_LMplot.jpg', sep = '' ))
plot ( predicted_vs_observed$real_values,predicted_vs_observed$LM.mean, main = paste('Linear model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$LM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)
dev.off()

# Spatial model
jpeg(paste(output_fp, '/', model_run_description, '_SMplot.jpg', sep = '' ))
plot ( predicted_vs_observed$real_values,predicted_vs_observed$SM.mean, main = paste('Spatial model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$SM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)
dev.off()



