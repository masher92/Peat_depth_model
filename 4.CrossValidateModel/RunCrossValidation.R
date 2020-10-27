################################################################################
# Set up processing environment
################################################################################
library(rgdal)
library(raster)
library(geoR)
library(dplyr)
library(geosphere)
library(leaflet)
library (rgeos)
library(gdistance)

setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis/")

# Source files containing functions
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/clean_pd.R")
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/cross_validate.R")
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/check_models.R")
source("Code/Peat_depth_model-master/5.CrossValidateModel/Functions/analyse_results.R")

################################################################################
# Define model parameters
################################################################################
aoi <- "Humberstone"
original_projection <- 'bng'  # 'bng' or 'wgs84'
# NB: if desired projection is set to bng, the spatial model doesn't run properly and gives the same
# results to the linear model, still not sure why this is.
desired_projection <- 'wgs84'  # 'bng' or 'wgs84'
duplication <- 'drop' # 'keep' or 'drop'

################################################################################
# Read in required data
################################################################################
pd_sample <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples_withCovariates", layer = paste(aoi, 'CleanedPD_withCovariates', sep = '_'))

# Filepath to folder in which to save outputs
output_fp <- paste("Data/Generated/CrossValidationResults/", aoi, sep = '')
model_run_description <- paste(aoi, projection, duplication, sep = '_')

################################################################################
# Process data
# Check for duplicated x,y locations - If duplication is set to drop then  delete duplicated locations
if (duplication == 'drop'){
  print ("duplicates removed")
  pd_sample = find_duplication(pd_sample)
  # Check for the presence of points which are <1m together.
  # Delete one of each of these pairs of points.
  pd_sample = find_nearestNeighours (pd_sample, original_projection)
} else
{print ("duplicates kept")}

# Convert projection system to that specified at start of file.
pd_sample <- convert_projection(pd_sample, desired_projection)
print (crs(pd_sample))

# Convert the spatial points dataframe to a dataframe for modelling
pd_sample_df <- data.frame(pd_sample)

# Add transformations of depth
pd_sample_df$sqrtdepth <- sqrt(pd_sample_df$depth)

################################################################################
# Check model performance using the whole dataset
lm_test <- check_lm(pd_sample_df, c("elevation", "Slope_5m"))
sm_test <- check_sm(pd_sample_df, c("elevation", "Slope_5m"))

################################################################################
# Perform cross validation
results <- cross_validate (pd_sample_df, c("elevation", "Slope_5m"))

# Store the results alongside the sample_df used to produce them
results_ls <- list(results, sample_df)

################################################################################
# Analyse results
################################################################################
options(scipen=999) # stops use of scientific notations

# Create dataframe summarising the bias, RMSE, coverage and interval width
summary_results <- create_results (aoi, results, sample_df)
# Compare the predicted values with the observed values
predicted_vs_observed <- create_predicted_vs_observed (results, sample_df)

################################################################################
# Create directory to save results in if it doesn't exist already
################################################################################
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



