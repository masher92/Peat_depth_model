library(rgdal)
library(raster)
library(geoR)
library(dplyr)
library(geosphere)
library(leaflet)
library (rgeos)
library(gdistance)

# Set working directory
setwd("E:/Msc/Dissertation/Code")

# Source files containing functions
source("Peat_depth_model/Functions/clean_pd.R")
source("Peat_depth_model/Functions/cross_validate.R")
source("Peat_depth_model/Functions/check_models.R")
source("Peat_depth_model/Functions/analyse_results.R")

# Define whether to use BNG or decimal degrees projection - 'wgs84' or 'bng'
# Define whether to keep the duplicates in or not - 'keep' or 'drop'
projection <- 'wgs84' 
duplication <- 'drop'

# Check metrics available
#dir("Data/Input/Metrics/Metrics_40")

# Read in metrics
dtm <-raster("Data/Input/DTM/dtm_h_clip.tif")
slope <-raster("Data/Input/DTM/slope_h_clip.tif")
#w_hhhh = raster("Data/Input/Metrics/Metrics_40/w_hhhh_40.0.asc")
#count_hhhh = raster("Data/Input/Metrics/Metrics_40/count_hhhh_40.0.asc")
#w_llll = raster("Data/Input/Metrics/Metrics_40/w_llll_40.0.asc")
#count_llll = raster("Data/Input/Metrics/Metrics_40/count_llll_40.0.asc")

# Define which variables to use as covariates, and set up a list with their names (for renaming in function)
covars <- c(dtm, slope)
covar_names <- c("elevation", "Slope_5m")

# Create dataframe with depth measurements and slope/elevation if it doesn't exist already
if (file.exists("E:/Msc/Dissertation/Code/Data/Generated/humberstone.shp")){
  print ("Reading shapefile from folder.")
  shp = readOGR(dsn = "Data/Generated", layer = "humberstone")} else
  {print ("Creating shapefile")
    shp = find_topographic("Humberstone_Peat_Depth_Points", covars, covar_names)}

# Check for the presence of duplicates in spatial location (both x and y coordinate)
# If duplication is set to drop then  delete duplicated locations
if (duplication == 'drop'){
  print ("duplicates removed")
  shp = find_duplication(shp)
  # Check for the presence of points which are <1m together.
  # Delete one of each of these pairs of points.
  shp = find_nearestNeighours (shp)} else
  {print ("duplicates kept")}

# Convert projection system to that specified at start of file.
shp <- convert_projection(shp, projection)
print (crs(shp))

# Convert the spatial points dataframe to a dataframe for modelling
dat <- data.frame(shp)

# Add transformations of depth
dat$sqrtdepth <- sqrt(dat$depth)
dat$logdepth <- log(dat$depth)


# Source files containing functions
source("Peat_depth_model/Functions/analyse_results.R")

# Load in results data
load("E:/Msc/Dissertation/Code/Peat_depth_model/Output/results_wgs84_nodupes.R")
# Need to also load dat

dat <- results[[2]]
results <- results[[1]]

## 
summary_results <- create_results ('Humberstone', results, dat)
# Compare the predicted values with the 
predicted_vs_observed <- create_predicted_vs_observed (results, dat)


# Plot results
# Plot (linear model)
plot ( predicted_vs_observed$real_values,predicted_vs_observed$LM.mean, main = paste('Linear model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$LM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

# Plot (spatial model)
plot ( predicted_vs_observed$real_values,predicted_vs_observed$SM.mean, main = paste('Spatial model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$SM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)


results = list(results, dat)