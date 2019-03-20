library(rgdal)
library(raster)
library(geoR)
library(dplyr)
library(geosphere)
library(leaflet)
library (rgeos)
library(gdistance)

# Working directory
setwd("E:/Msc/Dissertation/Code")

# Source files containing functions
source("Peat_depth_model/clean_pd.R")
source("Peat_depth_model/cross_validate.R")
source("Peat_depth_model/check_models.R")
source("Peat_depth_model/analyse_results.R")

# Define whether to use BNG or decimal degrees projection
projection <- 'bng'
duplication <- 'keep'

-------------------
  
# Read in dtm and slope rasters 
dtm <-raster("E:/Msc/Dissertation/Code/Data/Input/DTM/Dales_Nidderdale_DTM_5m.tif")
slope <-raster("E:/Msc/Dissertation/Code/Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")

# Create dataframe with depth measurements and slope/elevation if it doesn't exist already
if (file.exists("E:/Msc/Dissertation/Code/Data/Generated/humberstone.shp")){
  print ("Reading shapefile from folder.")
  shp = readOGR(dsn = "Data/Generated", layer = "humberstone")} else
  {print ("Creating shapefile")
   shp = find_topographic("Humberstone_Peat_Depth_Points", dtm, slope)}

# Check for the presence of duplicates in spatial location (both x and y coordinate)
# and delete duplicated locations
shp = find_duplication(shp)

# Check for the presence of points which are <1m together.
# Delete one of each of these pairs of points.
shp = find_nearestNeighours (shp)

# Convert projection system
shp <- convert_projection(shp, projection)

# Convert the spatial points dataframe to a dataframe for modelling
dat <- data.frame(shp)

# Add transformations of depth
dat$sqrtdepth <- sqrt(dat$depth)
dat$logdepth <- log(dat$depth)

-------------------
  
# Define which variables to use as covariates
covars <- c("elevation", "Slope_5m")

# Check model performance using the whole dataset
lm_test <- check_lm(dat)
sm_test <- check_sm(dat, covars)

# Perform cross validation
results <- cross_validate (dat)

-------------------

#### Analyse results
#Create results metrics
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


#
save(results, file ="E:/Msc/Dissertation/Code/Peat_depth_model/results_bng_nodupes.R")

