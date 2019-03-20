library(rgdal)
library(raster)
library(geoR)
library(dplyr)
library(geosphere)
library(leaflet)
library (rgeos)
library(gdistance)

# Source files containing functions
source("E:/Msc/Dissertation/Code/Peat_depth_model/clean_pd.R")
source("E:/Msc/Dissertation/Code/Peat_depth_model/cross_validate.R")
source("E:/Msc/Dissertation/Code/Peat_depth_model/check_models.R")

# Working directory
setwd("E:/Msc/Dissertation/Code/Data/Generated/")

# Read in dtm and slope rasters 
dtm <-raster("E:/Msc/Dissertation/Code/Data/Input/DTM/Dales_Nidderdale_DTM_5m.tif")
slope <-raster("E:/Msc/Dissertation/Code/Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")

# Create dataframe with depth measurements and slope/elevation if it doesn't exist already
if (file.exists("humberstone.shp")){
  print ("Reading shapefile from folder.")
  shp = readOGR(dsn = "E:/Msc/Dissertation/Code/Data/Generated", layer = "humberstone")} else
  {print ("Creating shapefile")
   shp = find_topographic("Humberstone_Peat_Depth_Points", dtm, slope)}

# Check for the presence of duplicates in spatial location (both x and y coordinate)
# and delete duplicated locations
shp = find_duplication(shp)

# Check for the presence of points which are <1m together.
# Delete one of each of these pairs of points.
shp = find_nearestNeighours (shp)

# Convert the spatial points dataframe to a dataframe for modelling
dat <- data.frame(shp)

# Add transformations of depth
dat$sqrtdepth <- sqrt(dat$depth)
dat$logdepth <- log(dat$depth)

# Define which variables to use as covariates
covars <- c("elevation", "Slope_5m")

# Check model performance using the whole dataset
lm_test <- check_lm(dat)
sm_test <- check_sm(dat, covars)

# Transform the projection of the data from BNG to DD



# Perform cross validation
results = cross_validate (dat)

######## Check projection
