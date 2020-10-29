#################################################################
# Set up environment
#################################################################
library(rgdal)
library(raster)
library(geoR)
library(dplyr)
library(geosphere)
library(leaflet)
library (rgeos)
library(gdistance)

# Set working directory
setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis/")

# Source files containing functions
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/cross_validate.R")
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/check_models.R")
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/analyse_results.R")

#################################################################
# Define variables
#################################################################
# Define whether to use BNG ('bng') or decimal degrees projection ('wgs84')
projection <- 'wgs84' 
# Define whether to keep the duplicates in ('keep') or not ('drop')
duplication <- 'keep'

#################################################################
# Read in covariate data
#################################################################
# Read in raster files to be used as co-variates
dtm <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_DTM_5m.tif")
slope <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")

# Set up a list with covariate names (for renaming in function)
covars <- c(dtm, slope) 
covar_names <- c("elevation", "Slope_5m") 


#################################################################
#
#################################################################
# Spatial points dataframe containing locations of measured peat depth samples and their depth, slope and elevation values 
pd_sample_pts_with_covars <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples_withCovariates", layer = "Humberstone_CleanedPD_withCovariates")

# Check for the presence of duplicates in spatial location (both x and y coordinate)
# If duplication is set to drop then  delete duplicated locations
if (duplication == 'drop'){
  print ("duplicates removed")
  pd_sample_pts_with_covars = find_duplication(pd_sample_pts_with_covars)
  # Check for the presence of points which are <1m together.
  # Delete one of each of these pairs of points.
  pd_sample_pts_with_covars = find_nearestNeighours (pd_sample_pts_with_covars)} else
  {print ("duplicates kept")}

# Convert projection system to that specified at start of file.
pd_sample_pts_with_covars <- convert_projection(pd_sample_pts_with_covars, projection)
print (crs(pd_sample_pts_with_covars))

# Convert the spatial points dataframe to a dataframe for modelling
dat <- data.frame(shp)

# Add transformations of depth
dat$sqrtdepth <- sqrt(dat$depth)

#------------------------------------------------------------------------------
#2. Fit linear model on the sample data
# This will contain the coefficients defining the relationship between the response
# and predictors. 
model.lm <- lm(formula=sqrtdepth~elevation + Slope_5m, data=dat)
model.lm.predictions <-
  predict(object=model.lm, newdata=dat, interval="prediction")

#------------------------------------------------------------------------------
#3. Fit spatial model on the sample data
# Convert dataframe to geodata
test.sp <-
  as.geodata(obj=dat, coords.col=c('coords.x1', 'coords.x2'), data.col= 'sqrtdepth', covar.col= covar_names)

# Jitter any duplicated data locations which appear to be identical
test.sp <- jitterDupCoords(test.sp, max=0.01)
## Fit the model
model.sm <-
  likfit(geodata=test.sp, trend=~elevation + Slope_5m,
         ini.cov.pars=c(15, 0.05), fix.nugget=FALSE,
         cov.model="exponential")


