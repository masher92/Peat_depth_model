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
root_fp = "C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/"
setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis/")

# Source files containing functions
source("Code/Peat_depth_model-master/Functions/clean_pd.R")
source("Code/Peat_depth_model-master/Functions/cross_validate.R")
source("Code/Peat_depth_model-master/Functions/check_models.R")
source("Code/Peat_depth_model-master/Functions/analyse_results.R")

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
# Create dataframe with depth measurements and slope/elevation if it doesn't exist already
if (file.exists("Data/Generated/humberstone.shp")){
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

#------------------------------------------------------------------------------
# 4. Create unmeasured locations which we want to predict for.
#m These are created in the process rastrer file - altho not certain this is best location to do this?
# Take the code from the runcrossvalidation for fitting th emodel
