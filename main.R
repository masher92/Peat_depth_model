library(rgdal)
library(raster)
library(geoR)
library(dplyr)
library(geosphere)
library(leaflet)
library (rgeos)

source("E:/Msc/Dissertation/Code/Peat/R/clean_pd.R")
source("E:/Msc/Dissertation/Code/Peat/R/cross_validate.R")

# Working directory
setwd("E:/Msc/Dissertation/Code/Data/Generated/")

# Read in dtm raster
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

# Perform cross validation
results = cross_validate (dat, slope, dtm)
