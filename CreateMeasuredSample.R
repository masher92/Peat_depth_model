################################################################################
# Set up processing environment
#library(rgdal)
#library(geoR)
#library(dplyr)
#library(geosphere)
#library(leaflet)
#library (rgeos)
#library(gdistance)

setwd("E:/Msc/Dissertation/Code/Peat_depth_model")

# Source files containing functions
source("Functions/clean_pd.R")

################################################################################
# Define parameters
aoi <- "Humberstone"
covar_names <- c("elevation", "Slope_5m") # Names of variables to be used in model 

################################################################################
# Read in required data
peat_depth_samples <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples", layer = paste(aoi, "PeatDepthPoints_cleaned", sep = ''))
dtm <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_DTM_5m.tif")
slope <-raster("Data/Input/DTM/Dales_Nidderdale_Moorland_Line_Slope_5m.tif")
#w_hhhh = raster("Data/Input/Metrics/Metrics_40/w_hhhh_40.0.asc")

################################################################################
# Process data
# Remove any NA depth values
sample <- peat_depth_samples[!is.na(peat_depth_samples@data$Depth),]

# Join to covariates
sample <- join_topographic_data(sample, c(dtm, slope), covar_names)

################################################################################
# Save data
save(sample, file= paste("Data/Generated/MeasuredSamples/", aoi, 'MeasuredSampleDataset.RData', sep = ''))




