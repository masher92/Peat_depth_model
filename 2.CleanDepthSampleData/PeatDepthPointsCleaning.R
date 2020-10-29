
# Set up each of the datasets containing peat depth samples points into a format suitable for use in
# RunModel file.
# This involves renaming the depth values column as Depth.
# Setting the Depth column as numeric
# Keeping just the Depth column

# Source config file containing working directory
source("Code/Peat_depth_model-master/config.R")

# Set the directory containing the code. 
setwd(working_directory)

#############################################################################################
# 1. Humberstone
#############################################################################################
# Read in spatial samples data
humberstone_peat_depth_samples <- readOGR(dsn = "Data/Input/PeatDepth", layer = "Humberstone_Peat_Depth_Points")
# Ensure depth column is 'Depth'
names(humberstone_peat_depth_samples@data)[names(humberstone_peat_depth_samples@data) == 'Pd'] <- 'Depth'
# Set depth columns as numeric
humberstone_peat_depth_samples@data$Depth <- as.numeric(as.character(humberstone_peat_depth_samples@data$Depth))
# Keep just the depth columns
humberstone_peat_depth_samples <- humberstone_peat_depth_samples[c('Depth')]
# Save to file
writeOGR(humberstone_peat_depth_samples, dsn = "Data/Generated/CleanedPeatDepthSamples" , layer = "HumberstonePeatDepthPoints_cleaned", driver="ESRI Shapefile")

#############################################################################################
# 2. Stake Moss
#############################################################################################
# Read in spatial samples data
stake_moss_peat_depth_samples <- readOGR(dsn = "Data/Input/PeatDepth", layer = "Stake_Moss_Peat_Depth_Points")
# Ensure depth column is 'Depth'
names(stake_moss_peat_depth_samples@data)[names(stake_moss_peat_depth_samples@data) == 'PD'] <- 'Depth'
# Set depth columns as numeric
stake_moss_peat_depth_samples@data$Depth <- as.numeric(as.character(stake_moss_peat_depth_samples@data$Depth))
# Keep just the depth columns
stake_moss_peat_depth_samples <- stake_moss_peat_depth_samples[c('Depth')]
# Save to file
writeOGR(stake_moss_peat_depth_samples, dsn = "Data/Generated/CleanedPeatDepthSamples" , layer = "StakeMossPeatDepthPoints_cleaned", driver="ESRI Shapefile")

#############################################################################################
# 3. Melbecks and Reeth
#############################################################################################
# Read in spatial samples data
melbecks_peat_depth_samples <- readOGR(dsn = "Data/Input/PeatDepth", layer = "Melbecks_and_Reeth_Peat_Depth_Points")
# Ensure depth column is 'Depth'
names(melbecks_peat_depth_samples@data)[names(melbecks_peat_depth_samples@data) == 'PD_NUM'] <- 'Depth'
# Set depth columns as numeric
melbecks_peat_depth_samples@data$Depth <- as.numeric(as.character(melbecks_peat_depth_samples@data$Depth))
# Keep just the depth columns
melbecks_peat_depth_samples <- melbecks_peat_depth_samples[c('Depth')]
# Save to file
writeOGR(melbecks_peat_depth_samples, dsn = "Data/Generated/CleanedPeatDepthSamples" , layer = "MelbecksPeatDepthPoints_cleaned", driver="ESRI Shapefile")

#############################################################################################
# 4. Newhouse
#############################################################################################
# Read in spatial samples data
newhouse_peat_depth_samples <- readOGR(dsn = "Data/Input/PeatDepth", layer = "Newhouse_Peat_Depth_Points")
# Ensure depth column is 'Depth'
names(newhouse_peat_depth_samples@data)[names(newhouse_peat_depth_samples@data) == 'PD'] <- 'Depth'
# Set depth columns as numeric
newhouse_peat_depth_samples@data$Depth <- as.numeric(as.character(newhouse_peat_depth_samples@data$Depth))
# Keep just the depth columns
newhouse_peat_depth_samples <- newhouse_peat_depth_samples[c('Depth')]
# Save to file
writeOGR(newhouse_peat_depth_samples, dsn = "Data/Generated/CleanedPeatDepthSamples" , layer = "NewhousePeatDepthPoints_cleaned", driver="ESRI Shapefile")



