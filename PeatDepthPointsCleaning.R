# Set up each of the datasets containing peat depth samples points into a format suitable for use in
# RunModel file.
# This involves renaming the depth values column as Depth.
# Setting the Depth column as numeric
# Keeping just the Depth column

setwd("E:/Msc/Dissertation/Code/Peat_depth_model")

humberstone_peat_depth_samples <- readOGR(dsn = "Data/Input/PeatDepth", layer = "Humberstone_Peat_Depth_Points")
names(humberstone_peat_depth_samples@data)[names(humberstone_peat_depth_samples@data) == 'Pd'] <- 'Depth'
humberstone_peat_depth_samples@data$Depth <- as.numeric(as.character(humberstone_peat_depth_samples@data$Depth))
humberstone_peat_depth_samples <- humberstone_peat_depth_samples[c('Depth')]
writeOGR(humberstone_peat_depth_samples, dsn = "Data/Generated/CleanedPeatDepthSamples" , layer = "HumberstonePeatDepthPoints_cleaned", driver="ESRI Shapefile")

stake_moss_peat_depth_samples <- readOGR(dsn = "Data/Input/PeatDepth", layer = "Stake_Moss_Peat_Depth_Points")
names(stake_moss_peat_depth_samples@data)[names(stake_moss_peat_depth_samples@data) == 'PD'] <- 'Depth'
stake_moss_peat_depth_samples@data$Depth <- as.numeric(as.character(stake_moss_peat_depth_samples@data$Depth))
stake_moss_peat_depth_samples <- stake_moss_peat_depth_samples[c('Depth')]
writeOGR(stake_moss_peat_depth_samples, dsn = "Data/Generated/CleanedPeatDepthSamples" , layer = "StakeMossPeatDepthPoints_cleaned", driver="ESRI Shapefile")

melbecks_peat_depth_samples <- readOGR(dsn = "Data/Input/PeatDepth", layer = "Melbecks_and_Reeth_Peat_Depth_Points")
names(melbecks_peat_depth_samples@data)[names(melbecks_peat_depth_samples@data) == 'PD_NUM'] <- 'Depth'
melbecks_peat_depth_samples@data$Depth <- as.numeric(as.character(melbecks_peat_depth_samples@data$Depth))
melbecks_peat_depth_samples <- melbecks_peat_depth_samples[c('Depth')]
writeOGR(melbecks_peat_depth_samples, dsn = "Data/Generated/CleanedPeatDepthSamples" , layer = "MelbecksPeatDepthPoints_cleaned", driver="ESRI Shapefile")

newhouse_peat_depth_samples <- readOGR(dsn = "Data/Input/PeatDepth", layer = "Newhouse_Peat_Depth_Points")
names(newhouse_peat_depth_samples@data)[names(newhouse_peat_depth_samples@data) == 'PD'] <- 'Depth'
newhouse_peat_depth_samples@data$Depth <- as.numeric(as.character(newhouse_peat_depth_samples@data$Depth))
newhouse_peat_depth_samples <- newhouse_peat_depth_samples[c('Depth')]
writeOGR(newhouse_peat_depth_samples, dsn = "Data/Generated/CleanedPeatDepthSamples" , layer = "NewhousePeatDepthPoints_cleaned", driver="ESRI Shapefile")



