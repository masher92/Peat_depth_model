
# Set up processing environment
library(dplyr)
library(rgdal)
library(splitstackshape)
setwd("E:/Msc/Dissertation/Code/Data")

#######################################################################################
# 1. Read in the sample
sample <- read.csv("Generated/humberstone_test_sample_500.csv")

#######################################################################################
# 2. Find the range of depth values associated with each slope-elevation category amongst existing samples.
samples = readOGR(dsn = "Generated", layer = "humberstone")
samples_df <- as.data.frame(samples)
# Add the same slope and elevation cuts 
samples_df$Slope_Cuts <- cut(samples_df$Slope_5m, breaks = c(0, 2, 4,6,8,10,15,20,25,30,Inf)) 
samples_df$Elevation_Cuts <- cut(samples_df$elevation, breaks = c(250,275,300,325,350,375,400, 425,Inf))
samples_df$slope_and_elevation <- paste(samples_df$Slope_Cuts, samples_df$Elevation_Cuts)
samples_df$slope_and_elevation <- as.character(samples_df$slope_and_elevation)

# Find the number of sample points within each of the slope-elevation categories
# Problem that slope/elevation splits which are too small will only have 0/1/2 members and so 
# the distribtuion which depth is drawn from will be very small.
# Dont think we actually use this?? Just for information
sample_distrib <- as.data.frame(table(samples_df$slope_and_elevation))
#sample_distrib$Var1 <- as.character(sample_distrib$Var1)
# Add columns containing the bounding slope and elevation values
sample_distrib$slope_lower <-as.numeric(sapply(regmatches(sample_distrib$Var1, gregexpr("[[:digit:]]+", sample_distrib$Var1)),`[`,1)) 
sample_distrib$slope_higher <-as.numeric(sapply(regmatches(sample_distrib$Var1, gregexpr("[[:digit:]]+", sample_distrib$Var1)),`[`,2)) 
sample_distrib$elevation_lower <-as.numeric(sapply(regmatches(sample_distrib$Var1, gregexpr("[[:digit:]]+", sample_distrib$Var1)),`[`,3))
sample_distrib$elevation_higher <- as.numeric(sapply(regmatches(sample_distrib$Var1, gregexpr("[[:digit:]]+", sample_distrib$Var1)),`[`,4))  

#######################################################################################
# 3. Add a depth to each of the rows in the sample_df
select_depth_from_distrib <- function (samples_df, slope_elevation_category){
"
Samples_df contains the slope and elevation of sampled depth points
slope_elevation_category is a category for which we are interested in assigning a reasonable depth value to.  
The function filters sample_df according to slope_elevation_category and takes a random sample from the depth values in the 
distribution.
"
  # Filter the samples_df 
  filtered_by_category <- samples_df[samples_df$slope_and_elevation == slope_elevation_category ,]
  # Randomly select a depth value from the available samples with this slope/elevation category
  # If there is just one, then use this (sample doesn't work as expected on DF length 1)
  if (nrow(filtered_by_category) == 1){
    sampled_depth <- filtered_by_category$depth
    print ("Sample length is 1")
  } else {
    sampled_depth <- sample(filtered_by_category$depth, 1)
    print ("Sample length > 1")}
  return (sampled_depth)}

# Move through each row in the stratified sample and use the select_depth_from_distrib to assign a suitable depth value
depths <- list ()
for (i in (1:nrow(sample))){
  print(i)
  # If the slope_elevation category is found in the samples, then select randomly a depth value from the depth values in the samples
  if (sample$slope_and_elevation[[i]] %in% sample_distrib$Var1){
    depth <- select_depth_from_distrib(samples_df, sample$slope_and_elevation[[i]])
    print ("DOne")
    # If the slope and elevation category is not found in samples then assign it to a nearby category which is in the samples.
  } else {
    # Find the lower slope value of the band which is closest to the slope value
    slope_row_no <-which(abs(unique(sample_distrib$slope_lower)-sample$Slope_5m[[i]])==min(abs(unique(sample_distrib$slope_lower)-sample$Slope_5m[[i]])))
    slope_value<- unique(sample_distrib$slope_lower)[slope_row_no]
    
    # Filter the dataframe to only contain those with this slope_value
    matching_slope <- sample_distrib[(sample_distrib$slope_lower == slope_value),]
    
    # Of those with the nearest slope value, find the closest matching lower elevation bands value
    elev_row_no <-which(abs(unique(matching_slope$elevation_lower)-sample$elevation[[i]])==min(abs(unique(matching_slope$elevation_lower)-sample$elevation[[i]])))
    elev_value <- unique(matching_slope$elevation_lower)[elev_row_no]
    
    # Turn this into a slope_elevation category 
    proxy_cat <- as.character(sample_distrib[(sample_distrib$slope_lower == slope_value) & (sample_distrib$elevation_lower == elev_value),]$Var1)
    
    # Randomly select a depth value using this proxy category
    depth <- select_depth_from_distrib(samples_df, proxy_cat)}
  # Add the depth value to the list
  depths[[i]] <- depth  
}
# Convert the depths to a dataframe, and join to the samples DF
depths <- t(as.data.frame(depths))
sample$depth <- depths

# Check the distribution
qplot(
  x = Slope_5m,
  y = depth,
  data = sample,
  color = sample$Elevation_Cuts # color by factor color (I know, confusing)
)

qplot(
  x = elevation,
  y = depth,
  data = sample,
  color = sample$Slope_Cuts # color by factor color (I know, confusing)
)


plot (sample$Slope_5m, sample$depth,  col=sample$Elevation_Cuts)
plot (sample$elevation, sample$depth,  col=sample$Elevation_Cuts)     
plot (sample$slope_and_elevation, sample$depth)   

# Save
write.csv(sample, "Generated/humberstone_test_sample.csv", row.names =  FALSE)
write.csv(sample, "Generated/humberstone_test_sample_700_2_10_60_with_depths.csv", row.names =  FALSE)

