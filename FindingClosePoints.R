#######################################################################################
# Set up processing environment
library(dplyr)
library(rgdal)
library(splitstackshape)
library(leaflet)
library(tictoc)
library(dplyr)
library(fields)
setwd("E:/Msc/Dissertation/Code/Data")

#######################################################################################
# Create functions for use in processing.
find_near_neighbours <- function (df, min_dist, max_dist, n_close_points){
  "
Adds a column to dataframe containing rows of X, Y coordinates specifying the number of 
other points in the dataframe which are within a specified distance of each x, y coordinate. 
: param df: A dataframe with X and Y coordinates stored as columns (in BNG projection)
: param dist_min: The minimum distance apart which points may be from one another
: param dist_max: The maximum distance apart which points may be from one another
: return new_sample_df:  
"
  # Find the distance between each pair of points in the sample.
  dists <- as.matrix(dist(df[,c('coords.x1', 'coords.x2')], 'euclidean'))
  # For each sample point, find the number of points within a distance of it.
  df$close_points <- apply(dists, 1, function(x)sum(x> min_dist & x < max_dist))
  # Calculate the number of rows in the df without the minimum number of close points.
  close_points<- nrow(df[df$close_points < n_close_points,])
  # Print outcome
  print(sprintf("Points without %s neighbours within %s:%s m =  %s", n_close_points, min_dist, max_dist,close_points))
  return (df)}

create_df_sample <- function (original_df, min_dist, max_dist, n_close_points){
  "
Creates a sample from a dataframe of coordinates, according to requirements that the sample should
have the same distribution of slope and elevation values as the original dataframe and that each point 
should be within a certain distance of at least one other - as specified in sample_constraints.
: param df: A dataframe containing x and y coordinates as column from which a sample should be taken.
: param sample_constraints: A dictionary containing constraints for the sampling procedure. 
: return sample: A dataframe containing containing a sample of the input dataframe sampled according to sample_constraints.
"
  tic ("Sampling complete after 0 resamples with")
  # Take a sample from the dataframe that matches the proportional split between slope and elevation in the whole AOI
  sample <- stratified(original_df, c("slope_and_elevation"), 900/nrow(original_df))
  # For each point in the sample, find the number of neighbours it has within a range between min_dist and max_dist
  sample = find_near_neighbours(sample, min_dist, max_dist, n_close_points)
  # If 0 rows have less than the requirements for n_close_points then sampling is done.
  # Else, the dataset is resampled until this condition is met.
  done <- 'Not Done'
  counter <- 0
  if (nrow(sample[sample$close_points < n_close_points,]) == 0){
    toc(log = TRUE)
  } else{
    while (done !='Done') {
      nrow(sample[sample$close_points < n_close_points])
      # Create a coutner to record how many iterations were needed.
      counter = counter + 1
      # Resample the dataset, removing any points without sufficient near neighbours
      # and replacing them with a point with the same slope/elevation profile from the original dataset .
      # Recount the near neighbours to each point.
      sample =  resample_far_points(sample, aoi_df, n_close_points)
      sample = find_near_neighbours (sample, min_dist, max_dist, n_close_points)
      # If 0 rows have less than the requirements for n_close_points then sampling is done.
      # Else, the dataset is returned for resampling.
      if (nrow(sample[sample$close_points < n_close_points,]) == 0){
        toc(log = TRUE)
        done = 'Done'}
    }
  } 
  return (sample)}


resample_far_points <- function(sample_df, original_df, n_close_points){
  "
Removes rows from a dataframe which have less than a specified number of points close to them.
For each row which is removed, randomly selects a row from the  original dataframe which has the same
'Slope/Elevation' category as the removed row. 
Join the newly sampled rows back onto the sample dataframe.
: param sample_df: Dataframe containing rows which were sampled from original_df
: param original_df: Dataframe from which the sample was drawn.
: param n_close_points: Number of points required by user to be 'close' to each point in the sample. 
: return new_sample_df:  Dataframe containing new sample of same size as input sample_df 
"   
  # Keep only those points which have > n_close_points close to them.
  new_sample_df = sample_df[sample_df$close_points > n_close_points,]
  # Delete the close points column (so it can be recreated)
  new_sample_df$close_points <- NULL
  # Store the rows needing to be replaced with a new row with same slope/elevation criteria
  rows_to_be_replaced = sample_df[sample_df$close_points <= n_close_points,]
  # Add one row matching each of these criteria to the new sample dataframe 
  for (i in (1:nrow(rows_to_be_replaced))){
    # For each of these create a subset of df meeting criteria
    df = original_df[original_df$slope_and_elevation == rows_to_be_replaced$slope_and_elevation[i],]
    # Randomly sample one row from those available
    row_to_add <- sample_n(df, 1)
    # Add it to dataframe
    new_sample_df <- rbind(new_sample_df, row_to_add)}
  return (new_sample_df)
}


#######################################################################################
# 1. Find the distribution of slope and elevation categories across the whole AOI

# Read in dataframe containing coordinates, slope and elevation values from within the AOI
aoi_df <- read.csv("Generated/humberstone_aoi_df.csv")

# Create categorical slope and elevation variables, and a combined variable
aoi_df$Slope_Cuts <- cut(aoi_df$Slope_5m, breaks = c(0, 2, 4,6,8,10,15,20,25,30,Inf)) 
aoi_df$Elevation_Cuts <- cut(aoi_df$elevation, breaks = c(225,250,275,300,325,350,375,400, 425,Inf))
aoi_df$slope_and_elevation <- paste(aoi_df$Slope_Cuts, aoi_df$Elevation_Cuts)


#########################################################################
# Sampling constraints
n_close_points = 2
min_dist = 0.01
max_dist = 80 

#  Create the sample
sample = create_df_sample(aoi_df, min_dist, max_dist, n_close_points)

# Plot the sample points (first convert to SPDF and change CRS (leaflet can only plot in DD))
spdf <- SpatialPointsDataFrame(coords = sample[,c(1:2)], data = sample[,c(3:4)],
                               proj4string = CRS("+init=epsg:27700"))
spdf <- spTransform(spdf, CRS("+init=epsg:4326"))
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>%  
  addTiles() %>% 
  addCircles(data = spdf, weight = 3, opacity=1, color = 'black')   %>% 
  addCircles(data = weird_spdf, weight = 3, opacity=1, color = 'orange')


