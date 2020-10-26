#######################################################################################
# Create functions for use in processing.
#######################################################################################
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

resample_far_points <- function(sample_df, original_df, n_close_points, min_dist, max_dist){
  "
  Removes rows from a dataframe which have less than a specified number of points close to them.
  For each row which is removed, randomly selects a row from the  original dataframe which has the same
  'Slope/Elevation' category as the removed row. 
  Join the newly sampled rows back onto the sample dataframe.
  : param sample_df: Dataframe containing rows which were sampled from original_df
  : param original_df: Dataframe from which the sample was drawn.
  : param n_close_points: Number of points required by user to be 'close' to each point in the sample. 
  : return new_sample_df:  Dataframe containing new sample of same size as input sample_df 
  
  
  # Extra part commented out - thought it might improve performance - by first checking distances before adding
  # but the distance matrix is slow to calculate, so this actually makes it slower. 
  "   
  # Keep only those points which have > n_close_points close to them.
  new_sample_df = sample_df[sample_df$close_points > n_close_points,]
  # Delete the close points column (so it can be recreated)
  new_sample_df$close_points <- NULL
  # Store the rows needing to be replaced
  rows_to_be_replaced = sample_df[sample_df$close_points <= n_close_points,]
  # Create list to store rows which couldn't be replaced
  missed_cats <- data.frame()
  
  # Add one row matching each of these criteria to the new sample dataframe 
  for (i in (1:nrow(rows_to_be_replaced))){
    #print(i)
    # DF contaning subset of rows from whole AOI with same slope/elevation criteria as row to be replaced
    df = original_df[original_df$slope_and_elevation == rows_to_be_replaced$slope_and_elevation[i],]
    
    # Where there are still lots of rows to be replaced, do this randomly
    # i.e. taking a random sample from all of the points in the area matching that criteria
    if (nrow(rows_to_be_replaced) >= 10){
      # Update on what is happening
      #print(">8 rows to be replaced")
      # Add a randomly sampled row from those available (with enough close points)
      new_sample_df <- rbind(new_sample_df, sample_n(df, 1))}
    
    # Where there are not many rows left to be replaced, take a more targeted approach (this is more time consuming per row - hence not using it earlier)
    # i.e. randomly sampling only from rows which are close to existing samples
    if (nrow(rows_to_be_replaced) < 10){
      # Update on what is happening
      print ("Tactical replacement")
      # Find the distance between each of the locations meeting the criteria and the existing sample points
      dists <- rdist(df[,c('coords.x1', 'coords.x2')],  new_sample_df[,c('coords.x1', 'coords.x2')])
      #  Use this to count the number of existing points close to all of the possible locations to add to the sample.
      df$close_points <- apply(dists, 1, function(x)sum(x> min_dist & x < max_dist))
      # Keep only those with the minimum number of close points
      df <- df[df$close_points >= n_close_points,]
      
      # If there is >= 1 data point which matches the slope/elevation criteria and is close to the existing sample 
      # The randomly select one of these to add to the sample.
      if(nrow(df)>0){
        # Delete the close points column 
        df$close_points <- NULL
        # Add a randomly sampled row from those available to the sample
        new_sample_df <- rbind(new_sample_df, sample_n(df, 1))
        
        # If there are no data points which match the slope/elevation criteria and are close to the existing sample 
        # Then print statement saying whether t
      }else {
        print ("none suitable")
        # Store the category which it has not been possible to replace
        missed_cat <- rows_to_be_replaced$slope_and_elevation[i]
        # Add location not possible to replace to DF containing locations not possible to replace
        missed_cats <- rbind(missed_cats, rows_to_be_replaced[i])
        # Print statement detailing whether this category had a slope >2m
        if (as.numeric(sapply(regmatches(missed_cat, gregexpr("[[:digit:]]+", missed_cat)),`[`,1))> 2){
          print ("but slope >2 degrees so leaving alone")
          # Save the row we want to add back on eventually
          to_add <- rows_to_be_replaced[i]
        }else{
          print ("ERROR")
        }
      } 
    }
    # Return to any categories which have NO existing sample points close to them
    #  if (nrow(df) == 0){
    #    # Randomly chose one of the rows to add
    #    row_to_add <- sample_n(df, 1)
    #    
    #    # Find points from the whole dataframe which are near to points in category
    #    dists <- rdist(aoi_df[,c('coords.x1', 'coords.x2')],  row_to_add[,c('coords.x1', 'coords.x2')])
    #    aoi_df$close_points <- apply(dists, 1, function(x)sum(x> min_dist & x < max_dist))
    # Remove points from the same category
    #    aoi_df <- aoi_df[aoi_df$slope_and_elevation != rows_to_be_replaced$slope_and_elevation[i], ]
    
    # Randomly chose one of these
    #    row_to_add2 <- sample_n(aoi_df, 1)
    #
    #    cat <- row_to_add2$slope_and_elevation
    #    row_to_remove <- new_sample_df[new_sample_df$slope_and_elevation == cat,]
    #aoi_df = aoi_df[aoi_df$close_points > n_close_points,]
    # But need to know which point it is near
    
    # Find the most common slope/elevation type
    #most_common <- names(sort(table(original_df$slope_and_elevation),decreasing=TRUE))[1]
    # Look for one of these near the category which we....
    #most_common_df = original_df[original_df$slope_and_elevation == most_common,]
    #dists <- rdist(most_common_df[,c('coords.x1', 'coords.x2')],  df[,c('coords.x1', 'coords.x2')])
    #most_common_df$close_points <- apply(dists, 1, function(x)sum(x> min_dist & x < max_dist))
  }
  #return (new_sample_df)
  return (list(new_sample_df, missed_cats))
}

create_df_sample <- function (original_df, min_dist, max_dist, n_close_points, n_samples){
  "
  Creates a sample from a dataframe of coordinates, according to requirements that the sample should
  have the same distribution of slope and elevation values as the original dataframe and that each point 
  should be within a certain distance of at least one other - as specified in sample_constraints.
  : param df: A dataframe containing x and y coordinates as column from which a sample should be taken.
  : param sample_constraints: A dictionary containing constraints for the sampling procedure. 
  : return sample: A dataframe containing containing a sample of the input dataframe sampled according to sample_constraints.
  "
  start_time <- Sys.time()
  # Take a sample from the dataframe that matches the proportional split between slope and elevation in the whole AOI
  sample <- stratified(original_df, c("slope_and_elevation"), n_samples/nrow(original_df))
  # For each point in the sample, find the number of neighbours it has within a range between min_dist and max_dist
  sample = find_near_neighbours(sample, min_dist, max_dist, n_close_points)
  # If 0 rows have less than the requirements for n_close_points then sampling is done.
  # Else, the dataset is resampled until this condition is met.
  done <- 'Not Done'
  counter <- 0
  if (nrow(sample[sample$close_points < n_close_points,]) == 0){
    print(paste("Sampling complete after 0 resamples within", round((Sys.time() - start_time),1), "seconds"))
  } else{
    while (done !='Done') {
      # Create a coutner to record how many iterations were needed.
      counter <- counter + 1
      # Resample the dataset, removing any points without sufficient near neighbours
      # and replacing them with a point with the same slope/elevation profile from the original dataset .
      # Recount the near neighbours to each point.
      resampling_results  =  resample_far_points(sample, aoi_df, n_close_points, min_dist, max_dist)
      sample <- resampling_results[[1]]
      sample = find_near_neighbours (sample, min_dist, max_dist, n_close_points)
      # If 0 rows have less than the requirements for n_close_points then sampling is done.
      # Else, the dataset is returned for resampling.
      if (nrow(sample[sample$close_points < n_close_points,]) == 0){
        done = 'Done'
        # Rows to add back in
        extra_rows <- resampling_results[[2]]
        sample <- rbind(sample, extra_rows)
        print(paste("Sampling complete after", counter,  "resamples within", round((Sys.time() - start_time),1), "seconds"))
      }
    }
  }
  return (sample)}