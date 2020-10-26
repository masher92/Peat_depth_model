####################################################################################################
# Define function to be used
####################################################################################################

select_depth_from_measured_samples <- function (pd_sample_pts_with_covars, row_to_check, slope_elevation_category){
  
"
Description
----------
    For each location in a dataset of synthetic sample locations which need to be assigned depths:
       Find all of the measured peat depth sample points with the same slope/elevation category as the synthetic sample location
       Find the geographic distance between each of these points with the s/e category and the synthetic sample point
       Assign the synthetic sample point the depth of the point which is geographically closest to it.
Parameters
----------
    pd_sample_pts_with_covars: Spatial points data frame (spdf)
        A spdf containing the locations of points where the peat depth has been sampled, including values for their depth, slope and elevation
    row_to_check: Spatial points data frame (spdf)
        A spdf containing data referring to one point in a dataset of locations to be part of a synthetic depth dataset, for which a depth
        value needs to be assigned
    slope_elevation_category: string
        A string defining the slope/elevation category of this point
Returns
-------
    row_with_depth: Spatial points data frame (spdf)
        A spdf containing data referring to one point in a dataset of locations to be part of a synthetic depth dataset, with a depth assigned
    smallest_distance: num 
        The distance between this synthetic sample location and the nearest measured sample location from which the depth value was taken 
"
  
  # Keep the rows in the dataframe of measured peat depth samples which have the same slope/elevation category as 
  # the synthetic sample location for which the depth is being assigned
  samples_with_matching_s_e_category <- pd_sample_pts_with_covars[pd_sample_pts_with_covars$slope_and_elevation == slope_elevation_category ,]
  
  # Find the distance between the location of each of these and the location of the synthetic sample point
  # Start with a unrealistically high distance; iterate through the points and if any distance is lower then 
  # take this point to be the closest 
  location <- c(row_to_check$longitude, row_to_check$latitude)
  smallest_distance <- 1000000
  for (i in (1:nrow(samples_with_matching_s_e_category))){
    dist <- distm(location, samples_with_matching_s_e_category[i,]@coords, fun = distHaversine)
    if (dist < smallest_distance){
      smallest_distance <- dist
      closest_row <- i}
  }
  
  # Take just the row in the dataframe of measured peat depth samples with the same slope/elevation category as the synthetic sample
  # location in question, which is geographically closest to the synthetic sample point in question
  closest_point <- samples_with_matching_s_e_category[closest_row,]
  
  # Assign the synthetic sample this depth
  row_to_check$depth <- closest_point$depth
  
  # Convert to datafrme
  row_with_depth = as.data.frame(row_to_check)
  
  return(list(row_with_depth, smallest_distance))}


assign_depths <- function (pd_sample_pts_with_covars, synthetic_sample_locations){
"
Description
----------
    For a dataframe containing locations to be used in a synthetic sample:
      Converts the slope and elevation of these locations into a categorical slope/elevation variable
      Loops through each row in the dataframe of synthetic sample locations, for each row:
        Finds points in a dataset of locations where the depth has been measured which have the closest matching slope/elevation 
        category to the synthetic sample row
        Finds which of these points is geographically closest to the synthetic sample row
        Assigns the depth value of this measured sample point to the synthetic sample row
Parameters
----------
    pd_sample_pts_with_covars: Spatial points data frame (spdf)
        A spdf containing the locations of points where the peat depth has been sampled, including values for their depth, slope and elevation
    synthetic_sample_locations: Dataframe
        A dataframe containing the location, and slope and elevation values, of points to be included in a synthetic sample dataset
Returns
-------
    synthetic_sample: data frame 
        A dataframe containing locations in a synthetic sample with their slope, elevation and depth values
"
    
  #######################################################################################################
  # Create categorical slope and elevation variables in the spatial points dataframe containing the locations
  # with measured peat depth, and in the dataframe containing the locations to be used in the synthetic sample
  #######################################################################################################
  # Remove points with a depth of 0
  pd_sample_pts_with_covars <- pd_sample_pts_with_covars[pd_sample_pts_with_covars$depth >0,]
  
  # Locations with measured depths
  pd_sample_pts_with_covars$Slope_Cuts <- cut(pd_sample_pts_with_covars$Slope_5m, breaks = c(0, 2, 4,6,8,10,12,14,16,18,20,22, 24, 26,28,30,Inf)) 
  pd_sample_pts_with_covars$Elevation_Cuts <- cut(pd_sample_pts_with_covars$elevation, breaks = c(250,275,300,325,350,375,400, 425,450,Inf))
  pd_sample_pts_with_covars$slope_and_elevation <- as.character(paste(pd_sample_pts_with_covars$Slope_Cuts, pd_sample_pts_with_covars$Elevation_Cuts))
  
  # Locations to be used in synthetic sample
  synthetic_sample_locations$Slope_Cuts <- cut(synthetic_sample_locations$Slope_5m, breaks =c(0, 2, 4,6,8,10,12,14,16,18,20,22, 24, 26,28,30,Inf)) 
  synthetic_sample_locations$Elevation_Cuts <- cut(synthetic_sample_locations$elevation, breaks = c(250,275,300,325,350,375,400, 425,450,Inf))
  synthetic_sample_locations$slope_and_elevation <- as.character(paste(synthetic_sample_locations$Slope_Cuts, synthetic_sample_locations$Elevation_Cuts))
  
  #######################################################################################################
  # Find the number of sample points within each of the slope-elevation categories
  # And define the top and bottom values in each of these categories
  #######################################################################################################
  cleaned_pd_covar_distribution <- as.data.frame(table(pd_sample_pts_with_covars$slope_and_elevation))
  
  cleaned_pd_covar_distribution$slope_lower <-as.numeric(sapply(regmatches(cleaned_pd_covar_distribution$Var1, gregexpr("[[:digit:]]+", cleaned_pd_covar_distribution$Var1)),`[`,1)) 
  cleaned_pd_covar_distribution$slope_higher <-as.numeric(sapply(regmatches(cleaned_pd_covar_distribution$Var1, gregexpr("[[:digit:]]+", cleaned_pd_covar_distribution$Var1)),`[`,2)) 
  cleaned_pd_covar_distribution$elevation_lower <-as.numeric(sapply(regmatches(cleaned_pd_covar_distribution$Var1, gregexpr("[[:digit:]]+", cleaned_pd_covar_distribution$Var1)),`[`,3))
  cleaned_pd_covar_distribution$elevation_higher <- as.numeric(sapply(regmatches(cleaned_pd_covar_distribution$Var1, gregexpr("[[:digit:]]+", cleaned_pd_covar_distribution$Var1)),`[`,4))  
  
  #######################################################################################################
  # 
  #######################################################################################################
  
  # Create empty version of dataframe to repopulate
  synthetic_sample = synthetic_sample_locations[0,]
  synthetic_sample <- data.frame(matrix(0, ncol = 8, nrow = 0))
  colnames(synthetic_sample) <- colnames(synthetic_sample_locations) 
  colnames(synthetic_sample)[8] <- "depth"
  
  # Convert projections, assigning depths needs WGS84
  pd_sample_pts_with_covars <- spTransform(pd_sample_pts_with_covars, CRS("+init=epsg:4326"))
  synthetic_sample_locations_spdf <- SpatialPointsDataFrame(coords = synthetic_sample_locations[c(1,2)], 
                                                data = synthetic_sample_locations[c(3,4,5,6,7)],
                                                proj4string =  CRS("+init=epsg:27700"))
  synthetic_sample_locations_spdf <- spTransform(synthetic_sample_locations_spdf, CRS("+init=epsg:4326"))  
  synthetic_sample_locations <- as.data.frame(synthetic_sample_locations_spdf)
  
  # Move through each row in the dataframe containing the synthetic sample locations
  # Check whether the slope/elevation category of that row is found in the slope/elevation
  # categories defined using the measured peat depth points
  # If it is, then use select_depth_from_distrib to assign a suitable depth value from the points
  # in the measured peat depth points in the same category
  # If it is not, then 
  distances <- c()
  for (i in (1:nrow(synthetic_sample_locations_spdf))){
    print(i)
    row <- synthetic_sample_locations_spdf[i,]
    
    # If the slope/elevation category matches the categories found in the dataframe of measured peat depth samples:
    # Define a suitable depth for that synthetic sample location
    if (row$slope_and_elevation %in% cleaned_pd_covar_distribution$Var1){
      print("Row in existing category")
      slope_elevation_category <- row$slope_and_elevation 
      row_with_depth <- select_depth_from_measured_samples(pd_sample_pts_with_covars, row, slope_elevation_category)[[1]]
      distances[[i]] <- select_depth_from_measured_samples(pd_sample_pts_with_covars, row, slope_elevation_category)[[2]]
      } else {
        print("Row not in existing category same category")
        # If the slope/elevation category does not match the categories found in the dataframe of measured peat depth samples:
        # Find the slope/elevation category in the measured peat depth samples which is the best fit to this synthetic sample location 
        
        # Find the lower slope value of the band which is closest to the slope value at this synthetic sample location
        slope_row_no <-which(abs(unique(cleaned_pd_covar_distribution$slope_lower)-row$Slope_5m)==min(abs(unique(cleaned_pd_covar_distribution$slope_lower)-row$Slope_5m)))
        slope_value<- unique(cleaned_pd_covar_distribution$slope_lower)[slope_row_no]
        
        # Filter the dataframe to only contain those with this slope_value
        matching_slope <- cleaned_pd_covar_distribution[(cleaned_pd_covar_distribution$slope_lower == slope_value),]
        
        # Of those with the nearest slope value, find the closest matching lower elevation bands value
        elev_row_no <-which(abs(unique(matching_slope$elevation_lower)-row$elevation)==min(abs(unique(matching_slope$elevation_lower)-row$elevation)))
        elev_value <- unique(matching_slope$elevation_lower)[elev_row_no]
        
        # Turn this into a slope_elevation category 
        proxy_cat <- as.character(cleaned_pd_covar_distribution[(cleaned_pd_covar_distribution$slope_lower == slope_value) & (cleaned_pd_covar_distribution$elevation_lower == elev_value),]$Var1)
        

        row_with_depth <- select_depth_from_measured_samples(pd_sample_pts_with_covars, row, proxy_cat)[[1]]
        distances[i] <- select_depth_from_measured_samples(pd_sample_pts_with_covars, row, proxy_cat)[[2]]
        }
  # Add the row with added depths onto a dataframe
  synthetic_sample <- rbind(synthetic_sample, row_with_depth)
      }
  
  return(synthetic_sample)
}


