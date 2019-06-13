convertToSPDF <- function  (datafile){
  # Convert back to shapefile
  shp <- SpatialPointsDataFrame(coords = datafile[c(4:5)], data = datafile[c(1,3,7,8)],
                                proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs
                                                  +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
  return (shp)
}

# Function which:
#   Joins a shapefile containing locations with peat depth measurements, with values from rasters.
#   It takes as inputs:
#   1. A shapefile with peat depth measurements.
#   2. A list containing the rasters of interest.
#   3. A list containing the names of the rasters of interests for renaming.
# And returns as an output:
#   A dataframe containing x/y coordinates, peat depth values and all the values from the rasters of interest.
join_topographic_data <- function (pd_samples_shp, covars, covar_names) {
  # Create dataframe
  df <- data.frame(pd_samples_shp)
  # Extract the values from the rasters which match the points in the pd_samples dataset
  for (covar in covars) {
    my.pts <- as.data.frame(extract(covar, pd_samples_shp))
    df <- cbind(df, my.pts)
  } 
  # Rename columns
  colnames(df)[c(1:3, 5:ncol(df))] <- c('depth', 'longitude', 'latitude', covar_names)
  
  # Convert back to shapefile
  shp <- SpatialPointsDataFrame(coords = df[c(2,3)], data = df[c(1,5:ncol(df))],
                                proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs
                                                  +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
  return(shp)
}


# Function which:
#   Checks for the presence of coordinates with duplicated X and Y coordinates
#   Removes the duplicates
# It takes as input:
#   1. a spatial points data frame
find_duplication <- function (shp) {
  # Convert to a shapefile
  print ("Table detailing the presence of duplicates in this dataset")
  print(data.frame(table(duplicated(shp@coords[,c(1:2)]))))
  # Delete the duplicates
  shp <- remove.duplicates(shp)
  # test they have been removed
  print ("Table detailing the successful removal of duplicates from the dataset")
  print(data.frame(table(duplicated(shp@coords[,c(1:2)]))))
  return (shp)
}

# Function which:
#   Finds the distance between each x/y coordinate and its nearest neighbour.
#   Deletes one from any pair which are more than 1m close to each other.
# It takes as inputs:
# 1.
find_nearestNeighours <- function (shp, projection) {
  # Convert to a dataframe
  dat <- as.data.frame(shp)
  # calculate pairwise distances between points
  if (projection == 'wgs84'){
    d <- gDistance(shp, byid=T) # rgeos package
  }else if(projection == 'bng') {
    d <- distm(shp)
  }
  
  # Find second shortest distance (closest distance is of point to itself, therefore use second shortest)
  # For each row (which represents one sample point) place in an ascending order and select the 2nd point)
  min.d <- apply(d, 1, function(x) sort(x, decreasing=F)[2])
  # Join to original dataframe
  df <- cbind(dat, min.d)
  
  # Find rows where distance to nn is more than 1
  more_than_1 <- df[df$min.d > 1,]
  # Find rows where distance to nn is less than 1
  less_than_1 <- df[df$min.d < 1,]
  # Keep one version of each of the pairs
  to_keep <-less_than_1[which(!duplicated(less_than_1[,c('min.d')])==T),]
  # Join the two dfs together
  dat <- rbind(more_than_1, to_keep)
  
  # Calculate number of rows which are <1m apart, print statement
  print(paste0("This sample contained ",nrow(less_than_1), " samples which are <1m apart.
                Now, one from each neighbouring pair has been removed"))
  
  # Convert back to shapefile
  shp <- SpatialPointsDataFrame(coords = dat[c('longitude', 'latitude')], data = dat[c('depth', 'elevation', 'Slope_5m')],
                                proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs
                                                  +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
  
  return (shp)
  }


# Function which:
#   Finds the distance between each x/y coordinate and its nearest neighbour.
#   Deletes one from any pair which are more than 1m close to each other.
# It takes as inputs:
convert_projection <- function (input, projection_system_required) {
  # If a dataframe 
  if(projection_system_required == 'wgs84'){
    projection_system = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'}
  if (projection_system_required == 'bng'){
    projection_system = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs towgs84='446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894'"
  } 
  converted = spTransform(input,CRS(projection_system))
  
  # If a dataframe 
  #if(is.data.frame(dtm)){
  #  print("Its a dataframe")
  #  
  #} else {
  #  print("Its not a dataframe")
  #  # Convert projection
  #  converted = spTransform(input,CRS(projection_system_required))
  #}
  return (converted)}



