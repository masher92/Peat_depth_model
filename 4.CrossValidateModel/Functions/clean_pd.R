find_duplication <- function (spdf) {
 "
Description
----------
    Checks through a spatial points dataframe and finds any rows of data with duplicated X,Y coordinates, and removes these rows
Parameters
----------
    spdf: Spatial points data frame (spdf)
        A spdf containing the locations of points and related data
Returns
-------
    spdf: Spatial points data frame (spdf)
        A spdf containing the locations of points and related data, with any rows with duplicated lat/lons values removed
"  
   
  # Convert to a shapefile
  print ("Table detailing the presence of duplicates in this dataset")
  print(data.frame(table(duplicated(spdf@coords[,c(1:2)]))))
  # Delete the duplicates
  spdf <- remove.duplicates(spdf)
  # test they have been removed
  print ("Table detailing the successful removal of duplicates from the dataset")
  print(data.frame(table(duplicated(spdf@coords[,c(1:2)]))))
  return (spdf)
}

find_nearestNeighours <- function (spdf, projection) {
"
Description
----------
    Checks through a spatial points dataframe and finds the distance between the location of each point and its nearest neighbour.
    Deletes one from any pair which are more than 1m close to each other.
Parameters
----------
    spdf: Spatial points data frame (spdf)
        A spdf containing the locations of points and related data
    projection: string
        A string specifyingt the projection, either 'bng' or 'wgs84
Returns
-------
    new_spdf: Spatial points data frame (spdf)
        A spdf containing the locations of points and related data, with one from each of any pair of locations which
        are <1m apart removed.
"  
  
  # Convert the spdf to a dataframe
  dat <- as.data.frame(spdf)
  # Rename columns
  colnames(dat)[c(4,5)] <- c('longitude', 'latitude')
  # Calculate pairwise distances between points
  if (projection == 'bng'){
    d <- gDistance(spdf, byid=T) # rgeos package
  }else if(projection == 'wgs84') {
    d <- distm(spdf)}
  
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
  print(paste0("This sample contained ",nrow(less_than_1), " samples which are <1m apart. Now, one from each neighbouring pair has been removed"))
  
  # Convert back to shapefile
  original_crs = crs(spdf)
  new_spdf <- SpatialPointsDataFrame(coords = dat[c('longitude', 'latitude')], data = dat[c('depth', 'elevation', 'Slope_5m')],
                                proj4string = CRS(original_crs@projargs))
  
  return (new_spdf)
}

