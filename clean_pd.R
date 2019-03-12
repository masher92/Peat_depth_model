# Install libraries
require(rgdal)
library(raster)
library(gdistance)


convertToSPDF <- function  (datafile){
  # Convert back to shapefile
  shp <- SpatialPointsDataFrame(coords = datafile[c(4:5)], data = datafile[c(1,3,7,8)],
                                proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs
                                                  +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
  return (shp)
}

# Function which:
#   Joins a shapefile containing locations with peat depth measurements, with
#   a DEM and slope shapefile.
#   It takes as inputs:
#   1. A shapefile with peat depth measurements.
#   2. A dtm containing elevation data
#   3. A shape file containing slope data.
# And returns as an output:
#   A dataframe containing x/y coordinates alonside slope, elevation and peat depth values.
find_topographic <- function (peat_points, dtm, slope) {
  shp <- readOGR(dsn = "E:/Msc/Dissertation/Peat/Data_Analysis/1.original_data/Peat_Depth_Data_180824/Peat_Points", layer = peat_points)
  # Delete any extra coordinate columns (present in the stake moss file)
  shp@coords <- shp@coords[,1:2]
  # Convert ID and depth to numeric
  shp@data$ID <- 1:nrow(shp)
  # Create dataframe
  df <- data.frame(shp)

  # Join with topographic data
  my.pts.elevation <- as.data.frame(extract(dtm, shp))
  colnames(my.pts.elevation) <- c('elevation')
  my.pts.slope <- as.data.frame(extract(slope, shp))
  colnames(my.pts.slope) <- c('Slope_5m')
  my_pts <- cbind(df, my.pts.elevation, my.pts.slope)
  colnames(my_pts)[3:5] <- c('depth', 'longitude', 'latitude')

  # Check for any NA depth points and remove
  my_pts <- my_pts[!is.na(my_pts$depth),]
  # Convert depth to numeric
  my_pts$depth <- as.numeric(as.character(my_pts$depth))

  # Convert back to shapefile
  shp <- SpatialPointsDataFrame(coords = my_pts[c(4:5)], data = my_pts[c(1,3,7,8)],
                                proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs
                                                  +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
  writeOGR(shp, dsn ="E:/Msc/Dissertation/Code/Data/Generated", layer = "humberstone.shp", driver="ESRI Shapefile" )
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
find_nearestNeighours <- function (shp) {
  # Convert to a dataframe
  dat <- as.data.frame(shp)
  # calculate pairwise distances between points
  d <- gDistance(shp, byid=T) # rgeos package
  #d <- distm(shp)
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
  to_keep <-less_than_1[which(!duplicated(less_than_1[,c(7)])==T),]
  # Join the two dfs together
  dat <- rbind(more_than_1, to_keep)

  # Calculate number of rows which are <1m apart, print statement
  print(paste0("This sample contained ",nrow(less_than_1), " samples which are <1m apart.
                Now, one from each neighbouring pair has been removed"))

  # Convert back to shapefile
  shp <- SpatialPointsDataFrame(coords = dat[c(5:6)], data = dat[c(1,2,3,4,7)],
                                proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs
                                                  +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))

  return (shp)
  }











