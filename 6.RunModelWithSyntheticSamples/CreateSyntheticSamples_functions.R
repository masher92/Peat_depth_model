create_regular_grid <- function (grid_spacing) {
  # Coordinates of a larger square area around the AOI
  coords = matrix(c(-1.896798, 54.061784,
                    -1.779337, 54.061784,
                    -1.779337,54.014181, 
                    -1.896798, 54.014181), 
                  ncol = 2, byrow = TRUE)
  
  # Convert to spatialpolygon, and specify it has WGS84 projection
  bbox = Polygon(coords)
  bbox = SpatialPolygons(list(Polygons(list(bbox), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  # Convert projection - needs to be in BNG for metres used in defining the grid
  bbox <- spTransform(bbox, CRS("+init=epsg:27700"))
  
  # Create a grid over the area of this bounding box, with the distance of spaces between the points
  # defined by grid_spacing 
  grid <- spsample(bbox, cellsize = grid_spacing, type = "regular")
  return(grid)
}

shift_grid <- function(grid, grid_spacing, n_shifts){
  # Define the range of coordinates possible
  long_range <- seq(0,grid_spacing, by =5)
  lat_range <-  seq(0,grid_spacing, by =5)
  
  # Create dataframe to store results
  all_positions <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Long", "Lat"))
  
  # Create a dataframe containing amounts to shift x, y coordiantes
  # Each should be unique
  for (i in (1:80)){
    position <- data.frame(sample(long_range,1), sample(lat_range,1))
    if (nrow(match_df(all_positions, position))==0){
      all_positions <- rbind(all_positions, position)}}
  
  # Trim to be just 50 long
  all_positions <- all_positions[c(1:n_shifts),]
  
  # Check if any of the rows are duplicated
  print(data.frame(table(duplicated(grid_shifts[,c(1:2)]))))
  
  return (all_positions)}
