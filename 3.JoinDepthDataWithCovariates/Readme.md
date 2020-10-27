## 3. JoinDepthData_withCovariates

### JoinDepthData_withCovariates.R
This script takes a shapefile containing the locations at which peat depth measurements have been measured.  
It also loads in raster data containing slope and elevation values covering the same area.  
For each point in the sample of peat depth points it extracts the values from the slope/elevation raster cells within which the point falls.  
A new shapefile is created which contains the locations at which peat depth measurements have been measured, but with new data variables containing the slope and elevation values at these points. 
This is saved to file.  
