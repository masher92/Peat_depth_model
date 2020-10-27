## Establishing covariate values at locations with depth measurements

Both the linear model and the geostatistical model use slope and elevation data as covariates.  
As such, it is necessary to define slope and elevation values at the locations where the depth has been measured.  
Slope and elevation values are downloaded at 5km resolution from Digimap at: https://digimap.edina.ac.uk/roam/download/os

### JoinDepthData_withCovariates.R
This script takes a shapefile containing the locations at which peat depth measurements have been measured.  
It also loads in raster data containing slope and elevation values covering the same area.  
For each point in the sample of peat depth points it extracts the values from the slope/elevation raster cells within which the point falls.  
A new shapefile is created which contains the locations at which peat depth measurements have been measured, but with new data variables containing the slope and elevation values at these points.  
This is saved to file.  
