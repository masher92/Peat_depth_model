# Peat_depth_model

The purpose of this model is to interpolate between... TBF.

The model takes as inputs:
* Shapefile containing x, y coordinates of locations and accompanying peat depth measurements.
* Raster containing slope values over an area of interest.
* Raster containing elevation values over an area of interest.

PeatDepthPointsCleaning
* Prelimanary cleaning of the peat depth sample data for 4 AOIs in the Yorkshire Dales to ensure they all take the same structure e.g. X, Y coordinates accompanied by a peat depth data column 'Depth'

RunModel:
* Matches each of the locations where the peat depth has been sampled to slope and elevation values from the DEM.
* Cleans the sample data - removes any points with NA for peat depth.
* Cleans the sample data - removes any duplicated points or points which are overly close together
* Adds log and sqrt transformations of the depth data
* Fits a linear model and spatial model (both with slope and elevation as covariates) and uses 10-fold cross validation
to analyse the model effectiveness and to compare the predicted values with the observed values

PredictUnmeasuredLocations:
* Uses the existing peat depth sample data to fit the model and then uses this fitted model to make predictions across the whole AOI.

CREATING A SYNTHETIC SAMPLE DATASET