# Geostatistical modelling of peat depth

Mapping the spatial distribution of peatland depth is vital for quantifying the size of the peatland carbon pool, and assessing the risk presented by its release into the atmosphere under global peatland degradation. Peat depth can be predicted at the landscape scale through interpolation between manual depth samples. Whilst linear interpolation using topographic parameters has some predictive skill, geostatistical models with covariates have been shown to increase prediction accuracy by incorporating both the autogenic and allogenic factors that influence peat accumulation (Young et al, 2018). Realisation of geostatistical model benefits is contingent on samples being spatially autocorrelated. Focusing on a blanket peatland in Yorkshire, this research further evidences the benefits of this approach, before investigating the influence of sample design on geostatistical model performance.

## Table of contents

1. [ Motivation. ](#motiv)
2. [ Installation. ](#install)
3. [ How to use. ](#use)
4. [ Example Usage. ](#ex)
5. [ License. ](#lic)
6. [ References. ](#ref)

<a name="motiv"></a>
## Motivation



<a name="use"></a>
## How to use

The input to the model is a dataframe containing the following information:
* Lat and long coordinates 

model takes as an input a dataframe containing 
* Dataframe containing x, y coordinates of locations and accompanying peat depth measurements.
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

## Creating a synthetic sample dataset
### Regular grid
Uses makegrid function (gstat), with a given cell spacing
Trim to AOI boundary/moorland line boundary
Assign depths using synthetic depth generation method

### Regular grid with short distance subset
As above
Randomly select some of the points around which to base short distance clusters
Create a buffer around these points of Xm, and then find the points from the geodataframe of all points within the AOI that fall within this buffer
Randomly select from these points X number of points (depending on number of points to have in each cluster)

### Spatial coverage sample
Uses stratify function (spcosa) to split the study area into (equally?) sized areas of a specified number.
One sample point is then located within each sub-area. Literature suggests this should be the centroid of each sub-area; however, this does not seem to be what is happening. Not sure why not...

### Spatial coverage sample with short distance subset
As above
Randomly select some of the points around which to base short distance clusters
Create a buffer around these points of Xm, and then find the points from the geodataframe of all points within the AOI that fall within this buffer
Randomly select from these points X number of points (depending on number of points to have in each cluster)

##### Extra bits
1. Find distribution of slope and elevation values across the AOI (convert raster into a dataframe of lat, long coordinates with associated slope/elevation values. Use this to create slope/elevation categorical categories).
2. Use this to take a sample with the same proportion of each slope/elevation category as the study area as a whole (using stratified sampling)
3. Check for sample points which have no other sample points close to them, and then resample these points by randomly selecting a new sample point from the study area which is within the same slope/elevation category. 

1. Convert slope and elevation rasters into dataframes containing slope and elevation of each location (lat, long coordinates)

<a name="ref"></a>
## References
Young, D.M., Parry, L.E., Lee, D. and Ray, S., 2018. Spatial models with covariates improve estimates of peat depth in blanket peatlands. Plos one, 13(9), p.e0202691.

