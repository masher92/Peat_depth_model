# Geostatistical modelling of peat depth

Mapping the spatial distribution of peatland depth is vital for quantifying the size of the peatland carbon pool, and assessing the risk presented by its release into the atmosphere under global peatland degradation. Peat depth can be predicted at the landscape scale through interpolation between manual depth samples. Whilst linear interpolation using topographic parameters has some predictive skill, geostatistical models with covariates have been shown to increase prediction accuracy by incorporating both the autogenic and allogenic factors that influence peat accumulation (Young et al, 2018). 

Geostatistical model accuracy relies on samples being both spatially dependent and evenly distributed. A gridded layout is often favoured for peat depth sampling; however, evidence from Digital Soil Mapping (DSM) indicates that geostatistical model accuracy can be improved with a spatial coverage sample which avoids the rigidity of a grid design whilst still dispersing samples well (Walvoort et al., 2010). DSM research by Wadoux et al. (2019) also indicates the importance of including some tightly spaced observations to allow the model to characterise spatial dependency over short distances.

Focussing on a blanket peatland in the Yorkshire Dales, the work is split into two main sections:  
1. Determining whether for this study area, in agreement with the findings of Young et al (2018) in blanket peatland in Dorset, a geostatistical model of peat depth with slope and elevation as covariates, improves on the performance of a linear model based on these topographic parameters alone.  
2. Exploring how sampling strategies influence geostatistical model accuracy, through investigating i) the grid resolution required to harness geostatistical model benefits and (ii) whether alternative sample layouts allow benefits to be realised with fewer sample points.

## Table of contents

1. [ Motivation. ](#motiv)
2. [ Installation. ](#install)
3. [ Workflow. ](#workflow)  
  a. [ Comparing geostatistical and linear model performance. ](#workflowa)  
  b. [ Exploring the impact of sampling strategy ](#workflowb)  
4. [ Example Usage. ](#ex)
5. [ Next stages. ](#nextup)
6. [ License. ](#lic)
7. [ References. ](#ref)

<a name="motiv"></a>
## Motivation
Mapping the spatial distribution of peatland depth is vital for quantifying the size of the peatland carbon pool, and assessing the risk presented by its release into the atmosphere under global peatland degradation. Peat depth can be predicted at the landscape scale through interpolation between manual depth samples. Whilst linear interpolation using topographic parameters has some predictive skill, geostatistical models with covariates have been shown to increase prediction accuracy by incorporating both the autogenic and allogenic factors that influence peat accumulation (Young et al, 2018). Realisation of geostatistical model benefits is contingent on samples being spatially autocorrelated. Focusing on a blanket peatland in Yorkshire, this research further evidences the benefits of this approach, before investigating the influence of sample design on geostatistical model performance.

<a name="install"></a>
## Installation
Running this code requires installation of R.
Packages required include:
* Spcosa
* raster
* rgdal
* geosphere
* rgeos
* geoR

<a name="workflow"></a>
## Workflow
<a name="workflowa"></a>
###  Determining whether a geostatistical model of peat depth improves on the performance of a linear model 


<a name="workflowb"></a>
###  Exploring how sampling strategies influence geostatistical model accuracy.

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

<a name="nextup"></a>
## Next stages
1. Find distribution of slope and elevation values across the AOI (convert raster into a dataframe of lat, long coordinates with associated slope/elevation values. Use this to create slope/elevation categorical categories).
2. Use this to take a sample with the same proportion of each slope/elevation category as the study area as a whole (using stratified sampling)
3. Check for sample points which have no other sample points close to them, and then resample these points by randomly selecting a new sample point from the study area which is within the same slope/elevation category. 

1. Convert slope and elevation rasters into dataframes containing slope and elevation of each location (lat, long coordinates)

<a name="ref"></a>
## References
Young, D.M., Parry, L.E., Lee, D. and Ray, S., 2018. Spatial models with covariates improve estimates of peat depth in blanket peatlands. Plos one, 13(9), p.e0202691.

