# Geostatistical modelling of peat depth

Mapping the spatial distribution of peatland depth is vital for quantifying the size of the peatland carbon pool, and assessing the risk presented by its release into the atmosphere under global peatland degradation. Peat depth can be predicted at the landscape scale through interpolation between manual depth samples. Whilst linear interpolation using topographic parameters has some predictive skill, geostatistical models with covariates have been shown to increase prediction accuracy by incorporating both the autogenic and allogenic factors that influence peat accumulation (Young et al, 2018). Realisation of geostatistical model benefits is contingent on samples being spatially autocorrelated. Focusing on a blanket peatland in Yorkshire, this research further evidences the benefits of this approach, before investigating the influence of sample design on geostatistical model performance.

## Table of contents

1. [ Motivation. ](#motiv)
2. [ Installation. ](#install)
3. [ How to use. ](#workflow)
4. [ Example Usage. ](#ex)
5. [ Next stages. ](#nextup)
6. [ License. ](#lic)
7. [ References. ](#ref)

<a name="motiv"></a>
## Motivation
The overarching aim of this research is to investigate means of improving the accuracy of landscape-scale empirical models of peat depth based on discrete manual depth measurements, through provision of evidence to inform and improve manual depth data collection principles. It aims to provide advice that would be readily implementable by peat practitioners and those researching in peatlands.

The study focuses on a blanket peatland in the Yorkshire Dales and pursues this aim through the following objectives:
* To corroborate the findings of Young et al (2018) in a blanket peatland in Dorset, that a geostatistical model of peat depth, with slope and elevation as covariates, improves on the performance of a linear model based on these topographic parameters alone.
* To investigate the minimum distance required between sample points in a regular grid to ensure that spatial autocorrelation, a prerequisite for increased geostatistical
model accuracy, is observed.
* To explore alternative approaches to manual sampling in blanket peatlands which facilitate accurate geostatistical modelling of depth whilst minimising sample number
requirements.

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

