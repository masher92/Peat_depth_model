# Geostatistical modelling of peat depth

## Table of contents

1. [ Motivation. ](#motiv)
2. [ Installation. ](#install)
3. [ Workflow. ](#workflow)  
  a. [ Comparing geostatistical and linear model performance. ](#workflowa)  
  b. [ Exploring the impact of sampling strategy ](#workflowb)  
4. [ Example Usage. ](#ex)
5. [ Next stages. ](#nextup)
6. [ References. ](#ref)

<a name="motiv"></a>
## Motivation
Mapping the spatial distribution of peatland depth is vital for quantifying the size of the peatland carbon pool, and assessing the risk presented by its release into the atmosphere under global peatland degradation. Peat depth can be predicted at the landscape scale through interpolation between manual depth samples. Whilst linear interpolation using topographic parameters has some predictive skill, geostatistical models with covariates have been shown to increase prediction accuracy by incorporating both the autogenic and allogenic factors that influence peat accumulation ([Young et al, 2018)](#young2018). 

* [Section 3a.](#workflowa) of this work determines for a blanket peatland in the Yorkshire Dales whether a geostatistical model of peat depth with slope and elevation as covariates, improves on the performance of a linear model based on these topographic parameters alone. This seeks to corroborate the findings of [Young et al (2018)](#young2018) in a blanket peatland in Dorset.

Geostatistical model accuracy relies on samples being both spatially dependent and evenly distributed. A gridded layout is often favoured for peat depth sampling; however, evidence from Digital Soil Mapping (DSM) indicates that geostatistical model accuracy can be improved with alternative sampling approaches.

* [Section 3b.](#workflowa) of this work explores how sampling strategies influence geostatistical model accuracy, through investigating i) the grid resolution required to harness geostatistical model benefits and (ii) whether alternative sample layouts allow benefits to be realised with fewer sample points. Alternative sampling strategies considered include:
  * A spatial coverage sample which avoids the rigidity of a grid design whilst still dispersing samples well ([Walvoort et al., 2010](#walvoort2010)).
  * The addition of clusters of points at shorter distances from one another to both spatial coverage samples and samples collected on a regular grid. DSM research by [Wadoux et al (2019)](#wadoux2019) indicates the importance of including some tightly spaced observations to allow the model to characterise spatial dependency over short distances.


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
###  Comparing geostatistical and linear model performance
The accuracy of two different statistical models of peat depth was compared, based on those employed by Young et al. (2018):
1. A linear model using slope and elevation as covariates 
2. A geostatistical model configured with slope and elevation covariates using universal kriging

The input to both model is a dataframe with each row containing:
* The latitude and longitude of a location
* The peat depth measured at that location
* The slope and elevation value at that location

Predictive performance of both models is assessed using 10-fold cross-validation.  
The peat depth measurements dataset is randomly split into ten subsets. Iteratively, the model is fitted on nine subsets and tested on the remaining one, until each subset has been used as testing data. This whole process is repeated ten times, with each based on a different starting random subset split, to reduce the probability of model artefacts due to subset selection.  

<ins> 1. DefineStudyArea </ins>  
This directory contains code which defines the boundaries of the study area within which we are attempting to predict peat depth.  
<ins> 2. CleanDepthSampleData </ins>  
This directory contains code which cleans peat depth sample data for four study areas in the Yorkshire Dales to ensure they all take the same structure e.g. X, Y coordinates accompanied by a peat depth data column 'Depth'.   
<ins> 3. JoinDepthDataWithCovariates </ins>  
This directory contains code which extracts for each location with a measured peat depth value a slope and elevation value using raster files at 5m resolution from Digimap.     
<ins> 4. CrossValidateModel </ins>  
This directory contains code which uses the dataset resulting from the above stages (containing locations in the study area with peat depth measurements and slope and elevation values) to cross validate the performance of both a linear and geostatistical model. The results of the cross-validation are used to create a table of results containing the following metrics: bias, root mean squared error, coverage, prediction interval width and correlation coefficient.   
<ins> 5. PredictUnmeasuredLocations </ins>  
Uses the existing peat depth sample data to fit the model and then uses this fitted model to make predictions across the whole AOI.  

<a name="workflowb"></a>
###  Exploring the impact of sampling strategy
The second component of this research evaluates the impact on geostatistical model performance of the method used to collect the peat depth samples used in the model. 

<ins> 6. TestSamplingStrategies </ins>  
Synthetic peat depth samples are constructed with a range of sizes using four configurations:
* Regular grid
* Regular grid with short distance subset
* Spatial Coverage sample
* Spatial coverage sample with short distance subset

For each sampling configuration and sample size the predictive performance of both the linear and geostatistical model are assessed using 10-fold cross-validation, and the results are then compared.

<a name="nextup"></a>
## Next stages
Additional stages which could be explored and would contribute further to the outputs of this research:  
1. This researches focusses solely on the sample’s coverage of geographic space. Further improvements of geostatistical model performance may be possible through additional optimisation of sample coverage of the covariate feature space. Options for achieving this:
    * Use stratified sampling to take samples with the same proportion of slope/elevation categories as the study area as a whole. 
    * When choosing the short range subset this could be biased towards choosing extra sample points in areas with certain topographic characteristics

<a name="ref"></a>
## References
<a name="wadoux2019"></a>
Wadoux, A. M.-C., Marchant, B. P., & Lark, R. M. (2019). Efficient sampling for geostatistical surveys. European Journal of
Soil Science, 1–15.  
<a name="walvoort2010"></a>
Walvoort, D. J., Brus, D., & De Gruijter, J. (2010). An R package for spatial coverage sampling  
<a name="young2018"></a>
Young, D.M., Parry, L.E., Lee, D. and Ray, S., 2018. Spatial models with covariates improve estimates of peat depth in blanket peatlands. Plos one, 13(9), p.e0202691.

