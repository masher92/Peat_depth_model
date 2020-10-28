## Assessing model accuracy with cross-validation
The "RunCrossValidation.R" script assesses the performance of both a linear and geostatistical model for predicting depth across a study area, using 10-fold cross-validation.

#### Model training data

The models are fitted using a sample of points with peat depth measurements.  The locations of these samples are shown in Figure 1.  
These samples are primarily collected on a regularly spaced grid, however, there are some gaps in the grid and some sample points that are more clustered together.  
This sample has been joined with slope and elevation data, and these variables are used as the predictor variables. The square root of depth is the response variable.   

<p align="center">
<img src="Figs/PeatDepthSample_locations.png" width="700"  title="Full study area outline" />
<p align="center">Figure 1. Location of peat depth measurements within study area <p align="center">

The peat depth sample being used here contains some duplicated points and some very geographically close points. The script contains an option to remove these points.  
The script also contains an option to carry out the cross-validation using the sample data in either British National Grid (BNG) or WGS84 projection. Currently, if the model is run in BNG the geostatistical model doesn't work properly and gives the same results to the linear model. Still not sure why this is.

#### Assessing model accuracy
The results of the cross-validation are used to create a table of results containing the following metrics, for both the linear and the geostatistical model:
* Bias
* Root Mean Squared Error (RMSE)
* Coverage
* Prediction interval width 
* Correlation Coefficient  

These results tables are saved in csv files.

Predicted values vs. observed values are also plotted.  
For each point where depth was measured, this depth is the observed value and the predicted value is the mean of the values predicted across the cross-validation runs.

<p align="center">
<img src="Figs/LM_CV.PNG" width="400"  title="Full study area outline" />
<img src="Figs/SM_CV.PNG" width="400"  title="Full study area outline" />
<p align="center">Figure 2. Predicted vs observed depth values, for linear model (left) and spatial model (right) <p align="center">
