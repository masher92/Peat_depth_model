## Model Cross-Validation

This script cross-validates the performance of both a linear and geostatistical model for predicting depth across a study area.
The models are fitted using a sample of points with peat depth measurements, this sample has been joined with slope and elevation data, and these variables are used as the predictor variables. 
The square root of depth is the response variable.   

The peat depth sample being used contained some duplicated points and some very geographically close points. The script contains an option to remove these points. 

Once the results of the cross-validation have been obtained, the script creates a table of results containing the following metrics:
* Bias
* Root Mean Squared Error (RMSE)
* Coverage
* Prediction interval width 
* Correlation Coefficient  

These results are saved in csv files.

The script also plots predicted value vs. observed values. 
For each point where depth was measured, this depth is the observed value and the predicted value is the mean of the values predicted across the cross-validation runs.
