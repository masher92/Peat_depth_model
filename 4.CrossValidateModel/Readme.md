## Model Cross-Validation

This script cross-validates the performance of both a linear and geostatistical model for predicting depth across a study area.
The models are fitted using a sample of points with peat depth measurements, this sample has been joined with slope and elevation data, and these variables are used as the predictor variables. 
The square root of depth is the response variable.   

The peat depth sample being used here contains some duplicated points and some very geographically close points. The script contains an option to remove these points. 

Once the results of the cross-validation have been obtained, the script creates a table of results containing the following metrics, for both the linear and the geostatistical model:
* Bias
* Root Mean Squared Error (RMSE)
* Coverage
* Prediction interval width 
* Correlation Coefficient  

These results tables are saved in csv files.

The script also plots predicted value vs. observed values.  
For each point where depth was measured, this depth is the observed value and the predicted value is the mean of the values predicted across the cross-validation runs.

### Functions
This script calls various functions contained within the Functions/ directory.  

##### clean_pd.R
* find_duplication -- searches for peat depth points in the sample with duplicated lat/long coordinates. If it finds them, deletes duplicates to keep only one   
* find_nearestNeighours -- searches for peat depth points in the sample which are less than 1m apart. If it finds them, deletes points to keep only one   

##### check_models.R

##### cross_validate.R

##### analyse_results.R
