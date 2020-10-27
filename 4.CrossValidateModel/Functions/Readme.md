### Functions
This script calls various functions contained within the Functions/ directory.  

##### Clean_pd.R
* Find_duplication.py 
    * Searches for peat depth points in the sample with duplicated lat/long coordinates. If it finds them, deletes duplicates to keep only one   
* Find_nearestNeighours.py 
    * Searches for peat depth points in the sample which are less than 1m apart. If it finds them, deletes points to keep only one   

##### Check_models.R
* Check_lm.py --
* Check_sm.py --

##### Cross_validate.R
* Cross_validate.py 

##### Analyse_results.R
* Create_results.py -- Creates a table detailing for both the linear and geostatistical model statistics of model performance (bias, RMSE, coverage, interval width) 
* Create_predicted_vs_observed.py -- Creates a table detailing for each location with a measured peat depth value, the mean peat depth value predicted at that location by both linear and geostatistical models across the cross-validation runs.
