### Functions
The "RunCrossValidation.R" script calls various functions contained within this functions subdirectory.

##### Clean_pd.R
* Find_duplication.py 
    * Searches for peat depth points in the sample with duplicated lat/long coordinates. If it finds them, deletes duplicates to keep only one   
* Find_nearestNeighours.py 
    * Searches for peat depth points in the sample which are less than 1m apart. If it finds them, deletes points to keep only one   

##### Check_models.R
* Check_lm.py  
    * Checks the performance of the linear model fitted on the whole dataset
* Check_sm.py  
    *  Checks the performance of the geostatistical model fitted on the whole dataset

##### Cross_validate.R
* Cross_validate.py  
    * Performs 10-fold cross-validation on both the linear and geostatistical model and returns a 3D array, which for each of the 10 cross validation runs includes:  
         * A 2D array with X rows (where X is the number of measured sample points) and 6 columns containing:  
            (1) LM predicted value (2) LM lower prediction interval (3) LM Upper prediction interval
            (4) LM predicted value (5) LM lower prediction interval (6) LM Upper prediction interval

##### Analyse_results.R
* Create_results.py
    * Creates a table detailing for both the linear and geostatistical model statistics of model performance (bias, RMSE, coverage, interval width) 
* Create_predicted_vs_observed.py
    * Creates a table detailing for each location with a measured peat depth value, the mean peat depth value predicted at that location by both linear and geostatistical models across the cross-validation runs.
