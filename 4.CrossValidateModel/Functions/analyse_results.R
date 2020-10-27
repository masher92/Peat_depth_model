create_results <- function (area, results, dat){
"
Description
----------
    Uses a results array containing the values predicted in CV by both a linear and geostatistical model
    to create a summary results dataframe with values for bias, rmse, coverage and interval width for both models
Parameters
----------
    area: string
        A string specifying the name of the study area
    results: array
        A 3D array containing the results of the CV. This contains for each of the 10 cross validation runs:
            A 2D array with X rows (where X is the number of measured sample points) and 6 columns containing: 
            (1) LM predicted value (2) LM lower prediction interval (3) LM Upper prediction interval
            (4) LM predicted value (5) LM lower prediction interval (6) LM Upper prediction interval
    dat: Dataframe
        A dataframe containing rows with lat/long of locations with peat depth values
Returns
-------
    summary_results: Dataframe
        A dataframe containing values for bias, rmse, coverage and interval width for both models
"   
  # Count the cases
  n <- nrow(dat)
  #Bias - measures whether the predictions are too large or too small on average
  #- want to be as close to zero as possible but due to random variation will not
  # be exactly zero.
  bias <- function (col_no) {mean(results[,col_no, ] - matrix(rep(dat$sqrtdepth,10), nrow=n, ncol=10,byrow=FALSE))}
  #RMSE - measures average difference between the true and predicted values
  #ignoring sign. want to be as small as possible                                                            
  rmse <- function (col_no) {sqrt(mean((results[,col_no, ] - matrix(rep(dat$sqrtdepth,10), nrow=n, ncol=10,byrow=FALSE))^2))}
  # Coverage - measures the probability that the 95% prediction intervals contain
  # the true value want to be 0.95                                                                 
  coverage <- function (col_no) {mean(matrix(rep(dat$sqrtdepth,10), nrow=n, ncol=10, byrow=FALSE) >
                                    results[ ,col_no-1, ] & matrix(rep(dat$sqrtdepth,10), nrow=n, ncol=10,
                                                                       byrow=FALSE) < results[ ,col_no, ])}
  # Interval width - the width of the 95% prediction intervals
  # want to be as small as possible provided that the coverage above is around 0.95.
  # If the coverage is much lower than 0.95 then this is meaningless. 
  i_w <- function (col_no) {mean(results[ ,col_no, ] - results[ ,col_no-1, ])}
  
  ## Store results in a dataframe
  summary_results <- data.frame(results_metric = c( 'bias', 'RMSE', 'coverage', 'interval width'),
                                sm = c(bias(4), rmse(4)^2, coverage(6), i_w(6)^2),
                                lm = c(bias(1), rmse(1)^2, coverage(3), i_w(3)^2))
  colnames(summary_results)[2:3] <- c(paste(area, 'sm', sep = '.'), paste(area, 'lm', sep = '.'))
  return(summary_results)
}

create_predicted_vs_observed <- function (results, dat){
"
Description
----------
    Uses an array containing all the values predicted in each iteration of cross validation to create a table comparing
    real measured depth values with the mean values predicted for that point across cross-validation runs, by both a linear 
    and geostatistical model
Parameters
----------
    results: array
        A 3D array containing the results of the CV. This contains for each of the 10 cross validation runs:
            A 2D array with X rows (where X is the number of measured sample points) and 6 columns containing: 
            (1) LM predicted value (2) LM lower prediction interval (3) LM Upper prediction interval
            (4) LM predicted value (5) LM lower prediction interval (6) LM Upper prediction interval
    dat: Dataframe
        A dataframe containing rows with lat/long of locations with peat depth values
Returns
-------
    predicted_vs_observed: Dataframe
        A dataframe containing for each measured peat depth point, its real measured value and the mean value predicted across the cross-validation
        runs. This is provided both in terms of square-root of depth and depth. 
"   
  # Convert to dataframe and save
  results_df <- as.data.frame(results)
  
  # Create a dataframe with the real values for each site, plus the mean of the ten predictions
  # made through cross-validation for both the linear model and spatial model.
  predicted_vs_observed <- data.frame(real_values_sqrt  = dat$sqrtdepth,
                                      LM.mean_sqrt = apply(results_df[,seq(1, 60, by=6)],1,mean),
                                      SM.mean_sqrt = apply(results_df[,seq(4, 60, by=6)],1,mean),
                                      real_values = dat$depth,
                                      LM.mean = round((apply(results_df[,seq(1, 60, by=6)],1,mean))^2,5),
                                      SM.mean = round((apply(results_df[,seq(4, 60, by=6)],1,mean))^2,5))   
  return (predicted_vs_observed)
}



#############################
## How does RMSE calculation work?
# Each row is a location, each column is prediction of a cross-validation run
#test_pred <- results[,4, ]
## Each row is a location, each column is the real value repeated 
#test_real <- matrix(rep(dat$sqrtdepth,10), nrow=n, ncol=10,byrow=FALSE)
## Find the difference between the real and predicted values
#residuals <- test_pred - test_real
## Square the residuals
#residuals_squared <- diff^2
## Find average of squared residuls
#mean_sq_residuals <- mean(residuals_squared)
## Find square root of mean squared residuals
#
#rmse <- function (col_no) {sqrt(mean((results[,col_no, ] - matrix(rep(dat$sqrtdepth,10), nrow=n, ncol=10,byrow=FALSE))^2))}
#rmse <- function (col_no) {sqrt(mean((test_pred - test_real)^2))}
#rmse <- function (col_no) {sqrt(mean((residuals)^2))}
#rmse <- function (col_no) {sqrt(mean(residuals_squared))}
#rmse <- function (col_no) {sqrt(mean_sq_residuals)}

