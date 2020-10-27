check_lm <- function (dat, covar_names){
"
Description
----------
    Fits a linear model on all of the data (as opposed to CV which only ever fits it on a subset of the data)
    Calculates the residuals (difference between observed value of the dependent variable and the predicted value
    - from the regression equation line).
Parameters
----------
    dat: Data frame
        A dataframe containing the locations of points and related data
    covar_names: list
        A list containing strings with the names of the variables being used as predictors
Returns
-------
    resid: list?
        A 
"   
  
  # Define the formula to be used in linear model
  Formula <- formula(paste("sqrtdepth~ ", 
                           paste(covar_names, collapse=" + ")))
  # Apply it to all the locations with measured depth values
  mod.lm.test <- lm(Formula, dat)
  # Summarise the results
  summary(mod.lm.test)
  # Find the residuals
  resid <- residuals(mod.lm.test)
  plot(resid)
  qqnorm(resid)
  qqline(resid)
  dat$resid <- resid
  return (resid)
}

check_sm <- function (dat, covar_names){
  "
Description
----------
    Fits a geostatistical model on all of the data (as opposed to CV which only ever fits it on a subset of the data)
    Calculates the residuals (difference between observed value of the dependent variable and the predicted value
    - from the regression equation line).
Parameters
----------
    dat: Data frame
        A dataframe containing the locations of points and related data
    covar_names: list
        A list containing strings with the names of the variables being used as predictors4
Returns
-------
    resid: list?
        A 
"   
  sp_depth <- as.geodata(obj=dat, coords.col=c('longitude', 'latitude'), data.col="sqrtdepth", covar.col = covar_names)
  
  # Jitter the 2 duplicated data locations which appear to be identical
  names(sp_depth)
  #coords, data, covariate (source, elevation)
  sp_depth <- jitterDupCoords(sp_depth, max=0.01)
  
  # Plot the spatial object
  # In the top left plot the data have been split into 4 colours based on
  # quartiles (red is highest)
  plot(sp_depth)
  
  # Define the formula to be used in spatial model
  Formula_sm <- formula(paste("~", 
                           paste(covar_names, collapse=" + ")))
  
  # Fit a geostatistical model to all the data
  model.geo <- likfit(geodata=sp_depth, trend= Formula_sm,
                      ini.cov.pars=c(15, 0.05), fix.nugget=FALSE,
                      cov.model="exponential")
  summary =summary(model.geo)
  return(summary)
}


