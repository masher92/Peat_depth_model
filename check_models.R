# Fit a linear model on all of the data.
# Calculate residuals (difference between observed value of the dependent variable and 
# the predicted value - from the regression equation line).
check_lm <- function (dat){
  mod.lm.test <- lm(sqrtdepth~elevation + Slope_5m, data=dat)
  summary(mod.lm.test)
  resid <- residuals(mod.lm.test)
  plot(resid)
  qqnorm(resid)
  qqline(resid)
  dat$resid <- resid
  return (resid)
}

# Fit a sample spatial model on all of the data.
# 
check_sm <- function (dat, covars){
  ####
  sp_depth <- as.geodata(obj=dat, coords.col=5:6, data.col=2, covar.col = covars)
  
  # Jitter the 2 duplicated data locations which appear to be identical
  names(sp_depth)
  #coords, data, covariate (source, elevation)
  sp_depth <- jitterDupCoords(sp_depth, max=0.01)
  
  # Plot the spatial object
  # In the top left plot the data have been split into 4 colours based on
  # quartiles (red is highest)
  plot(sp_depth)
  
  # Fit a geostatistical model to all the data
  model.geo <- likfit(geodata=sp_depth, trend=~elevation + Slope_5m,
                      ini.cov.pars=c(15, 0.05), fix.nugget=FALSE,
                      cov.model="exponential")
  summary =summary(model.geo)
  return(summary)
}





