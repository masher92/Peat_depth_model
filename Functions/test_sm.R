# Question: Am I fitting the model on the same data for which I am testing it on?
# But should this matter? Is it overfitting?
# Cross-validation is fitting it on 9/10s and testing on the other 1/10th, not the same as this. 
# You cannot test the model on the same data you used to fit it. 

fit_spatial_model_and_predict <- function(sample_df, measured_samples_fp){
  
  #########################################################################
  # Process data to fit spatial model on 
  #########################################################################
  # Add column for square root of depth
  sample_df$sqrtdepth <- sqrt(sample_df$depth)
  # Remove any points with unrealistically high slope values
  sample_df <- sample_df[sample_df$Slope_5m <30,]
  
  #########################################################################
  # Fit a spatial model using this data
  #########################################################################
  # Create geodata object
  samples_geodata <- as.geodata(obj=sample_df, coords.col=c('longitude', 'latitude'), data.col= 'sqrtdepth', covar.col= c("elevation", "Slope_5m"))
  
  # Jitter any duplicated data locations which appear to be identical
  samples_geodata <- jitterDupCoords(samples_geodata, max=0.01)
  
  # Look at the data
  plot(samples_geodata, lowess = TRUE)
  
  ## Fit the model
  model.sm <- likfit(geodata=samples_geodata, trend=~elevation + Slope_5m,
                     ini.cov.pars=c(15, 0.05), fix.nugget=FALSE,
                     cov.model="exponential")
  
  #########################################################################
  # Process data to make predictions on
  #########################################################################
  load(paste(measured_samples_fp))
  measured_sample <- sample
  
  # Convert the spatial points dataframe to a dataframe for modelling
  measured_sample_df <- data.frame(measured_sample)
  
  # Add transformations of depth
  measured_sample_df$sqrtdepth <- sqrt(measured_sample_df$depth)
  
  #########################################################################
  # Use fitted model to make predictions for measured data. 
  #########################################################################
  # Apply the fitted model to the data for which we want to make predictions
  control.sm <-
    krige.control(type.krige="OK", trend.d=~sample_df$elevation +
                    sample_df$Slope_5m, trend.l=~measured_sample_df$elevation +
                    measured_sample_df$Slope_5m, obj.model=model.sm)
  
  kriging.sm <-
    krige.conv(geodata=samples_geodata, locations=measured_sample_df[ ,c('longitude', 'latitude')], krige=control.sm)
  
  # Calculate the upper and lower prediction intervals
  model.sm.predictions <- as.data.frame(
    cbind(prediction = kriging.sm$predict,
          lwr = kriging.sm$predict - 1.96 * sqrt(kriging.sm$krige.var),
          upr = kriging.sm$predict+ 1.96 * sqrt(kriging.sm$krige.var)))
  
  # Join it with the original dataframe
  model.sm.predictions_df <- data.frame(measured_sample_df, sqrt_prediction = model.sm.predictions[,1],
                                        lwr = model.sm.predictions[,2], upr = model.sm.predictions[,3])
  
  model.sm.predictions_df$prediction <- model.sm.predictions_df$sqrt_prediction ^2
  
  # Create new columns - depth and sqrt depth
  predictions <- model.sm.predictions_df[,c('longitude', 'latitude', 'Slope_5m', 'elevation', 'depth', 'prediction',
                                            'sqrtdepth', 'sqrt_prediction')]

  return (predictions) 
  }
  
  
  
 