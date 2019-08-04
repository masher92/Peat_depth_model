source("Functions/generate_synthetic_sample.R")

assign_depths_using_sm <- function(measured_samples_fp, synthetic_dataset){
  #########################################################################
  # Fit spatial model to measured samples 
  #########################################################################
  # Read in measured_samples  
  load(measured_samples_fp)
  measured_sample <- sample
  
  # Convert the spatial points dataframe to a dataframe for modelling
  measured_sample_df <- data.frame(measured_sample)
  
  # Add transformations of depth
  measured_sample_df$sqrtdepth <- sqrt(measured_sample_df$depth)
  
  # Fit spatial model on the measured sample data
  # Convert dataframe to geodata
  measured_samples_geodata <-
    as.geodata(obj=measured_sample_df, coords.col=c('longitude', 'latitude'), data.col= 'sqrtdepth', 
               covar.col= c("elevation", "Slope_5m"))
  
  # Jitter any duplicated data locations which appear to be identical
  measured_samples_geodata <- jitterDupCoords(measured_samples_geodata, max=0.01)
  
  ## Fit the model
  model.sm <- likfit(geodata=measured_samples_geodata, trend=~elevation + Slope_5m,
                     ini.cov.pars=c(15, 0.05), fix.nugget=FALSE,
                     cov.model="exponential")
  
  #########################################################################
  # Use spatial model to give depths to the synthetic gridded sample points
  #########################################################################
  plot(measured_samples_geodata, lowess = TRUE)
  
  # Apply the fitted model to the synthetic gridded sample data 
  control.sm <-
    krige.control(type.krige="OK", trend.d=~measured_sample_df$elevation +
                    measured_sample_df$Slope_5m, trend.l=~synthetic_dataset$elevation +
                    synthetic_dataset$Slope_5m, obj.model=model.sm)
  
  kriging.sm <-
    krige.conv(geodata=measured_samples_geodata, locations=synthetic_dataset[ ,c('longitude', 'latitude')], krige=control.sm)
  
  # Calculate the upper and lower prediction intervals
  model.sm.predictions <- as.data.frame(
    cbind(prediction = kriging.sm$predict,
          lwr = kriging.sm$predict - 1.96 * sqrt(kriging.sm$krige.var),
          upr = kriging.sm$predict+ 1.96 * sqrt(kriging.sm$krige.var)))
  
  # Join it with the original dataframe
  model.sm.predictions_df <- data.frame(synthetic_dataset, prediction = model.sm.predictions[,1],
                                        lwr = model.sm.predictions[,2], upr = model.sm.predictions[,3])
  
  ## Set any negative prediction values to 0
  model.sm.predictions_df$prediction[model.sm.predictions_df$prediction <0] <- 0
  
  # Trim just the columns needed
  synthetic_dataset_with_depths <- model.sm.predictions_df[,c(1:5)]
  
  # Rename and calculate depth (not sqrted)
  names(synthetic_dataset_with_depths)[names(synthetic_dataset_with_depths) == 'prediction'] <- 'depth'
  synthetic_dataset_with_depths$depth <- synthetic_dataset_with_depths$depth ^2
  

  return(synthetic_dataset_with_depths)  
}

assign_depths_from_distrib <- function (measured_samples_fp, synthetic_sample){
  
  #########################################################################
  # Assign sample depths
  #########################################################################
  load(measured_samples_fp)
  measured_samples <- sample
  
  # Find the range of depth values associated with each slope-elevation category amongst existing samples.
  measured_samples$Slope_Cuts <- cut(measured_samples$Slope_5m, breaks = c(0,2, 4,6,8,10,15,20,25,30,Inf)) 
  measured_samples$Elevation_Cuts <- cut(measured_samples$elevation, breaks = c(250,275,300,325,350,375,400, 425,450,Inf))
  measured_samples$slope_and_elevation <- as.character(paste(measured_samples$Slope_Cuts, measured_samples$Elevation_Cuts))
  
  # Remove points with a depth of 0
  measured_samples <- measured_samples[measured_samples$depth >0,]
  
  # Find the number of sample points within each of the slope-elevation categories
  measured_samples_distrib <- as.data.frame(table(measured_samples$slope_and_elevation))
  # Add columns containing the bounding slope and elevation values
  measured_samples_distrib$slope_lower <-as.numeric(sapply(regmatches(measured_samples_distrib$Var1, gregexpr("[[:digit:]]+", measured_samples_distrib$Var1)),`[`,1)) 
  measured_samples_distrib$slope_higher <-as.numeric(sapply(regmatches(measured_samples_distrib$Var1, gregexpr("[[:digit:]]+", measured_samples_distrib$Var1)),`[`,2)) 
  measured_samples_distrib$elevation_lower <-as.numeric(sapply(regmatches(measured_samples_distrib$Var1, gregexpr("[[:digit:]]+", measured_samples_distrib$Var1)),`[`,3))
  measured_samples_distrib$elevation_higher <- as.numeric(sapply(regmatches(measured_samples_distrib$Var1, gregexpr("[[:digit:]]+", measured_samples_distrib$Var1)),`[`,4))  
  
  # Move through each row in the synthetic sample and use the select_depth_from_distrib to assign a suitable depth value
  # Find the range of depth values associated with each slope-elevation category amongst existing samples.
  synthetic_sample$Slope_Cuts <- cut(synthetic_sample$Slope_5m, breaks = c(0, 2, 4,6,8,10,15,20,25,30,Inf)) 
  synthetic_sample$Elevation_Cuts <- cut(synthetic_sample$elevation, breaks = c(250,275,300,325,350,375,400, 425,Inf))
  synthetic_sample$slope_and_elevation <- as.character(paste(synthetic_sample$Slope_Cuts, synthetic_sample$Elevation_Cuts))
  
  depths <- list ()
  for (i in (1:nrow(synthetic_sample))){
    print(i)
    # If the slope_elevation category is found in the samples, then select randomly a depth value from the depth values in the samples
    if (synthetic_sample$slope_and_elevation[[i]] %in% measured_samples_distrib$Var1){
      depth <- select_depth_from_distrib(measured_samples, synthetic_sample$slope_and_elevation[[i]])
      print ("DOne")
      # If the slope and elevation category is not found in samples then assign it to a nearby category which is in the samples.
    } else {
      # Find the lower slope value of the band which is closest to the slope value
      slope_row_no <-which(abs(unique(measured_samples_distrib$slope_lower)-synthetic_sample$Slope_5m[[i]])==min(abs(unique(measured_samples_distrib$slope_lower)-synthetic_sample$Slope_5m[[i]])))
      slope_value<- unique(measured_samples_distrib$slope_lower)[slope_row_no]
      
      # Filter the dataframe to only contain those with this slope_value
      matching_slope <- measured_samples_distrib[(measured_samples_distrib$slope_lower == slope_value),]
      
      # Of those with the nearest slope value, find the closest matching lower elevation bands value
      elev_row_no <-which(abs(unique(matching_slope$elevation_lower)-synthetic_sample$elevation[[i]])==min(abs(unique(matching_slope$elevation_lower)-synthetic_sample$elevation[[i]])))
      elev_value <- unique(matching_slope$elevation_lower)[elev_row_no]
      
      # Turn this into a slope_elevation category 
      proxy_cat <- as.character(measured_samples_distrib[(measured_samples_distrib$slope_lower == slope_value) & (measured_samples_distrib$elevation_lower == elev_value),]$Var1)
      
      # Randomly select a depth value using this proxy category
      depth <- select_depth_from_distrib(measured_samples, proxy_cat)}
    # Add the depth value to the list
    depths[[i]] <- depth  
  }
  # Convert the depths to a dataframe, and join to the synthetic samples DF
  depths <- t(as.data.frame(depths))
  synthetic_sample$depth <- depths
  synthetic_sample <- synthetic_sample[,c(1:4,8)]
  
  synthetic_sample$depth <- as.numeric(synthetic_sample$depth)
  
  return(synthetic_sample)}


assign_depths_from_distrib_newmethod <- function (measured_samples_fp, synthetic_sample){
  
  distances <- c()
  
  #########################################################################
  # Assign sample depths
  #########################################################################
  load(measured_samples_fp)
  measured_samples <- sample
  
  # Find the range of depth values associated with each slope-elevation category amongst existing samples.
  measured_samples$Slope_Cuts <- cut(measured_samples$Slope_5m, breaks = c(0, 2, 4,6,8,10,12,14,16,18,20,22, 24, 26,28,30,Inf)) 
  measured_samples$Elevation_Cuts <- cut(measured_samples$elevation, breaks = c(250,275,300,325,350,375,400, 425,450,Inf))
  measured_samples$slope_and_elevation <- as.character(paste(measured_samples$Slope_Cuts, measured_samples$Elevation_Cuts))
  
  # Remove points with a depth of 0
  measured_samples <- measured_samples[measured_samples$depth >0,]
  
  # Find the number of sample points within each of the slope-elevation categories
  measured_samples_distrib <- as.data.frame(table(measured_samples$slope_and_elevation))
  # Add columns containing the bounding slope and elevation values
  measured_samples_distrib$slope_lower <-as.numeric(sapply(regmatches(measured_samples_distrib$Var1, gregexpr("[[:digit:]]+", measured_samples_distrib$Var1)),`[`,1)) 
  measured_samples_distrib$slope_higher <-as.numeric(sapply(regmatches(measured_samples_distrib$Var1, gregexpr("[[:digit:]]+", measured_samples_distrib$Var1)),`[`,2)) 
  measured_samples_distrib$elevation_lower <-as.numeric(sapply(regmatches(measured_samples_distrib$Var1, gregexpr("[[:digit:]]+", measured_samples_distrib$Var1)),`[`,3))
  measured_samples_distrib$elevation_higher <- as.numeric(sapply(regmatches(measured_samples_distrib$Var1, gregexpr("[[:digit:]]+", measured_samples_distrib$Var1)),`[`,4))  
  
  # Move through each row in the synthetic sample and use the select_depth_from_distrib to assign a suitable depth value
  # Find the range of depth values associated with each slope-elevation category amongst existing samples.
  synthetic_sample$Slope_Cuts <- cut(synthetic_sample$Slope_5m, breaks =c(0, 2, 4,6,8,10,12,14,16,18,20,22, 24, 26,28,30,Inf)) 
  synthetic_sample$Elevation_Cuts <- cut(synthetic_sample$elevation, breaks = c(250,275,300,325,350,375,400, 425,450,Inf))
  synthetic_sample$slope_and_elevation <- as.character(paste(synthetic_sample$Slope_Cuts, synthetic_sample$Elevation_Cuts))
  
  # Create empty version of dataframe to repopulate
  synthetic_sample_with_depths = synthetic_sample[FALSE,]
  synthetic_sample_with_depths$depth <- NULL
  
  for (i in (1:nrow(synthetic_sample))){
    print(i)
    row <- synthetic_sample[i,]
    if (row$slope_and_elevation %in% measured_samples_distrib$Var1){
      #row <- synthetic_sample[i,]
      slope_elevation_category <- row$slope_and_elevation 
      row_with_depth <- select_depth_from_distrib_new(measured_samples, row, slope_elevation_category)[[1]]
      distances[[i]] <- select_depth_from_distrib_new(measured_samples, row, slope_elevation_category)[[2]]
    } else {
      print("no same category")
      # Find the lower slope value of the band which is closest to the slope value
      slope_row_no <-which(abs(unique(measured_samples_distrib$slope_lower)-row$Slope_5m)==min(abs(unique(measured_samples_distrib$slope_lower)-row$Slope_5m)))
      slope_value<- unique(measured_samples_distrib$slope_lower)[slope_row_no]
      
      # Filter the dataframe to only contain those with this slope_value
      matching_slope <- measured_samples_distrib[(measured_samples_distrib$slope_lower == slope_value),]
      
      # Of those with the nearest slope value, find the closest matching lower elevation bands value
      elev_row_no <-which(abs(unique(matching_slope$elevation_lower)-row$elevation)==min(abs(unique(matching_slope$elevation_lower)-row$elevation)))
      elev_value <- unique(matching_slope$elevation_lower)[elev_row_no]
      
      # Turn this into a slope_elevation category 
      proxy_cat <- as.character(measured_samples_distrib[(measured_samples_distrib$slope_lower == slope_value) & (measured_samples_distrib$elevation_lower == elev_value),]$Var1)
      
      # Select a depth value using this proxy category
      row_with_depth <- select_depth_from_distrib_new(measured_samples, row, proxy_cat)[[1]]
      distances[i] <- select_depth_from_distrib_new(measured_samples, row, proxy_cat)[[2]]
      
    }
    # Add the row with added depths onto a dataframe
    synthetic_sample_with_depths <- rbind(synthetic_sample_with_depths, row_with_depth) 
  }
  
  return(list(synthetic_sample_with_depths, distances))
}