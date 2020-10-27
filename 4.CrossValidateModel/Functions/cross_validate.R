cross_validate <- function (dat, covar_names) {
"
Description
----------
    F
Parameters
----------
    dat: Data frame
        A dataframe containing the locations of points with depth, slope and elevation values
    covar_names: list
        A list containing strings with the names of the variables being used as predictors
Returns
-------
    results: Array
        A 3D array. For each of the 10 cross validation runs:
            A 2D array with X rows (where X is the number of measured sample points) and 6 columns containing: 
            (1) LM predicted value (2) LM lower prediction interval (3) LM Upper prediction interval
            (4) LM predicted value (5) LM lower prediction interval (6) LM Upper prediction interval
"   

  # Store the number of points in the dataframe
  n <- nrow(dat)

  #------------------------------------------------------------------------------
  # Conduct the cross validation 
  # Create an empty array (specifying the number of rows, columns, number of arrays)
  # This will store the results from each iteration of the cross-validation
  results <- array(NA, c(n, 6,10))
  colnames(results) <- c("pred_LM", "LPI_LM", "UPI_LM", "pred_SM", "LPI_SM",
                         "UPI_SM")

  # Loop over the 10 replications of splitting the data into 10 equally size groups.
  for(r in 1:10) {
    print(r)
    {
      data.order <- sample(1:n)
      split_m_total <- length(data.order)
      split_m_G2 <- as.integer(split_m_total / 10)
      split_m_size <- length(data.order) %% 10
      
      #Therefore split_m_size chunks will have size split_m_G2 + 1 and the
      #remaining will have size split_size_G2
      split_m_G1 <- split_m_G2 + 1

      #For all data
      #To allow for changes in number of rows in dat
      split.matrix <-
        data.frame(sites=data.order,
                   group = c(kronecker(1:split_m_size, rep(1,split_m_G1)),
                             kronecker((split_m_size + 1):10, rep(1,split_m_G2))))

    # Undertake the 10-fold cross validation
    for (i in 1:10) {
      ## Set up the fitting and the prediction data sets
      dat.fit <- dat[split.matrix[split.matrix$group!=i,1] , ]
      dat.pred <- dat[split.matrix[split.matrix$group==i,1] , ]

      # Fit the linear model
      Formula_lm <- formula(paste("sqrtdepth~ ", paste(covar_names, collapse=" + ")))
      model.lm <- lm(Formula_lm, dat)
      # Apply it to the linear model
      model.lm.predictions <-
        predict(object=model.lm, newdata=dat.pred, interval="prediction")

      # Fit the spatial model
      # Set up the spatial data object for the fitting data set
      # Ensure you add in the correct covariates here that you wish to use
      test.sp <-
        as.geodata(obj=dat.fit, coords.col=c('longitude', 'latitude'), data.col= 'sqrtdepth', 
                   covar.col= covar_names)
      # Jitter anyduplicated data locations which appear to be identical
      test.sp <- jitterDupCoords(test.sp, max=0.01)

      ## Fit the models
      # Define the formula to be used in spatial model
      Formula_sm <- formula(paste("~", paste(covar_names, collapse=" + ")))
      
      # Fit a geostatistical model to all the data
      model.sm <- likfit(geodata=test.sp, trend= Formula_sm,
                          ini.cov.pars=c(15, 0.05), fix.nugget=FALSE,
                          cov.model="exponential")
      
      # There are alternative spatial correlation models  so you could use these
      # instead of exponential (e.g. spherical)
      print(r) #Monitor progress; each of the 10 r will be run 10(i) times.
      print(i)
      
      # Trend.d and trend.l need to be the same as Formula_sm but drawing data from dat.fit and dat.pred
      covars.fit <- paste("dat.fit$",covar_names, sep ='')
      Formula_sm_fit <- formula(paste("~", paste(covars.fit, collapse=" + ")))
      covars.pred <- paste("dat.pred$",covar_names, sep ='')
      Formula_sm_pred <- formula(paste("~", paste(covars.pred, collapse=" + ")))
      
      # Do the predictions
      control.sm <-
        krige.control(type.krige="OK", trend.d=Formula_sm_fit, 
                        trend.l=Formula_sm_pred, obj.model=model.sm)
      print("here")
      kriging.sm <-
        krige.conv(geodata=test.sp, locations=dat.pred[,c('longitude', 'latitude')], krige=control.sm)

      model.sm.predictions <-
        cbind(kriging.sm$predict,
              kriging.sm$predict - 1.96 * sqrt(kriging.sm$krige.var),
              kriging.sm$predict+ 1.96 * sqrt(kriging.sm$krige.var))

      # Save the results but removing the last n.dummy values added to make the
      # kriging function work
      n.preds <- nrow(dat.pred)

      for(j in 1:n.preds)
      {
        # Choose the row (site) the  prediction corresponds to.
        which.row <- which(rownames(dat.pred)[j]==rownames(dat))

        # Save the results
        results[which.row, 1:3, r] <- model.lm.predictions[j, ]
        results[which.row, 4:6, r] <- model.sm.predictions[j, ]
      }
    }
    }
  }
  return (results)

  }
