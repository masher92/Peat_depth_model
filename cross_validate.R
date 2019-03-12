cross_validate <- function (dat, slope, elevation) {

  # Add transformation
  dat$sqrtdepth <- sqrt(dat$depth)
  dat$logdepth <- log(dat$depth)

  # Store the number of cases
  n <- nrow(dat)

  #------------------------------------------------------------------------------
  #4. # Conduct the cross validation exercise
  #Array - rows, columns, number of arrays
  results <- array(NA, c(n, 6,10))
  colnames(results) <- c("pred_LM", "LPI_LM", "UPI_LM", "pred_SM", "LPI_SM",
                         "UPI_SM")

  # Loop over the 10 replications of splitting the data into 10 equally size groups.
  for(r in 1:2) {
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
      model.lm <- lm(formula=sqrtdepth~elevation + Slope_5m, data=dat.fit)
      model.lm.predictions <-
        predict(object=model.lm, newdata=dat.pred, interval="prediction")

      # Fit the spatial model
      # Set up the spatial data object for the fitting data set
      # Ensure you add in the correct covariates here that you wish to use
      test.sp <-
        as.geodata(obj=dat.fit, coords.col=c('coords.x1', 'coords.x2'), data.col= 'sqrtdepth', covar.col= c(3,4))
      # Jitter anyduplicated data locations which appear to be identical
      test.sp <- jitterDupCoords(test.sp, max=0.01)

      ## Fit the models
      model.sm <-
        likfit(geodata=test.sp, trend=~elevation + Slope_5m,
               ini.cov.pars=c(15, 0.05), fix.nugget=FALSE,
               cov.model="exponential")
      # There are alternative spatial correlation models  so you could use these
      # instead of exponential (e.g. spherical)
      print(r) #Monitor progress; each of the 10 r will be run 10(i) times.
      print(i)

      # Do the predictions
      control.sm <-
        krige.control(type.krige="OK", trend.d=~dat.fit$elevation +
                        dat.fit$Slope_5m, trend.l=~dat.pred$elevation +
                        dat.pred$Slope_5m, obj.model=model.sm)

      kriging.sm <-
        krige.conv(geodata=test.sp, locations=dat.pred[c('x', 'y')], krige=control.sm)

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
