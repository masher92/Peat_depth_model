
#------------------------------------------------------------------------------
#1. Calculate metrics of interest
# Define functions for calculating matrix of interest

# Create function
create_results <- function (area, results, dat){
  
  # Load the results of the cross validation
  load(results)
  
  # Load the dat file
  load(dat)
  # COunt the cases
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
                                sm = c(bias(4), rmse(4), coverage(6), i_w(6)),
                                lm = c(bias(1), rmse(1), coverage(3), i_w(3)))
  colnames(summary_results)[2:3] <- c(paste(area, 'sm', sep = '.'), paste(area, 'lm', sep = '.'))
  return(summary_results)
}

# Send files to function
stake_moss_summ <- create_results('stake_moss', "6.cross_validation_results/stake_moss.R", "6.cross_validation_results/stake_moss_dat.R")
humberstone_summ <- create_results('humberstone', "6.cross_validation_results/humberstone.R", "6.cross_validation_results/humberstone_all_dat.R")
newhouse_summ <- create_results("newhouse", "6.cross_validation_results/newhouse.R", "6.cross_validation_results/newhouse_dat.R")
melbecks_summ <- create_results("melbecks", "6.cross_validation_results/melbecks.R", "6.cross_validation_results/melbecks_dat.R")

## Join
total_summ <- cbind(stake_moss_summ[1:3], humberstone_summ[2:3], newhouse_summ[2:3], melbecks_summ[2:3])
total_summ[,-c(1)] <-round(total_summ[,-c(1)],4)
#------------------------------------------------------------------------------
#2. Plots comparing mean predicted value to observed values


create_predicted_vs_observed <- function (results, dat){
  load(results)
  load(dat)
  # COnvert to dataframe and save
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

humberstone_pvo <- create_predicted_vs_observed("6.cross_validation_results/humberstone.R","6.cross_validation_results/humberstone_dat.R")                                 
melbecks_pvo <- create_predicted_vs_observed("6.cross_validation_results/melbecks.R","6.cross_validation_results/melbecks_dat.R")                                 
newhouse_pvo <- create_predicted_vs_observed("6.cross_validation_results/newhouse.R","6.cross_validation_results/newhouse_dat.R")                                 
stake_moss_pvo <- create_predicted_vs_observed("6.cross_validation_results/stake_moss.R","6.cross_validation_results/stake_moss_dat.R")                                 

# Plots
# Plot (without square root)
plot ( humberstone_pvo$real_values,humberstone_pvo$LM.mean, main = paste('Linear model. CC =  ', round(cor(humberstone_pvo$real_values, humberstone_pvo$LM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

# Plot (without square root)
plot ( humberstone_pvo$real_values,humberstone_pvo$SM.mean, main = paste('Spatial model. CC =  ', round(cor(humberstone_pvo$real_values, humberstone_pvo$SM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

# Plot (without square root)
plot ( melbecks_pvo$real_values,melbecks_pvo$LM.mean, main = paste('Linear model. CC =  ', round(cor(melbecks_pvo$real_values, melbecks_pvo$LM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

# Plot (without square root)
plot ( melbecks_pvo$real_values,melbecks_pvo$SM.mean, main = paste('Spatial model. CC =  ', round(cor(melbecks_pvo$real_values, melbecks_pvo$SM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

# Plot (without square root)
plot ( newhouse_pvo$real_values,newhouse_pvo$LM.mean, main = paste('Linear model. CC =  ', round(cor(newhouse_pvo$real_values, newhouse_pvo$LM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

# Plot (without square root)
plot ( newhouse_pvo$real_values,newhouse_pvo$SM.mean, main = paste('Spatial model. CC =  ', round(cor(newhouse_pvo$real_values, newhouse_pvo$SM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

# Plot (without square root)
plot ( stake_moss_pvo$real_values,stake_moss_pvo$LM.mean, main = paste('Linear model. CC =  ', round(cor(stake_moss_pvo$real_values, stake_moss_pvo$LM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

# Plot (without square root)
plot ( stake_moss_pvo$real_values,stake_moss_pvo$SM.mean, main = paste('Spatial model. CC =  ', round(cor(stake_moss_pvo$real_values, stake_moss_pvo$SM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

#------------------------------------------------------------------------------
#3. Variograms

# Plot the semi-variogram to test for the presence of spatial autocorrelation
residuals <- residuals(model.lm)
dat.fit$residuals <- residuals
resid.sp <- as.geodata(obj=dat, coords.col=4:5, data.col=9,
                       covar.col= c(7,8))
resid.sp <- jitterDupCoords(resid.sp, max=0.01)
vari1 <- variog(resid.sp)
plot(vari1, type = "b", main = "Variogram:?") 
vari1.mc <- variog.mc.env(resid.sp, obj.variog=vari1)

plot(vari1, envelope.obj = vari1.mc, xlab="Distance (°)",
     ylab="Estimated semi-variogram") 


