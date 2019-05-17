# Set working directory
setwd("E:/Msc/Dissertation/Code")

# Source files containing functions
source("Peat_depth_model/Functions/analyse_results.R")

# Load in results data
load("E:/Msc/Dissertation/Code/Peat_depth_model/Output/results_bng_dupes.R")

# Seperate out the results array and the dat (dataframe containing details about all sample points)
dat <- results[[2]]
results <- results[[1]]

#Create dataframe summarising the bias, RMSE, coverage and interval width
summary_results <- create_results ('Humberstone', results, dat)
# Compare the predicted values with the observed values
predicted_vs_observed <- create_predicted_vs_observed (results, dat)

# Plot results
# Linear model
plot ( predicted_vs_observed$real_values,predicted_vs_observed$LM.mean, main = paste('Linear model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$LM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)

# Spatial model
plot ( predicted_vs_observed$real_values,predicted_vs_observed$SM.mean, main = paste('Spatial model. CC =  ', round(cor(predicted_vs_observed$real_values, predicted_vs_observed$SM.mean),2), sep = ''),
       xlab = 'Observed Value', ylab = 'Predicted Value', xlim = c(0,400), ylim = c(0,400))
abline(a=0,b=1)


