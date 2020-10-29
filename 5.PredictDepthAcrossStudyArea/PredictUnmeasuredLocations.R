#################################################################
# Set up environment
#################################################################
library(rgdal)
library(raster)
library(geoR)
library(dplyr)
library(geosphere)
library(leaflet)
library (rgeos)
library(gdistance)

# Set working directory
setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis/")

# Source files containing functions
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/cross_validate.R")
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/check_models.R")
source("Code/Peat_depth_model-master/4.CrossValidateModel/Functions/analyse_results.R")

#################################################################
# Define variables
#################################################################
# Define whether to use BNG ('bng') or decimal degrees projection ('wgs84')
projection <- 'wgs84' 
# Define whether to keep the duplicates in ('keep') or not ('drop')
duplication <- 'keep'

#################################################################
# Read in required data
#################################################################
####### Data to fit model with 
# Spatial points dataframe containing locations of measured peat depth samples and their depth, slope and elevation values 
pd_sample_pts_with_covars <- readOGR(dsn = "Data/Generated/CleanedPeatDepthSamples_withCovariates", layer = "Humberstone_CleanedPD_withCovariates")

####### Data to predict with
# Dataframe containing locations at 5m intervals with slope and elevation values (generated from above raster layers
# in "DefineStudyArea/DefinePtsWithinRegion_withCovariateValues.R")
# This is used in defining locations of extra points to add in synthetic samples with short distance subset
aoi_5mIntervals_pts <- read.csv("Data/Generated/UnmeasuredLocations/humberstone_aoi_df.csv")
# Convert this to a spatial points dataframe
aoi_5mIntervals_pts_spdf <- SpatialPointsDataFrame(coords = aoi_5mIntervals_pts[c(1:2)], data = aoi_5mIntervals_pts[c(3,4)],proj4string =  CRS("+init=epsg:27700"))
# Convert to wgs84
aoi_5mIntervals_pts_spdf <- spTransform(aoi_5mIntervals_pts_spdf, CRS("+init=epsg:4326"))
# Back to df
aoi_5mIntervals_pts <- as.data.frame(aoi_5mIntervals_pts_spdf)

#################################################################
# Prepare data
#################################################################
# Check for the presence of duplicates in spatial location (both x and y coordinate)
# If duplication is set to drop then  delete duplicated locations
if (duplication == 'drop'){
  print ("duplicates removed")
  pd_sample_pts_with_covars = find_duplication(pd_sample_pts_with_covars)
  # Check for the presence of points which are <1m together.
  # Delete one of each of these pairs of points.
  pd_sample_pts_with_covars = find_nearestNeighours (pd_sample_pts_with_covars)} else
  {print ("duplicates kept")}

# Convert projection system to that specified at start of file.
if (projection == 'wgs84') { 
  pd_sample_pts_with_covars = spTransform(pd_sample_pts_with_covars, CRS("+init=epsg:4326"))
} 

# Convert the spatial points dataframe to a dataframe for modelling
dat <- data.frame(pd_sample_pts_with_covars)

# Add transformations of depth
dat$sqrtdepth <- sqrt(dat$depth)

#------------------------------------------------------------------------------
#2. Fit linear model on the sample data
#------------------------------------------------------------------------------
# This will contain the coefficients defining the relationship between the response
# and predictors. 
model.lm <- lm(formula=sqrtdepth~elevation + Slope_5m, data=dat)
model.lm.predictions <-
  predict(object=model.lm, newdata=dat, interval="prediction")

#------------------------------------------------------------------------------
#3. Fit spatial model on the sample data
#------------------------------------------------------------------------------
# Convert dataframe to geodata
test.sp <-
  as.geodata(obj=dat, coords.col=c('coords.x1', 'coords.x2'), data.col= 'sqrtdepth', covar.col= c("elevation", "Slope_5m") )

# Jitter any duplicated data locations which appear to be identical
test.sp <- jitterDupCoords(test.sp, max=0.01)
## Fit the model
model.sm <-
  likfit(geodata=test.sp, trend=~elevation + Slope_5m,
         ini.cov.pars=c(15, 0.05), fix.nugget=FALSE,
         cov.model="exponential")

#------------------------------------------------------------------------------
# 4. Predictions at unmeasured locations - linear
#------------------------------------------------------------------------------
# Apply the fitted model to the data for which we want to make predictions (unmeasuredlocations)
# to generate predicted depths and upper and lower confidence intervals. 
model.lm.preds_errors <-predict(object=model.lm, newdata=aoi_5mIntervals_pts, interval="prediction")
model.lm.preds_errors_df <- data.frame(aoi_5mIntervals_pts, prediction = model.lm.preds_errors[,1],
                                       lwr = model.lm.preds_errors[,2], upr = model.lm.preds_errors[,3])

# Add a column for the interval width
model.lm.preds_errors_df$interval_width <- model.lm.preds_errors_df$upr - model.lm.preds_errors_df$lwr

## Set any negative prediction values to 0 (and lwr and upr?)
model.lm.preds_errors_df$prediction[model.lm.preds_errors_df$prediction<0] <- 0
#model.lm.preds_errors_df$lwr[model.lm.preds_errors_df$lwr<0] <- 0
#model.lm.preds_errors_df$upr[model.lm.preds_errors_df$upr<0] <- 0

# If prediction is 0 set the interval width to 0
#model.lm.preds_errors_df$interval_width[model.lm.preds_errors_df$prediction == 0] <- 0

# Add squared values to dataframe
model.lm.preds_errors_df$prediction_sq = model.lm.preds_errors_df$prediction^2
model.lm.preds_errors_df$lwr_sq = model.lm.preds_errors_df$lwr^2
model.lm.preds_errors_df$upr_sq = model.lm.preds_errors_df$upr^2 
model.lm.preds_errors_df$interval_width_sq <- model.lm.preds_errors_df$interval_width^2

#model.lm.preds_errors_df$prediction[model.lm.preds_errors_df$prediction == 0] <- NA
# delete rows with 0
#model.lm.preds_errors_df <- model.lm.preds_errors_df[model.lm.preds_errors_df$prediction != 0,]

# Convrt projection back to BNG
model.lm.preds_errors_spdf <- SpatialPointsDataFrame(coords = model.lm.preds_errors_df[c(3:4)], data = model.lm.preds_errors_df[c(1,2,5:12)],proj4string =  CRS("+init=epsg:4326"))
model.lm.preds_errors_spdf <- spTransform(model.lm.preds_errors_spdf, CRS("+init=epsg:27700"))

pixels_lm <- SpatialPixelsDataFrame(model.lm.preds_errors_spdf, tolerance = 0.99999, model.lm.preds_errors_spdf@data)
raster_lm_iw <- raster(pixels_lm[,'interval_width_sq'])
raster_lm_preds <- raster(pixels_lm[,'prediction_sq'])
plot(raster_lm_preds)

#------------------------------------------------------------------------------

# 5. Predictions at unmeasured locations - spatial
# Apply the fitted model to the data for which we want to make predictions (datframe_slope_elevation)
control.sm <-
  krige.control(type.krige="OK", trend.d=~sample_df$elevation +
                  sample_df$Slope_5m, trend.l=~unmeasuredlocations$elevation +
                  unmeasuredlocations$Slope_5m, obj.model=model.sm)
kriging.sm <-
  krige.conv(geodata=test.sp, locations=unmeasuredlocations[,c('longitude', 'latitude')], krige=control.sm)

# Calculate the upper and lower prediction intervals
model.sm.predictions <- as.data.frame(
  cbind(prediction = kriging.sm$predict,
        lwr = kriging.sm$predict - 1.96 * sqrt(kriging.sm$krige.var),
        upr = kriging.sm$predict+ 1.96 * sqrt(kriging.sm$krige.var)))

# Join it with the original dataframe
model.sm.predictions_df <- data.frame(unmeasuredlocations, prediction = model.sm.predictions[,1],
                                      lwr = model.sm.predictions[,2], upr = model.sm.predictions[,3])

# Add a column for the interval width
model.sm.predictions_df$interval_width <- model.sm.predictions_df$upr - model.sm.predictions_df$lwr

## Set any negative prediction values to 0 (and lwr and upr?)
model.sm.predictions_df$prediction[model.sm.predictions_df$prediction<0] <- 0
#model.sm.predictions_df$lwr[model.sm.predictions_df$lwr<0] <- 0
#model.sm.predictions_df$upr[model.sm.predictions_df$upr<0] <- 0

# If prediction is 0 set the interval width to 0
#model.sm.predictions_df$interval_width[model.sm.predictions_df$prediction == 0] <- 0

# Add squared values to dataframe
model.sm.predictions_df$prediction_sq = model.sm.predictions_df$prediction^2
model.sm.predictions_df$lwr_sq = model.sm.predictions_df$lwr^2
model.sm.predictions_df$upr_sq = model.sm.predictions_df$upr^2 
model.sm.predictions_df$interval_width_sq <- model.sm.predictions_df$interval_width^2

#model.sm.predictions_df <- model.sm.predictions_df[model.sm.predictions_df$prediction != 0,]
#Set 0 prediction values to NA
#model.sm.predictions_df$prediction[model.sm.predictions_df$prediction == 0] <- NA

# Convert to raster
#test <- model.sm.predictions_df[c(1,2,7)]    
#model.sm.predictions <- rasterFromXYZ(model.sm.predictions_df, crs = "+proj=longlat +datum=WGS84 +no_defs")  #Convert first two columns as lon-lat and third as value                

# COnvrt projection back to BNG
model.sm.preds_errors_spdf <- SpatialPointsDataFrame(coords = model.sm.predictions_df[c(3:4)], data = model.sm.predictions_df[c(1,2,5:12)],proj4string =  CRS("+init=epsg:4326"))
model.sm.preds_errors_spdf <- spTransform(model.sm.preds_errors_spdf, CRS("+init=epsg:27700"))
model.sm.predictions_df <- as.data.frame(model.sm.preds_errors_spdf)

#model.lm.preds_errors_spdf <- SpatialPointsDataFrame(coords = model.lm.preds_errors_df[c(3:4)], data = model.lm.preds_errors_df[c(1,2,5:12)],proj4string =  CRS("+init=epsg:4326"))
pixels_sm <- SpatialPixelsDataFrame(model.sm.preds_errors_spdf, tolerance = 0.99999, model.sm.preds_errors_spdf@data)
raster_sm_iw <- raster(pixels_sm[,'interval_width_sq'])
raster_sm_preds <- raster(pixels_sm[,'prediction_sq'])
plot(raster_sm_preds)
plot(raster_sm_iw)


####################### 
# Find differences between predictions
diff <- raster_sm_preds - raster_lm_preds
plot(raster_lm_preds,  col=terrain.colors(20))
title("Predicted depths (cm)", adj = 0.0, line = 0.8, cex.main =3)
plot(raster_sm_preds,  col=terrain.colors(20))
plot(diff, col=terrain.colors(20))

library(tmap)
tm_shape(aoi)+ tm_polygons(border.col = 'gray30' ,  col = 'white', alpha = 0.5)+
  tm_shape(diff_iw, bbox = bbox_new) +
  tm_raster("cover", palette = terrain.colors(10), title = "") +
  tm_layout(title = 'ddd', title.position = c(1,1), frame = TRUE, bg.color = NA, legend.text.size  = 1.0, legend.title.size = 2.2, 
            legend.position =c('left', 'bottom'))

diff_iw <- raster_sm_iw - raster_lm_iw
plot(raster_lm_iw,  col=terrain.colors(10))
title("Prediction interval widths (cm)", adj = 0.0, line = 0.7, cex.main =3)
plot(raster_sm_iw,  col=terrain.colors(10))
plot(diff_iw, col=terrain.colors(10))


#----------------- 
# 6. Plotting

sample <- SpatialPointsDataFrame(coords = sample_with_depths[c(3,4)], data = sample_with_depths[c(1,2,5:9)],proj4string =  CRS("+init=epsg:4326"))
#all_samples_spdf <- spTransform(all_samples_spdf, CRS("+init=epsg:4326"))

# Plot in leaflet - to allow comparison of areas predicted to be deep and deep sample points. 
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>%  
  addRasterImage(raster_sm_iw) %>%
  addCircles(data = sample, weight = 3, opacity =1,
             label=as.character(sample$depth)) 
#addPolylines(data = aoi_rp, weight = 3, opacity =1)

raster_lm <- raster(pixels[,'elevation'])
plot(raster_lm)
raster_lm <- raster(pixels[,'interval_width_sq'])

pal <- colorNumeric(palette = "YlOrRd",domain = values(raster_lm),na.color = "transparent")
pal2 <- colorNumeric(palette = "YlOrRd",domain = sample$depth ,na.color = "transparent")

leaflet() %>% 
  addProviderTiles("Stamen.TonerHybrid") %>%  
  addPolygons (data= aoi_trimmed, fillOpacity =0, col = 'black', weight =1) %>%
  addRasterImage(raster_lm, colors = pal, opacity = 0.8) %>%
  addCircles(data = sample, weight = 3, opacity =1, fillOpacity = 1, fillColor = ~pal2(depth), color = ~pal2(depth),
             label=as.character(sample$depth)) %>% 
  addLegend(pal = pal2, values = sample$depth,
            title = "Depth (cm)")


leaflet() %>% 
  addProviderTiles("Stamen.TonerHybrid") %>%  
  addPolygons (data= aoi_trimmed, fillOpacity =0, col = 'black', weight =1) %>%
  addRasterImage(raster_lm, colors = pal, opacity = 0.8) %>%
  #addCircles(data = sample, weight = 3, opacity =1, fillOpacity = 1, fillColor = ~pal2(depth), color = ~pal2(depth),
  #           label=as.character(sample$depth)) %>% 
  addLegend(pal = pal, values = values(raster_lm),
            title = "Depth (cm)")



######################################################################
#New Plotting
######################################################################
# Prediction itnervals
brk <- c(80,100,120,140,160,180)
brk <- c(80,85,90,95,100,105,110,120,130,140,150,160,170,180)
plot(raster_lm_iw, breaks=brk, col=terrain.colors(11), legend=F)
title("Prediction interval widths (cm)", adj = 0.0, line = 1.2, cex.main =1.5)
plot(raster_lm_iw, breaks=brk, col=terrain.colors(10), legend.only=T, box=F)
plot(raster_sm_iw, breaks=brk, col=terrain.colors(6), legend=F)
plot(raster_sm_iw, breaks=brk, col=terrain.colors(6), legend.only=T, box=F)


# predictions
brk <- c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300)
#brk <- c(0,  50,  100,  150, 200,  250, 300)
plot(raster_lm_preds, breaks=brk, col=terrain.colors(13), legend=F)
title("Predicted depths (cm)", adj = 0.0, line = 1.2, cex.main =1.5)
plot(raster_lm_preds, breaks=brk, col=terrain.colors(13), legend.only=T, box=F)
plot(raster_sm_preds, breaks=brk, col=terrain.colors(13), legend=F)
plot(raster_sm_preds, breaks=brk, col=terrain.colors(13), legend.only=T, box=F)

# Slope
slope<- raster(pixels_sm[,'Slope_5m'])
brk <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)
brk <- c(0, 5,  10,  15, 20,25,30)
plot(slope, breaks=brk, col=terrain.colors(7), legend=F)
plot(slope,  breaks=brk, col=terrain.colors(7),  legend.only=T, box=F)

# elevation
elevation <- raster(pixels_sm[,'elevation'])
brk <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)
brk <- c(230, 250,  270,  290, 310, 330, 350, 370, 390, 410, 430)
plot(elevation, breaks=brk, col=terrain.colors(20), legend=T)
plot(elevation,  breaks=brk, col=terrain.colors(7),  legend.only=T, box=F)

plot(slope)
plot(raster_lm_iw)

plot(sample_df$longitude, sample_df$latitude, col ='lightblue', fillcol = 'white', pch=20)

# Load required packages
library(rasterVis)

# open file
raster_sm <- raster(pixels[,'Slope_5m'])

# Set color palette
myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))

# Plot
levelplot(raster_sm, par.settings=myTheme, margin=F)


#
# Slope and elevation for study site
par(mar=c(4,4,1,2)+.9)
hist(model.sm.predictions_df$prediction_sq, breaks = 50, xlab = 'Predicted peat depth (cm)', xlim = c(0,300), 
     main ='', ylab = 'Number of locations' , col = 'darkgreen', cex.lab =1.1, ylim = c(0,30000))
abline(v = median(model.sm.predictions_df$prediction_sq, na.rm = TRUE), col = "black", lty = 1, lwd = 3)

hist(model.lm.preds_errors_df$prediction_sq, breaks = 50, xlab = 'Predicted peat depth (cm)', xlim = c(0,120), 
     main ='', ylab = 'Number of locations' , col = '#E69F00', cex.lab =1.1, ylim = c(0,10000))
abline(v = median(model.lm.preds_errors_df$prediction_sq, na.rm = TRUE), col = "black", lty = 1, lwd = 3)

hist(model.sm.predictions_df$prediction_sq)  
hist(mydata, col="lightblue")
abline(v = 54, col="red", lwd=3, lty=2)

# Plot sample locations coloured by depths
ggplot()+
  theme (plot.title = element_text(size = 10))+
  #theme_light() +
  #theme(panel.background = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #  geom_raster(data=model.lm.preds_errors_df, aes(x=longitude, y=latitude, fill=prediction_sq), show.legend = TRUE) +
  #scale_fill_distiller(palette = "RdYlGn", direction =1, limits = c(min(sample_df$Depth), max(sample_df$Depth)))+
  #scale_fill_gradient(limits=range(df$PredictedDepths), high = 'darkblue', low = 'lightblue') + 
  #scale_fill_brewer(limits=range(df$PredictedDepths), type = "seq", palette = 1, direction = 1, aesthetics = "fill")
  #geom_point(data = sample_df, aes(x=Slope_5m, y=elevation, fill=ObservedDepths), pch=21, colour = 'black') +
  geom_polygon(data =aoi,aes(x = long, y = lat), col = 'grey', fill= 'white')+
  geom_point(data = sample_df, aes(x=longitude, y=latitude, colour=depth), size =3.5, pch =20) +
  scale_color_distiller(palette = "Reds", direction =1) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(colour = "Depth (cm)") 
