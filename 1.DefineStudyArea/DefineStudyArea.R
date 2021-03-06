library(rgdal)

# Set working directory
setwd("C:/Users/gy17m2a/OneDrive - University of Leeds/Msc/Dissertation/DataAnalysis/")

##################################################
# Determine the boundaries within which to construct the synthetic dataset 
# This involves trimming the AOI to within the Moorland line (to ensure exclusion of areas which aren't peatland)
##################################################
# Shapefile with extent of Moorland line
ml <- readOGR(dsn = "Data/Input/Moorland_Line_and_Project_Area", layer = "180208_Dales_and_Nidderdale_Moorland_OSGB")
# Shapefile containing extent of the Humberstone AOI
aoi <- readOGR(dsn = "Data/Input/Site_AOIs", layer = 'Humberstone_AOI')

# ?
ml_buff <- gBuffer(ml, byid=TRUE, width=0)
aoi_buff <- gBuffer(aoi, byid=TRUE, width=0)
aoi_trimmed <- crop(ml_buff, aoi_buff)

# Check plotting
aoi_trimmed_wgs84 <- spTransform(aoi_trimmed, CRS("+init=epsg:4326"))
aoi_wgs84 <- spTransform(aoi, CRS("+init=epsg:4326"))
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>%  
  addTiles() %>% 
 # addPolygons(data = aoi_trimmed_wgs84, fillOpacity = 0, weight = 3, color  = 'black') %>%
  addPolygons(data = aoi_wgs84, fillOpacity = 0, weight = 3, color  = 'black')

# Save to file
writeOGR(obj=aoi_trimmed, dsn="Data/Generated/StudyAreaTrimmedToMoorlandLine", layer="aoi_trimmed", driver="ESRI Shapefile")

