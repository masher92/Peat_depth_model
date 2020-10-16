# Impact of peat depth sampling strategy on geostatistical model performance
A major component of this research was evaluating the impact on the geostatistical model performance of the method used to collect the peat depth samples used in the model. 
In particular, testing focussed upon:
* The density of samples collected in a grid formation in order to realise geostatistical model benefits; and
* Whether alternative sampling methods could improve the accuracy of geostatistical model predictions

## Sampling strategies

### Assigning Depth Values
Collecting manual depth samples for testing this would have required an unfeasible amount of time and labour, and so instead, a method was formulated for generating synthetic depths programmatically. Samples of four different 

### Regular grid
Uses makegrid function (gstat), with a given cell spacing
Trim to AOI boundary/moorland line boundary
Assign depths using synthetic depth generation method

### Regular grid with short distance subset
As above
Randomly select some of the points around which to base short distance clusters
Create a buffer around these points of Xm, and then find the points from the geodataframe of all points within the AOI that fall within this buffer
Randomly select from these points X number of points (depending on number of points to have in each cluster)

### Spatial coverage sample
Uses stratify function (spcosa) to split the study area into (equally?) sized areas of a specified number.
One sample point is then located within each sub-area. Literature suggests this should be the centroid of each sub-area; however, this does not seem to be what is happening. Not sure why not...

### Spatial coverage sample with short distance subset
As above
Randomly select some of the points around which to base short distance clusters
Create a buffer around these points of Xm, and then find the points from the geodataframe of all points within the AOI that fall within this buffer
Randomly select from these points X number of points (depending on number of points to have in each cluster)
