# Impact of depth sampling strategy on geostatistical model accuracy
A major component of this research was evaluating the impact on the geostatistical model performance of the method used to collect the peat depth samples used in the model.
In particular, we were interested in:
1. The density of samples in a grid formation required to realise geostatistical model benefits; and
2. Whether alternative sampling methods could improve the accuracy of geostatistical model predictions possible with a certain number of sample points.

This research investigated this by creating samples containing sample points whose locations are in a regular grid, as well as in several other formations created by using the following sampling strategies:

* Regular grid
* Regular grid with short distance subset
* Spatial Coverage sample
* Spatial coverage sample with short distance subset

For each of these, samples with various numbers of sample points were defined. Collecting manual depth samples using all of these strategies, and various sample point densities, would have required an unfeasible amount of time and labour, and so instead, a method was formulated for generating synthetic depths programmatically. These synthetic sample datasets were then used in geostatistical depth modelling and the accuracy of the predictions was assessed in each case, to make inferences about the impact of the sampling strategy on model performance.

## Using R to define sample locations
Slope and elevation files used to define poitns?

#### Regular grid
Sample locations were defined on a regular grid using the "makegrid" function in the "gstat" package. This function takes as an input the number of points to include in the grid, this was adjusted until grids of the desired spacing were created. Grids were created over the bounding box of the study area and subsequently trimmed to the boundary of the study area.

#### Regular grid with short distance subset
Regular grids were created, using the method defined above. Subsequently, a small subset of the grid points were selected randomly, and these were used to base short distance clusters around. For each point in this subset, a buffer of Xm was created around the point.
Create a buffer around these points of Xm, and then find the points from the geodataframe of all points within the AOI that fall within this buffer
Randomly select from these points X number of points (depending on number of points to have in each cluster)

#### Spatial coverage sample
Uses stratify function (spcosa) to split the study area into (equally?) sized areas of a specified number.
One sample point is then located within each sub-area. Literature suggests this should be the centroid of each sub-area; however, this does not seem to be what is happening. Not sure why not...

#### Spatial coverage sample with short distance subset
As above
Randomly select some of the points around which to base short distance clusters
Create a buffer around these points of Xm, and then find the points from the geodataframe of all points within the AOI that fall within this buffer
Randomly select from these points X number of points (depending on number of points to have in each cluster)

## Using R to assign depth values to sample locations
Collecting manual depth samples for testing this would have required an unfeasible amount of time and labour, and so instead, a method was formulated for generating synthetic depths programmatically. Samples of four different
