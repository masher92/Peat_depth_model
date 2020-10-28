## Assessing impact of depth sampling strategy on model accuracy 
This research evaluates the impact on the geostatistical model performance of the method used to collect the peat depth samples used in the model.
In particular, focussing on:
1. The density of samples in a grid formation required to realise geostatistical model benefits; and
2. Whether alternative sampling methods could improve the accuracy of geostatistical model predictions possible with a certain number of sample points.

This was investigated by creating samples containing sample points with locations in the following formations:

* Regular grid
* Regular grid with short distance subset
* Spatial Coverage sample
* Spatial coverage sample with short distance subset

For each of these, samples of various sizes (i.e. various numbers of sample points) are defined.  
Collecting manual depth samples using all of these strategies, and various sample sizes, would require an unfeasible amount of time and labour, and so instead, synthetic depths are generated programmatically. 
Both linear and geostatistical model performance is then assessed through cross-validation using these synthetic peat depth datasets.

The purpose of this part of the work is to assess whether model accuracy is influenced by the sampling strategy used. However, model accuracy could be influenced by the exact locations where the synthetic sample points happen to fall (e.g. if, by chance, a sample included lots of points in deep peat then performance of the model might be improved). In order to account for this, 50 variations of each sample configuration and sample size are generated and used in model cross-validation.

"CreateSyntheticSample_functions.R".

In each of the other R scripts, 



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

Diss <- "Spatial coverage samples were generated using the spcosa-package in R (Walvoort et al., 2010).
The package uses the k-means clustering algorithm to split the study region into N equally spaced
sub-regions, with one sample positioned randomly in each sub-region (Fig 4.3). Values of N were
chosen to match the sample numbers associated with the grids tested in Section 4.3.1. The algorithm
was run fty times, with a dierent sub-region delineation generated each time."

#### Spatial coverage sample with short distance subset
As above
Randomly select some of the points around which to base short distance clusters
Create a buffer around these points of Xm, and then find the points from the geodataframe of all points within the AOI that fall within this buffer
Randomly select from these points X number of points (depending on number of points to have in each cluster)

## Using R to assign depth values to sample locations
Collecting manual depth samples for testing this would have required an unfeasible amount of time and labour, and so instead, a method was formulated for generating synthetic depths programmatically. Samples of four different
