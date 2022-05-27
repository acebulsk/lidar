# calculate the leaf area density over the plot by elevation bin

library(lidR)
library(lidRviewer)
library(sf)
set_lidr_threads(6)

# rough custom area in non wind affected zone
bbox <- c(626976.415, 5631975.953, 627012.022, 5632022.681)

fltr <- paste("-drop_z_below 2075 -drop_z_above 2100 -keep_xy", paste(bbox, collapse = " "))
# fltr <- paste("-drop_z_below 2075 -drop_z_above 2100 -keep_xy 626963.9 5632008.2 627009.1 5632027.8")

las <- readLAS('data/22_126_FT.las', select = "xyzrn", filter = fltr)
plot(las)

# scalar. The distance to the simulated cloth to classify a point cloud into ground and non-ground. The default is 0.5.
ct <- 0.2
# scalar. The distance between particles in the cloth. This is usually set to the average distance of the points in the point cloud. The default value is 0.5.
cr <- 0.5

algo <- csf(sloop_smooth = F, class_threshold = ct, cloth_resolution = cr, rigidness = 1)
las <- classify_ground(las, algo)
las <- normalize_height(las, tin())

plot(las, color = 'Classification')

lad <- LAD(las$Z)

hist(las$Z, n=50)
plot(lad, type="l", xlab="Elevation", ylab="Leaf area density")
