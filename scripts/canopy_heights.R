# calculate tree heights

library(lidR)
library(lidRviewer)
library(sf)
set_lidr_threads(6)

# plot <- read_sf('../../research/fieldsites/fortress/fortress_forest_plot.shp') |> 
#   dplyr::filter(id == 1) |> 
#   st_transform(st_crs(32611))

# bbox <- st_bbox(plot) 

# custom area in non wind affected zone
bbox <- c(626976.415, 5631975.953, 627012.022, 5632022.681)

fltr <- paste("-drop_z_below 2075 -drop_z_above 2098 -keep_xy", paste(bbox, collapse = " "))
# fltr <- paste("-drop_z_below 2075 -drop_z_above 2100 -keep_xy 626963.9 5632008.2 627009.1 5632027.8")

las <- readLAS('/media/alex/CRHO PHOTOS/fortress/lidar-data/22_243_FT.las', select = "xyzrn", filter = fltr)
plot(las)

# scalar. The distance to the simulated cloth to classify a point cloud into ground and non-ground. The default is 0.5.
ct <- 0.2
# scalar. The distance between particles in the cloth. This is usually set to the average distance of the points in the point cloud. The default value is 0.5.
cr <- 0.5

algo <- csf(sloop_smooth = F, class_threshold = ct, cloth_resolution = cr, rigidness = 1)
las <- classify_ground(las, algo)
las <- normalize_height(las, tin())

saveRDS(las, 'data/22_243_FT_ground_class_normalized.rds')

plot(las, color = 'Classification')

ttops <- locate_trees(las, lmf(ws = 5))

range(ttops$Z)

saveRDS(ttops, 'data/22_243_FT_ttops_5m_norm.rds')

# ttops <- readRDS('data/forest_plot_ttops.rds')

chm <- rasterize_canopy(las, 0.25, pitfree(subcircle = 0.2))

saveRDS(chm, 'data/22_243_FT_chm_5m_norm.rds')

# 2d vis
plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

chm_proj <- chm
crs(chm_proj) <- 'EPSG:32611'

# 3d vis
ttops_fltr <- ttops[ttops$Z < 18.48600 & ttops$treeID != 46 & !ttops$treeID %in% c(5, 9, 51),]
x <- plot(las, bg = "white", size = 4)
add_treetops3d(x, ttops_fltr)

# interactive vis
library(tmap)
ttops_fltr2 <- ttops
st_crs(ttops_fltr2) <- 'EPSG:32611'

tmap_mode('plot')
tm_shape(chm_proj) +
  tm_raster() +
  tm_shape(ttops_fltr2) +
  tm_dots()
tmap_mode('view')
tm_shape(chm_proj) +
  tm_raster() +
  tm_shape(ttops_fltr2) +
  tm_dots()

# mean canopy height
mean(ttops$Z)

