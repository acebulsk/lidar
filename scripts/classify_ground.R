library(lidR)
library(lidRviewer)
set_lidr_threads(6)
library(tidyverse)
library(sf)

plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 4, colour_by = NULL)
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}

# fortress example

# fortress bounding box rough 

# random
p1 <- c(626963.9, 5632008.2)
p2 <- c(627000.1, 5632018.8)

# spruce east of tower
p1 <- c(627011.63, 5631997.3)
p2 <- c(627014.68, 5631997.3)

plot <- read_sf('../../research/fieldsites/fortress/fortress_forest_plot.shp') %>% 
  st_transform(st_crs(32611))

bbox <- st_bbox(plot) 

# fltr <- paste("-drop_z_below 2060 -drop_z_above 2100 -keep_xy", paste(bbox, collapse = " "))
fltr <- paste("-drop_z_below 2075 -drop_z_above 2100 -keep_xy 626963.9 5632008.2 627009.1 5632027.8")

las1 <- readLAS('data/22_126_FT.las', select = "xyzrn", filter = fltr)
las1
las2 <- readLAS('data/22_130_FT.las', select = "xyzrn", filter = fltr)
las2
# las <- readLAS('22_047_FT.las', select = "xyzrn", filter = fltr)

ll <- c(las1, las2)

plot(las1, axis = T)
plot(las2, axis = T)

# plot(las, backend = "lidRviewer")

# scalar. The distance to the simulated cloth to classify a point cloud into ground and non-ground. The default is 0.5.
ct <- 0.1
# scalar. The distance between particles in the cloth. This is usually set to the average distance of the points in the point cloud. The default value is 0.5.
cr <- 0.5


algo <- csf(sloop_smooth = F, class_threshold = ct, cloth_resolution = cr, rigidness = 1)

ll_class <- lapply(ll, classify_ground, algorithm = algo)

df_class <- do.call("rbind", ll_class)

plot_crossection(df_class, width = .025,p1 = p1, p2 = p2, colour_by = factor(Classification))

plot_crossection(ll_class[[1]], width = .025,p1 = p1, p2 = p2, colour_by = factor(Classification))
plot_crossection(ll_class[[2]], width = .025,p1 = p1, p2 = p2, colour_by = factor(Classification))

las1 <- classify_poi(ll_class[[1]], class = 1L)
las2 <- classify_poi(ll_class[[2]], class = 2L)

all <- rbind(las1, las2)


plot_crossection(all, width = .025,p1 = p1, p2 = p2, colour_by = factor(Classification))

plotly::ggplotly()
plot(df_gnd, backend = "lidRviewer")



g1 <- plot_crossection(las, width = .1,p1 = p1, p2 = p2)

g2 <- plot_crossection(las2, width = .05, p1 = p1, p2 = p2)
g3 <- plot_crossection(las3, width = .1,p1 = p1, p2 = p2)