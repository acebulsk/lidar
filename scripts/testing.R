library(lidR)
library(lidRviewer)
set_lidr_threads(6)
library(tidyverse)
library(sf)

# plot_crossection <- function(las,
#                              p1 = c(min(las@data$X), mean(las@data$Y)),
#                              p2 = c(max(las@data$X), mean(las@data$Y)),
#                              width = 4, colour_by = NULL)
# {
#   colour_by <- enquo(colour_by)
#   data_clip <- clip_transect(las, p1, p2, width)
#   p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.05, alpha = 0.5) + coord_equal() + theme_minimal() + ylim(c(2075, 2090))
#   
#   if (!is.null(colour_by))
#     p <- p + aes(color = !!colour_by) + labs(color = "")
#   
#   return(p)
# }

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

# examples 

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile)
# plot(las, backend = "lidRviewer")

las <- classify_ground(las, algorithm = pmf(ws = 5, th = 3))

plot(las, color = "Classification", size = 3, bg = "white", backend = "lidRviewer") 

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzrn")
algo <- csf(sloop_smooth = T, class_threshold = 1, cloth_resolution = 1, time_step = 1)
las <- classify_ground(las, algorithm = algo) |> filter(classification == 1)

plot(las, color = "Classification", size = 3, bg = "white", backend = "lidRviewer") 

p1 <- c(273420, 5274455)
p2 <- c(273570, 5274460)


plot_crossection(las, p1 , p2, colour_by = factor(Classification))


