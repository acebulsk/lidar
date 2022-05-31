# bring in full directory as lasCatalog and add date to each file using filename, 
# write to new directory 

library(lidR)
library(lidRviewer)
set_lidr_threads(6)
library(tidyverse)
library(sf)


add_date <- function(filename, survdate, path = '', filter = "", select = "*", name, desc){
  las <- readLAS(filename, select = select, filter = filter)

  if(is.empty(las)) return(NULL)
  
  las_updt <- add_lasattribute(las, survdate, name, desc)
  
  writeLAS(las_updt, paste0(path, 'dated_', basename(filename)))
}

fltr <- paste("-drop_z_below 2075 -drop_z_above 2100 -keep_xy 626963.9 5632008.2 627009.1 5632027.8")
slct <- "xyzrn"

ctg <- readLAScatalog('data/raw')

ctg_files <- ctg$filename

surv_dates <- gsub('data/raw/22_', '', 
                  gsub('_FT.las', '', ctg_files)) |> as.numeric()

l <- list(ctg_files, surv_dates)

# write chunks to new directory
out <- pmap(l, add_date, path = 'data/processed/dated/', filter = fltr, select = select, name = "surveydate", desc = "Date of LiDAR scan.")

