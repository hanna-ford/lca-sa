# Where to run
# This is the "development" script where the process is tested on a small area
# and can be run on a local machine. This script includes both the pre-processing of the common
# files and the model run.
#
# Data Preparations
# 1. Load required libraries and setup variable ** Remember to add your file locations **
# 2. Load required files and setup lists
# 3. Import lake and river shapefiles for setup - Single serial step
# 4. Clip the river and lakes shapefiles to the raster extent - Single serial step
# 5. Buffer rivers based on scale rank and combine polygons with lakes as "Barrier" - Single serial step
# 6. Create raster data versions of barriers - Single serial step
# 7. Create Grid of Sample Points, limit to points along edges of study area - Single serial step
# 8. MoveCost Calculations - Parallel step


## 1: Load required libraries and setup variable ----------


## Install required packages
## commented packages are either installed at cluster level or
## ended up not being used.

# install.packages(c("sp", "sf", "rgdal", "stars", "terra", "rgeos", "raster", "proj4", "foreach", "doParallel", "movecost", "stringr", "dplyr", "gdalUtilities", "ggplot2"))

library(sp)
library(sf)
library(rgdal) ## will be retired in 2023
library(stars)
library(terra)
library(rgeos)
library(raster)
library(proj4)
library(Matrix)
library(gdistance)
library(leastcostpath) ## movecost is built off this and gdistance, planning to use those directly
library(foreach)
library(doParallel)
library(stringr)
library(dplyr)
library(gdalUtilities)
library(ggplot2)

# library(movecost) ## building a new version that doesn't use this
# library(snow)
# library(doSNOW)
# library(parallel)
# library(igraph)
# library(bigstatsr)
# library(itertools)
# library(readxl)
# library(lwgeom)
# library(foreign)

# Setup for iteration
# When preparing in an interactive, virtual session the myjob and jobitr variables
#  will need to be set manually unless a slurm job exists for the session.
myjob <- Sys.getenv("SLURM_JOB_ID")
# jobitr <- as.numeric(Sys.getenv('JOBITR'))

# comment this out before running in batch;
# if running single this should indicate which point is being processed
jobitr <- 1

# Setup variables pointing to various directories
# These are the locations for Pinnacle - note that before running the script
# the base files will need to be uploaded to Pinnacle.

# scratch.dir <- paste0("/scratch/", myjob)
# storage.inputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/varinputs")
# storage.outputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/results")
# data.outputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/dataoutputs")

# ## File locations for Cupcake
# scratch.dir <- paste0("J:/temp/lca-sa")
# storage.inputs <- paste0("J:/Box Sync/JoshuaRobinson/lca-sa/varinputs")
# storage.outputs <- paste0("J:/Box Sync/JoshuaRobinson/lca-sa/results")
# data.outputs <- paste0("J:/Box Sync/JoshuaRobinson/lca-sa/dataoutputs")

## File locations for Teacup
scratch.dir <- paste0("C:/Temp/JRobinson/lca-sa")
storage.inputs <- paste0("C:/Users/hlford/Box/JoshuaRobinson/lca-sa/varinputs")
storage.outputs <- paste0("C:/Users/hlford/Box/JoshuaRobinson/lca-sa/results")
data.outputs <- paste0("C:/Users/hlford/Box/JoshuaRobinson/lca-sa/dataoutputs")

## Set the working directory to scratch
setwd(scratch.dir)

## create required directories, if needed
## Check if the folder "Data" exists in the current directory, if not creates it
ifelse(!dir.exists("./tmp/"), dir.create("./tmp/"), "Folder exists already")

## set some of the options that will be used frequently
## including where the tmp directory will be
raster::rasterOptions(format = "GTiff", overwrite = TRUE, tmpdir = paste0(scratch.dir, "/tmp/"), timer = TRUE)


## 2: Load required files and setup lists ----------


# untar the DEM (source: OpenTopography SRTM15+)
dem.untar <- utils::untar(paste0(storage.inputs, "/rasters_SRTM15Plus.tar.gz"), exdir = paste0(scratch.dir, "/tmp"))

# make a raster out of the the tar file; cropping will happen below
dem <- terra::rast(paste0(scratch.dir, "/tmp/output_SRTM15Plus.tif"))

# update the crs on all the files so they are the same
terra::crs(dem)

# set the crs for this project
crs.thisproject <- terra::crs(dem)

# Cleanup intermediate files
rm(dem.untar)


# 3: Import lake and river shapefiles ----------


# Import the Natural Earth 10m Rivers (source: Natural Earth Physical vectors collection)
rivers <-
  sf::st_read(paste0(storage.inputs, "/ne_10m_rivers_lake_centerlines_scale_rank.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)

lakes <-
  sf::st_read(paste0(storage.inputs, "/ne_10m_lakes.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)

coast <-
  sf::st_read(paste0(storage.inputs, "/ne_10m_coastline.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)


# 4: Clip the river and lakes shapefiles to the raster extent ----------


# Create the cropping extent
# if a new DEM is introduced with a different extent, then these values would need to be updated.

ext(dem)

new_extent <- extent(26.7958333333335, 29.7291666666669, -30.8916666666668, -28.4208333333334)
class(new_extent)


# PREP Coastline
coast.valid <- coast[which(!is.na(coast$scalerank)), ]

# convert to a single geometry
coast.types <- vapply(sf::st_geometry(coast.valid), function(x) {
  class(x)[2]
}, "")

unique(coast.types)

clip.coast <-
  coast.valid[grepl("*MULTILINESTRING", coast.types), ] %>% # ignore the geometry collections
  sf::st_crop(x = ., y = new_extent) %>%
  sf::st_write(., paste0(scratch.dir, "/", "clip_coast.shp"), delete_layer = TRUE)


# PREP Rivers
rivers.valid <- rivers[which(!is.na(rivers$scalerank)), ]

# convert to a single geometry
rivers.types <- vapply(sf::st_geometry(rivers.valid), function(x) {
  class(x)[2]
}, "")

unique(rivers.types)

clip.rivers <-
  rivers.valid[grepl("*MULTILINESTRING", rivers.types), ] %>% # ignore the geometry collections
  sf::st_crop(x = ., y = new_extent) %>%
  sf::st_write(., paste0(scratch.dir, "/", "clip_rivers.shp"), delete_layer = TRUE)

# Drop the un-needed fields so that the append for barrier will be clean
clip.rivers.clean <- clip.rivers[, -(15:40)]
clip.rivers.clean <- clip.rivers.clean[, -(3:4)]
clip.rivers.clean <- clip.rivers.clean[, -(6:12)]

# Filtering for only rivers with a scale rank of 7, 8, 9 (largest rivers)
rivers.scalerank <- clip.rivers.clean[which(clip.rivers.clean$scalerank >= 7), ]

# Cleanup intermediate files
rm(rivers, rivers.valid, rivers.types, clip.rivers, clip.rivers.clean)

# Export results to folder for use later
sf::st_write(rivers.scalerank, paste0(storage.outputs, "/", "clip_rivers_sr.shp"), delete_layer = TRUE)


# PREP Lakes
lakes.valid <- lakes[which(!is.na(lakes$scalerank)), ]

# Convert to a single geometry
lakes.types <- vapply(sf::st_geometry(lakes.valid), function(x) {
  class(x)[2]
}, "")

unique(lakes.types)

clip.lakes <-
  lakes.valid[grepl("*MULTIPOLYGON", lakes.types), ] %>% # ignore the geometry collections
  sf::st_crop(x = ., y = new_extent) %>%
  sf::st_write(., paste0(storage.outputs, "/", "clip_lakes.shp"), delete_layer = TRUE)

# Drop the un-needed fields
clip.lakes.clean <- clip.lakes[, -(15:41)]
clip.lakes.clean <- clip.lakes.clean[, -(4:4)]
clip.lakes.clean <- clip.lakes.clean[, -(6:13)]

# Filtering for only lakes with a scale rank of 0, 3 (largest lakes)
lakes.scalerank <- clip.lakes.clean[which(clip.lakes.clean$scalerank <= 3), ]

# Cleanup intermediate files
rm(lakes, lakes.valid, lakes.types, clip.lakes, clip.lakes.clean)

# Export results to folder for use later
sf::st_write(lakes.scalerank, paste0(storage.outputs, "/", "clip_lakes_sr.shp"), delete_layer = TRUE)


# 5: Buffer rivers based on scale rank and combine polygons with lakes as "Barrier" ----------


# Create buffer around rivers (100m-total / 50m-each side)
# This can be changed, but recommend testing changes in base script before implementing on
# larger scale.
rivers.scalerank.buffer <- terra::buffer(vect(rivers.scalerank), 100)
rivers.buffer <- sf::st_as_sf(rivers.scalerank.buffer)

# Combine buffered rivers and lakes
barrier <- rbind(lakes.scalerank, rivers.buffer)

# PREP the barrier files and export results to folder for use later
barrier.valid <- barrier[which(!is.na(barrier$scalerank)), ]
barrier.sp <- as(barrier.valid, "Spatial")
sf::st_write(barrier.valid, paste0(storage.outputs, "/", "barrier_sp.shp"), delete_layer = TRUE)

# Cleanup intermediate files
rm(rivers.scalerank, rivers.scalerank.buffer, lakes.scalerank, rivers.buffer, barrier, barrier.valid)

# Sanity check: Make a map to see if it all looks correct
plot(dem)
plot(barrier.sp, add = TRUE)


# 6: create a slope-based cost surface ----------


# cost functions currently implemented within leastcostpath

cfs <- c(
  "tobler", "tobler offpath", "irmischer-clarke male",
  "irmischer-clarke offpath male", "irmischer-clarke female",
  "irmischer-clarke offpath female", "modified tobler",
  "wheeled transport", "herzog", "llobera-sluckin", "campbell 2019"
)

# neighbors can be 4, 8, 16, 32, or 48. A greater number of neighbors will result in cost surface and LCP approximating reality better - but be aware that above 8 there is the possibility to "jump" barriers.

neigh <- 8

slope_cs <- leastcostpath::create_slope_cs(dem = raster(dem), cost_function = "tobler", neighbours = neigh)

plot(raster(slope_cs), col = grey.colors(100))
plot(barrier.sp, add = TRUE)


# 7: Create barrier cost surface ----------


# Create a blank raster on to which the river and lake polys will be rasterized
blank.r <- raster::setValues(dem, NA)

# Destination dataset (raster dataset to be written/burned to)
dst_filename <- paste0("barriers", ".tif", sep = "")
# # Export results to folder for use later
terra::writeRaster(blank.r, dst_filename, overwrite = TRUE)

barrier.path <- paste0(storage.outputs, "/", "barrier_sp.shp")

# Rasterize the dataset using gdal outside of R; bring result back into R
# Note that this is set by at=TRUE to burn all pixels that are touched by the vector
barriers.rastmp <- gdalUtilities::gdal_rasterize(barrier.path, dst_filename, b = 1, at = TRUE, a = "scalerank")

barrier.rast <- raster::raster(barriers.rastmp)

plot(barrier.rast)

# Create a barrier of altitude values less than or equal to -125
# this is to adjust the "coastline" per publication notes
# The altitude limitation can be changed, but should be tested if changing

altitude <- dem <= -125
altitude[altitude == 0] <- NA

plot(altitude)

altitude.raster <- raster(altitude)

# the values NOT NA will be assigned the field argument value. If 0 (default) then movement within the area will be completely prohibited
natural_cs <- leastcostpath::create_barrier_cs(raster = barrier.rast, barrier = barrier.rast, neighbours = neigh, field = 0, background = 1)

altitude_cs <- leastcostpath::create_barrier_cs(raster = altitude.raster, barrier = altitude.raster, neighbours = neigh, field = 0, background = 1)

plot(raster(altitude_cs))
plot(raster(natural_cs))

# multiplying the two cost surfaces ensures that barriers continue to completely inhibit movement (i.e. slope_cs values * 0 = 0)
pre_cs <- natural_cs * altitude_cs
slope_altitude_cs <- slope_cs * pre_cs

plot(raster(slope_altitude_cs), col = grey.colors(100))


# 8: Create Grid of Sample Points ----------


# This whole bit is for testing where no coastline exits:

# regular sampling
sample <- terra::spatSample(dem, size = c(100), method = "regular", as.points = TRUE, values = TRUE, xy = FALSE, warn = TRUE)

# Sanity check: Make a map to see if it all looks correct
plot(dem)
plot(sample, add = TRUE)


# Filtering for only points around edges
sample.f2 <- as(sample, "Spatial")
sample.f3.bbox <- sf::st_as_sfc(st_bbox(sample.f2))
sample.f3.buffer.int <- terra::buffer(vect(sample.f3.bbox), -10000)
sample.final.sv <- terra::erase(sample, sample.f3.buffer.int)

# # This bit is for testing once we have coastline on all sides
#   # setting the clipped coast as SpatVector
#   clip.coast.vect <- terra::vect(clip.coast)
#
#   # creating a buffer of the coast
#   coast.buffer.int <- terra::buffer(clip.coast.vect, -10000)
#
#   # sampling within the coastal buffer only
#   sample <- terra::spatSample(coast.buffer.int, size = c(250), method="random")
#
#   # Sanity check: Make a map to see if it all looks correct
#   plot(dem.2)
#   plot(coast.buffer.int, add=TRUE)
#   plot(sample, add = TRUE)


# Sanity check: Make a map to see if it all looks correct
plot(raster(slope_altitude_cs), col = grey.colors(100))
plot(sample, add = TRUE)
plot(sample.final.sv, add = TRUE, col = "red")
plot(barrier.sp, add = TRUE)

# Export results to folder for use later
terra::writeVector(sample.final.sv, "sample_final.shp", filetype = "ESRI Shapefile", overwrite = TRUE)


# 9: LCP Calculations ----------


sample.final.sp <- as(sample.final.sv, "Spatial")

# force garbage collection
gc()

# i <- paste0(jobitr)

# run just the LCPs
# run from a single point to all points
# to run to and from for each point
mc.bmc <- leastcostpath::create_FETE_lcps(
  cost_surface = slope_altitude_cs,
  locations = sample.final.sp[11:18, ],
  cost_distance = FALSE,
  parallel = FALSE,
  ncores = 1
)

# sanity check - plot the data so far
plot(raster(slope_altitude_cs), col = grey.colors(100))
plot(sample.final.sp[11:18, ], add = TRUE, col = "red")
plot(barrier.sp, col = "blue", add = TRUE)
plot(mc.bmc, col = "black", add = TRUE)
