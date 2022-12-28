# Where to run
# This portion or the script can be ran to setup the files on pinnacle portal; this script will
# setup the common files that will be needed for the parallelized version to run. My recommendation is
# to run this is an interactive, virtual session for R.
#
# Data Preparations
# 1. Load required libraries and setup variable
# 2. Load required files and setup lists
# 3. Import lake and river shapefiles for setup - Single serial step
# 4. Clip the river and lakes shapefiles to the raster extent - Single serial step
# 5. Buffer rivers based on scale rank and combine polygons with lakes as "Barrier" - Single serial step
# 6. Create raster data versions of barriers - Single serial step
# 7. Create Grid of Sample Points, limit to points along edges of study area - Single serial step
# 8: Copy created files to the data output file on my Home Directory



## 1: Load required libraries and setup variable ----------


## Install required packages
## commented packages are either installed at cluster level or
## ended up not being used.

#install.packages(c("sp", "sf", "rgdal", "stars", "terra", "rgeos", "raster", "proj4", "foreach", "doParallel", "movecost", "stringr", "dplyr", "gdalUtilities", "ggplot2"))

library(sp)
library(sf)
library(rgdal) ## will be retired in 2023
library(stars)
library(terra)
library(rgeos)
library(raster)
library(proj4)
library(foreach)
library(doParallel)
library(movecost)
library(stringr)
library(dplyr)
library(gdalUtilities)
library(ggplot2)

#library(snow)
#library(doSNOW)
#library(parallel)
#library(igraph)
#library(bigstatsr)
#library(itertools)
#library(readxl)
#library(lwgeom)
#library(foreign)

# Setup for iteration
# When preparing in an interactive, virtual session the myjob and jobitr variables
#  will need to be set manually unless a slurm job exists for the session.
# myjob <- Sys.getenv('SLURM_JOB_ID')
jobitr <- as.numeric(Sys.getenv('JOBITR'))

# comment this out before running in batch; 
# if running single this should indicate which point is being processed
jobitr <- 1

# Setup variables pointing to various directories
# These are the locations for Pinnacle - note that before running the script
# the base files will need to be uploaded to Pinnacle.

scratch.dir <- paste0("/scratch/", myjob)
storage.inputs <- paste0("/scrfs/storage/hlford/home/data/varinputs")
storage.outputs <- paste0("/scrfs/storage/hlford/home/data/results")
data.outputs <- paste0("/scrfs/storage/hlford/home/data/data_outputs")

## Set the working directory to scratch
setwd(scratch.dir)

## create required directories, if needed
## Check if the folder "Data" exists in the current directory, if not creates it
ifelse(!dir.exists("./tmp/"), dir.create("./tmp/"), "Folder exists already")


## set some of the options that will be used frequently
## including where the tmp directory will be
raster::rasterOptions(format = "GTiff", overwrite = TRUE, tmpdir = paste0(scratch.dir, "/tmp/"), timer = TRUE)


## 2: Load required files and setup lists ----------


# untar the DEM (source: OpenTopography SRTM15+), the catchment area (source: OpenTopography SRTM15+), the pit removal (source: Open Topography SRTM15+)
dem.untar <-  utils::untar(paste0(storage.inputs,"/rasters_SRTM15Plus.tar.gz"), exdir = paste0(scratch.dir, "/tmp"))

# make a raster out of the the tar file; cropping will happen below
dem <- raster::raster(paste0(scratch.dir, "/tmp/output_SRTM15Plus.tif"))

# update the crs on all the files so they are the same
raster::crs(dem)

# set the crs for this project
crs.thisproject <- raster::crs(dem)

# Cleanup intermediate files
rm(dem.untar)

setwd(scratch.dir)


# 3: Import lake and river shapefiles ----------


# Import the Natural Earth 10m Rivers (source: Natural Earth Physical vectors collection)
rivers <-
  sf::st_read(paste0(storage.inputs, "/ne_10m_rivers_lake_centerlines_scale_rank/ne_10m_rivers_lake_centerlines_scale_rank.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)

lakes <-
  sf::st_read(paste0(storage.inputs, "/ne_10m_lakes/ne_10m_lakes.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)


# 4: Clip the river and lakes shapefiles to the raster extent ----------


# Create the cropping extent
# to determine the extent this is from extent(dem) and then the values are entered here
# if a new DEM is introduced with a different extent, then these values would need to be updated.
new_extent <- extent(9.795833, 42.18333, -35.95833, -12.25)
class(new_extent)

# PREP Rivers
rivers.valid <- rivers[which(!is.na(rivers$scalerank)), ]

# convert to a single geometry
rivers.types <- vapply(sf::st_geometry(rivers.valid), function(x) {
  class(x)[2]
}, "")

unique(rivers.types)

clip.rivers <-
  rivers.valid[grepl("*MULTILINESTRING", rivers.types), ] %>%  #ignore the geometry collections
  sf::st_crop(x = ., y = new_extent) %>%
  sf::st_write(., paste0(scratch.dir, "/", "clip_rivers.shp"), delete_layer = TRUE) 

# Drop the un-needed fields so that the append for barrier will be clean
clip.rivers.clean <- clip.rivers[,-(15:40)]
clip.rivers.clean <- clip.rivers.clean[,-(3:4)]
clip.rivers.clean <- clip.rivers.clean[,-(6:12)]

# Filtering for only rivers with a scale rank of 7, 8, 9 (largest rivers)
rivers.scalerank <- clip.rivers.clean[which(clip.rivers.clean$scalerank >= 7), ]

# Cleanup intermediate files
rm(rivers, rivers.valid, rivers.types, clip.rivers, clip.rivers.clean)

# Export results to folder for use later
sf::st_write(rivers.scalerank, paste0(storage.dir, "/", "clip_rivers_sr.shp"), delete_layer = TRUE) 

# PREP Lakes
lakes.valid <- lakes[which(!is.na(lakes$scalerank)), ]

# Convert to a single geometry
lakes.types <- vapply(sf::st_geometry(lakes.valid), function(x) {
  class(x)[2]
}, "")

unique(lakes.types)

clip.lakes <-
  lakes.valid[grepl("*MULTIPOLYGON", lakes.types), ] %>%  #ignore the geometry collections
  sf::st_crop(x = ., y = new_extent) %>%
  sf::st_write(., paste0(storage.dir, "/", "clip_lakes.shp"), delete_layer = TRUE) 

# Drop the un-needed fields
clip.lakes.clean <- clip.lakes[,-(15:41)]
clip.lakes.clean <- clip.lakes.clean[,-(4:4)]
clip.lakes.clean <- clip.lakes.clean[,-(6:13)]

# Filtering for only lakes with a scale rank of 0, 3 (largest lakes)
lakes.scalerank <- clip.lakes.clean[which(clip.lakes.clean$scalerank <= 3), ]

# Cleanup intermediate files
rm(lakes, lakes.valid, lakes.types, clip.lakes, clip.lakes.clean)

# Export results to folder for use later
sf::st_write(lakes.scalerank, paste0(storage.dir, "/", "clip_lakes_sr.shp"), delete_layer = TRUE) 


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
sf::st_write(barrier.valid, paste0(storage.dir, "/", "barrier_sp.shp"), delete_layer = TRUE) 

# Cleanup intermediate files
rm(rivers.scalerank, rivers.scalerank.buffer, lakes.scalerank, rivers.buffer, barrier, barrier.valid)

# Sanity check: Make a map to see if it all looks correct
plot(dem)
plot(barrier.sp, add=TRUE)


# 6x: Create raster data versions of barriers ----------


# While movecost indicates that it is looking for a spatial lines or poly file for this input,
# I have not had success in getting that to work, however, a raster version does seem to work
# and is an expected input of the underlying process from leastcostpath package.

# Create a blank raster on to which the polys will be rasterized
blank.r <- raster::setValues(dem, NA)

# Destination dataset (raster dataset to be written/burned to)
dst_filename <- paste0("barriers",".tif",sep = "")
# # Export results to folder for use later
terra::writeRaster(blank.r, dst_filename, overwrite = TRUE, format = "GTiff")

barrier.path <- paste0(storage.dir, "/", "barrier_sp.shp")

# Rasterize the dataset using gdal outside of R; bring result back into R
# Note that this is set by at=TRUE to burn all pixels that are touched by the vector
barriers.rastmp <- gdalUtilities::gdal_rasterize(barrier.path, dst_filename, b=1, at=TRUE, a="scalerank")

barrier.rast<- raster::raster(barriers.rastmp)


# 7x: Create Grid of Sample Points ----------


# Make the zeros NAs and make elevation <= -125 == zero as well; 
# this is to adjust the "coastline" per publication notes

dem.2 <- calc(dem, fun=function(x){ x[x == 0] <- NA; return(x)} )
dem.2 <- calc(dem, fun=function(x){ x[x <= -125] <- NA; return(x)} )

plot(dem.2)

# Casting as a spatial raster "spatRaster"
dem.3 <- rast(dem.2)

# Create a regular sampling grid over the dem
sample <- terra::spatSample(dem.3, size = c(100), method="regular", as.points=TRUE, values=TRUE, xy=FALSE, warn=TRUE)

#Sanity check: Make a map to see if it all looks correct
plot(dem.3)
plot(sample, add = TRUE)

sample.f2 <- as(sample, "Spatial")

#Filtering for only points on time-scale adjusted coastal boundaries and above >= -125m
sample.f3 <- sample.f2[which(sample.f2@data$layer >= -125), ]
# Create a bounding box around the samples and buffer it inside and out to create an edge zone
# for selecting points for processing
sample.f3.bbox <- sf::st_as_sfc(st_bbox(sample.f3))
sample.f3.extent <- vect(sample.f3.bbox)
sample.f3.buffer.int <- terra::buffer(sample.f3.extent, -10000)
sample.f4 <- vect(sample.f3)
sample.final.sv <- terra::erase(sample.f4, sample.f3.buffer.int)

# Sanity check: Make a map to see if it all looks correct
plot(dem.3)
plot(sample.f3, add = TRUE)
plot(sample.final.sv, col="green", add=TRUE)
plot(barrier.sp, col="blue", add=TRUE)

# Export results to folder for use later
terra::writeVector(sample.final.sv, "sample_final.shp", filetype="ESRI Shapefile", overwrite=TRUE)

# Cleanup intermediate files
rm(dem, dem.2, sample, sample.f2, sample.f3, sample.f3.bbox, sample.f3.buffer.int, sample.f3.extent, sample.f4)


# 8: Copy created files to the data output file on my Home Directory ----------


#Copied all scratch files over to varinputs, so I don't have to re-run everything
scratch.files <- list.files(scratch.dir)
file.copy(file.path(scratch.dir, scratch.files), data.outputs, overwrite = TRUE)