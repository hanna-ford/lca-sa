# Where to run
# This portion of the script can be ran to setup the files on pinnacle portal; this script will
# setup the common files that will be needed for the parallelized version to run. My recommendation is
# to run this is an interactive, virtual session for R; but it could also be ran locally and the inputs upload to Pinnacle.
#
# Data Preparations
# 1. Load required libraries and setup variables
# 2. Load required files and setup lists
# 3. Import coast, lake, and river shapefiles for setup - Single serial step
# 4. Clip the coast, lake, and river shapefiles to the raster extent - Single serial step
# 5. Buffer rivers based on scale rank and combine polygons with lakes as "Barrier" - Single serial step
# 6. Create a slope-based cost surface - Single serial step
# 7. Create barrier cost surface based on step 5 and on elevation - Single serial step
# 8. Check sample points against cost surface - Single serial step
# 9. Copy created files to the data output file on my Home Directory


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
library(Matrix)
library(gdistance)
library(leastcostpath) ## movecost is built off this and gdistance, planning to use those directly
library(foreach)
library(doParallel)
library(stringr)
library(dplyr)
library(gdalUtilities)
library(ggplot2)

# Setup for iteration
# When preparing in an interactive, virtual session the myjob and jobitr variables
#  will need to be set manually unless a slurm job exists for the session.
myjob <- Sys.getenv('SLURM_JOB_ID')
# jobitr <- as.numeric(Sys.getenv('JOBITR'))

# comment this out before running in batch; 
# if running single this should indicate which point is being processed
jobitr <- 1

# Setup variables pointing to various directories
# These are the locations for Pinnacle - note that before running the script
# the base files will need to be uploaded to Pinnacle.

scratch.dir <- paste0("/scratch/", myjob)
storage.inputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/varinputs")
storage.outputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/results")
data.outputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/dataoutputs")

# ## File locations for Cupcake
# scratch.dir <- paste0("J:/temp/lca-sa")
# storage.inputs <- paste0("J:/Box Sync/JoshuaRobinson/lca-sa/varinputs")
# storage.outputs <- paste0("J:/Box Sync/JoshuaRobinson/lca-sa/results")
# data.outputs <- paste0("J:/Box Sync/JoshuaRobinson/lca-sa/dataoutputs")

# ## File locations for Teacup
# scratch.dir <- paste0("C:/Temp/JRobinson/lca-sa")
# storage.inputs <- paste0("C:/Users/hlford/Box/JoshuaRobinson/lca-sa/varinputs")
# storage.outputs <- paste0("C:/Users/hlford/Box/JoshuaRobinson/lca-sa/results")
# data.outputs <- paste0("C:/Users/hlford/Box/JoshuaRobinson/lca-sa/dataoutputs")

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
dem.untar <-  utils::untar(paste0(storage.inputs,"/final_rasters_SRTM15Plus.tar.gz"), exdir = paste0(scratch.dir, "/tmp"))

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

points <-
  sf::st_read(paste0(storage.inputs, "/Coast_points.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)


# 4: Clip the river and lakes shapefiles to the raster extent ----------


# Create the cropping extent
# to determine the extent this is from extent(dem) and then the values are entered here
# if a new DEM is introduced with a different extent, then these values would need to be updated.

ext(dem)

new_extent <- extent(9.79583333333352, 42.1833333333336, -35.9583333333335, -12.2500000000001)
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
sf::st_write(rivers.scalerank, paste0(storage.outputs, "/", "clip_rivers_sr.shp"), delete_layer = TRUE) 


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
  sf::st_write(., paste0(storage.outputs, "/", "clip_lakes.shp"), delete_layer = TRUE) 

# Drop the un-needed fields
clip.lakes.clean <- clip.lakes[,-(15:41)]
clip.lakes.clean <- clip.lakes.clean[,-(4:4)]
clip.lakes.clean <- clip.lakes.clean[,-(6:13)]

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
plot(barrier.sp, add=TRUE)


# 6: Create a slope-based cost surface ----------


# cost functions currently implemented within leastcostpath

cfs <- c("tobler", "tobler offpath", "irmischer-clarke male", 
         "irmischer-clarke offpath male", "irmischer-clarke female", 
         "irmischer-clarke offpath female","modified tobler", 
         "wheeled transport", "herzog", "llobera-sluckin", "campbell 2019")

# neighbors can be 4, 8, 16, 32, or 48. A greater number of neighbors will result in cost surface and LCP approximating reality better - but be aware that above 8 there is the possibility to "jump" barriers.

neigh <- 16

slope_cs <- leastcostpath::create_slope_cs(dem = raster(dem), cost_function = "tobler", neighbours = neigh)

plot(raster(slope_cs), col = grey.colors(100))
plot(barrier.sp, add=TRUE)

gc()

# 7: Create barrier cost surface based on step 5 and on elevation ----------


# Create a blank raster on to which the river and lake barrier polys will be rasterized
blank.r <- raster::setValues(dem, NA)

# Destination dataset (raster dataset to be written/burned to)
dst_filename <- paste0("barriers",".tif",sep = "")
# # Export results to folder for use later
terra::writeRaster(blank.r, dst_filename, overwrite = TRUE)

barrier.path <- paste0(storage.outputs, "/", "barrier_sp.shp")

# Rasterize the dataset using gdal outside of R; bring result back into R
# Note that this is set by at=TRUE to burn all pixels that are touched by the vector
barriers.rastmp <- gdalUtilities::gdal_rasterize(barrier.path, dst_filename, b=1, at=TRUE, a="scalerank")

barrier.rast<- raster::raster(barriers.rastmp)

plot(barrier.rast)

# Create a barrier of altitude values less than or equal to -150
# this is to adjust the "coastline" per publication notes
# The altitude limitation can be changed, but should be tested if changing

altitude <- dem <= -150
altitude[altitude == 0] <- NA

plot(altitude)

altitude.raster <- raster(altitude)

# the values NOT NA will be assigned the field argument value. If [the field arg] is 0 (default) then movement within the area will be completely prohibited
# natural_cs adopts the river and lake barriers
natural_cs <- leastcostpath::create_barrier_cs(raster = barrier.rast, barrier = barrier.rast, neighbours = neigh, field = 0, background = 1)

# altitude_cs adopts the altitude barriers
altitude_cs <- leastcostpath::create_barrier_cs(raster = altitude.raster, barrier = altitude.raster, neighbours = neigh, field = 0, background = 1)

plot(raster(altitude_cs))
plot(raster(natural_cs))

# multiplying the two cost surfaces ensures that barriers continue to completely inhibit movement (i.e. slope_cs values * 0 = 0)
pre_cs <- natural_cs * altitude_cs
slope_altitude_cs <- slope_cs * pre_cs

plot(raster(slope_altitude_cs), col = grey.colors(100))

# Export the transition layer to RDS so that it can be called into the lcp generation
saveRDS(slope_altitude_cs, "cstobler.rds")

# Read the saved transition layer back into R
cs <- readRDS("cstobler.rds")

#force garbage collection
gc()


# 8: Check sample points against cost surface ----------

points.sp <- as(points, "Spatial")

# Check the locations against the cost surface to identify points that will not be able to create an LCP (outliers/points off mainland)
isolated.pts <- check_locations(slope_altitude_cs, points.sp)

# turn the vector of isolated points into a data frame
isolated.pts.df <- data.frame(isolated.pts)

# add a column to the df that has the negative value assigned
isolated.pts.df$rm <- as.integer(isolated.pts.df$isolated.pts * -1)

print(isolated.pts.df$rm)

# Remove the isolated points from the data
points.sp.final <- points.sp[c(-4, -39, -141), ]

plot(raster(cs), col = grey.colors(100))
plot(points.sp.final, add=TRUE)

save <- st_as_sfc(points.sp.final)

sf::st_write(save, paste0(storage.outputs, "/", "points_final.shp"), delete_layer = TRUE) 

# 9: Copy created files to the data output file on my Home Directory ----------

#Copied all scratch files over to varinputs, so I don't have to re-run everything
scratch.files <- list.files(scratch.dir)
file.copy(file.path(scratch.dir, scratch.files), data.outputs, overwrite = TRUE)