# Where to run
# This portion of the script can be ran on your local machine to review files after lcp generation. The files will have to be downloaded from Pinnacle before running.
#
#
# Data Preparations
# 1. Load required libraries and setup variables
# 2: Load required files and grab the completed shapefiles
# 3. Create plots pf LCPs for review
# 4. Prep for next step: Rasterization of the LCPs for accumulation raster


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
# library(foreach)
# library(itertools)
# library(doParallel)
library(stringr)
library(dplyr)
library(gdalUtilities)
# library(ggplot2)


# Setup for iteration
# When preparing in an interactive, virtual session the myjob and jobitr variables
#  will need to be set manually unless a slurm job exists for the session.
# myjob <- Sys.getenv("SLURM_JOB_ID")
# jobgroup <- as.numeric(Sys.getenv('JOGROUP')) # origin

# comment this out before running in batch;
# if running single this should indicate which point is being processed
# jobgroup <- 1 # origin



# Setup variables pointing to various directories
# These are the locations for Pinnacle - note that before running the script
# the base files will need to be uploaded to Pinnacle.

# # ## File locations for Pinnacle
# scratch.dir <- paste0("/scratch/", myjob)
# scratch.shp.dir <- paste0("/scratch/", myjob, "/shpout")
# storage.inputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/varinputs")
# storage.outputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/results")
# data.outputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/dataoutputs")
# data.outputs.shp <- paste0("/scrfs/storage/hlford/home/data/lca-sa/dataoutputs/outshp")

## File locations for Teacup
scratch.dir <- paste0("C:/Temp/JRobinson/lca-sa")
storage.inputs <- paste0("C:/Temp/JRobinson/lca-sa/varinputs")
storage.outputs <- paste0("C:/Temp/JRobinson/lca-sa/results")
data.outputs <- paste0("C:/Temp/JRobinson/lca-sa/dataoutputs")
data.outputs.shp <- paste0("C:/Temp/JRobinson/lca-sa/dataoutputs/outshp")

## Set the working directory to scratch
setwd(scratch.dir)

## create required directories, if needed
## Check if the folder "Data" exists in the current directory, if not creates it
ifelse(!dir.exists("./tmp/"), dir.create("./tmp/"), "Folder exists already")
ifelse(!dir.exists("./shpout/"), dir.create("./shpout/"), "Folder exists already")

## set some of the options that will be used frequently
## including where the tmp directory will be
raster::rasterOptions(format = "GTiff", overwrite = TRUE, tmpdir = paste0(scratch.dir, "/tmp/"), timer = TRUE)


## 2: Load required files and grab the completed shapefiles ----------

# Import the cost surface as a transitional layer
# Cost surface is created using the prep-common-files.R script
cs <- readRDS(paste0(data.outputs, "/cstobler.rds"))

# Pick up the coordinate system
crs.thisproject <- terra::crs(cs)

# Test re-import of a single shapefile
completed.runs <-
  sf::st_read(paste0(data.outputs.shp, "/lcp-1-15.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)

# Test re-import of all the shapefiles that have been completed
shpfile.list <- as.list(list.files(data.outputs.shp, pattern = "*.shp.shp", full.names = TRUE))
shpfile.toread <- lapply(shpfile.list, read_sf)
completed.runs.sp <- do.call(rbind, shpfile.toread)


## 3. Create plots pf LCPs for review ----------

# Plot the data so far
plot(raster(cs), col = grey.colors(100))
plot(completed.runs.sp, col = "red", add = TRUE)




