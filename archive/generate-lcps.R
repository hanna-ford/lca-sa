# Where to run
# This portion of the script can be ran to setup the files on pinnacle portal; this script will
# import the common files that will be needed for the parallelized version to run. 
#
# Data Preparations
# 1. Load required libraries and setup variables
# 2. Load required files and setup lists
# 3. Run the subset of LCPs for this iteration
# 4. Copy created files to the data output file on my Home Directory


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
library(itertools)
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

## Set the working directory to scratch
setwd(scratch.dir)

## create required directories, if needed
## Check if the folder "Data" exists in the current directory, if not creates it
ifelse(!dir.exists("./tmp/"), dir.create("./tmp/"), "Folder exists already")

## set some of the options that will be used frequently
## including where the tmp directory will be
raster::rasterOptions(format = "GTiff", overwrite = TRUE, tmpdir = paste0(scratch.dir, "/tmp/"), timer = TRUE)


## 2: Load required files and setup lists ----------


# Import the cost surface as a transitional layer
cs <- readRDS("cstobler.rds")

# Pick up the coordinate system
crs.thisproject <- terra::crs(cs)

# Import the sample points (with isolated points removed)
points <-
  sf::st_read(paste0(storage.outputs, "/points_final.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)

points.sp <- as(points, "Spatial")


## TEST CASE
#points.random <-sample(points.sp$FID, 3, replace=FALSE)

# Select the points that are in the randome sample for testing;
# the regular sample nature of the points data is so that in testing
# the points that are selected are always neighbors. 
#points.sp.final <- points.sp[c(506, 397, 425), ]


## 3: Run the subset of LCPs for this iteration ----------


#Generate LCPs using the From Everywhere - To Everywhere function

#ptm <- proc.time()

mc.bmc <- leastcostpath::create_FETE_lcps(
  cost_surface = cs, 
  locations = points.sp.final, 
  cost_distance = FALSE,
  parallel = TRUE,
  ncores = 20)
  
#proc.time() - ptm

#sanity check - plot the data so far
plot(raster(cs), col = grey.colors(100))
plot(points.sp.final, add=TRUE, col="red")
plot(mc.bmc, col = "red", add=TRUE)
