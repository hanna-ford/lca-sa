# Where to run
# This portion of the script can be ran to setup the files on pinnacle portal; this script will
# import the common files that will be needed for the parallelized version to run.
#
# Runtime notes: A full set of a single iteration group should take between 50-60 hours to run on himem nodes
#
# Data Preparations
# 1. Load required libraries and setup variables
# 2. Load required files and setup lists
# 3. Run the subset of LCPs for this iteration group
# 4. Copy created files to the data output file on my Home Directory


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
myjob <- Sys.getenv("SLURM_JOB_ID")
jobgroup <- as.numeric(Sys.getenv("JOBGROUP")) # origin
# jobitr <- as.numeric(Sys.getenv('JOBITR')) # destination

# comment this out before running in batch;
# if running single this should indicate which point is being processed
# jobgroup <- 1 # origin
# jobitr <- 397 # destination


# Setup variables pointing to various directories
# These are the locations for Pinnacle - note that before running the script
# the base files will need to be uploaded to Pinnacle.

# ## File locations for Pinnacle
scratch.dir <- paste0("/scratch/", myjob)
scratch.shp.dir <- paste0("/scratch/", myjob, "/shpout")
storage.inputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/varinputs")
storage.outputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/results")
data.outputs <- paste0("/scrfs/storage/hlford/home/data/lca-sa/dataoutputs")
data.outputs.shp <- paste0("/scrfs/storage/hlford/home/data/lca-sa/dataoutputs/outshp")

## Set the working directory to scratch
setwd(scratch.dir)

## create required directories, if needed
## Check if the folder "Data" exists in the current directory, if not creates it
ifelse(!dir.exists("./tmp/"), dir.create("./tmp/"), "Folder exists already")
ifelse(!dir.exists("./shpout/"), dir.create("./shpout/"), "Folder exists already")

## set some of the options that will be used frequently
## including where the tmp directory will be
raster::rasterOptions(format = "GTiff", overwrite = TRUE, tmpdir = paste0(scratch.dir, "/tmp/"), timer = TRUE)


## 2: Load required files and setup lists ----------


# Import the cost surface as a transitional layer
# Cost surface is created using the prep-common-files.R script
cs <- readRDS(paste0(data.outputs, "/cstobler.rds"))

# Pick up the coordinate system
crs.thisproject <- terra::crs(cs)

# Import the sample points (with isolated points removed)
# Points are created using ArcGIS and then checked for isolated points in the prep-common-files.R script
# the result is saved out as shp and then brought back in for use on each run
points <-
  sf::st_read(paste0(storage.outputs, "/points_final.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)

points.sp <- as(points, "Spatial")


## TEST CASE
# points.random <-sample(points.sp$FID, 3, replace=FALSE)

# Select the points that are in the random sample for testing;
# the regular sample nature of the points data is so that in testing
# the points that are selected are always neighbors or near neighbors.
# points.sp.final <- points.sp[c(506, 397, 425), ]


## 3: Run each LCP as a single iteration ----------

cmx <- data.frame(points.sp@data$FID)
cmx$origin <- print(jobgroup)
colnames(cmx)[1] <- "destination"

# Remove the self-referencing row
cmatrix <- cmx[which(cmx$destination != jobgroup), ]


# Generate LCP using the Create LCP function and cost surface from above
# The lcps need to be done in serial or the node runs out of memory
# each iteration dumps the shapefile, runs garbage collection, then moves to the next lcp calculation
# cmatrix.test <- cmatrix[0:15, ]

# List for the loop to iterate over that has the self-referenced row removed
cmxlst <- as.list(cmatrix$destination)

# Sanity check
# for (i in cmxlst) print(i)

for (i in cmxlst) {
  # Run the lcp
  mc.bmc <- leastcostpath::create_lcp(
    cost_surface = cs,
    origin = points.sp[points.sp@data$FID == jobgroup, ],
    destination = points.sp[points.sp@data$FID == i, ],
    directional = TRUE,
    cost_distance = FALSE
  )

  # Prep the shapefule for saving by tagging in the origin and destination
  mc.bmc@data$org <- jobgroup
  mc.bmc@data$dest <- i

  # Convert for saving
  # out.shp <- st_as_sfc(mc.bmc)
  # out.shp$origin <- jobgroup
  # out.shp$destination <- i

  # Using OGR to Save out the result as a shapefile for use later in accumulation surface because st_write was giving me trouble with attributes
  writeOGR(obj = mc.bmc, dsn = scratch.shp.dir, layer = paste0("lcp-", jobgroup, "-", i, ".shp"), driver = "ESRI Shapefile", overwrite_layer = TRUE)

  gc()

  # Report out for tracking in log file
  run <- paste0("lcp-", jobgroup, "-", i, " - DONE")
  print(run)
}


# Copy the individual shapefiles back to storage
scratch.files <- list.files(scratch.shp.dir)
file.copy(file.path(scratch.shp.dir, scratch.files), data.outputs.shp, overwrite = TRUE)
