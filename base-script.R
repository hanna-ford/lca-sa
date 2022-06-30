################################################################################
# Data Preparations
# 1. Create input layers: 
# 2. Setup  raster files
# 3. Setup  raster files
#
################################################################################


################################################################################
# Nx: Load required libraries and setup variable ----------
################################################################################

# Install required packages
# commented packages are either installed at cluster level or
# ended up not being used.

library(sp)
library(sf)
library(rgdal) ## will be retired in 2023
library(stars)
library(terra)
library(rgeos)
library(raster)
library(proj4)
library(foreach)
library(movecost)
library(stringr)
library(dplyr)
library(gdalUtilities)
library(ggplot2)

#library(snow)
#library(doSNOW)
#library(parallel)
#library(doParallel)
#library(igraph)
#library(bigstatsr)
#library(itertools)
#library(readxl)
#library(lwgeom)
#library(foreign)

# #### Model 4 output generation - in theory should be the same as the ArcGIS Model.
# myjob <- Sys.getenv('SLURM_JOB_ID')
# jobitr <- as.numeric(Sys.getenv('JOBITR'))
# 
# # comment this out before running in batch; if running single this should indicate which CDL to process (as it would be found in the list model.list.full)
# jobitr <- 10

# Setup variables pointing to various directories
# scratch.dir <- paste0("/scratch/",myjob)
# storage.shapefiles <- paste0("/scrfs/storage/hlford/home/data/shapefiles")
# storage.cdls <- paste0("/scrfs/storage/hlford/home/data/cdls")
# storage.inputs <- paste0("/scrfs/storage/hlford/home/data/varinputs")
# storage.outputs <- paste0("/scrfs/storage/hlford/home/data/results")
# data.outputs <- paste0("/scrfs/storage/hlford/home/data/data_outputs")

storage.inputs.tar <- paste0("C:/Users/hlford/Box/JoshuaRobinson/Zipped Originals")
storage.inputs <- paste0("C:/Users/hlford/Box/JoshuaRobinson/Unzipped files")
scratch.dir <- paste0("C:/Temp/JRobinson/lca-sa")

##### Set the working directory to scratch
setwd(scratch.dir)

# create required directories, if needed
# Check if the folder "Data" exists in the current directory, if not creates it
ifelse(!dir.exists("./tmp/"), dir.create("./tmp/"), "Folder exists already")


# set some of the options that will be used frequently
# including where the tmp directory will be
rasterOptions(format = "GTiff", overwrite = TRUE, tmpdir = paste0(scratch.dir, "/tmp/"),
              timer = TRUE)


################################################################################
# Nx: Load required files and setup lists ----------
################################################################################

#untar the DEM (source: OpenTopography SRTM15+), the catchment area (source: OpenTopography SRTM15+), the pit removal (source: Open Topography SRTM15+)
dem.untar <-  utils::untar(paste0(storage.inputs.tar,"/rasters_SRTM15Plus.tar.gz"), exdir = paste0(scratch.dir, "/tmp"))

#make a raster out of the the tar file; cropping will happen below
dem <- raster::raster(paste0(scratch.dir, "/tmp/output_SRTM15Plus.tif"))

#update the crs on all the files so they are the same
raster::crs(dem)

#set the crs for this project
crs.thisproject <- raster::crs(dem)

#Import the World Clim data (source: )
setwd(storage.inputs)
wc.bio1 <- paste0(storage.inputs, "/lig_30s_bio/lig_30s_bio_1.bil")
wc.bio12 <- paste0(storage.inputs, "/lig_30s_bio/lig_30s_bio_12.bil")
wc.biog1 <- paste0(storage.inputs, "/wc_2_5m_CCSM_21k_bio/2_5m/wc_2_5m_CCSM_21k_bio_1.bil")
wc.biog12 <- paste0(storage.inputs, "/wc_2_5m_CCSM_21k_bio/2_5m/wc_2_5m_CCSM_21k_bio_12.bil")

wc.b1 <- raster::raster(wc.bio1, RAT = FALSE)
wc.b12 <- raster::raster(wc.bio12, RAT = FALSE)
wc.bg1 <- raster::raster(wc.biog1, RAT = FALSE)
wc.bg12 <- raster::raster(wc.biog12, RAT = FALSE)

setwd(scratch.dir)

################################################################################
# Nx: Import study area shapefiles ----------
################################################################################

#Import the Natural Earth 10m Rivers (source: Natural Earth Physical vectors collection)
rivers <-
  sf::st_read(paste0(storage.inputs, "/ne_10m_rivers_lake_centerlines_scale_rank/ne_10m_rivers_lake_centerlines_scale_rank.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)

lakes <-
  sf::st_read(paste0(storage.inputs, "/ne_10m_lakes/ne_10m_lakes.shp")) %>%
  sf::st_transform(., crs.thisproject) %>%
  sf::st_make_valid(.)


################################################################################
# 1x: Clip the Shaprefiles to the raster extent ----------
################################################################################

#Create the cropping extent
new_extent <- extent(1.491667, 60.37917, -38.92917, 3.725)
class(new_extent)

#PREP Rivers
rivers.valid <- rivers[which(!is.na(rivers$scalerank)), ]

#convert to a single geometry
rivers.types <- vapply(sf::st_geometry(rivers.valid), function(x) {
  class(x)[2]
}, "")

unique(rivers.types)

clip.rivers <-
  rivers.valid[grepl("*MULTILINESTRING", rivers.types), ] %>%  #ignore the geometry collections
  sf::st_crop(x = ., y = new_extent) %>%
  sf::st_write(., paste0(scratch.dir, "/", "clip_rivers.shp"), delete_layer = TRUE) 

#Filtering for only rivers with a scale rank of 8, 9 (largest rivers)
rivers.scalerank <- clip.rivers[which(clip.rivers$scalerank == c("8", "9")), ]
rivers.scalerank$rastcode <- 1
sf::st_write(rivers.scalerank, paste0(scratch.dir, "/", "clip_rivers_sr.shp"), delete_layer = TRUE) 


#PREP Lakes
lakes.valid <- lakes[which(!is.na(lakes$scalerank)), ]

#convert to a single geometry
lakes.types <- vapply(sf::st_geometry(lakes.valid), function(x) {
  class(x)[2]
}, "")

unique(lakes.types)

clip.lakes <-
  lakes.valid[grepl("*MULTIPOLYGON", lakes.types), ] %>%  #ignore the geometry collections
  sf::st_crop(x = ., y = new_extent) %>%
  sf::st_write(., paste0(scratch.dir, "/", "clip_lakes.shp"), delete_layer = TRUE) 

lakes.scalerank <- clip.lakes
lakes.scalerank$rastcode <- 1
sf::st_write(lakes.scalerank, paste0(scratch.dir, "/", "clip_lakes_sr.shp"), delete_layer = TRUE) 

#Sanity check: Make a map to see if it all looks correct

plot(dem)
plot(lakes.scalerank, add = TRUE)
plot(rivers.scalerank, add = TRUE)

################################################################################
# 1x: Create raster data versions of the Lakes and Rivers inputs ----------
################################################################################

#create a blank raster on to which the polys will be rasterized
blank.r <- raster::setValues(dem, NA)

#gdal_rasterize
#check for valid installation
# gdalUtilities::gdal_setInstallation()
# valid_install <- !is.null(getOption("gdalUtils_gdalPath"))

################################################################################
# 1x: Rasterize Rivers ----------
################################################################################

#destination dataset (raster dataset to be written/burned to)
dst_filename <- paste0("rivers.cr",".tif",sep = "")
terra::writeRaster(blank.r, dst_filename, overwrite = TRUE, format = "GTiff")

#source dataset (poly data)
clip.rivers.path <- paste0(scratch.dir, "/", "clip_rivers_sr.shp")

#rasterize the dataset using gdal outside of R; bring result back into R
#note that this is set by at=TRUE to burn all pixels that are touched by the vector
rivers.raster <- gdalUtilities::gdal_rasterize(clip.rivers.path, dst_filename, b=1, at=TRUE, a="rastcode")

#make a raster out of the the tar file; cropping will happen below
riversR <- raster::raster(paste0(scratch.dir, "/rivers.cr.tif"))

################################################################################
# 1x: Rasterize Lakes ----------
################################################################################

#destination dataset (raster dataset to be written/burned to)
dst_filename <- paste0("lakes.cr",".tif",sep = "")
terra::writeRaster(blank.r, dst_filename, overwrite = TRUE, format = "GTiff")

#source dataset (poly data)
clip.lakes.path <- paste0(scratch.dir, "/", "clip_lakes_sr.shp")

#rasterize the dataset using gdal outside of R; bring result back into R
#note that this is set by at=TRUE to burn all pixels that are touched by the vector
lakes.raster <- gdalUtilities::gdal_rasterize(clip.lakes.path, dst_filename, b=1, at=TRUE, a="rastcode")

#make a raster out of the the tar file; cropping will happen below
lakeR <- raster::raster(paste0(scratch.dir, "/lakes.cr.tif"))

#set zeros to NA 
lakeR <- calc(lakeR, fun=function(x){ x[x == 0] <- NA; return(x)} )
riversR <- calc(riversR, fun=function(x){ x[x == 0] <- NA; return(x)} )

#Sanity check: Make a map to see if it all looks correct
plot(dem)
plot(lakeR, add = TRUE)
plot(riversR, add = TRUE)



################################################################################
# 1x: Make lakes and rivers un-crossable or un-serviceable ----------
################################################################################






################################################################################
# Nx: Copy created files to the data output file on my Home Directory ----------
################################################################################  

#Copied all scratch files over to varinputs, so I don't have to re-run everything
# scratch.files <- list.files(scratch.dir)
# file.copy(file.path(scratch.dir, scratch.files), data.outputs, overwrite = TRUE)