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
library(rgdal)
#library(stars)
library(terra)
library(rgeos)
library(raster)
library(proj4)
library(foreach)
library(movecost)
library(stringr)
library(dplyr)
library(gdalUtilities)

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
scratch.dir <- paste0("C:/Users/hlford/Box/JoshuaRobinson/lca-sa")

##### Set the working directory to scratch
setwd(scratch.dir)

# create required directories, if needed
# Check if the folder "Data" exists in the current directory, if not creates it
ifelse(!dir.exists(".//tmp//"), dir.create(".//tmp//"), "Folder exists already")

# check that they were created
dir.exists(".//tmp//")

# set some of the options that will be used frequently
# including where the tmp directory will be
rasterOptions(format = "GTiff", overwrite = TRUE, tmpdir = paste0(scratch.dir, "//tmp//"),
              timer = TRUE)

tmpDir(create=TRUE)

################################################################################
# Nx: Load required files and setup lists ----------
################################################################################

#untar the DEM (source: OpenTopography SRTM15+), the catchment area (source: OpenTopography SRTM15+), the pit removal (source: Open Topography SRTM15+)
dem.untar <- untar(paste0(storage.inputs.tar,"/rasters_SRTM15Plus.tar.gz"), list = TRUE)
cat.untar <- untar(paste0(storage.inputs.tar,"/Dinfarea.tar.gz"), list = TRUE)
pit.untar <- untar(paste0(storage.inputs.tar,"/pitRemove.tar.gz"), list = TRUE)

#make a raster out of the the tifs; cropping will happen below
dem <- raster::raster(paste0(dem.untar), RAT = TRUE)
#cat <- raster::raster(paste0(cat.untar), RAT = TRUE) ## is not wanting to be a raster
#pit <- raster::raster(paste0(pit.untar), RAT = TRUE)  ## is not wanting to be a raster

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
# 1x: Import, define, and clip PADUS, TRANS, and ELECT shapefiles ----------
################################################################################
#get the various required shapefiles; first as a list; then bring them in and name them
varinputs.list.full <- as.list(list.files(paste0(storage.inputs), pattern = "*.shp$", full.names = FALSE, recursive = FALSE))

#define the name
padus.poly <- paste0(storage.inputs, "/", varinputs.list.full[6])
trans.poly <- paste0(storage.inputs, "/", varinputs.list.full[7])
elect.poly <- paste0(storage.inputs, "/", varinputs.list.full[5])


#PREP PADUS
padus <-
  sf::st_read(padus.poly) %>%
  sf::st_transform(., crs.thisproject) %>%
  lwgeom::st_make_valid(.)

padus.valid <- padus[which(!is.na(padus$Category)), ]
padus.valid$rastcode <- 1

#convert to a single geometry
padus.types <- vapply(sf::st_geometry(padus.valid), function(x) {
  class(x)[2]
}, "")

unique(padus.types)

clip.padus <-
  padus.valid[grepl("*MULTIPOLYGON", padus.types), ] %>%  #ignore the geometry collections
  dplyr::filter(sf::st_intersects(x = ., y = this.study.area, sparse = FALSE)) %>%
  sf::st_write(., paste0(scratch.dir, "/", "clip_padus.shp"), delete_layer = TRUE)


#PREP TRANS
trans <-
  rgdal::readOGR(dsn = storage.inputs, layer = "Trans_RoadSegment") %>%
  #trans <- sf::st_read(trans.poly) #sf will not read in the shapefile, it will crash R
  #no rastcode needed, using the MTFCC Code already in table
  sp::spTransform(., crs.thisproject)

clip.trans <-
  st_as_sf(trans) %>%
  filter(sf::st_intersects(x = ., y = this.study.area, sparse = FALSE)) %>%
  sf::st_write(., paste0(scratch.dir, "/", "clip_trans.shp"), delete_layer = TRUE)


#PREP ELECT
elect <-
  sf::st_read(elect.poly) %>%
  sf::st_transform(., crs.thisproject) %>%
  lwgeom::st_make_valid(.)

elect.valid <- elect[which(elect$STATUS == "IN SERVICE"), ]
elect.valid$rastcode <- 1

clip.elect <-
  elect.valid %>%
  filter(sf::st_intersects(x = ., y = this.study.area, sparse = FALSE)) %>%
  sf::st_write(., paste0(scratch.dir, "/", "clip_elect.shp"), delete_layer = TRUE)

################################################################################
# Nx: Setup SSURGO and CDL raster datasets, create RATs ----------
################################################################################
#get raster attribute tables for the cdl and ssurgo datasets
#ssurgo
ssurgo.rat.list <- as.list(list.files(paste0(storage.inputs), pattern = "*.dbf$", full.names = FALSE, recursive = FALSE))

sdbf.file <- paste0(storage.inputs, "/", ssurgo.rat.list[[6]])
ssurgo.dbf <- foreign::read.dbf(sdbf.file)

s.rat <- levels(ssurgo)[[1]]
s.rat$nccpi <- as.data.frame(cbind("ID" = ssurgo.dbf$Value, "nccpi" = ssurgo.dbf$nccpi, "nccpicorn" = ssurgo.dbf$nccpics, "nccpismgr" = ssurgo.dbf$nccpism, "nccpicot" = ssurgo.dbf$nccpicot, "nccpisoy" = ssurgo.dbf$nccpisoy))

levels(ssurgo) <- s.rat

beginCluster()

nccpi.corn <- clusterR(ssurgo, deratify, args = list(att = "nccpicorn", count = TRUE), filename = "nccpicorn")
nccpi.smgr <- clusterR(ssurgo, deratify, args = list(att = "nccpismgr", count = TRUE), filename = "nccpismgr")
nccpi.cot <- clusterR(ssurgo, deratify, args = list(att = "nccpicot", count = TRUE), filename = "nccpicot")
nccpi.soy <- clusterR(ssurgo, deratify, args = list(att = "nccpisoy", count = TRUE), filename = "nccpisoy")

endCluster()


#cdl
cdbf.file <- paste0(storage.cdls, "/", cdl.name, ".tif.vat.dbf")
cdl.dbf <- foreign::read.dbf(cdbf.file)
cdl.dbf$clname <- as.character(cdl.dbf$Class_Name)

cdl.rat <- vector(mode = "list", length = 1)

c.rat <- levels(cdl)

c.rat$cdl_value <- as.data.frame(cbind("ID" = cdl.dbf$Value, "CDL Value" = cdl.dbf$Value, "CDL Classname" = cdl.dbf$clname))

levels(cdl) <- c.rat  

################################################################################
# Nx: Clip CDL to study areas ----------
################################################################################

#the data are for CONUS, so first it must be cropped to the study area
#this is where M1 where stop
thisSA.cdl <- raster::crop(cdl, this.study.area, snap = "near", filename = paste0(cdl.year, "_thisSA_cdl"))
thogSA.cdl <- raster::crop(cdl, thog.study.area, snap = "near", filename = paste0(cdl.year, "_thogSA_cdl"))

################################################################################
# 1x: Clip SSURGO to study areas ----------
################################################################################

thisSA.nccpi.corn <- raster::crop(nccpi.corn, this.study.area, snap = "near", filename = "thisSA_nccpicorn", overwrite = TRUE)
thisSA.nccpi.smgr <- raster::crop(nccpi.smgr, this.study.area, snap = "near", filename = "thisSA_nccpismgr", overwrite = TRUE)
thisSA.nccpi.cot <- raster::crop(nccpi.cot, this.study.area, snap = "near", filename = "thisSA_nccpicot", overwrite = TRUE)
thisSA.nccpi.soy <- raster::crop(nccpi.soy, this.study.area, snap = "near", filename = "thisSA_nccpisoy", overwrite = TRUE)

thogSA.nccpi.corn <- raster::crop(nccpi.corn, thog.study.area, snap = "near", filename = "thogSA_nccpicorn", overwrite = TRUE)
thogSA.nccpi.smgr <- raster::crop(nccpi.smgr, thog.study.area, snap = "near", filename = "thogSA_nccpismgr", overwrite = TRUE)
thogSA.nccpi.cot <- raster::crop(nccpi.cot, thog.study.area, snap = "near", filename = "thogSA_nccpicot", overwrite = TRUE)
thogSA.nccpi.soy <- raster::crop(nccpi.soy, thog.study.area, snap = "near", filename = "thogSA_nccpisoy", overwrite = TRUE)


################################################################################
# 1x: Create raster data versions of the PADUS, TRANS, ELECT inputs ----------
################################################################################

#create a blank raster on to which the polys will be rasterized
blank.r <- raster::setValues(thisSA.cdl, NA)

#gdal_rasterize
#check for valid installation
gdalUtils::gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))

################################################################################
# 1x: Rasterize PADUS ----------
################################################################################

#destination dataset (raster dataset to be written/burned to)
dst_filename <- paste0("thisSA.padus",".tif",sep = "")
raster::writeRaster(blank.r, dst_filename, overwrite = TRUE, format = "GTiff")

#source dataset (poly data)
path.padus.clip <- paste0(scratch.dir, "/", "clip_padus.shp")

#rasterize the dataset using gdal outside of R; bring result back into R
thisSA.padus <- gdalUtils::gdal_rasterize(path.padus.clip , dst_filename, b=1, at=TRUE, a="rastcode", l="clip_padus", verbose=TRUE, output_Raster=TRUE)

################################################################################
# 1x: Rasterize TRANS ----------
################################################################################

#destination dataset (raster dataset to be written/burned to)
dst_filename <- paste0("thisSA.trans",".tif",sep = "")
raster::writeRaster(blank.r, dst_filename, overwrite = TRUE, format = "GTiff")

#source dataset (poly data)
path.trans.clip <- paste0(scratch.dir, "/", "clip_trans.shp")

#rasterize the dataset using gdal outside of R; bring result back into R
thisSA.trans <- gdalUtils::gdal_rasterize(path.trans.clip , dst_filename, b=1, at=TRUE, a="MTFCC_CODE", l="clip_trans", verbose=TRUE, output_Raster=TRUE)

################################################################################
# 1x: Rasterize ELECT ----------
################################################################################

#destination dataset (raster dataset to be written/burned to)
dst_filename <- paste0("thisSA.elect",".tif",sep = "")
raster::writeRaster(blank.r, dst_filename, overwrite = TRUE, format = "GTiff")

#source dataset (poly data)
path.elect.clip <- paste0(scratch.dir, "/", "clip_elect.shp")

#rasterize the dataset using gdal outside of R; bring result back into R
thisSA.elect <- gdalUtils::gdal_rasterize(path.elect.clip , dst_filename, b=1, at=TRUE, a="rastcode", l="clip_elect", verbose=TRUE, output_Raster=TRUE)

################################################################################
# Nx: Copy created files to the data output file on my Home Directory ----------
################################################################################  

#Copied all scratch files over to varinputs, so I don't have to re-run everything
scratch.files <- list.files(scratch.dir)
file.copy(file.path(scratch.dir, scratch.files), data.outputs, overwrite = TRUE)