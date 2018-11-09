## Libraries
library(foreach)
library(doParallel)
registerDoParallel(10)
message(paste("################ Using ",getDoParWorkers()," cores..."))

library("tidyverse")
devtools::load_all(".")  # load the CoralSDM package

if(system("hostname",intern = T)=="srv-u10-26.cbls.ccr.buffalo.edu"){
  library(hpc)
  moduleInit()
  module("load lmod/6.0.1 StdEnv intel/15.0 hdf5/1.8.15p1 netcdf python/anaconda grass nco cdo")
  dyn.load("/util/academic/libpng/1.6.17/lib/libpng16.so.16")
  dyn.load("/util/academic/grass/proj.4-4.9.1/lib/libproj.so")
#  library(rgdal)
  dyn.load("/util/academic/grass/gdal-2.2.0/lib/libgdal.so.20")
  library(sf)
}

if(system("hostname",intern = T)!="srv-u10-26.cbls.ccr.buffalo.edu") {
  library(tidyverse)
  library(rgdal)
  library(sf)
  library(raster)
}
