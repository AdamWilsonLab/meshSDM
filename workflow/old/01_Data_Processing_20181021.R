## Libraries
library(tidyverse)
library(foreach)
library(doParallel)

if(system("hostname",intern = T)=="srv-u10-26.cbls.ccr.buffalo.edu"){
  library(hpc)
  moduleInit()
  module("load lmod/6.0.1 StdEnv intel/15.0 hdf5/1.8.15p1 netcdf python/anaconda R grass nco cdo")
  dyn.load("/util/academic/libpng/1.6.17/lib/libpng16.so.16")
  dyn.load("/util/academic/grass/proj.4-4.9.1/lib/libproj.so")
  library(rgdal)
  dyn.load("/util/academic/grass/gdal-2.2.0/lib/libgdal.so.20")
  library(sf)
}

if(system("hostname",intern = T)!="srv-u10-26.cbls.ccr.buffalo.edu") {
library(rgdal)
library(sf)
library(raster)
}
#library(lidR)

#devtools::install_github("AdamWilsonLab/PointCloudViewer")
proj="+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Overall Workflow
#
# Agisoft
#
#   * Import photos, build 3D model
#   * Classify points
#   * Identify recruits as polygons
#   * Export to txt file of points
#
# CloudCompare
#
#   * Subsample points or use mesh vertices?
#   * Edit -> Scalar Fields -> Export Coordinates to SF
#   * Compute a gaussian smooth (Edit->Scalar Fields -> Gaussian Filter) of each dimension (x, y, and z) using the desired neighborhood (0.01, 0.1, etc.) for EACH dimension and each 'scale' (e.g. 0.0025, 0.01, 0.1, etc.)
#   * Roughness
#   * Surface Density
#   * Compute Illumination (Plugins -> P.C.V.)
#   * Edit -> Normals -> convert to -> Dip Dip Direction (slope and aspect)
#   * Export to an ascii file and import into R.
#
# R
# * Import to R
# * Compute the 3d angle from each point to 1) it's 'smoothed' point and 2) the normal vector for that point.
# * Compute the distance from each point to it's smoothed surface in 3d space
# * If the angle is greater than 90, make the distance negative (a "hole").  If less - it stays positive and is a 'hill'.


# Cloud Compare Command line

## define path to CloudCompare
# cc="/Applications/CloudCompare.app/Contents/MacOS/CloudCompare"
# preamble="-AUTO_SAVE OFF -NO_TIMESTAMP -O "
#
# input="data/old/vertices_cropped.txt"
# #processing=" -COORD_TO_SF X -CURV GAUSS 0.01 -CURV GAUSS 0.1 "
# #processing="-COORD_TO_SF X -GAUSS 0.01"
# processing=paste(c(
#         " -CLEAR_NORMALS -REMOVE_ALL_SFS -OCTREE_NORMALS 0.1 -COORD_TO_SF X -COORD_TO_SF Y -COORD_TO_SF Z ",
#         " -DENSITY 0.1 -TYPE VOLUME -DENSITY 10 -TYPE VOLUME",
#         " -ROUGH 0.01 -CURV GAUSS 0.01 ",
#         " -ROUGH 0.1 -CURV GAUSS 0.1 "),
# collapse=" ")
#
# export="-C_EXPORT_FMT ASC -ADD_HEADER -SEP COMMA -EXT csv  -SAVE_CLOUDS"
#
# cmd=paste(cc,preamble,input,processing,export)
# cmd
#
# system(cmd)
#
#
## Import data


files=data.frame(
  path=list.files("data/20181012/",
                   pattern=".*Cloud_.*txt",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(path),
    quad=sub("_Cloud_.*","",sub("[.]txt","",fname)),
    scale=paste0("s_",sub("^.*_Cloud_","",sub("[.]txt","",fname)))
  )%>%
  dplyr::select(-fname)%>%
  spread(scale, path)

rfiles=data.frame(
  path=list.files("data/20181012/",
                  pattern=".*[OcR|ScR].*shp",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(path),
    quad=sub("_ScR_.*$","",sub("_OcR_.*$","",fname)))




#foreach(i=1:nrow(files)) %dopar%{
f=files[4,]

d=read_csv(f$s_005)%>%slice(-1)

# Subset and rename data
#Select only the variables you want to use in the model.  This just simplifies the data and makes the names shorter. Only the variables included int he below will be kept for analysis.

env=d%>%
  dplyr::select(X="//X",
         Y=Y,
         Z=Z,
         Rf=Rf,
         Gf=Gf,
         Bf=Bf,
         classn=Classification,
         Ny=Ny,
         Nx=Nx,
         Nz=Nz,
         gc_5="Gaussian curvature (0.005)",
         X_smooth_5="Coord. X.smooth(0.005)",
         Y_smooth_5="Coord. Y.smooth(0.005)",
         Z_smooth_5="Coord. Z.smooth(0.005)",
         rough_5="Roughness(0.005)",
         aspect="Dip direction (degrees)",
         density="Surface density (r=0.005)",
         slope="Dip (degrees)")%>%
    mutate(
      pres=0,
      taxa=NA,
      class=case_when(
        classn ==  9   ~ "OcR_Recruit",
        classn ==  11 ~ "ScR_Recruit",
        TRUE                  ~  "other"
      ))

#  Reclass Classificaiton to factor with labels.

# Distance to Smooth
#env$dist_10=dist3D(env$X,env$X_smooth_10,env$Y,env$Y_smooth_10,env$Z,env$Z_smooth_10)
env$dist_5=dist3D(env$X,env$X_smooth_5,env$Y,env$Y_smooth_5,env$Z,env$Z_smooth_5)


# Angle to smooth
# env$angle_10=apply(env[,c("X","Y","Z",
#                 "X_smooth_10",
#                 "Y_smooth_10",
#                 "Z_smooth_10",
#                 "Nx","Ny","Nz")],1,angle3D)

env$angle_5=apply(env[,c("X","Y","Z",
                "X_smooth_5",
                "Y_smooth_5",
                "Z_smooth_5",
                "Nx","Ny","Nz")],1,angle3D)

# Correct Sign of hole
#env$sign_10=ifelse(env$angle_10<90,-1,1)
#env$hole_10=env$dist_10*env$sign_10
#env$gcs_10=env$gc_10*env$sign_10

env$sign_5=ifelse(env$angle_5<90,-1,1)
env$hole_5=env$dist_5*env$sign_5
env$gcs_5=env$gc_5*env$sign_5

## Create spatial object
# Convert `env` from a data.frame to a spatial `sf` object to enable intersection with the recruits.
env=env%>%
  st_as_sf(coords=1:3)%>%
  st_set_crs(proj)

# add coordinates back to data
env[,c("X","Y","Z")]=st_coordinates(env)

## Merge with recruit data

# union points and polygons
datadir="data/20181012//EC_T2_4R/EC_T2_4R_CL_DATA/EC_T2_4R_RECRUITS/"
OcR_plg<-read_sf(datadir,"EcT2_4r_OcR_plg")%>%
  st_set_crs(NA)%>%  # needed due to strange prj of the shapefile
  st_set_crs(proj)%>%  # assign UTM projection
  st_cast("MULTIPOLYGON") # close all the rings to make complete polygons

OcR_pnt<-read_sf(datadir,"EcT2_4r_OcR_pnt")%>%
  st_set_crs(NA)%>%  # needed due to strange prj of the shapefile
  st_set_crs(proj)%>%  # assign UTM projection
  st_buffer(0.001)%>%  # assign UTM projection
  st_cast("MULTIPOLYGON")%>% # close all the rings to make complete polygons
  st_zm(drop=F,what="Z") # add empty Z dimension to merge with polygons

ScR_plg<-read_sf(datadir,"EcT2_4r_ScR_plg")%>%
  st_set_crs(NA)%>%  # needed due to strange prj of the shapefile
  st_set_crs(proj)%>%  # assign UTM projection
  st_cast("MULTIPOLYGON") # close all the rings to make complete polygons

ScR_pnt<-read_sf(datadir,"EcT2_4r_ScR_pnt")%>%
  st_set_crs(NA)%>%  # needed due to strange prj of the shapefile
  st_set_crs(proj)%>%  # assign UTM projection
  st_buffer(0.001)%>%  # assign UTM projection
  st_cast("MULTIPOLYGON")%>% # close all the rings to make complete polygons
  st_zm(drop=F,what="Z") # add empty Z dimension to merge with polygons

OcR=rbind(OcR_plg,OcR_pnt)%>%mutate(FID=1:n())
ScR=rbind(ScR_plg,ScR_pnt)%>%mutate(FID=1:n())

rec=rbind(ScR,OcR)%>%
  rename(taxa=LAYER,genus=NAME)%>%
  mutate(FID=1:n())%>%
  st_buffer(0)


ggplot(rec,aes(color=taxa,fill=genus))+
  geom_sf()

# Merge points and polygons
rec_int=st_intersection(rec,env)%>%
  mutate(pres=1)%>%
  group_by(FID)


#
class_OcR=env%>%filter(class=="OcR_Recruit")
#plot(class_OcR[1])


## Explore point 3d distances to link classified points to vector point
st_bbox3d(class_OcR)
st_bboxbuffer3d(class_OcR,5)
st_crop()
dist=st_distance(class_OcR,env)


tid=st_distance(OcR,class_OcR)%>%
  apply(2,which.min)
class_OcR$rec=OcR$FID[tid]
class_OcR=class_OcR%>%
  mutate(taxa="OcR")

class_ScR=env%>%filter(class=="ScR_Recruit")
tid2=st_distance(ScR,class_ScR)%>%
  apply(2,which.min)
class_ScR$rec=ScR$FID[tid2]
class_ScR=class_ScR%>%
  mutate(taxa="ScR")


box_buf=0.005
rec_pts=rbind(class_OcR, class_ScR)%>%
  group_by(rec,taxa)%>%
  summarize(xmin=min(X),xmax=max(X),ymin=min(Y),ymax=max(Y),zmin=min(Z),zmax=max(Z))%>%
  st_bboxbuffer3d(dist=box_buf)

## Add distanace to points instead of alpha hull

env_pts=foreach(1:nrow(rec_pts),.combine=rbind) %dopar%{
  bbox=rec_pts[i,]
  td=filter(env,
            !class%in%c("ScR_Recruit","OcR_Recruit"),
            between(X,bbox$xmin2,bbox$xmax2),
            between(Y,bbox$ymin2,bbox$ymax2),
            between(Z,bbox$zmin2,bbox$zmax2))
  td2=st_distance(rec_pts[i,],td)%>%
    apply(2,which.min)

  #td2=td%>%summarise_if(is.numeric,median)%>%
  #  st_cast("POINT",do_split=F)%>%
  #  dplyr::select(-FID)
}

ggplot(rec_pts,aes(col=as.factor(taxa)))+
  geom_rect(aes(xmin=xmin2, xmax=xmax2, ymin=ymin2, ymax=ymax2))+
  geom_sf()+
  geom_sf(data=ScR,inherit.aes = F,col="red")+
  geom_sf(data=OcR,inherit.aes = F,col="red")+
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))


foreach(1:nrow())

# Summarize points within each recruit polygon.
# The choice of summary metric is important and needs to be thought through.

obs=rec_int%>%
  summarise_if(is.numeric,median)%>%
  st_cast("POINT",do_split=F)%>%
  dplyr::select(-FID)

 d=bind_rows(
   st_set_geometry(obs,NULL),
   st_set_geometry(env,NULL)
 )

#d=bind_rows(obs,env)



# add an 'id' column to uniquely identify each point.
d$id=1:nrow(d)

# Save Data

save(d,file="data/model_EC_T2_4R.Rdata")
write.csv(d,file="data/model_EC_T2_4R.csv")


ggplot(env,aes(x=slope,density))+
  geom_hex()

