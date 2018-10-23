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
devtools::load_all(".")  # load the CoralSDM package
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


cfiles=data.frame(
  path=list.files("data/20181022/",
                   pattern=".*cloud_.*txt",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(path),
    quad=sub("_cloud_.*","",sub("[.]txt","",fname)),
    scale=paste0("s_",sub("^.*_cloud_","",sub("[.]txt","",fname)))
  )%>%
  dplyr::select(-fname)%>%
  spread(scale, path)

rfiles=data.frame(
  rpath=list.files("data/20181022/",
                  pattern=".*rec.*shp",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(rpath),
    quad=sub("_rec.*$","",fname))%>%
  dplyr::select(-fname)

files=left_join(cfiles,rfiles,by="quad")


#foreach(i=1:nrow(files)) %dopar%{
f=files[2,]

d005=read_csv(f$s_005)%>%slice(-1)
d01=read_csv(f$s_01)%>%slice(-1)
d02=read_csv(f$s_02)%>%slice(-1)

# Subset and rename data
#Select only the variables you want to use in the model.  This just simplifies the data and makes the names shorter. Only the variables included int he below will be kept for analysis.

env=cbind(
    dplyr::select(d005,X="//X",
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
         aspect_5="Dip direction (degrees)",
         density_5="Volume density (r=0.005)",
         slope_5="Dip (degrees)"),
    dplyr::select(d01,
                  gc_10="Gaussian curvature (0.01)",
                  X_smooth_10="Coord. X.smooth(0.01)",
                  Y_smooth_10="Coord. Y.smooth(0.01)",
                  Z_smooth_10="Coord. Z.smooth(0.01)",
                  rough_10="Roughness(0.01)",
                  density_10="Volume density (r=0.01)"),
    dplyr::select(d02,
                  gc_20="Gaussian curvature (0.02)",
                  X_smooth_20="Coord. X.smooth(0.02)",
                  Y_smooth_20="Coord. Y.smooth(0.02)",
                  Z_smooth_20="Coord. Z.smooth(0.02)",
                  rough_20="Roughness(0.02)",
                  density_20="Volume density (r=0.02)"))%>%
    mutate(
      pres=case_when(
        classn %in%c(9,11) ~  1,
        TRUE ~  0),
      taxa=case_when(
        classn ==  9   ~ "ocr",
        classn ==  11 ~ "scr",
        TRUE                  ~  "env"),
      class=case_when(
        classn ==  9   ~ "ocr",
        classn ==  11 ~ "scr",
        TRUE                  ~  "other"
      ))

#  Reclass Classificaiton to factor with labels.

# Distance to Smooth
env$dist_5=dist3D(env$X,env$X_smooth_5,env$Y,env$Y_smooth_5,env$Z,env$Z_smooth_5)
env$dist_10=dist3D(env$X,env$X_smooth_10,env$Y,env$Y_smooth_10,env$Z,env$Z_smooth_10)
env$dist_20=dist3D(env$X,env$X_smooth_20,env$Y,env$Y_smooth_20,env$Z,env$Z_smooth_20)


# Angle to smooth
env$angle_20=apply(env[,c("X","Y","Z",
                          "X_smooth_20",
                          "Y_smooth_20",
                          "Z_smooth_20",
                          "Nx","Ny","Nz")],1,angle3D)

env$angle_10=apply(env[,c("X","Y","Z",
                 "X_smooth_10",
                 "Y_smooth_10",
                 "Z_smooth_10",
                 "Nx","Ny","Nz")],1,angle3D)

env$angle_5=apply(env[,c("X","Y","Z",
                "X_smooth_5",
                "Y_smooth_5",
                "Z_smooth_5",
                "Nx","Ny","Nz")],1,angle3D)

# Correct Sign of hole
env$sign_20=ifelse(env$angle_20<90,-1,1)
env$hole_20=env$dist_20*env$sign_10
env$gcs_20=env$gc_20*env$sign_10

env$sign_10=ifelse(env$angle_10<90,-1,1)
env$hole_10=env$dist_10*env$sign_10
env$gcs_10=env$gc_10*env$sign_10

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

## Save point cloud object

save(env,file=file.path("data",paste0(f$quad,".Rdata")))


##########################################
## Merge with recruit data

# union points and polygons

rec<-read_sf(f$rpath)%>%
  st_set_crs(NA)%>%  # needed due to strange prj of the shapefile
  st_set_crs(proj)%>%  # assign UTM projection
  separate(NAME,into=c("taxa","id"),sep="_")%>%
  mutate(genus=gsub("[0-9]","",id),
         nid=as.numeric(gsub("[a-z]","",id)))

if(F){
ggplot(rec,aes(color=taxa,fill=genus))+
  geom_sf()
ggplot(rec,aes(color=id))+
  geom_sf()
}
# Merge points and polygons
#rec_int=st_intersection(rec,env)%>%
#  mutate(pres=1)%>%
#  group_by(FID)


#
env_rec=env%>%filter(class%in%c("ocr","scr"))

# link markers with dense point cloud
# replace with st_nearest_feature?
tid=st_distance(env_rec,rec)%>%
  apply(1,which.min)
env_rec$rec=rec$id[tid]
env_rec$genus=rec$genus[tid]
env_rec$nid=rec$nid[tid]
env_rec$taxa=rec$taxa[tid]

box_buf=0.01 # bounding box around each recruit's cloud to potentially include in torus


if(F){
  library(rgl)
  ## Extract and explore tori
  obs_torus=env_rec%>%
    group_by(rec,taxa, class)%>%
    summarize(r_n=n(),r_z=diff(range(Z)),r_area=diff(range(X))*diff(range(Y)))%>%  #calculate recruit stats - others?
    group_by(rec,taxa,class, r_n, r_z, r_area)%>%
    do(torus(.,env,dist=0.005,boxdist = box_buf))

    obs_torus$col=factor(obs_torus$type,labels=c("black","red","green"))
    table(obs_torus$rec)
    td=obs_torus#filter(obs_torus,rec=="agar2")
    points3d(as.matrix(td[,c("X","Y","Z")]),color=as.character(td$col))#,col=dist)
  }

obs=env_rec%>%
  group_by(rec,taxa, class)%>%
  summarize(r_n=n(),r_z=diff(range(Z)),r_area=diff(range(X))*diff(range(Y)))%>%  #calculate recruit stats - others?
  group_by(rec,taxa,class, r_n, r_z, r_area)%>%
  do(torus(.,env,dist=0.005,boxdist = box_buf,fun=median)) # Summarize env in the torus

ggplot(env_rec,aes(col=as.factor(taxa)))+
#  geom_rect(aes(xmin=xmin2, xmax=xmax2, ymin=ymin2, ymax=ymax2))+
  geom_sf()+
  geom_sf(data=rec,inherit.aes = F)#+
#  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))


 d=bind_rows(
   st_set_geometry(obs,NULL),
   st_set_geometry(env,NULL)
 )

d=bind_rows(obs,env)



# add an 'id' column to uniquely identify each point.
d$id=1:nrow(d)

# Save Data

save(d,file="data/model_EC_T2_4R.Rdata")
write.csv(d,file="data/model_EC_T2_4R.csv")


ggplot(env,aes(x=slope,density))+
  geom_hex()

