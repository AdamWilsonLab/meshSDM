
library(tidyverse)
library(Morpho)
library(Rvcg)
library(rgl)
library(scales)
library(viridis)
library(colourvalues)
library(sf)
library(foreach)
library(doParallel)
registerDoParallel(3)
devtools::load_all(".", reset=F)

## Other settings / parameters
proj="+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
dataversion="20190415"  #folder to look for quad data
outputdir=file.path("output/data/quad_scale")

# find all quads
qfiles=data.frame(
  mesh_path=list.files(file.path("rawdata",dataversion),
                       pattern=".*mesh[.]ply",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("_mesh.*","",fname))%>%
  dplyr::select(-fname)

# find all points
cfiles=data.frame(
  point_path=list.files(file.path("rawdata",dataversion),
                        pattern=".*subsampled_.*txt",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(point_path),
    quad=sub("_subsampled_.*","",sub("[.]txt","",fname)),
    scale=paste0("s_",sub("^.*_subsampled_","",sub("[.]txt","",fname)))
  )%>%
  dplyr::select(-fname)


# find all shapefiles
rfiles=data.frame(
  occurrence_path=list.files(file.path("rawdata",dataversion),
                             pattern=".*rec.*shp",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(occurrence_path),
    quad=sub("_rec.*$","",fname))%>%
  dplyr::select(-fname)

# check all quads are in rfiles and cfiles
#all(rfiles$quad%in%cfiles$quad)
#all(cfiles$quad%in%rfiles$quad)
#cfiles$quad[!cfiles$quad%in%rfiles$quad]
#grep("eut135r",rfiles$quad)

files=cfiles%>%
  left_join(rfiles,by="quad")%>%
  left_join(qfiles,by="quad")%>%
  select(quad,scale,point_path,mesh_path,occurrence_path)%>%
  mutate(output_path=file.path(outputdir,paste0(quad,"_",sub("s_","",scale),".rds")))


# point_cloud_files=c("tempdata/ect110r_subsampled_5.txt")
# mesh_file="tempdata/ect110r.ply"
# occurrence_file="tempdata/ect110r_rec.shp"


i=1

loop_output=foreach(i=1:nrow(files),
                    .combine=bind_rows, #rbind the summary table
                    .errorhandling = "remove",  # remove quads with errors
                    .inorder=F,
                    .packages=c("colourvalues")
) %dopar%{

  f=files[i,]
  devtools::load_all(".", reset=F)

  pts=process_point_cloud(
    path=f$point_path,
    id=f$quad,
    scale=f$scale,
    offset=1) # offset to shift coordinates to align points and mesh


  #############################
  ## Import, process mesh
  ## Returns mesh with attached attributes
  ## Currently calculates the median of continuous values and
  ## percentages of categorical values.
  mesh=process_mesh(mesh_file=f$mesh_path,pts,mesh_tol=0.001)


  ##########################################
  ####  Extract data for occurrence points
  ## link occurrence points with the mesh and update the attribute table
  mesh_ocr = process_occurrences(occurrence_file=f$occurrence_path,
                                 mesh=mesh,pts=pts,class="ocr",proj=proj)
  mesh_scr = process_occurrences(f$occurrence_path,
                                 mesh,pts,class="scr",proj=proj)


  ## Add them to the mesh data object
  mesh$data$pres_ocr=mesh_ocr$data$pid
  mesh$data$pres_ocr_torus=mesh_ocr$data$pres_torus

  mesh$data$pres_scr=mesh_scr$data$pid
  mesh$data$pres_scr_torus=mesh_scr$data$pres_torus


  # number of points in each polygon
  if(F) hist(mesh$data$n_points,breaks = 100)

  saveRDS(mesh,file=f$output_path)

  # return a summary table
  return(data.frame(
    quad=f$quad,
    n=nrow(mesh$data)
  ))
}
