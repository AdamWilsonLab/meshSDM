# Mesh test

library(tidyverse)
library(Morpho)
library(Rvcg)
library(scales)
library(viridis)
devtools::load_all(".")


meshfiles=data.frame(
  mesh_path=list.files(file.path("output/data/quad"),
                       pattern=".rds",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("[.].*$","",fname))%>%
  dplyr::select(-fname)%>%
  as.tbl()


# put predictions back on a particular landscape:
#quad="eut49r"  #choose which quad
#quad="ect14l"  #choose which quad
quad="ect210r"
quad="ect25l"

mesh=meshfiles$mesh_path[meshfiles$quad==quad] %>% readRDS()

#plotmesh(mesh)

mesh %>%
#  meshbase() %>%
  plotmesh(col = mesh$data$slope_10)


mesh %>%
  cutMeshPlane(v1=c(-0.14,-0.2,-0.13),
               v2 = c(0.18,-0.2,-0.07),
               v3 = c(0.01,-0.2,-0.12),
               normal = NULL,
             keep.upper = T) %>%
  cutMeshPlane(v1=c(-0.14,-0.07,-0.13),
               v2 = c(0.18,-0.07,-0.07),
               v3 = c(0.01,-0.07,-0.12),
               normal = NULL,
               keep.upper = F) %>%
#  meshbase() %>%
  plotmesh(col = .$data$slope_10)

mesh %>%
  meshbase() %>%
  vcgWrite()


## get corners of the bounding box
require(Rvcg)
require(rgl)
require(mesheR)
bbox <- getMeshBox(mesh,pca=FALSE)
## get a triangular mesh consisting of 3 triangles cutting a slice from the y-z plane
slice <- BBoxSlices(bbox,axis = 1,percent = 0.7)
myplot <- plot3d(mesh,col="white")
shade3d(slice,col=2,alpha=0.5)


plotmesh(slice)
## now use the slice to get the difference

cuthead <- vtkBooleanOp(mesh,slice,type=2)
## create normals for smooth rendering
cuthead <- vcgUpdateNormals(cuthead)
shade3d(cuthead,col="white")
