##########################################

## Build file list


# find all shapefiles
files=data.frame(
  mesh_path=list.files(file.path("rawdata",dataversion,"output"),
                             pattern=".rds",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("[.]*$","",fname))%>%
  dplyr::select(-fname)



# visualize sampled quads

mesh=readRDS("rawdata/20190415/output/ect27l_100.rds")
plotmesh(mesh,mesh$data$mesh_curve)


## plot classification
class=as.vector(unlist(apply(mesh$data[,c("coral","ground","octocoral","other","rock","sand","scr","ocr","sponge")],1,which.max)))

plotmesh(mesh,class)

#plotmesh(mesh,visible)

