
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
registerDoParallel(30)
devtools::load_all(".", reset=F)

## Other settings / parameters
proj="+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
dataversion="20190415"  #folder to look for quad data
outputdir=file.path("rawdata",dataversion,"output")

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
# point_cloud_file=c("tempdata/ect110r_subsampled_5.txt")
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
  mesh=process_mesh(mesh_file=f$mesh_path,pts,mesh_tol=0.001)

  ##########################################
  ####  Extract data for occurrence points
  ## link occurrence points with the mesh and update the attribute table
  mesh_ocr = process_occurrences(f$occurrence_path,mesh,pts,class="ocr",proj=proj)
  mesh_scr = process_occurrences(f$occurrence_path,mesh,pts,class="scr",proj=proj)


  mesh$data$pres_ocr=mesh_ocr$data$pres_adj
  mesh$data$pres_scr=mesh_scr$data$pres_adj


  if(F) {
    plotmesh(mesh,mesh$data$coral)
    plotmesh(mesh,mesh$data$pres_ocr)
  }


  # number of points in each polygon
  if(F) hist(mesh$data$n_points,breaks = 100)



  saveRDS(mesh,file=f$output_path)
}




##########################################

mesh=readRDS("rawdata/20190415/output/eut27l_10.rds")
plot_mesh(mesh,mesh$data$pres_torus,palette = "magma")


## Maxent
library(maxnet)
n_background=10000

model_data=bind_rows(
  filter(mesh$data,pres_adj==1),  # keep all presences
  filter(mesh$data,pres_adj==0)%>%
    na.omit()%>%sample_n(n_background) # use n_background samples
)

m1=maxnet(p=model_data$pres_adj,
          data=model_data%>%
            select(angle_median,
                   sign_median,
                   rough_median,
                   dist_median,
                   #                slope_median, # include desired variables here
                   hole_q25
            ))

plot(m1,type="logistic")

# put predictions back on landscape:
# predict will remove all NA values making it hard to reconnect to the mesh
# so do it first here:
predict_data=na.omit(mesh$data)
# then do prediction
m1_predict=predict(m1,newdata=predict_data,type="logistic",clamp=T)
# then join back with faces_data to be sure everything lines up)
predict_data2=left_join(mesh$data,cbind(predict_data,m1pred=m1_predict),by="fid")

if(F) {
  shade3d_var(mesh,predict_data2$m1pred, palette="magma")
  #  points3d(st_coordinates(pts_occ),col="blue",size=10) #classified points
  points3d(mesh$data[mesh$data$pres_adj==1,c("x","y","z")],col="red",size=10) #matched face for each
}

if(F) {
  #fading from points to mesh
  xlim=c(-.1,.1)
  ylim=c(-.2,-.3)
  plot3d(mesh,type="shade",alpha=rescale_mid(mesh$vb[1,],c(0,1),mid=.01),
         xlim=xlim,ylim=ylim,aspect=F,smooth=F,forceClipRegion=T,add=F)
  wire3d(mesh,type="wire",alpha=rescale_mid(mesh$vb[1,],c(0,1),mid=.8),
         aspect=F,smooth=F,add=T)
  #  points3d(st_coordinates(pts_occ),col="blue",size=10) #classified points
  points3d(pts[,c("x","y","z")],size=.05,col="darkblue",
           alpha=rescale_mid(pts$x,c(1,0),mid=.01),add=T)   #points3d(mesh$data[,c("x","y","z")],size=.1) #matched face for each

  summary_table=data.frame(
    quad=f$quad,
    points=nrow(mesh$data),
    occurrences=sum(mesh$data$pres_adj),
    n_faces=ncol(mesh$vb))

  print(summary_table)

  return(summary_table)

}

###########################################
## plots

# gaussian curvature of the mesh
meshcurve=vcgCurve(mesh)
meshcurvel=sign(meshcurve$gaussvb)*log(abs(meshcurve$gaussvb)+1)
shade3d_var(mesh,meshcurvel)


######
# visible points from above
# might be useful to illustrate/quantify how much is lost if you did 2.5D
# e.g. you can estimate how much of the suitable habitat isn't visible from above

# first pick a viewpoint - here I use the middle of x and y
mesh_centroid=apply(mesh$vb,1,range)%>%
  apply(2,function(x) (x[2]-x[1])/2)

visible_vid=getVisibleVertices(mesh, mesh_centroid[1:3]+c(0,0,1), offset = 0.01, cores = 1)
# get vector of T/F whether you can see each face
visible=1:nfaces(mesh)%in%getFaces(mesh,visible_vid)

shade3d_var(mesh,ifelse(visible,1,0))

ggplot(faces_data,aes(y=m1pred))


plot3d(mesh)
plot3d(mesh,type = "wire")
points3d(st_coordinates(occ),col="blue",size=10)
text3d(st_coordinates(occ),col="red",texts = sub("test","",occ$NAME),size=10)
points3d(pts[,1:3],col=colourvalues::colour_values(pts$hole))
points3d(st_coordinates(pts_occ),col=as.numeric(as.factor(pts_occ$name)))


##########################
## Summaries
vcgArea(mesh)/(0.25*.25)

faces_data%>%
  gather(key=param,value = val,-fid,-x,-y,-z,-pres)%>%
  ggplot(aes(x=val,group=pres,col=as.factor(pres)))+
  geom_density()+
  facet_wrap(~param,scales="free")

#plot3d(mesh)
plot3d(mesh,type = "wire")
#points3d(faces[,-1],col="blue",size=3)
plot3d(pts_mesh)
