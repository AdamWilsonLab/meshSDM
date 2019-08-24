# explore a hybrid mesh-point approach
devtools::load_all(".")
library(tidyverse)
library(Morpho)
library(Rvcg)
library(rgl)
library(scales)
library(viridis)
library(colourvalues)
library(sf)
library(foreach)


proj="+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

point_cloud_files=c("tempdata/ect110r_subsampled_5.txt")
point_cloud_file=c("tempdata/ect110r_subsampled_5.txt")

mesh_file="tempdata/ect110r.ply"
occurrence_file="tempdata/ect110r_rec.shp"

#process_all<-function(id,point_cloud_files,mesh_file,point_file){
  pts_long=foreach(point_cloud_file=point_cloud_files,
                   .combine=bind_rows # multiple scales will be rbinded
                   )%do%{
                      pts=process_point_cloud(
                        point_cloud_file,
                        id, #quadrat id
                        scale=5, # scale id
                        offset=1) # offset to shift coordinates to align points and mesh
  }

  # reshape points to wide format
  # (one column for each variable in each scale - one row for each point)

  pts_wide=pts_long%>%
    mutate(variable=paste(variable,scale,sep="_"))%>%
    spread("variable","value")

#############################
## Import, process mesh
## Returns mesh with attached attributes
mesh=process_mesh(mesh_file,pts,mesh_tol=0.001,attach_points=T)

##########################################
####  Extract data for occurrence points
## link occurrence points with the mesh and update the attribute table
mesh2 = process_occurrences(occurrence_file,mesh,class="ocr")


if(F) {
  shade3d_var(mesh,faces_data$pres_torus,palette = "magma")
  shade3d_var(mesh,faces_data$pres)
}


# number of points in each polygon
if(F) hist(mesh$data$n_points,breaks = 100)


save(mesh,file="tempdata/mesh.Rdata")

##########################################
load("tempdata/mesh.Rdata")

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
