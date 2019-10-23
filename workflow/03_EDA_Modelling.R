library(tidyverse)
library(Morpho)
library(Rvcg)
library(scales)
library(viridis)

### load the combined data
datal<-readRDS("output/datalong.rds")
dataw<-readRDS("output/datawide.rds")


############
datal %>%
  filter(var%in%c("rough","hole","gcs","aspect","slope","visible")) %>%
  ggplot(aes(x=as.factor(pres_ocr),y=value))+
  geom_violin()+
  facet_grid(var~scale,scales="free")


datal %>%
  filter(var%in%c("rough","hole","gcs","aspect","slope","visible")) %>%
  ggplot(aes(y=value,x=quad))+
  geom_violin()+
  facet_wrap(~var,scales="free")

# visualize sampled quads
#mesh=readRDS("rawdata/20190415/output/ect27l_100.rds")
#plotmesh(mesh,mesh$data$mesh_curve)


## plot classification
#class=as.vector(unlist(apply(mesh$data[,c("coral","ground","octocoral","other","rock","sand","scr","ocr","sponge")],1,which.max)))
#plotmesh(mesh,class)

#plotmesh(mesh,visible)

# percent missing by variable

summary_nas <- datal%>%group_by(var,scale)%>%summarize(n=n(),na=sum(is.na(value)),p=na/n)


##############################################################
## Maxent
library(maxnet)

# number of background values to use (total across all quads)
# could modify to have some number in each quad to ensure it's balanced
n_background=10000

model_vars=c("aspect_10","hole_5","hole_10","hole_100","rough_5","rough_10")
pres_vars=c("pres_scr","pres_ocr")

model_data=bind_rows(
  filter(dataw,pres_ocr==1),  # keep all presences
  filter(dataw,pres_ocr==0)%>%
    na.omit()%>%
    sample_n(n_background))%>% # use n_background samples
select(one_of(c(pres_vars,model_vars)))

# fit maxent
m1=maxnet(p=model_data$pres_ocr,data=select(model_data,model_vars))

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

