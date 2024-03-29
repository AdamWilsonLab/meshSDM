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


proj="+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


#pts=read_csv("tempdata/ect110r_subsampled_5.txt")%>%
pts=read_table2("tempdata/3d_tile/3DTILES.data/3DTILES_ASCII_subsampled/3DTILES_subsampled_5.txt")%>%
  slice(-1)%>%
  dplyr::select(x="\\\\X",
                y="Y",
                z="Z",
                r=one_of(c("Rf","R")),
                g=one_of(c("Gf","G")),
                b=one_of(c("Bf","B")),
                class=Classification,
                x_smooth=contains("Coord._X.smooth"),
                y_smooth=contains("Coord._Y.smooth"),
                z_smooth=contains("Coord._Z.smooth"),
                Ny=Ny,
                Nx=Nx,
                Nz=Nz,
                gc=contains("Gaussian_curvature"),
                rough=contains("Roughness"),
                aspect=contains("Dip_direction_(degrees)"),
                density=contains("Volume density"),
                slope="Dip_(degrees)") %>%
    mutate(x=x-1,
         y=y-1,
         z=z-1,
         x_smooth=x_smooth-1,
         y_smooth=y_smooth-1,
         z_smooth=z_smooth-1)
colnames(pts)

pts$dist=fdist(pts$x,pts$x_smooth, pts$y,pts$y_smooth, pts$z,pts$z_smooth)

# Angle to smooth
pts$angle=apply(pts[,c("x","y","z",
                           "x_smooth",
                           "y_smooth",
                           "z_smooth",
                           "Nx","Ny","Nz")],1,angle3D)
pts$sign= -cos(pts$angle*(pi/180))
pts$signt= pts$angle<90
pts$hole=pts$dist*pts$sign
pts$gcs=pts$gc*pts$sign

pts=mutate(pts,class=case_when(
  class ==  2   ~ "ground",
  class ==  3   ~ "coral",
  class ==  4   ~ "octocoral",
  class ==  5   ~ "sponge",
  class ==  6   ~ "rock_igneous",
  class ==  9   ~ "ocr",
  class ==  11 ~ "scr",
  class ==  15 ~ "rock",
  class ==  17 ~ "algae",
  class ==  18 ~ "sand",
  TRUE          ~  "other"
))

### explore adding normals
#pts_mesh=as.mesh3d(pts[,c("x","y","z")])
#pts_mesh=vcgUpdateNormals(pts_mesh)

#############################
## Import & process mesh
mesh_full=vcgPlyRead("tempdata/3d_tile/3DTiles.ply",clean = T)
mesh_full=vcgPlyRead("tempdata/ect110r.ply",clean = T)

# potentially smooth/clean. If not desired, simply set mesh=mesh_full
mesh_side=0.001 #mesh will be simplified to have an average side length of this value in m.

mesh=vcgSmooth(mesh_full,iteration = 5,type="taubin",lambda = .5,mu=1)%>%
  vcgClean(sel=c(0,1,5,6,7),tol=mesh_side)

## summary stats
vcgMeshres(mesh)$res


if(F) plot3d(mesh,type="wire",aspect = F)

### Match points to faces of the mesh
pts_close=vcgClost(x=as.matrix(pts[,c("x","y","z")]),
                   mesh = mesh,
                    borderchk = TRUE)
pts$fid=pts_close$faceptr

faces=data.frame(
  fid=1:ncol(mesh$it),
  vcgBary(mesh))
names(faces)=c("fid","x","y","z")

faces_data_all <- pts%>%
  group_by(fid)%>%
  summarize(angle_median=median(angle,na.rm=T),
            slope_median=median(slope,na.rm=T),
            gcs_median=median(gcs,na.rm=T),
            rough_median=median(rough,na.rm=T),
            rough_max=max(rough,na.rm=T),
            hole_median=median(hole,na.rm=T),
            hole_min=min(hole,na.rm=T),
            hole_q25=quantile(hole,probs = 0.25,na.rm=T),
            sign_median=median(sign,na.rm=T),
            dist_median=median(dist,na.rm=T),
            density_median=median(density),
            coral=any(class=="coral"),
            rock_igneous=any(class=="rock_igneous"),
            n_points=n()
  )

#  join the processed point data to the faces data
#  This faces_data table holds all the environmental data for the mesh
faces_data <- left_join(faces,faces_data_all,by="fid")%>%
  arrange(fid)


  plotmesh(mesh,faces_data$hole_10, # the column to use to color mesh
    title="Hole")

dim(faces_data)
ncol(mesh$it)

if(F){
shade3d_var(mesh,faces_data$rock_igneous)
shade3d_var(mesh,faces_data$hole_median)
shade3d_var(mesh,faces_data$hole_q25)
shade3d_var(mesh,faces_data$rough_median)
shade3d_var(mesh,faces_data$rough_max)
shade3d_var(mesh,faces_data$angle_median)
shade3d_var(mesh,faces_data$sign_median)
shade3d_var(mesh,faces_data$dist_median)
shade3d_var(mesh,faces_data$slope_median)
shade3d_var(mesh,log(abs(faces_data$gcs_median+1)))
}


# number of points in each polygon
if(F) hist(faces_data$n_points,breaks = 100)

##########################################
####  Extract data for occurrence points
occ=read_sf("tempdata/ect110r_rec.shp")%>%
  filter(grepl("ocr",NAME))%>%
#  occ=read_sf("tempdata/3d_tile/3DTILES.data/OcTiles.shp")%>%
  st_set_crs(NA)%>%
  st_set_crs(proj)%>%
  mutate(oid=1:n())#%>%
  #filter(NAME%in%paste0("test",c(1,2,9,10,4,5,6,11,14,15,17,18,19,20,21))) # keep only hole-dwellers if desired

### Match points to faces of the mesh
pts_occ=pts%>%filter(classn%in%c("ocr"))%>%
  st_as_sf(coords=c("x","y","z"))%>%
  st_set_crs(proj)

dist3d<-function(m1,m2){
  ydist=apply(m1, 1, function(i){
  which.min(fdist(i[1],m2[,1],
            i[2],m2[,2],
            i[3],m2[,3]))
})}

tid=dist3d(st_coordinates(pts_occ),st_coordinates(occ))

# Version below works without special dist function, update to use this instead.
# dist3d2<-function(m1,m2){
#   ydist=apply(m1, 1, function(i){
#     which.min(as.matrix(dist(rbind(i,m2)))[-1,1])
#   })}
#
# tid2=dist3d2(st_coordinates(pts_occ),st_coordinates(occ))
#cbind(tid,tid2)

pts_occ$oid=occ$oid[tid]
pts_occ$name=occ$NAME[tid]

occ_mesh=vcgClost(x=st_coordinates(pts_occ),
                  mesh = mesh,
                  borderchk = TRUE)

# add polygon id to pts_occ
pts_occ$fid=occ_mesh$faceptr

# identify polygons with at least one recruit point
pts_fid_all=pts_occ%>%
  group_by(name,fid)%>%
  summarize(n=n())%>%
  st_set_geometry(NULL)


####  Assing single 'face' for for each occurrence
####
####  two methods:
####  1) take triangle with most points OR
####  2) take donut around recruit

# identify polygon with majority of recruit points
pts_fid_max=pts_fid_all%>%
  ungroup()%>%
  group_by(name)%>%
  arrange(desc(n))%>%
  slice(1)

# Method 1:
faces_data$pres_max=ifelse(faces_data$fid%in%pts_fid_max$fid,1,0)

# Method 2
## Find single face closest to the median of the recruit torus
# 1: first create a data.frame with all the variables to include in the median calculation
#    This needs to have one column called "fid" with the face id, and the rest of the columns
#    will be used for the median.
tdata=faces_data%>%
  select(fid,angle_median,slope_median,gcs_median,
         rough_median,rough_max,hole_median,
         hole_min,hole_q25,sign_median,dist_median)
# 2: Now calculate the median and return the fid of the face closest to that median for each recruit
pts_fid_torus <- pts_fid_all%>%
  group_by(name)%>% # group by name to do calculation for each recruit
    do(mesh_adj(fids=.$fid,mesh=mesh,type=c("border")))  #identify border faces
# add these fids to the faces_data table.
faces_data$pres_torus=ifelse(faces_data$fid%in%pts_fid_torus$fid,1,0)

pts_fid_adj=pts_fid_torus%>%
    do(mesh_median(.$fid, data=tdata, return=c("index"))) # find median face in border

# add these fids to the faces_data table.
faces_data$pres_adj=ifelse(faces_data$fid%in%pts_fid_adj$fid,1,0)

if(F) {
  shade3d_var(mesh,faces_data$pres_torus,palette = "magma")
  shade3d_var(mesh,faces_data$pres)
}

##########################################
##########################################
## Write out data object
mesh$data=faces_data
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
