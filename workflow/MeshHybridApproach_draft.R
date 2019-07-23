# explore a hybrid mesh-point approach
library(tidyverse)
library(Rvcg)
library(rgl)
library(scales)
library(Morpho)
library(foreach)
library(viridis)
library(colourvalues)
library(sf)

proj="+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


pts=read_table2("data/3d_tile/3DTILES.data/3DTILES_ASCII_subsampled/3DTILES_subsampled_10.txt")%>%
  slice(-1)%>%
  cleancols()%>%
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
  classn ==  2   ~ "ground",
  classn ==  3   ~ "coral",
  classn ==  4   ~ "octocoral",
  classn ==  5   ~ "sponge",
  classn ==  6   ~ "rock_igneous",
  classn ==  9   ~ "ocr",
  classn ==  11 ~ "scr",
  classn ==  15 ~ "rock",
  classn ==  17 ~ "algae",
  classn ==  18 ~ "sand",
  TRUE          ~  "other"
))

### explore adding normals
#pts_mesh=as.mesh3d(pts[,c("x","y","z")])
#pts_mesh2=vcgUpdateNormals(pts_mesh)

#############################
## Import & process mesh
mesh=vcgPlyRead("data/3d_tile/3DTiles.ply",clean = T)

vcgMeshres(mesh)$res

#mesh$vb=mesh$vb+1


#mesh=vcgUpdateNormals(mesh, type = 0, pointcloud = c(10, 0))
#mesh=vcgPlyRead("data/ect110r.ply",clean = T)
mesh2=vcgSmooth(mesh,iteration = 5,type="taubin",lambda = .5,mu=1)%>%
  vcgClean(sel=c(0,1,5,6,7),tol=0.001)

if(F) plot3d(mesh2,type="wire",aspect = F)

### Match points to faces of the mesh
pts_close=vcgClost(x=as.matrix(pts[,c("x","y","z")]),
                   mesh = mesh2,
                    borderchk = TRUE)
pts$fid=pts_close$faceptr

faces=data.frame(
  fid=1:ncol(mesh2$it),
  vcgBary(mesh2))
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
            hole_max=max(hole,na.rm=T),
            sign_median=median(sign,na.rm=T),
            dist_median=median(dist,na.rm=T),
            density_median=median(density),
            coral=any(class=="coral"),
            rock_igneous=any(class=="rock_igneous"),
            n_points=n()
  )

#  join the processed point data to the faces data
faces_data <- left_join(faces,faces_data_all,by="fid")%>%
  arrange(fid)

shade3d_var(mesh2,faces_data$angle_median)
shade3d_var(mesh2,faces_data$Ny)
shade3d_var(mesh2,faces_data$Nx)
shade3d_var(mesh2,faces_data$Nz)


shade3d_var(mesh2,faces_data$rock_igneous)
shade3d_var(mesh2,faces_data$hole_q25)
shade3d_var(mesh2,faces_data$rough_median)
shade3d_var(mesh2,faces_data$rough_max)
shade3d_var(mesh2,faces_data$sign_median)
shade3d_var(mesh2,faces_data$signt_min)
shade3d_var(mesh2,faces_data$dist_median)
shade3d_var(mesh2,faces_data$slope_median)
shade3d_var(mesh2,log(faces_data$n_points))
shade3d_var(mesh2,log(abs(faces_data$gcs_median+1)))

# number of points in each polygon
hist(faces_data$n_points,breaks = 100)

##########################################
####  Extract data for occurrence points
occ=read_sf("data/3d_tile/3DTILES.data/OcTiles.shp")%>%
  st_set_crs(NA)%>%
  st_set_crs(proj)%>%
  mutate(oid=1:n())

### Match points to faces of the mesh

pts_occ=pts%>%filter(class%in%c("ocr"))%>%
  st_as_sf(coords=c("x","y","z"))%>%
  st_set_crs(proj)

dist3d<-function(m1,m2){
  ydist=apply(m1, 1, function(i){
  which.min(fdist(i[1],m2[,1],
            i[2],m2[,2],
            i[3],m2[,3]))
})}

tid=dist3d(st_coordinates(pts_occ),st_coordinates(occ))

pts_occ$oid=occ$oid[tid]
pts_occ$name=occ$NAME[tid]

occ_mesh=vcgClost(x=st_coordinates(pts_occ),
                  mesh = mesh2,
                  borderchk = TRUE)

# add polygon id to pts_occ
pts_occ$fid=occ_mesh$faceptr

# identify polygon with majority of recruit points
pts_fid=pts_occ%>%
  group_by(name,fid)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(name)%>%
  arrange(desc(n))%>%
  slice(1)%>%
  st_set_geometry(NULL)%>%
  select(name,fid)

occ2=left_join(occ,pts_fid,by=c("NAME"="name"))

faces_data$pres=0
faces_data$pres=ifelse(faces_data$fid%in%pts_fid$fid,1,0)

shade3d_var(mesh2,faces_data$pres)


# join the processed point data to the occ data
#occ_data <- left_join(occ,faces_data_all,by="fid")



###########################################
## plots

pts_mesh2_curve=vcgCurve(mesh2)
mesh2=mesh
meshcurve=vcgCurve(mesh2)
meshcurvel=sign(meshcurve$gaussvb)*log(abs(meshcurve$gaussvb)+1)

shade3d_var(mesh2,meshcurvel)

plot3d(mesh2)
plot3d(mesh2,type = "wire")
points3d(faces[,-1],col="blue",size=3)
points3d(st_coordinates(occ),col="blue",size=10)
text3d(st_coordinates(occ),col="blue",texts = occ$NAME,size=10)
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

#plot3d(mesh2)
plot3d(mesh2,type = "wire")
#points3d(faces[,-1],col="blue",size=3)
plot3d(pts_mesh2)
