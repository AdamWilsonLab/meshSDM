process_occurrences<-function(occurrence_file,mesh,pts,class="ocr",proj){

  # If occurrence file is missing, return all 0s
  if(is.null(occurrence_file)) {
    mesh$data$pres_adj=0
    mesh$data$pres_torus=0
    return(mesh)
  }


  occ=read_sf(occurrence_file)%>%
  filter(grepl(class,NAME))%>%
  st_set_crs(NA)%>%
  st_set_crs(proj)%>%
  mutate(oid=1:n())#%>%

# If no records of this class, add all zeros and return
  if(nrow(occ)==0) {
    mesh$data$pres_adj=0
     return(mesh)
  }
### Match points to faces of the mesh
pts_occ=pts[pts$class==as.character(class),]%>%
  st_as_sf(coords=c("x","y","z"))%>%
  st_set_crs(proj)

## Find the point in shapefile closest to each classified point in point cloud
tid=dist3d(st_coordinates(pts_occ),st_coordinates(occ))

pts_occ$oid=occ$oid[tid]
pts_occ$name=occ$NAME[tid]

occ_mesh=vcgClost(x=st_coordinates(pts_occ),
                  mesh = mesh,
                  borderchk = TRUE)

# add polygon id to pts_occ
pts_occ$fid=occ_mesh$faceptr

# count recruit point in each polygon
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
# pts_fid_max=pts_fid_all%>%
#   ungroup()%>%
#   group_by(name)%>%
#   arrange(desc(n))%>%
#   slice(1)
#
# # Method 1:
# faces_data$pres_max=ifelse(faces_data$fid%in%pts_fid_max$fid,1,0)

# Method 2
## Find single face closest to the median of the recruit torus
# 1: first create a data.frame with all the variables to include in the median calculation
#    This needs to have one column called "fid" with the face id, and the rest of the columns
#    will be used for the median.
tdata=mesh$data%>%
select(
  -starts_with("pres"))%>%
  #        -starts_with("x"),
  #        -starts_with("y"),
  #        -starts_with("z"),
  #        -starts_with(c("N")))%>%
  na.omit()


# 2: Now calculate the median and return the fid of the face closest to that median for each recruit
pts_fid_torus <- pts_fid_all%>%
  group_by(name)%>% # group by name to do calculation for each recruit
  do(mesh_adj(fids=.$fid,mesh=mesh,type=c("border")))  #identify border faces that make up the torus (ring of faces around the recruit)

# add these fids to the data table.
mesh$data$pres_torus=ifelse(mesh$data$fid%in%pts_fid_torus$fid,1,0)

pts_fid_adj=pts_fid_torus%>%
  do(mesh_median(.$fid, data=tdata, return=c("index"),nstp = 2000)) # find median face in border

# add these fids to the attribute table.
mesh$data$pres_adj=ifelse(mesh$data$fid%in%pts_fid_adj$fid,1,0)

return(mesh)

}



dist3d<-function(m1,m2){
  ydist=apply(m1, 1, function(i){
    which.min(fdist(i[1],m2[,1],
                    i[2],m2[,2],
                    i[3],m2[,3]))
  })}

