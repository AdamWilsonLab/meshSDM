# potentially smooth/clean. If not desired, simply set mesh=mesh_full
# mesh_tol=0.001 #mesh will be simplified to have an average side length of this value in m

process_mesh<-function(mesh_file,pts,mesh_tol=0.001,attach_points=T){
  mesh_full=vcgPlyRead(mesh_file,clean = T)

  mesh=vcgSmooth(mesh_full,iteration = 5,type="taubin",lambda = .5,mu=1)%>%
    vcgClean(sel=c(0,1,5,6,7),tol=mesh_tol)

  ### Match points to faces of the mesh
  pts_close=vcgClost(x=as.matrix(pts[,c("x","y","z")]),
                     mesh = mesh,
                     borderchk = TRUE)
  pts$fid=pts_close$faceptr

  # add id (fid) to uniquely identify each face
  # add 3d centroid of face
  faces=data.frame(
    fid=1:ncol(mesh$it),
    vcgBary(mesh))
  names(faces)=c("fid","x","y","z")

# calculatesummarize
  faces_data_all <- pts%>%
    group_by(fid)%>%
    summarize_if(is.numeric,median)

#  coral=any(class=="coral"),

  # faces_data_all <- pts%>%
  #   group_by(fid)%>%
  #   summarize_all(angle_median=median(angle,na.rm=T),
  #             slope_median=median(slope,na.rm=T),
  #             gcs_median=median(gcs,na.rm=T),
  #             rough_median=median(rough,na.rm=T),
  #             rough_max=max(rough,na.rm=T),
  #             hole_median=median(hole,na.rm=T),
  #             hole_min=min(hole,na.rm=T),
  #             hole_q25=quantile(hole,probs = 0.25,na.rm=T),
  #             sign_median=median(sign,na.rm=T),
  #             dist_median=median(dist,na.rm=T),
  #             density_median=median(density),
  #             coral=any(class=="coral"),
  #             rock_igneous=any(class=="rock_igneous"),
  #             n_points=n()
  #   )

  #  join the processed point data to the faces data
  #  This faces_data table holds all the environmental data for the mesh
  faces_data <- left_join(faces,faces_data_all,by="fid")%>%
    arrange(fid)

  if(attach_points) mesh$data=faces_data

  return(mesh)

}
