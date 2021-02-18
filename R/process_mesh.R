#' @import Rvcg
#' @import tidyverse

# potentially smooth/clean. If not desired, simply set mesh=mesh_full
# mesh_tol=0.001 #mesh will be simplified to have an average side length of this value in m

process_mesh<-function(mesh_file,pts,mesh_tol=0.001){

  # read in mesh file
  mesh_full=vcgPlyRead(mesh_file,clean = T,updateNormals = F)

  mesh=vcgSmooth(mesh_full,iteration = 5,type="taubin",lambda = .5,mu=1) %>%
 #   vcgBallPivoting(radius = 1,clustering = 0.2, angle = pi/2, deleteFaces = FALSE) %>%
    vcgClean(sel=c(0,1,6,1),tol=mesh_tol)%>%
    vcgUpdateNormals()


  ### Match points to faces of the mesh
  pts_close=vcgClost(x=as.matrix(pts[,c("x","y","z")]),
                     mesh = mesh,
                     borderchk = TRUE)
  #summary(pts_close$quality) #look at distances
  pts$fid=pts_close$faceptr
  pts$point_border=pts_close$border
  pts$point_dist=pts_close$quality


  faces=data.frame(
    fid=1:ncol(mesh$it),                    # face index
    vcgBary(mesh))%>%                       # add barycenter coordinates for each face
    dplyr::select(fid=fid,x=X1,y=X2,z=X3)%>%        # rename columns
    mutate(
      mesh_border=vcgBorder(mesh)$borderit,
      mesh_curve=vcgCurve(mesh)$gaussitmax
      ) # add other variables from vcg functions

# calculate median point values for each face
  faces_data_all <- pts%>%
    group_by(fid)%>%
    dplyr::select(-x,-y,-z)%>% # don't take median of the coordinates
#    summarize_if(is.numeric,median, na.rm=T)%>%
    summarise(across(is.numeric,median),npts=n()) %>%
    mutate(scale=pts$scale[1],id=pts$id[1]) #add metadata back to dataset

# Fill gaps by taking median of nearby points
# perhaps subset the mesh to only the ones with missing data and then find the closest points again?
# commented section below is not working.
#  nas=na.omit(faces_data_all) %>% attr("na.action") %>% as.numeric()
#
#pts_close2=vcgClost(x=as.matrix(pts[,c("x","y","z")]),
#                   mesh = mesh,
#                   borderchk = TRUE)
#pts$fid=pts_close$faceptr
#pts$point_border=pts_close$border
#ts$point_dist=pts_close$quality

  # generate percentage of points in each class for each face
  # e.g. what % of points in each face were coral?
  faces_data_classes <- pts%>%
    group_by(fid,class)%>%
    summarize(class_percent=n())%>%
    group_by(fid)%>%
    mutate(class_percent=class_percent/sum(class_percent)*100)%>%
    spread(class,class_percent,fill = 0)

# also add a single class for each face from the majority class
#  faces_data_class<-pts %>%
#    group_by(fid)%>%
#  summarize(class=%>%


  #  join the processed point data to the faces data
  #  This faces_data table holds all the environmental data for the mesh
  faces_data <- left_join(faces,faces_data_all,by="fid")%>%
    left_join(faces_data_classes,by="fid")%>%
    arrange(fid)

  # Other metrics

  # Visibility - visible points from above
  # first pick a viewpoint - here I use the middle of x and y
  mesh_centroid=apply(mesh$vb,1,range)%>%
    apply(2,function(x) (x[2]-x[1])/2)+
    c(0,0,2,0)  # then add some distance in z (like a camera above the surface)
  visible_vid=getVisibleVertices(mesh, mesh_centroid[1:3], cores = 1)
  # get vector of T/F whether you can see each face
  faces_data$visible=1:nfaces(mesh)%in%getFaces(mesh,visible_vid)


  # attach data to mesh object
  mesh$data=faces_data

  if(F){

  library(plotly)
  plotmesh(mesh,mesh$data$hole,flatshading=T)
  }


  return(mesh)

}
