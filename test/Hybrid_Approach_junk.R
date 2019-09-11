## Junk related to hybrid approach

# try kde3d approach
#res=50  # increase to increase resolution

#xyz.dens <- kde3d(xyz[,1],xyz[,2],xyz[,3], n=res)
#mesh <- vcgIsosurface(xyz.dens$d,threshold=10,spacing = 0.0001)
#plot3d(mesh,col="pink")
#shade3d(vcgSmooth(mesh,"HC",iteration=3),col="pink")

# vcgBallPivoting approach
#m1=vcgBallPivoting(as.matrix(xyz),
#                   radius = 0.01, clustering = 0.01,
#                   angle = pi/2,deleteFaces = FALSE)



# get list of faces and vertex indices
#faces_list=vcgVFadj(mesh)


#v1=pts_close$barycoords[,1]
#faces[faces$x[1]==v1,]
#v1[1]%in%faces$x


# build table with vertex indices for each face
#  faces=foreach(i=1:length(faces_list),.combine=bind_rows)%do%
#  data.frame(fid=i,vert=faces_list[[i]])


# for each env point, find the closest barycenter and keep the fid.
#xyz_fid=apply(pts_vb, 1, function(z){
#  which.min(fdist(z[["x"]],faces$x,
#            z[["y"]],faces$y,
#            z[["z"]],faces$z))
#})

pts_close=vcgClostKD(xyz[samp,1:3], mesh2,
                     sign = TRUE,
                     borderchk = TRUE, k = 100)
#pts_vb=as.data.frame(t(pts_close$vb)[,1:3])
#colnames(pts_vb)=c("x","y","z")


nrow(faces)

getFaces(mesh2,2)


# Find closest centroid

pts_dists=vcgKDtree(
  as.matrix(faces[,2:4]),
  as.matrix(pts_vb), k=1,threads = 2)
table(table(pts_dists$index))

# add face fid to point dataset
pts$fid=pts_dists$index



################
ucoords=pts_close$vb%>%
  t()%>%
  as.data.frame()
nrow(ucoords)
nrow(distinct(ucoords))

nfaces(mesh)

pts_close$vb

hist(pts_close$quality)

plot3d(pts_close)

