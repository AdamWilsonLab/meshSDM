# NOT WORKING - this does nothing.
#pkgload::load_code()
#devtools::load_all()

#mesh= vcgPlyRead("tempdata/ect110r.ply",clean = F, updateNormals = T)
#plotmesh(mesh)

meshbase<-function(mesh, adjust_z=0.01){
  require(Rvcg)
  base_z=min(mesh$vb[3,])-adjust_z

  # get clean edges
  mesh_clean=  mesh %>%  vcgClean(sel=c(6),tol=0.01)
  edge_clean = vcgGetEdge(mesh_clean) %>%
    filter(border==1)
  edge_verts=mesh_clean$vb[,unique(c(edge_clean$vert1,edge_clean$vert2))]

  edge_dists=apply(mesh$vb, 2, function(z){
    min(fdist(z[1],edge_verts[1,],
              z[2],edge_verts[2,],
              z[3],edge_verts[3,]))
  })
  edge_keep <- which(edge_dists<=0.01)  #identify the true edges

  edge = vcgGetEdge(mesh) %>%
    filter(border==1)
  edge_vb=unique(c(edge$vert1,edge$vert2))

  # update edge values to new z value
  mesh$vb[3, edge_vb[edge_vb%in%edge_keep]]=base_z
  return(mesh)
    }
