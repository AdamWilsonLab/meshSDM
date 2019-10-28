# NOT WORKING - this does nothing.

meshbase<-function(mesh, height=0.1){

  bb=meshcube(mesh)
  bb=vcgBox(mesh)

  mesh2=mergeMeshes(mesh,bb)

  plotmesh(mesh2)

}


library(mesheR)
data(humface)
bboxmesh <-  getMeshBox(humface,tri=TRUE) %>%
  meshOffset(offset=c(-40))

edge=vcgGetEdge(humface)


plotmesh(mergeMeshes(humface,bboxmesh))

outside <- outsideBBox(humface,bbox)
humshrink <- rmVertex(humface,outside)

c1=cropOutsideBBox(humface,bboxmesh)

plotmesh(c1)
