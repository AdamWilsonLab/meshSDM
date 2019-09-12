library(tidyverse)
library(Rvcg)
library(plotly)

mesh_file="tempdata/ect110r.ply"
mesh_tol=0.001

tmesh=vcgPlyRead(mesh_file,clean = T)%>%
  vcgSmooth(iteration = 5,type="taubin",lambda = .5,mu=1)%>%
  vcgClean(sel=c(0,1,5,6,7),tol=mesh_tol)


plot_ly(
  x = tmesh$vb[1,], y = tmesh$vb[2,], z = tmesh$vb[3,],
  i = tmesh$it[1,]-1, j = tmesh$it[2,]-1, k = tmesh$it[3,]-1,
  intensity = tmesh$vb[3,],
  type = "mesh3d"
) %>% layout(scene = list(aspectmode = "data"))


#  facecolor = toRGB(viridisLite::inferno(3444)),
#facecolor = toRGB(as.vector(tmesh$vb[3,])),
#  facecolor = toRGB(as.vector(tmesh$material$color)),
