plotmesh=function(mesh,color_vec=NULL,...){
require(plotly)
require(tidyverse)

  cramp=colour_ramp(viridis::viridis(100,option="inferno"))
  if(is.null(color_vec))
    facecolor=cramp(rescale(color_vec))

  plot_ly(
    x = mesh$vb[1,], y = mesh$vb[2,], z = mesh$vb[3,],
    i = mesh$it[1,]-1, j = mesh$it[2,]-1, k = mesh$it[3,]-1,
    facecolor = facecolor,
    type = "mesh3d",...
  ) %>% layout(scene = list(aspectmode = "data"))
  }
