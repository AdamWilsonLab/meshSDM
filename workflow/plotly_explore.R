require(plotly)
require(tidyverse)
library(Rvcg)
library(morpho)

mesh1=vcgCone(0.5,0.5,h=1)
mesh2=vcgCone(1,1,h=1)


  plot_ly(
    x = mesh1$vb[1,], y = mesh1$vb[2,], z = mesh1$vb[3,],
    i = mesh1$it[1,]-1, j = mesh1$it[2,]-1, k = mesh1$it[3,]-1,
    intensity = mesh$vb[3,],
    type = "mesh3d",
  )%>% layout(scene = list(aspectmode = "data"))


  plot_ly(x=~mesh2$vb[1,], y = ~mesh2$vb[2,], z = ~mesh2$vb[3,], type = 'scatter3d', mode = 'lines',
          opacity = 1, line = list(width = 6, reverscale = FALSE))

