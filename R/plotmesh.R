# https://plot.ly/r/reference/#mesh3d
# https://plot.ly/r/3d-mesh/
# https://plot.ly/r/trisurf/
# https://laustep.github.io/stlahblog/posts/plotly_trisurf.html


plotmesh=function(mesh,col=NULL,ramp=NULL, ...){

  require(plotly)
  require(tidyverse)

  # Check if mesh is mesh
  # Check if col is vector same size as mesh
  # check if ramp is a color ramp function

  if(is.null(ramp))
    ramp=colour_ramp(viridis::viridis(100,option="inferno"))

  if(is.null(col))
    col=1:ncol(mesh$it)

  colors=ramp(rescale(col))

  plot_ly(
    x = mesh$vb[1,], y = mesh$vb[2,], z = mesh$vb[3,],
    i = mesh$it[1,]-1, j = mesh$it[2,]-1, k = mesh$it[3,]-1,
    ids=as.character(1:ncol(mesh$it)),
    name="test name",
    facecolor = colors,
    colorscale="YlOrRd",
    type = "mesh3d") %>%
    layout(scene = list(aspectmode = "data"))
}


if(F){

  plotmesh(mesh,col=mesh$data$hole)

}
