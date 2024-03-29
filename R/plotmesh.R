#' Use plotly library to plot a 3d mesh object
#' @param x a mesh object
#' @return Makes a plotly plot
#' @import plotly
#' @import tidyverse
#' @examples   plotmesh(mesh)
#' plotmesh(mesh,col=mesh$data$hole)
#' @references  https://plot.ly/r/reference/#mesh3d, https://plot.ly/r/3d-mesh/ https://plot.ly/r/trisurf/ https://laustep.github.io/stlahblog/posts/plotly_trisurf.html

plotmesh=function(mesh,col=NULL,ramp=NULL,title=NULL, ...){

  # todo:
  # Check if mesh is mesh
  # Check if col is vector same size as mesh
  # check if ramp is a color ramp function

  # extract coordinates
  x <- mesh$vb[1,]
  y <- mesh$vb[2,]
  z <- mesh$vb[3,]

  # set up color ramp
  if(is.null(ramp))
    ramp=colour_ramp(viridis::viridis(100,option="inferno"))

  # get default colors if col is not provided
  if(is.null(col)){
    m <- matrix(c(x,y,z), ncol=3, dimnames=list(NULL,c("x","y","z")))
    col <- apply(t(mesh$it),MARGIN=1,function(row){mean(m[row,3])})
  }

  colors=ramp(rescale(col))
  # val_range=range(col,na.rm=T)
  # vals=(1/(val_range[2]-val_range[1]))* (col-val_range[1])
  #
  # color_key=data.frame(val=seq(val_range[1],val_range[2],len=10),
  #                      val_scaled=seq(0,1,len=10)) %>%
  #   mutate(color=ramp(val_scaled),
  #          x=mean(mesh$vb[1,]),
  #          y=mean(mesh$vb[2,]),
  #          z=mean(mesh$vb[3,]))
  #
  # colors=ramp(vals)

  plot_ly(
    x = x, y = y, z = z,
    i = mesh$it[1,]-1, j = mesh$it[2,]-1, k = mesh$it[3,]-1,
#    ids=as.character(1:ncol(mesh$it)),
#    name="test name",
#    text=paste("Value=",round(col,2)),
#    hoverinfo = 'text',
    type = "mesh3d",
    facecolor = colors,
  #  colorbar=list(
  #    title=ifelse(is.null(title),"",title))
    ...) %>%
    layout(scene = list(aspectmode = "data"))

  # td=cbind.data.frame(val=round(col,2),colors) %>%
  #   unique() %>% arrange(val)
  #
  #
  #   plot_ly(
  #   x = color_key$x, y = color_key$y, z = color_key$z,
  #   marker = list(
  #     autocolorscale = F,
  #     color = color_key$val_scaled,
  #     colorbar = "middle",
  #     colorscale="YlOrRd",
  #   line = list(width = 0),
  #   opacity = 0.9,
  #   size = 1,
  #   symbol = "circle",
  #   showscale=TRUE),
  #   opacity = 0.5,
  #   type="scatter3d",
  #   mode= "markers"
  # )


  }

