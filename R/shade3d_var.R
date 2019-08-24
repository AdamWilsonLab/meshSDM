shade3d_var<-function(mesh,data,clear_rgl=T,palette = "inferno"){

  if(nfaces(mesh)!=length(data)) stop(paste(
    "Mesh has ",nfaces(mesh)," faces but ",length(data)," data points provided."))
  if(clear_rgl) rgl.clear()
  x=pretty(seq(min(data,na.rm=T),max(data,na.rm=T),len=10))
  cols=colourvalues::colour_values(x,palette = palette)
  mesh$material$color=colourvalues::colour_values(data,palette = palette)
#  shade3d(mesh, meshColor = "faces",aspect=1)
  plot3d(mesh, meshColor = "faces",aspect=F)
  legend3d("topright", col=cols, legend=x, cex=1,pch=16)
}
