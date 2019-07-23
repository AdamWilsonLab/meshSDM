shade3d_var<-function(mesh,data,clear_rgl=T){
  if(clear_rgl) rgl.clear()
  x=pretty(seq(min(data,na.rm=T),max(data,na.rm=T),len=10))
  cols=colourvalues::colour_values(x)
  mesh$material$color=colourvalues::colour_values(data)
#  shade3d(mesh, meshColor = "faces",aspect=1)
  plot3d(mesh, meshColor = "faces",aspect=F)
  legend3d("topright", col=cols, legend=x, cex=1,pch=16)
}
