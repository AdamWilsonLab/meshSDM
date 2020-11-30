st_bboxbuffer3d <- function(x, dist){
  if(!"sf"%in%class(x)) stop("x is not an sf object.")
  res=st_coordinates(x)%>%
    apply(2,range)
  if(dim(res)[2]==2)
    stop("Object only has two dimensions, use st_bbox instead")
  if(dim(res)[2]>3)
    stop("Object only too many dimensions")
  buf3d=c(xmin=as.numeric(res[1,"X"])-dist,
          ymin=as.numeric(res[1,"Y"])-dist,
          zmin=as.numeric(res[1,"Z"])-dist,
          xmax=as.numeric(res[2,"X"])+dist,
          ymax=as.numeric(res[2,"Y"])+dist,
          zmax=as.numeric(res[2,"Z"])+dist)
  return(buf3d)
}
