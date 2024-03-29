#' Compute 3D bounding box for a sf object
#'
#' @param x an sf object
#' @return a vector representing the 3D bounding box (xmin,ymin,zmin,xmax,ymax,zmax)

st_bbox3d <- function(x){
  if(!"sf"%in%class(x)) stop("x is not an sf object.")
    res=st_coordinates(x)%>%
    apply(2,range)
  if(dim(res)[2]==2)
    stop("Object only has two dimensions, use st_bbox instead")
  if(dim(res)[2]>3)
    stop("Object has more than three dimensions")
    dimnames(res)=NULL
  res3d=c(xmin=res[1,1],ymin=res[1,2],zmin=res[1,3],
    xmax=res[2,1],ymax=res[2,2],zmax=res[2,3])
  return(res3d)
}
