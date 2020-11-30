#' Compute 3D distances between two points
#'
#' @param x1 A data.frame object
#' @return Scalar value of Euclidian distance between points
#' @examples
#' a=c(1,2,3)
#' b=c(3,2,1)
#' c=c(5,6,7)
#' angle3D(c(a[1],a[2],a[3],b[1],b[2],b[3],c[1],c[2],c[3]))
#' @references https://stackoverflow.com/questions/39671579/compute-euclidean-distance-matrix-from-x-y-z-coordinates

fdist <- function(x1,x2,y1=NULL, y2=NULL, z1=NULL, z2=NULL){
  if(is.null(y1)) return(sqrt(sum((x1-x2)^2)))
  if(!is.null(y1)) return(sqrt( (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 ))
}
