
#'
#' Example
#' a=c(1,2,3)
#' b=c(3,2,1)
#' c=c(5,6,7)

#' angle3D(c(a[1],a[2],a[3],
#' b[1],b[2],b[3],
#' c[1],c[2],c[3]))


#dist3D <- function(x1, x2, y1, y2, z1, z2 ){
#  sqrt( (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 )
#}
# https://stackoverflow.com/questions/39671579/compute-euclidean-distance-matrix-from-x-y-z-coordinates
fdist <- function(x1,x2,y1=NULL, y2=NULL, z1=NULL, z2=NULL){
  if(is.null(y1)) return(sqrt(sum((x1-x2)^2)))
  if(!is.null(y1)) return(sqrt( (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 ))
}


angle3D=function(x){
  # https://stackoverflow.com/questions/19729831/angle-between-3-points-in-3d-space
  a=x[1:3]
  b=x[4:6]
  c=x[7:9]
  ab = c(b[1] - a[1], b[2] - a[2], b[3] - a[3])
  bc = c(c[1] - b[1], c[2] - b[2], c[3] - b[3])
  abVec = sqrt(ab[1] * ab[1] + ab[2] * ab[2] + ab[3] * ab[3])
  bcVec = sqrt(bc[1] * bc[1] + bc[2] * bc[2] + bc[3] * bc[3])
  abNorm = c(ab[1] / abVec, ab[2] / abVec, ab[3] / abVec)
  bcNorm = c(bc[1] / bcVec, bc[2] / bcVec, bc[3] / bcVec)
  res = abNorm[1] * bcNorm[1] + abNorm[2] * bcNorm[2] + abNorm[3] * bcNorm[3]
  return(acos(res)*180.0/pi)
}
