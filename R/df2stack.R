#' Convert a dataframe to a one-dimensional raster stack with
#' bands for each column in the dataframe.
#'
#' @param x A data.frame object
#' @return A raster stack with a layer for each column in the dataset
#' @examples
#' n=5
#' # Create a test data.frame to illustrate the function
#' x=data.frame(x=1:n,
#'             y=rep(1:2,each=2,len=n),
#'             pres=rbinom(n,size=1,p=0.1),
#'             v1=rnorm(n),
#'             v2=rnorm(n),
#'             v3=rnorm(n))
#' # Convert to a raster stack
#' x2=df2stack(x)
#' # show the coordinates of the raster
#' coordinates(x2)
#' plot(x2)
#' @import raster
#' @import tidyverse

df2stack <- function(x){
  if(!"data.frame"%in%class(x)) stop("x is not an data.frame")

  xl=lapply(X=1:ncol(x),FUN=function(i) {
    raster(t(as.matrix(x[,i])),
           xmn=0+.5,xmx=nrow(x)+.5, # to make y coordinates integers
           ymn=0.5,ymx=1.5) #to make all x coordinates = 1
  })
  xr=stack(xl)
  names(xr)=colnames(x)
  metadata(xr)=list(source="Created with mesh_to_raster() function. Warning - raster is a 1 dimensional representation of a 3 dimensional mesh. The coordinate system does not make sense and needs to be joined with the original mesh dataframe for any spatial operations including plotting.")
  return(xr)
}

