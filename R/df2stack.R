#' Convert a dataframe to a one-dimensional raster stack with
#' bands for each column in the dataframe.
#'
#' @param x A data.frame object
#' @return A raster stack with a layer (band) for each column in the dataset
#' @description This function implements a 'hack' to convert the data.frame associated with a mesh3D object into a 1D raster stack where each column in the raster corresponds to one row of the data.frame.  This is to create the file format required by many SDM modeling functions such as \code{ENMevaluate}.  But be careful, the raster created by this function has no spatial meaning.  Pixels that are adjacent in this raster are not necessarily adjacent in geographic space.  It is simply a restructuring of the data.frame.  So only use the resulting raster in ways that do not involve spatial relationships between pixels.  For example, calculating the slope, focal mean, or other functions on this raster would be meaningless (at best) and misleading (at worst).  If you are going to use this raster with ENMevaluate, please use the custom grouping option (and define your own groups) and not any of the methods that rely on spatial relationships.
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
#' # Visualize the little 1D raster to understand what this is doing.
#' plot(x2)
#' # use as.data.frame to convert back to a data.frame.
#' x3 = as.data.frame(x2)
#' # confirm the converted version is just like the original
#' expect_equal(x,x3)


#' @import raster
#' @import tidyverse

df2stack <- function(x, verbose=T){
  if(!"data.frame"%in%class(x)) stop("x is not an data.frame")

  xl=lapply(X=1:ncol(x),FUN=function(i) {
    raster(t(as.matrix(x[,i])),
           xmn=0+.5,xmx=nrow(x)+.5, # to make y coordinates integers
           ymn=0.5,ymx=1.5) #to make all x coordinates = 1
  })
  xr=stack(xl,quick=T)
  names(xr)=colnames(x)
  metadata(xr)=list(note="Created with mesh_to_raster() function.
                    Warning - raster is a 1 dimensional representation of a 3 dimensional mesh.
                    The coordinate system does not make sense without the mesh geometry.")
if(verbose) warning("Raster objects created with df2stack are a 1 dimensional representation of a data.frame.
                    The coordinate system inside the raster object refers to rows of the original data.frame
                    and not geographic relationships of pixels.  Do not perform any spatial operations
                    (e.g. terrain, focal) on this raster. Convert it back to a data.frame with as.data.frame()")
    return(xr)
}
