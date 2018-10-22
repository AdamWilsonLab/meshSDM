
donut_3d <- function(x, y, dist, boxdist, fun=NULL){
  if(!"sf"%in%class(x)) stop("x is not an sf object.")
  # make the buffered bounding box
  tbbox=st_bboxbuffer3d(x,dist=boxdist)
  # filter the full dataset by the bounding box on x
  yb=filter(y,
            pres==0,
            between(X,tbbox[["xmin"]],tbbox[["xmax"]]),
            between(Y,tbbox[["ymin"]],tbbox[["ymax"]]),
            between(Z,tbbox[["zmin"]],tbbox[["zmax"]]))

  yb$dist=as.numeric(st_distance(x,yb)[,])<=dist

  if(is.null(fun))  return(filter(yb,dist))

if(!is.null(fun)){
    obs=yb%>%
      summarise_if(is.numeric,fun)%>%
      st_cast("POINT",do_split=F)
  return(obs)
    }



  if(F){
  ggplot(yb,aes(x=X,y=Y,col=as.factor(dist)))+geom_point()

    library(rgl)
    viz=as.matrix(st_coordinates(yb)[1:10,])
    dimnames(viz)[[2]]=c("x","y","z")
    points3d(viz,)#,col=dist)
  }
}
