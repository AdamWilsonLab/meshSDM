#' @import tidyverse

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


torus <- function(x, y, dist, fun=NULL, method="2D"){
  if(!"sf"%in%class(x)) stop("x is not an sf object.")
  # make the buffered bounding box
  tbbox=st_bboxbuffer3d(x,dist=dist)
  # filter the full dataset by the bounding box on x
  yb1=filter(y,
            between(X,tbbox[["xmin"]],tbbox[["xmax"]]),
            between(Y,tbbox[["ymin"]],tbbox[["ymax"]]),
            between(Z,tbbox[["zmin"]],tbbox[["zmax"]]))%>%
    mutate(dist=Inf)

    # don't include recruit points in torus
    yb2=filter(yb1,!class%in%c("scr","ocr"))
#    yb2=yb1#filter(yb1,!class%in%c("scr","ocr"))

  if(method=="3D"){
    ymat <- st_coordinates(yb2)
    xmat <- st_coordinates(x)[,1:3]
     ydist=apply(ymat, 1, function(z){
      min(fdist(z["X"],xmat[,"X"],
                z["Y"],xmat[,"Y"],
                z["Z"],xmat[,"Z"]))
})
     yb2$dist <- ydist<=dist  #identify the torus

 }
if(method=="2D"){
  yb2$dist=as.numeric(st_distance(x,yb2)[,])<=dist  #identify the torus
}

  yb3=filter(yb2,dist)  #keep only the torus

if(is.null(fun))  {
  pts=rbind(
  yb1%>%dplyr::filter(class==x$class[1])%>%
    mutate(type="recruit"),
  yb2%>%dplyr::filter(!dist)%>%mutate(type="background"), #dplyr::select(X,Y,Z)%>%
  yb3%>%mutate(type="torus"))%>% #dplyr::select(X,Y,Z)%>%
  mutate(rec=x$rec)#,taxa=x$taxa,class=x$class)
  return(pts)
}

#  coral,sponge,recruit (scr/ocr)

if(!is.null(fun)){
    ny=nrow(yb3)

        yb3%>%st_set_geometry(NULL)%>%group_by(class)%>%
          summarize(n=n())%>% #count number of points in each class in the torus
          mutate(class=paste0("torus_percent_",class),freq = n / sum(n))%>% #calculate the percent of total torus
          spread(key=class,val=n)

        obs=yb3%>%summarise_if(is.numeric,fun)%>%
          mutate(class=getmode(yb3$class))%>%

  return(obs)
  }


  if(F){
  ggplot(yb3,aes(x=X,y=Y,col=as.factor(class)))+geom_point()

    library(rgl)
    viz=as.matrix(st_coordinates(pts))
    #dimnames(viz)[[2]]=c("x","y","z")
    points3d(viz,color=factor(pts$type,labels=c("red","black","green")))#,col=dist)
  }
}
