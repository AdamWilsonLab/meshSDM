process_point_cloud<-function(path,id,scale=NULL,offset=0){

  pts=read_csv(path)%>%
    slice(-1)%>%
    cleancols()%>%
    mutate(x=x-offset, #use offset to shift coordinates
           y=y-offset,
           z=z-offset,
           class=case_when(
             class ==  2   ~ "ground",
             class ==  3   ~ "coral",
             class ==  4   ~ "octocoral",
             class ==  5   ~ "sponge",
             class ==  6   ~ "rock_igneous",
             class ==  9   ~ "ocr",
             class ==  11 ~ "scr",
             class ==  15 ~ "rock",
             class ==  17 ~ "algae",
             class ==  18 ~ "sand",
             TRUE          ~  "other"
            ),
           x_smooth=x_smooth-offset,
           y_smooth=y_smooth-offset,
           z_smooth=z_smooth-offset,
)

  # add dist metric
  pts$dist=fdist(pts$x,pts$x_smooth, pts$y,pts$y_smooth, pts$z,pts$z_smooth)

  # Angle to smooth
  pts$angle=apply(pts[,c("x","y","z",
                         "x_smooth",
                         "y_smooth",
                         "z_smooth",
                         "Nx","Ny","Nz")],1,angle3D)
  pts$sign= -cos(pts$angle*(pi/180))
  pts$hole=pts$dist*pts$sign
  pts$gcs=pts$gc*pts$sign

    ## update with metadata
  pts$scale=scale
  pts$id=id

#  gather to long format for easier appending
  # pts_long=pts%>%
  #   gather(key="variable",
  #          value = "value",
  #          -x,-y,-z,-r,-g,-b,-fid,-id,-scale,-class)# these columns will not be gathered
  #
  ## rename to add scale to column names
#  if(!is.null(scale))
#    rename_at(pts, c(vars(-c(1:7,grep("class$",colnames(pts))))),
#               function(x) paste("d",x,scale,sep="_"))%>%

return(pts)

}
