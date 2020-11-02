#' @import plotly
#' @import dplyr
#' @import scales


clean_cloud <- function(file,prefix=NULL){

message(paste("################# Importing data"))

d=read_csv(file,progress = F,col_types = cols())%>%slice(-1)

env=
  select(d,
        X="Coord. X",
        Y="Coord. Y",
        Z="Coord. Z",
        Rf=one_of(c("Rf","R")),
        Gf=one_of(c("Gf","G")),
        Bf=one_of(c("Bf","B")),
        Nx=Nx,
        Ny=Ny,
        Nz=Nz,
        gc=contains("Gaussian curvature"),
        Xs=contains("Coord. X.smooth"),
        Ys=contains("Coord. Y.smooth"),
        Zs=contains("Coord. Z.smooth"),
        rough=contains("Roughness"),
        aspect="Dip direction (degrees)",
        density=contains("Volume density"),
        slope="Dip (degrees)")%>%
#  rowwise() %>%
  mutate(dist=fdist(X,Xs,Y,Ys,Z,Zs))#, #distance to smooth
#         angle = angle3D(X,Y,Z,Xs,Ys,Zs,Nx,Ny,Nz))

message(paste("################# Calculating hole metrics for quad:"))

          # Angle to smooth
env$angle=apply(env[,c("X","Y","Z",
                                    "Xs",
                                    "Ys",
                                    "Zs",
                                    "Nx","Ny","Nz")],1,angle3D)


          # Correct Sign of hole
          # # Clean up with mutate
env=mutate(env,
  sign=ifelse(angle<90,-1,1),
  sign2=-cos(angle*(pi/180)),
  hole=dist*sign,
  gcs=gc*sign,
  gcs_log=log1p(gc)*sign,
  hole2=dist*sign2,
  gcs2=gc*sign2,
  gcs_log2=log1p(gc)*sign2
  )

## add scale names to colnames
if(!is.null(prefix)){

env2=cbind.data.frame(
  select(env, X,Y,Z,Rf,Gf,Bf,Nx,Ny,Nz),
  select(env,-X,-Y,-Z,-Rf,-Gf,-Bf,-Nx,-Ny,-Nz,)%>%
    select_all(.funs = funs(paste0(.,"_",prefix)))
)

}

return(env)

}  #close function


