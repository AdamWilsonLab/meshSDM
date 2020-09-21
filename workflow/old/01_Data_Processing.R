source("workflow/00_setup.R")
proj="+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
library(rgdal)
# Overall Workflow
#
# Agisoft
#
#   * Import photos, build 3D model
#   * Classify points
#   * Identify recruits as polygons
#   * Export to txt file of points
#
# CloudCompare
#
#   * Subsample points or use mesh vertices?
#   * Edit -> Scalar Fields -> Export Coordinates to SF
#   * Compute a gaussian smooth (Edit->Scalar Fields -> Gaussian Filter) of each dimension (x, y, and z) using the desired neighborhood (0.01, 0.1, etc.) for EACH dimension and each 'scale' (e.g. 0.0025, 0.01, 0.1, etc.)
#   * Roughness
#   * Surface Density
#   * Compute Illumination (Plugins -> P.C.V.) -PCV
#   * Edit -> Normals -> convert to -> Dip Dip Direction (slope and aspect)
#   * Export to an ascii file and import into R.
#
# R
# * Import to R
# * Compute the 3d angle from each point to 1) it's 'smoothed' point and 2) the normal vector for that point.
# * Compute the distance from each point to it's smoothed surface in 3d space


# Cloud Compare Command line

## define path to CloudCompare
# cc="/Applications/CloudCompare.app/Contents/MacOS/CloudCompare"
# preamble="-AUTO_SAVE OFF -NO_TIMESTAMP -O "
#
# input="data/old/vertices_cropped.txt"
# input="data/ecr12.txt"

# #processing=" -COORD_TO_SF X -CURV GAUSS 0.01 -CURV GAUSS 0.1 "
# #processing="-COORD_TO_SF X -GAUSS 0.01"
# processing=paste(c(
#         " -CLEAR_NORMALS -REMOVE_ALL_SFS -OCTREE_NORMALS 0.1 -COORD_TO_SF X -COORD_TO_SF Y -COORD_TO_SF Z ",
#         " -DENSITY 0.1 -TYPE VOLUME -DENSITY 10 -TYPE VOLUME",
#         " -ROUGH 0.01 -CURV GAUSS 0.01 ",
#         " -ROUGH 0.1 -CURV GAUSS 0.1 "),
# collapse=" ")
#
# export="-C_EXPORT_FMT ASC -ADD_HEADER -SEP COMMA -EXT csv  -SAVE_CLOUDS"
#
# cmd=paste(cc,preamble,input,processing,export)
# cmd
#
# system(cmd)
#
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)

dataversion="20190415"

cfiles=data.frame(
  path=list.files(file.path("data",dataversion),
                  pattern=".*subsampled_.*txt",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(path),
    quad=sub("_subsampled_.*","",sub("[.]txt","",fname)),
    scale=paste0("s_",sub("^.*_subsampled_","",sub("[.]txt","",fname)))
  )%>%
  dplyr::select(-fname)%>%
  spread(scale, path)  # broken on horae!?!

rfiles=data.frame(
  rpath=list.files(file.path("data",dataversion),
                   pattern=".*rec.*shp",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(rpath),
    quad=sub("_rec.*$","",fname))%>%
  dplyr::select(-fname)

# check all quads are in rfiles and cfiles
#all(rfiles$quad%in%cfiles$quad)
#all(cfiles$quad%in%rfiles$quad)
#cfiles$quad[!cfiles$quad%in%rfiles$quad]
#grep("eut135r",rfiles$quad)

files=left_join(cfiles,rfiles,by="quad")


#files=files%>%
#  na.omit(files)


#files_long=gather(files,scale,file,-quad,-rpath)
#f=files_long[1,]
#res=clean_cloud(file = as.vector(f$file))

proc_report=foreach(i=1:nrow(files),  # parallel loop over quadrats
        .inorder=F,
        .errorhandling=c('pass'),
        .options.multicore=mcoptions) %dopar% {  # start the loop

          if(F) i=1  # If you want to run through this line by line, set i equal to the quadrat you want to test.

          f=files[i,] # selects which quadrat is being processed in this iteration

          message(paste("################# Beginning processing for quad: ",f$quad))

          # define output files to be created at the end of this loop
          outputfile_rdata=file.path("data",dataversion,"processed",paste0(f$quad,".Rdata"))
          outputfile_csv=file.path("data",dataversion,"processed",paste0(f$quad,".csv"))

          # check to see if the output files already exist and skip if already done.
          if(file.exists(outputfile_rdata)&file.exists(outputfile_csv)) {
            message(paste("################# Files exist for quad: ",f$quad, "skipping to the next quad"))
            return(paste(f$quad, "already completed"))
          }

          message(paste("################# Importing data for quad: ",f$quad))

          d05=read_csv(f$s_5,progress = F,col_types = cols())%>%slice(-1)
          d10=read_csv(f$s_10,progress = F,col_types = cols())%>%slice(-1)
          d20=read_csv(f$s_20,progress = F,col_types = cols())%>%slice(-1)
          d50=read_csv(f$s_50,progress = F,col_types = cols())%>%slice(-1)
          d100=read_csv(f$s_100,progress = F,col_types = cols())%>%slice(-1)


          if(F){
          ncol(d05)
          ncol(d10)
          ncol(d20)
          #needs commas - eut19r
          nrow(d05)
          nrow(d10)
          nrow(d20)
}

          # ect39l has different numbers of rows
          # Subset and rename data
          #Select only the variables you want to use in the model.  This just simplifies the data and makes the names shorter. Only the variables included int he below will be kept for analysis.

          env=cbind(
            dplyr::select(d05,X="Coord. X",
                          Y="Coord. Y",
                          Z="Coord. Z",
                          Rf=one_of(c("Rf","R")),
                          Gf=one_of(c("Gf","G")),
                          Bf=one_of(c("Bf","B")),
                          classn=Classification,
                          Ny=Ny,
                          Nx=Nx,
                          Nz=Nz,
                          gc_5="Gaussian curvature (0.005)",
                          X_smooth_5="Coord. X.smooth(0.005)",
                          Y_smooth_5="Coord. Y.smooth(0.005)",
                          Z_smooth_5="Coord. Z.smooth(0.005)",
                          rough_5="Roughness(0.005)",
                          aspect_5="Dip direction (degrees)",
                          density_5="Volume density (r=0.005)",
                          slope_5="Dip (degrees)"),
            dplyr::select(d10,
                          gc_10="Gaussian curvature (0.01)",
                          X_smooth_10="Coord. X.smooth(0.01)",
                          Y_smooth_10="Coord. Y.smooth(0.01)",
                          Z_smooth_10="Coord. Z.smooth(0.01)",
                          rough_10="Roughness(0.01)",
                          density_10="Volume density (r=0.01)",
                          slope_10="Dip (degrees)"),
            dplyr::select(d20,
                          gc_20="Gaussian curvature (0.02)",
                          X_smooth_20="Coord. X.smooth(0.02)",
                          Y_smooth_20="Coord. Y.smooth(0.02)",
                          Z_smooth_20="Coord. Z.smooth(0.02)",
                          rough_20="Roughness(0.02)",
                          density_20="Volume density (r=0.02)",
                          slope_20="Dip (degrees)"),
            dplyr::select(d50,
                          gc_50="Gaussian curvature (0.05)",
                          X_smooth_50="Coord. X.smooth(0.05)",
                          Y_smooth_50="Coord. Y.smooth(0.05)",
                          Z_smooth_50="Coord. Z.smooth(0.05)",
                          rough_50="Roughness(0.05)",
                          density_50="Volume density (r=0.05)",
                          slope_50="Dip (degrees)"),
            dplyr::select(d100,
                          gc_100="Gaussian curvature (0.1)",
                          X_smooth_100="Coord. X.smooth(0.1)",
                          Y_smooth_100="Coord. Y.smooth(0.1)",
                          Z_smooth_100="Coord. Z.smooth(0.1)",
                          rough_100="Roughness(0.1)",
                          density_100="Volume density (r=0.1)",
                          slope_100="Dip (degrees)"))%>%
            mutate(
              #      taxa=case_when(
              #        classn ==  9   ~ "ocr",
              #        classn ==  11 ~ "scr",
              #        TRUE                  ~  "env"),
              class=case_when(
                classn ==  2   ~ "ground",
                classn ==  3   ~ "coral",
                classn ==  4   ~ "octocoral",
                classn ==  5   ~ "sponge",
                classn ==  6   ~ "rock_igneous",
                classn ==  9   ~ "ocr",
                classn ==  11 ~ "scr",
                classn ==  15 ~ "rock",
                classn ==  17 ~ "algae",
                classn ==  18 ~ "sand",
                TRUE          ~  "other"
              ))


          message(paste("################# Calculating hole metrics for quad: ",f$quad))

          # Distance to Smooth
          env$dist_5=fdist( env$X,env$X_smooth_5, env$Y,env$Y_smooth_5, env$Z,env$Z_smooth_5)
          env$dist_10=fdist(env$X,env$X_smooth_10,env$Y,env$Y_smooth_10,env$Z,env$Z_smooth_10)
          env$dist_20=fdist(env$X,env$X_smooth_20,env$Y,env$Y_smooth_20,env$Z,env$Z_smooth_20)
          env$dist_50=fdist(env$X,env$X_smooth_50,env$Y,env$Y_smooth_50,env$Z,env$Z_smooth_50)
          env$dist_100=fdist(env$X,env$X_smooth_100,env$Y,env$Y_smooth_100,env$Z,env$Z_smooth_100)

          # distance to only z smooth
          env$distz_5=fdist( env$X,env$X, env$Y,env$Y, env$Z,env$Z_smooth_5)
          env$distz_10=fdist(env$X,env$X,env$Y,env$Y,env$Z,env$Z_smooth_10)
          env$distz_20=fdist(env$X,env$X,env$Y,env$Y,env$Z,env$Z_smooth_20)
          env$distz_50=fdist(env$X,env$X,env$Y,env$Y,env$Z,env$Z_smooth_50)
          env$distz_100=fdist(env$X,env$X,env$Y,env$Y,env$Z,env$Z_smooth_100)


          # Angle to smooth
          env$angle_100=apply(env[,c("X","Y","Z",
                                    "X_smooth_100",
                                    "Y_smooth_100",
                                    "Z_smooth_100",
                                    "Nx","Ny","Nz")],1,angle3D)

          env$angle_50=apply(env[,c("X","Y","Z",
                                    "X_smooth_50",
                                    "Y_smooth_50",
                                    "Z_smooth_50",
                                    "Nx","Ny","Nz")],1,angle3D)

          env$angle_20=apply(env[,c("X","Y","Z",
                                    "X_smooth_20",
                                    "Y_smooth_20",
                                    "Z_smooth_20",
                                    "Nx","Ny","Nz")],1,angle3D)

          env$angle_10=apply(env[,c("X","Y","Z",
                                    "X_smooth_10",
                                    "Y_smooth_10",
                                    "Z_smooth_10",
                                    "Nx","Ny","Nz")],1,angle3D)

          env$angle_5=apply(env[,c("X","Y","Z",
                                   "X_smooth_5",
                                   "Y_smooth_5",
                                   "Z_smooth_5",
                                   "Nx","Ny","Nz")],1,angle3D)

          # Correct Sign of hole
          # # Clean up with mutate
          env$sign_100= -cos(env$angle_100*(pi/180))#ifelse(env$angle_100<90,-1,1)
          env$hole_100=env$dist_100*env$sign_100
          env$gcs_100=env$gc_100*env$sign_100

          env$sign_50=-cos(env$angle_50*(pi/180)) #ifelse(env$angle_50<90,-1,1)
          env$hole_50=env$dist_50*env$sign_50
          env$gcs_50=env$gc_50*env$sign_50

          env$sign_20=-cos(env$angle_20*(pi/180)) #ifelse(env$angle_20<90,-1,1)
          env$hole_20=env$dist_20*env$sign_20
          env$gcs_20=env$gc_20*env$sign_20

          env$sign_10=-cos(env$angle_10*(pi/180)) #ifelse(env$angle_10<90,-1,1)
          env$hole_10=env$dist_10*env$sign_10
          env$gcs_10=env$gc_10*env$sign_10

          env$sign_5=-cos(env$angle_5*(pi/180)) #ifelse(env$angle_5<90,-1,1)
          env$hole_5=env$dist_5*env$sign_5
          env$gcs_5=env$gc_5*env$sign_5

          ## Create spatial object
          # Convert `env` from a data.frame to a spatial `sf` object to enable intersection with the recruits.
          env=env%>%
            st_as_sf(coords=c("X","Y","Z"))%>%
            st_set_crs(proj)

          # add coordinates back to data
          env[,c("X","Y","Z")]=st_coordinates(env)

          if(F) {
            save(env,file="test.Rdata")  #temporary save for debugging.
            load("test.Rdata")
          }

          ##########################################
          ## Merge with recruit data
          message(paste("################# Importing recruit data for quad: ",f$quad))

          if(is.na(f$rpath)){  # if there are no recruit data, just retun the env data.
            d=obs
          }

          if(!is.na(f$rpath)){ # if recruit shapefile exists, process it.
          # union points and polygons
          #rec1<-read_sf(f$rpath,crs=proj)  # was failing with projection error for quad ect110r
          rec1=st_as_sf(readOGR(f$rpath),crs=proj)

          #
          env_rec=env%>%filter(class%in%c("ocr","scr"))

          rec_bbox=st_bbox(rec1)
          rec_env=st_bbox(env_rec)

          shift=abs(round(rec_bbox-rec_env)[1:2])
          crd <- st_geometry(rec1)+shift  # shift recruit coordinates by 1 to account for "global shift/scale" in cloudcompare

          rec=rec1%>%
            st_set_geometry(NULL)%>%
            separate(NAME,into=c("taxa","id"),sep="_")%>%
            mutate(genus=gsub("[0-9]","",id),
                   nid=as.numeric(gsub("[a-z]","",id)))%>%
            st_set_geometry(crd) %>%
            st_set_crs(proj) # reset geometry


          if(F){
            ggplot(rec,aes(color=taxa,fill=genus))+
              geom_sf()
            ggplot(rec,aes(color=id))+
              geom_sf()
          }



          # link markers with dense point cloud
          # replace with st_nearest_feature?

          tid=st_distance(env_rec,rec)%>%
            apply(1,which.min)
          env_rec$rec=rec$id[tid]
          env_rec$genus=rec$genus[tid]
          env_rec$nid=rec$nid[tid]
          env_rec$taxa=rec$taxa[tid]


          if(F){
             library(plotly)
            ## Extract and explore tori
            obs_torus=env_rec%>%
              group_by(rec,taxa, class)%>%
              summarize(r_n=n(),r_z=diff(range(Z)),r_area=diff(range(X))*diff(range(Y)))%>%  #calculate recruit stats
              group_by(rec,taxa,class, r_n, r_z, r_area)%>%
              do(torus(.,env,dist=0.005))%>%
              filter(type!="background")

#            obs_torus$col=factor(obs_torus$type,labels=c("black","red","green"))
#            table(obs_torus$rec,obs_torus$class)

            ggplot(obs_torus,aes(x=X,y=Y,z=Z,col=as.factor(type)))+
              geom_point()+
              facet_wrap(~rec,scales="free")
#              geom_sf(data=rec,inherit.aes = T)

            for(r in unique(obs_torus$rec)){  #loop over recruits and write out an html interactive graphic
                p <- plot_ly(filter(obs_torus,rec==r), x = ~X, y = ~Y, z = ~Z,
                         color = ~class, colors = c("green","grey","red","blue"),
                 size=I(1))%>%
                 layout(title=paste0(f$quad,"_",r)) %>%
                add_markers()

                plotly_filename=file.path("img/rec",paste0(f$quad,"_",r,".html"))
                htmlwidgets::saveWidget(as_widget(p),
                                        file=file.path(normalizePath(dirname(plotly_filename)),basename(plotly_filename)),
                                        libdir="lib",
                                        title=paste0(f$quad,"_",r))
                }

          }


          # Calculate summary of recruit habitat
          message(paste("################# Calculating summaries for quad: ",f$quad))


          obs=env_rec%>%
            group_by(rec,taxa)%>%
            summarize(r_n=n(),r_z=diff(range(Z)),r_area=diff(range(X))*diff(range(Y)))%>%  #calculate recruit stats - others?
            group_by(rec,taxa, r_n, r_z, r_area)%>%
            # Add filter to remove adjacent recruits?????
            do(torus(.,env,dist=0.005,fun=median))%>% # Summarize env in the torus
            mutate(pres=1)

          Sys.setenv(GDAL_DATA="/usr/share/gdal/gcs.csv")

          if(F){
            ggplot(obs,aes(col=as.factor(taxa)))+
              geom_sf()+
              geom_sf(data=rec,inherit.aes = F)#+
          }

          # d=bind_rows(
          #   st_set_geometry(obs,NULL),
          #   st_set_geometry(env,NULL)
          # )
          d=bind_rows(obs,mutate(env,pres=0))
      } # end if recruit shapefile


          d$quad=f$quad  # add quad id to table

          d$class=as.factor(d$class)
          d$pres=as.logical(d$pres)
          d$taxa=as.factor(d$taxa)


          # add an 'id' column to uniquely identify each point.
          d$id=1:nrow(d)

          ################
          # Save Data
          message(paste("################# Saving data for quad: ",f$quad))

          # reorganize columns for easier opening with cloudcompare.
          d2=d%>%
            ungroup()%>%
            dplyr::select(
              X,Y,Z,Nx,Ny,Nz,
              contains("X"),    contains("Y"),    contains("Z"),
              -Bf,-Gf,-Rf,-r_n,-r_z,-geometry,
              contains("angle"),
              contains("sign"),
              contains("smooth"),
              contains("rough"),
              contains("slope"),
              contains("dist"),
              contains("aspect"),
              contains("gc"),
              r_area,Ny,Nx,Nz) # remove extras

          save(d2,file=outputfile_rdata)
          d2%>%
            dplyr::select(-geometry)%>%
            write_csv(path=outputfile_csv)

          return(data.frame(quad=f$quad,npoints=nrow(d)))
        }


## Summarize proc_report
names(proc_report)=files$quad

f_error=which(sapply(proc_report,function(x) !grepl("completed",x)))

files$quad[f_error]

proc_report[f_error]

#files$quad[c(7,11,16)]
#ggplot(d,aes(x=X,y=Y,col=class))+
#  geom_point()



