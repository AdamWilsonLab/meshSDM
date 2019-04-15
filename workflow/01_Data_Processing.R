source("workflow/00_setup.R")
proj="+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

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
# * If the angle is greater than 90, make the distance negative (a "hole").  If less - it stays positive and is a 'hill'.


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

dataversion="20190128"

cfiles=data.frame(
  path=list.files(file.path("data",dataversion),
                  pattern=".*cloud_.*txt",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(path),
    quad=sub("_cloud_.*","",sub("[.]txt","",fname)),
    scale=paste0("s_",sub("^.*_cloud_","",sub("[.]txt","",fname)))
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

files=left_join(cfiles,rfiles,by="quad")

files=files%>%
  select(-s_5)%>%
  na.omit(files)

foreach(i=1:nrow(files),  # parallel loop over quadrats
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

          d005=read_csv(f$s_005,progress = F,col_types = cols())%>%slice(-1)
          d01=read_csv(f$s_01,progress = F,col_types = cols())%>%slice(-1)
          d02=read_csv(f$s_02,progress = F,col_types = cols())%>%slice(-1)


          if(F){
          ncol(d005)
          ncol(d01)
          ncol(d02)
          #needs commas - eut19r
          nrow(d005)
          nrow(d01)
          nrow(d02)
}

          # ect39l has different numbers of rows
          # Subset and rename data
          #Select only the variables you want to use in the model.  This just simplifies the data and makes the names shorter. Only the variables included int he below will be kept for analysis.

          env=cbind(
            dplyr::select(d005,X="Coord. X",
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
            dplyr::select(d01,
                          gc_10="Gaussian curvature (0.01)",
                          X_smooth_10="Coord. X.smooth(0.01)",
                          Y_smooth_10="Coord. Y.smooth(0.01)",
                          Z_smooth_10="Coord. Z.smooth(0.01)",
                          rough_10="Roughness(0.01)",
                          density_10="Volume density (r=0.01)",
                          slope_10="Dip (degrees)"),
            dplyr::select(d02,
                          gc_20="Gaussian curvature (0.02)",
                          X_smooth_20="Coord. X.smooth(0.02)",
                          Y_smooth_20="Coord. Y.smooth(0.02)",
                          Z_smooth_20="Coord. Z.smooth(0.02)",
                          rough_20="Roughness(0.02)",
                          density_20="Volume density (r=0.02)",
                          slope_20="Dip (degrees)"))%>%
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


          # Angle to smooth
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
          env$sign_20=ifelse(env$angle_20<90,-1,1)
          env$hole_20=env$dist_20*env$sign_20
          env$gcs_20=env$gc_20*env$sign_20

          env$sign_10=ifelse(env$angle_10<90,-1,1)
          env$hole_10=env$dist_10*env$sign_10
          env$gcs_10=env$gc_10*env$sign_10

          env$sign_5=ifelse(env$angle_5<90,-1,1)
          env$hole_5=env$dist_5*env$sign_5
          env$gcs_5=env$gc_5*env$sign_5

          ## Create spatial object
          # Convert `env` from a data.frame to a spatial `sf` object to enable intersection with the recruits.
          env=env%>%
            st_as_sf(coords=c("X","Y","Z"))%>%
            st_set_crs(proj)

          # add coordinates back to data
          env[,c("X","Y","Z")]=st_coordinates(env)

          if(F) save(env,file="test.Rdata")  #temporary save for debugging.

          ##########################################
          ## Merge with recruit data
          message(paste("################# Importing recruit data for quad: ",f$quad))

          # union points and polygons
          rec1<-read_sf(f$rpath)

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
          # Merge points and polygons
          #rec_int=st_intersection(rec,env)%>%
          #  mutate(pres=1)%>%
          #  group_by(FID)



          # link markers with dense point cloud
          # replace with st_nearest_feature?

          ## Update below to use 3d fdist function.

          tid=st_distance(env_rec,rec)%>%
            apply(1,which.min)
          env_rec$rec=rec$id[tid]
          env_rec$genus=rec$genus[tid]
          env_rec$nid=rec$nid[tid]
          env_rec$taxa=rec$taxa[tid]

          if(F){
            library(rgl)
            ## Extract and explore tori
            obs_torus=env_rec%>%
              group_by(rec,taxa, class)%>%
              summarize(r_n=n(),r_z=diff(range(Z)),r_area=diff(range(X))*diff(range(Y)))%>%  #calculate recruit stats - others?
              group_by(rec,taxa,class, r_n, r_z, r_area)%>%
              do(torus(.,env,dist=0.005))

            obs_torus$col=factor(obs_torus$type,labels=c("black","red","green"))
            table(obs_torus$rec)
            td=obs_torus#filter(obs_torus,rec=="agar2")
            points3d(as.matrix(td[,c("X","Y","Z")]),color=as.character(td$col))#,col=dist)
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
          d$quad=f$quad  # add quad id to table

          d$class=as.factor(d$class)
          d$pres=as.logical(d$pres)
          d$taxa=as.factor(d$taxa)


          # add an 'id' column to uniquely identify each point.
          d$id=1:nrow(d)

          ################
          # Save Data
          message(paste("################# Saving data for quad: ",f$quad))


          save(d,file=outputfile_rdata)
          d%>%
            dplyr::select(-geometry)%>%
            write_csv(path=outputfile_csv)

          return(data.frame(quad=f$quad,npoints=nrow(d)))
        }

#files$quad[c(7,11,16)]
#ggplot(d,aes(x=X,y=Y,col=class))+
#  geom_point()



