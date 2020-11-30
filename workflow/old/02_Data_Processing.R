#Read in the individual quads and rbind them into a single dataset for modeling.

library(foreach)
library(doParallel)
library(sf)
library(tidyverse)
registerDoParallel(10)


nbg=10000  # number of background points to select from each quad

dataversion="20190415"

files=data.frame(
  path=list.files(file.path("data",dataversion,"processed"),pattern="Rdata", full=T),stringsAsFactors = F)%>%
  mutate(file=basename(path),
         quad=sub("[.]Rdata","",file))


sample_all_quads <-
  foreach(i=1:nrow(files),.combine=rbind.data.frame) %dopar% {
    f=files$path[i]
    d1 <- local({  # this weird thing loads the data for one quad and renames it as 'd'
      load(f)
      stopifnot(length(ls())==1)
      environment()[[ls()]]
    })

    subset_id=c(d1$id[d1$pres==1], # keep all presences
                d1$id[sample(which(d1$pres==0),nbg,replace = F)])  # sample only nbg background points

    d2=d1%>%
      ungroup()%>%
      filter(id%in%subset_id)%>%
      dplyr::select(-Bf,-Gf,-Rf,-r_n,-r_z,-geometry,
                    -contains("angle"),
                    -contains("sign"),
#                    -contains("smooth"),
                    -contains("dist"),
                    -contains("aspect"),
                    -contains("gc_"),
                    -r_area,-Ny,-Nx,-Nz) # remove extras


    return(d2)
  }


save(sample_all_quads,file=file.path("data",dataversion,
                                     paste0("all_quads_",dataversion,"_",
                                            format(nbg, scientific = FALSE),".Rdata")))


if (F){
#
d2=d1%>%
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
write_csv(d2,file.path("data",dataversion,paste0(files$quad[i],"_test.csv")))
}



