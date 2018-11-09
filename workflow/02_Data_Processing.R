#Read in the individual quads and rbind them into a single dataset for modeling.

source("../workflow/00_setup.R")

library(foreach)
library(doParallel)
library(rgdal)
library(sf)
library(tidyverse)


nbg=100000  # number of background points to select from each quad


files=data.frame(
  path=list.files("data",pattern="Rdata", full=T),stringsAsFactors = F)%>%
  mutate(file=basename(path),
         quad=sub("[.]Rdata","",file))

d <-
  foreach(i=1:nrow(files),.combine=rbind.data.frame) %dopar% {
    f=files$path[i]
    load(f)
    d1 <- local({  # this weird thing loads the data for one quad and renames it as 'd'
      load(f)
      stopifnot(length(ls())==1)
      environment()[[ls()]]
    })
    d1$quad=files$quad[i]
    d1$class=as.factor(d1$class)
    d1$pres=as.logical(d1$pres)

    subset_id=c(d1$id[d1$pres==1], # keep all presences
                d1$id[sample(which(d1$pres==0),nbg,replace = F)])  # sample only nbg background points

    d2=d1%>%
      ungroup()%>%
      filter(id%in%subset_id)%>%
      dplyr::select(-Bf,-Gf,-Rf,-r_n,-r_z,-geometry,
                    -contains("angle"),
                    -contains("sign"),
                    -contains("smooth"),
                    -contains("dist"),
                    -contains("aspect"),
                    -contains("gc_"),
                    -r_area,-Ny,-Nx,-Nz) # remove extras


    return(d2)
  }


save(d,file=paste("data/multi_quad_sample.Rdata"))
