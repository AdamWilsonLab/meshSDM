library(tidyverse)

cfiles=data.frame(
  path=list.files("data/20181109/",
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
  rpath=list.files("data/20181109/",
                  pattern=".*rec.*shp",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(rpath),
    quad=sub("_rec.*$","",fname))%>%
  dplyr::select(-fname)

files=left_join(cfiles,rfiles,by="quad")

save(files,file="output/files.Rdata")
