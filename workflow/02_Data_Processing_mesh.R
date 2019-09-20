##########################################

## Build file list
files=data.frame(
  mesh_path=list.files(file.path("rawdata",dataversion,"output"),
                             pattern=".rds",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("[.].*$","",fname))%>%
  separate(col = quad,into=c("quad","scale"),sep="_")%>%
  mutate(scale=as.numeric(scale))%>%
  dplyr::select(-fname)%>%
  as.tbl()


## Loop through files and generate a giant table with all data
#data=foreach(i=1:nrow(files),.combine=bind_rows)%dopar%{
data=foreach(i=1:10,.combine=bind_rows,.inorder = F)%dopar%{
  f=files[i,]
    mesh=readRDS(f$mesh_path)
    mesh$data$scale=f$scale
    mesh$data$quad=f$quad
    return(mesh$data)
}


# columns to drop from the data before reshaping
dropvars=c("mesh_border","mesh_curve","r","g","b","point_border","point_dist","id","Nx","Ny","Nz","x","y","z","dist","sign","gc","angle")

# idvars to not 'spread' - these are things that do not vary by scale
idvars=c("quad","scale","fid","x","y","z","pres_ocr","pres_ocr_torus","pres_scr","pres_scr_torus","coral","sponge","octocoral","rock","ground","ocr","scr","visible")

# reshape to long
datal=data %>%
  select(-contains("smooth"),-one_of(dropvars)) %>%
  gather(var,value,-one_of(idvars)) %>%
  mutate(var_scale=paste(var,scale,sep="_")) %>%
  filter(!is.na(value)) %>%
  as_tibble()

# spread by scale (make separate columns for each variable in each scale)
dataw <- datal %>%
  spread(var_scale,value)

### Save them
saveRDS(datal,"output/datalong.rds")
saveRDS(dataw,"output/datawide.rds")


############
datal %>%
  filter(var%in%c("rough","hole","gcs","aspect","slope","visible")) %>%
  ggplot(aes(x=as.factor(pres_ocr),y=value))+
    geom_violin()+
    facet_grid(var~scale,scales="free")



##


# visualize sampled quads
#mesh=readRDS("rawdata/20190415/output/ect27l_100.rds")
#plotmesh(mesh,mesh$data$mesh_curve)


## plot classification
#class=as.vector(unlist(apply(mesh$data[,c("coral","ground","octocoral","other","rock","sand","scr","ocr","sponge")],1,which.max)))
#plotmesh(mesh,class)

#plotmesh(mesh,visible)

