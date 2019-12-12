## Read in the individual quad data, rbind it,
## and convert to the format needed for EDA and modeling


library(tidyverse)
library(foreach)
library(doParallel)
registerDoParallel(30)
devtools::load_all(".", reset=F)

## Other settings / parameters
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
dataversion="20190415"  #folder to look for quad data
outputdir=file.path("rawdata",dataversion,"output")

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
data=foreach(i=1:nrow(files),.combine=bind_rows, .inorder = F)%dopar%{
#data=foreach(i=100:110,.combine=bind_rows)%dopar%{
  f=files[i,]
    mesh=readRDS(f$mesh_path)
    mesh$data$scale=f$scale
    mesh$data$quad=f$quad
  return(mesh$data)
}


# update recruit fid (location) so they are the same across all scales
# e.g. since each scale is processed separately, each scale can have a different median environment
# make them all match the finest scale - other ideas?
recruits<-
  data%>%
  #group_by(quad)
  filter(quad=="ect110r") %>%
  filter(!is.na(pres_scr)|!is.na(pres_scr)) %>%
  select(quad,fid,scale,pres_ocr,pres_scr) %>%
  spread(pres_scr,fid) %>%
  filter(scale==min(scale)) %>%
  gather("pres_scr","fid",-quad,-scale)

data2=  left_join(mesh$data,recruits,by=c("quad","scale","fid"))




# columns to drop from the data before reshaping
dropvars=c("mesh_border","mesh_curve","r","g","b","point_border",
           "point_dist","id","Nx","Ny","Nz","x","y","z","dist","sign","gc","angle",
           "pres_ocr_torus","pres_scr_torus")

classvars=c("coral","sponge","octocoral","rock","ground","ocr","scr","algae","rock_igneous","sand","other")

# idvars to not 'gather' - these are things that do not vary by scale
idvars=c("quad","scale","fid",
         "pres_ocr","pres_scr",
         classvars,
         "visible")

# reshape to long
datal=data %>%
  select(-contains("smooth"),-one_of(dropvars)) %>% # drop some variables
  mutate_at(classvars, function(x) ifelse(is.na(x),0,x)) %>%  # replace NA classes 0s
  gather(var,value,-one_of(idvars)) %>% #reshape variables to long except idvars
  mutate(
    var_scale=paste(var,scale,sep="_")) %>% # combine variable name and scale for easy spreading
  filter(!is.na(value)) %>% #drop missing observations
  as_tibble()

# spread by scale (make separate columns for each variable in each scale)
dataw <- datal %>%
  select(-scale,-var,-pres_ocr_torus,-pres_scr_torus) %>% # drop these so the spread includes all scales
  spread(var_scale,value)

### Save them
saveRDS(datal,"output/datalong.rds")
saveRDS(dataw,"output/datawide.rds")

