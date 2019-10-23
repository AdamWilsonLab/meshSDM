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
#data=foreach(i=1:10,.combine=bind_rows)%dopar%{
  f=files[i,]
    mesh=readRDS(f$mesh_path)
    mesh$data$scale=f$scale
    mesh$data$quad=f$quad
    return(mesh$data)
}


# columns to drop from the data before reshaping
dropvars=c("mesh_border","mesh_curve","r","g","b","point_border","point_dist","id","Nx","Ny","Nz","x","y","z","dist","sign","gc","angle")

classvars=c("coral","sponge","octocoral","rock","ground","ocr","scr","algae","rock_igneous","sand","other")

# idvars to not 'gather' - these are things that do not vary by scale
idvars=c("quad","scale","fid",
         "pres_ocr","pres_ocr_torus","pres_scr","pres_scr_torus",
         classvars,
         "visible")

# reshape to long
datal=data %>%
  select(-contains("smooth"),-one_of(dropvars)) %>% # drop some variables
  mutate_at(classvars, function(x) ifelse(is.na(x),0,x)) %>%  # replace NA classes 0s
  gather(var,value,-one_of(idvars)) %>% #reshape variables to long except idvars
  mutate(
#    value=ifelse(var%in%classvars&is.na(value),0,value), #classes not present in some quads are NA, replace with 0s
    var_scale=paste(var,scale,sep="_")) %>% # combine variable name and scale for easy spreading
  filter(!is.na(value)) %>% #drop missing observations
  as_tibble()

# spread by scale (make separate columns for each variable in each scale)
dataw <- datal %>%
  select(-scale,-var) %>% # drop these so the spread includes all scales
  spread(var_scale,value)

### Save them
saveRDS(datal,"output/datalong.rds")
saveRDS(dataw,"output/datawide.rds")

