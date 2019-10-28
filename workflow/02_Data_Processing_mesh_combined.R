## Read in the individual quad data, rbind it,
## and convert to the format needed for EDA and modeling


library(tidyverse)
library(foreach)
library(doParallel)
registerDoParallel(30)
devtools::load_all(".", reset=F)

## Other settings / parameters
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
outputdir=file.path("output/data")

##########################################

## Build file list
files=data.frame(
  mesh_path=list.files(file.path("output/data/quad"),
                             pattern=".rds",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("[.].*$","",fname))%>%
  dplyr::select(-fname)%>%
  as.tbl() %>%
  filter(quad!="ect16l")  #has two recruits in one face.


# columns to drop from the data before reshaping
dropvars=c("r","g","b","Nx","Ny","Nz","x","y","z")

classvars=c("coral","sponge","octocoral","rock","ground","ocr","scr","algae","rock_igneous","sand","other")

# idvars to not 'gather' - these are things that do not vary by scale
idvars=c("quad","fid",
         "pres_ocr","pres_scr",
         classvars,
         "visible")


dataw=foreach(i=1:nrow(files),.combine=bind_rows, .inorder = F)%dopar%{
  f=files[i,]
  mesh=readRDS(f$mesh_path)
  data=mesh$data %>%
#    group_by(fid) %>%
#    top_n(n = 1) %>%
    select(-dropvars) #%>%
#    mutate(pres_ocr=ifelse(is.na(pres_ocr),0,1),
#           pres_scr=ifelse(is.na(pres_scr),0,1))
  return(data)
}

# reshape to long
datal=dataw %>%
  gather(var,value,-one_of(idvars)) %>% #reshape variables to long except idvars
  separate(var,c("var","scale"),sep="_") %>% # combine variable name and scale for easy spreading
  filter(!is.na(value)) #drop missing observations

### Save them
saveRDS(dataw,"output/data/datawide.rds")
saveRDS(datal,"output/data/datalong.rds")

