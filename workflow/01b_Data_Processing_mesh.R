
library(tidyverse)
library(sf)
library(foreach)
library(doParallel)
registerDoParallel(30)
devtools::load_all(".", reset=F)

## Other settings / parameters
proj="+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
dataversion="20190415"  #folder to look for quad data
outputdir=file.path("output/data/quad")


### Merge scales into one mesh for each quad

# columns to drop from the data before reshaping
dropvars=c("mesh_border","mesh_curve","point_border",
           "x_smooth", "y_smooth", "z_smooth",
           "point_dist","id","dist","sign","gc","angle",
           "pres_ocr_torus","pres_scr_torus")

classvars=c("coral","sponge","octocoral","rock","ground","ocr","scr","algae","rock_igneous","sand","other")

valvars=c("rough","aspect","density","slope","hole","gcs")

# idvars to not 'gather' - these are things that do not vary by scale
idvars=c("quad","scale","fid",
         "x","y","z","r","g","b","Nx","Ny","Nz",
         "pres_ocr","pres_scr",
         classvars,
         "visible")


## Build file list

files=data.frame(
  mesh_path=list.files(file.path("output/data/quad_scale"),
                       pattern=".rds",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("[.].*$","",fname))%>%
  separate(col = quad,into=c("quad","scale"),sep="_")%>%
  mutate(scale=as.numeric(scale))%>%
  dplyr::select(-fname)%>%
  as_tibble()


fncols <- function(data, cname) {
  # https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- NA
  data
}

## Loop through quads and combine scales
data=foreach(q=unique(files$quad),.combine=bind_rows, .inorder = F)%dopar%{
  f1=filter(files,quad==q)%>%slice(1)
  mesh=readRDS(f1$mesh_path)


  tdata<-
      mesh$data %>%
    dplyr::select(-all_of(c(dropvars,valvars))) %>%
      mutate(quad=f1$quad,
             pres_ocr=ifelse(pres_ocr==0,NA,pres_ocr),
             pres_scr=ifelse(pres_scr==0,NA,pres_scr)) %>%
      fncols(classvars) %>%  # adds any missing classvar columns
      mutate_at(classvars, function(x) ifelse(is.na(x),0,x))  # replace NA classes 0s

  # now loop through other scales and gather the data
allscale_data = foreach(s=files$scale[files$quad==q])%do%{
    f=filter(files,quad==q,scale==s)
    mesh=readRDS(f$mesh_path)
    mesh$data %>%
      mutate(quad=f1$quad,scale=f$scale) %>%
      dplyr::select(-all_of(dropvars)) %>%
      fncols(classvars) %>%  # adds any missing classvar columns
      mutate_at(classvars, function(x) ifelse(is.na(x),0,x)) %>%  # replace NA classes 0s
      rename_at(valvars,function(x) paste(x,as.character(s),sep="_")) %>%
      arrange(fid) %>%
      dplyr::select(-all_of(idvars),-contains("scale"),fid) %>%
      as_tibble()
  } %>%
  reduce(left_join, by = "fid") #join them all back together

# confirm the fids match across scales
if(cbind(tdata$fid,allscale_data) %>%
  dplyr::select(contains("fid")) %>%
  apply(1,function(x) sum(diff(x))) %>%
  sum()!=0) stop("fids do not match across scales")

mesh$data=
  left_join(tdata,allscale_data,by="fid") %>%
  dplyr::select(fid,everything()) %>%  #rearrange columns so fid is first
  arrange(fid) %>%
  as_tibble()

if(sum(mesh$data$fid!=1:nrow(mesh$data))>0) error("Matching problem")

output_path=file.path(outputdir,paste0(q,".rds"))
saveRDS(mesh,file=output_path,compress = T)

results=data.frame(
  quad=f1$quad,
  n=nrow(mesh$data),
  n_pres=sum(!is.na(mesh$data$pres_ocr)),
  path=output_path
)
print(results)
return(results)
}
