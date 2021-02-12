library(tidyverse)
library(Morpho)
library(Rvcg)
library(scales)
library(viridis)
devtools::load_all(".")
library(raster)
library(sf)

### load the combined data
data<-readRDS("output/data/datawide.rds") %>%
  mutate(visible=as.numeric(visible),
         pres_ocr=ifelse(is.na(pres_ocr),0,1),
         pres_scr=ifelse(is.na(pres_scr),0,1),
         rock=rock+rock_igneous,
         transect=substr(quad,1,4),  #just the transect id
         transect_id=as.numeric(as.factor(transect)), #transect #
         quad_id=as.numeric(as.factor(quad)), #quad #
         row=1:n()
         ) %>%
  dplyr::select(-scale)


## EDA quads
if(F) {
  data %>%
    group_by(quad) %>%
    summarize(n=n(),
            n_ocr=sum(pres_ocr),
            n_scr=sum(pres_scr))

  data %>%
    group_by(transect) %>%
    summarize(n=n(),
              n_ocr=sum(pres_ocr),
              n_scr=sum(pres_scr))
  }

# reduce size of dataset for testing?
subsample=F
if(subsample){
  sn=1e6 #sample size
  data2=bind_rows(
  sample_n(filter(data,pres_ocr==0),sn),  # keep only a sample of 0s
  filter(data,pres_ocr==1)) %>%  # keep all presences
  mutate(row=1:n())  #reset the row counter - this will be used to link back to the original data

data=data2
}

# Convert to raster stack (dropping text fields)
# this is done to faciliate using ENMeval predict functions on rasters
rdata=data %>%
  dplyr::select(-transect,-quad) %>%
  df2stack()

# save/load the raster object if desired
if(F){
  save(rdata,file="output/data/dataraster.rds")
  load("output/data/dataraster.rds")
}

## Define groups
### This could be by quad or groups of quads
### These groups are used in the custom k-folding in ENMEval
table(data$transect_id)

# data$group=data$quad_id # this is an option, but some quads have very few presences which makes evaluation impossible
data$group=data$transect_id

# make sure all points are in a group
if(nrow(filter(data,group==0))>0) stop("some points are not in a group")

## Extract the information needed to fit with ENMeval
##


occ_ocr=cbind(x=data$row[data$pres_ocr==1],y=1) %>%  as.matrix()
occ_ocr.grp=data$group[data$pres_ocr==1]

occ_scr=cbind(x=data$row[data$pres_scr==1],y=1) %>%  as.matrix()
occ_scr.grp=data$group[data$pres_scr==1]



bg.sample=1000  #background sample size from each group
bg=data %>%
  group_by(group) %>%
  sample_n(bg.sample)

bg.coords=cbind(x=bg$row,y=1) %>% as.matrix()
bg.grp=bg$group

# Confirm all quads have some data
bg %>%
  group_by(bg$group) %>%
  summarize(n=n())

# Confirm all quads have some data
data %>%
  group_by(group) %>%
  filter(pres_ocr==1) %>%
  summarize(n=n())


data %>%
  group_by(group) %>%
  filter(pres_scr==1) %>%
  summarize(n=n())


##############################################################
## ENMeval
library(ENMeval)

model_vars=c("hole_5","hole_10","hole_100",
             "gcs_5","gcs_10","gcs_100",
             "slope_5","slope_10","slope_100",
             "rough_5","rough_10", "rough_100",
             "coral","sponge","rock","sand")

# Check missing data for
nas <- cbind.data.frame(group=occ_ocr.grp,extract(rdata[[model_vars]],occ_ocr,na.rm=F)) %>%
  rowwise() %>%
  mutate(na_vars = sum(is.na(c_across())),
         na_any=na_vars>0) %>%
  group_by(group) %>%
  summarise(n_presence=n(),
            n_na=sum(na_any),
            n_data=n_presence-n_na
            )

nas # look at NA's to see how many presences will be lost with this set of env vars.
# n_na is the number of ocr presences that have some missing data (and will be dropped)
# n_data is the remaining number that will be included.


#names(rdata[[model_vars]])

## Fit model for OCR
ocr_eval <- ENMevaluate(
                    occ = occ_ocr,
                     occ.grp = occ_ocr.grp,
                     env=rdata[[model_vars]], #select whatever layers you want to include
                     bg.coords = bg.coords,
                     bg.grp = bg.grp,
                     method='user',
#                     method='randomkfold',
#                    kfolds=10,
                    fc="LQ",
                    numCores=20,
                    parallel=T,
                    rasterPreds=T)

hist(ocr_eval@results$train.AUC)
ocr_eval@results

ocr_mod_which <- which.min(ocr_eval@results$AICc)
ocr_mod_best <- eval_ocr@models[[ocr_mod_which]]


# Don't use the rasters in the results object - it doesn't align with the original data
# instead use the prediction function like this:
ocr_pred_best <- raster::predict(rdata, ocr_mod_best,  type = "logistic", clamp = T)
data$ocr_suit <- values(ocr_pred_best)


## Fit model for SCR
scr_eval <- ENMevaluate(
  occ = occ_scr,
  occ.grp = occ_scr.grp,
  env=rdata[[model_vars]], #select whatever layers you want to include
  bg.coords = bg.coords,
  bg.grp = bg.grp,
  method='user',
  fc="LQ",
  numCores=20,
  parallel=T,
  rasterPreds=T)

scr_mod_which <- which.min(scr_eval@results$AICc)
scr_mod_best <- scr_eval@models[[scr_mod_which]]


# Don't use the rasters in the results object - it doesn't align with the original data
# instead use the prediction function like this:
scr_pred_best <- raster::predict(rdata, scr_mod_best,  type = "logistic", clamp = T)
data$scr_suit <- values(scr_pred_best)


### Compare species

combined_raster=stack(ocr_pred_best,scr_pred_best)
calc.niche.overlap(combined_raster)


# calculate differences between species
data$niche_dif=values(ocr_pred_best-scr_pred_best)

## A few example plots

ggplot(data,aes(x=as.factor(pres_ocr),y=ocr_suit))+
  geom_boxplot()+
  scale_y_log10()+
  xlab("OCR Presence")

ggplot(data,aes(x=as.factor(pres_scr),y=scr_suit))+
  geom_boxplot()+
  scale_y_log10()+
  xlab("SCR Presence")


## Compare ocr and scr
ggplot(data,aes(x=scr_suit,y=ocr_suit))+
  geom_hex(bins=100)+
  scale_fill_viridis_c(trans="log1p")

ggplot(data,aes(x=niche_dif,y=hole_10))+
  geom_hex(bins=100)+
  scale_fill_viridis_c(trans="log1p")

ggplot(data,aes(color=as.factor(pres_ocr),x=ocr_suit))+
  geom_density()+
  scale_x_log10()


#############################
#############################
#############################
### Mesh files
## Build file list
meshfiles=data.frame(
  mesh_path=list.files(file.path("output/data/quad"),
                       pattern=".rds",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("[.].*$","",fname))%>%
  dplyr::select(-fname)%>%
  as_tibble()

# put predictions back on a particular landscape:
quad="eut49r"  #choose which quad
quad="ect14l"  #choose which quad
quad="ect210r"
mesh=meshfiles$mesh_path[meshfiles$quad==quad] %>% readRDS()

# then join back with predict_results by fid and quad to be sure everything lines up)
mesh_predict=left_join(mesh$data, # pull data from mesh object
                       dplyr::select(data,quad,fid,ocr_suit,scr_suit,niche_dif), # keep only these columns for merging
                       by=c("quad","fid")) %>% arrange(fid)# %>%
#  mutate(bio=(coral+sponge+ocr+scr)>0.2,
#         suitability_nobio=ifelse(bio,0,scr_suit))


# plot the predicted suitability on the mesh
mesh %>%
  #  meshbase(clean_tol=0.4,adjust_z=0.1,edge_tol=0.01) %>%
  plotmesh(
#    mesh_predict$scr_suit, # the column to use to color mesh
#    mesh_predict$ocr_suit, # the column to use to color mesh
    mesh_predict$niche_dif, # the column to use to color mesh
    title="Suitability",showlegend = TRUE)
