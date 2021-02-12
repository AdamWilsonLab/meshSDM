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
         quad=as.numeric(as.factor(quad)),
         row=1:n()
         ) %>%
  dplyr::select(-scale)


## EDA quads
if(F) {
  data %>% group_by(quad) %>%
  summarize(n=n(),n_pres=sum(pres_ocr)) %>% View()
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

rdata=df2stack(data)

# save/load the raster object if desired
if(F){
  save(rdata,file="output/data/dataraster.rds")
  load("output/data/dataraster.rds")
}

## Define groups
### This could be by quad or groups of quads
### These groups are used in the custom k-folding in ENMEval
unique(data$quad)


# data$group=data$quad # this is an option, but some quads have very few presences which makes evaluation impossible

data$group=case_when( #instead, group quads somehow - maybe like this?
                    data$quad %in% 1:5 ~ 1,
                    data$quad %in% 6:10 ~ 2,
                    data$quad %in% 11:15 ~ 3,
                    data$quad %in% 16:20 ~ 4,
                    data$quad %in% 21:25 ~ 5,
                    data$quad %in% 26:30 ~ 6,
                    data$quad %in% 31:35 ~ 7,
                    data$quad %in% 36:40 ~ 8,
                    TRUE ~ 0)

# make sure all points are in a group
if(nrow(filter(data,group==0))>0) stop("some points are not in a group")

## Extract the information needed to fit with ENMeval
##

occ_ocr=cbind(x=data$row[data$pres_ocr==1],y=1) %>%  as.matrix()
occ_ocr.grp=data$group[data$pres_ocr==1]

occ_scr=cbind(x=data$row[data$pres_scr==1],y=1) %>%  as.matrix()
occ_scr.grp=data$group[data$pres_scr==1]



bg.sample=10000  #background sample size from each group
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

summary(nas) # look at NA's to see how many presences will be lost with this set of env vars.

#names(rdata[[model_vars]])

## Fit model for OCR
eval_ocr <- ENMevaluate(
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

## Fit model for SCR
eval_scr <- ENMevaluate(
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


hist(eval_ocr@results$train.AUC)
eval_ocr@results


## ENMEval changes the raster dimensions!
# dim(eval_ocr@predictions)
# dim(rdata)
# problem is in maxnet.predictRaster

# Align the new raster with the old one

data$ocr_suit=as.data.frame(eval_ocr@predictions$LQ_0.5)

ggplot(data,aes(x=as.factor(pres_ocr),y=ocr_suit))+
  geom_boxplot()+
  scale_y_log10()+
  xlab("OCR Presence")

ggplot(data,aes(color=as.factor(pres_ocr),x=ocr_suit))+
  geom_density()+
  scale_x_log10()

