#Quadrats are grouped in 8 groups to avoid spatial autocorrelation
#We selected 30000 background points per each group of quadrats and all presences

library(ENMTools)
library(rJava)
library(tidyverse)
library(Morpho)
library(Rvcg)
library(scales)
library(viridis)
library(raster)
library(sf)
library(dismo)
#install.extras()

### load the combined data

data <- readRDS("/projects/academic/adamw/am298/coral_microhabitat/data/datawide.rds") %>%
  mutate(visible=as.numeric(visible),
         pres_ocr=ifelse(is.na(pres_ocr),0,1),
         pres_scr=ifelse(is.na(pres_scr),0,1),
         rock=rock+rock_igneous,
         transect=substr(quad,1,4),  #just the transect id
         transect_id=as.numeric(as.factor(transect)), #transect #
         quad_id=as.numeric(as.factor(quad)), #quad #
         row=1:n())%>%  #Substitue 100% cover to 90% to prevent occurrences are projected on alive benthic organisms, when recruits are right nex to them
  mutate(
    octocoral_new = case_when(octocoral==100 & (pres_ocr==1 | pres_scr ==1) ~ 90,
                              TRUE                   ~ octocoral),
    sponge_new =case_when(sponge == 100 & (pres_ocr ==1 | pres_scr ==1) ~ 90,
                          TRUE                   ~sponge),
    coral_new = case_when(coral == 100 & (pres_ocr ==1 | pres_scr ==1) ~ 90,
                          TRUE                   ~coral),
    ground_new = case_when(ground==0 & (octocoral_new == 90 | sponge_new == 90 | coral_new == 90) & (pres_ocr==1 | pres_scr ==1) ~10,
                           TRUE                   ~ ground)
  ) %>%
  dplyr::select(-scale)

#str(data)

#head(data)

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
  sn=10e6 #sample size
  data2=bind_rows(
    sample_n(filter(data,
                    pres_ocr==0,
                    pres_scr==0),sn),  # keep only a sample of 0s
    filter(data,
           pres_ocr==1,
           pres_scr==1)) %>%  # keep all presences
    mutate(row=1:n())  #reset the row counter - this will be used to link back to the original data

  data=data2
}


# Convert to raster stack (dropping text fields)
rdata=data %>%
  dplyr::select(-transect,-quad) %>%
  df2stack()

## Define groups
### This could be by quad or groups of quads
table(data$transect_id)

# data$group=data$quad_id # this is an option, but some quads have very few presences which makes evaluation impossible
# anything defined in 'group' will be used for cross validation.
data$group=data$transect_id

# make sure all points are in a group
if(nrow(filter(data,group==0))>0) stop("some points are not in a group")

## Extract the information needed for fitting
##

occ_ocr=cbind(x=data$row[data$pres_ocr==1],y=1) %>%  as.matrix()
occ_ocr.grp=data$group[data$pres_ocr==1]

occ_scr=cbind(x=data$row[data$pres_scr==1],y=1) %>%  as.matrix()
occ_scr.grp=data$group[data$pres_scr==1]


bg.sample=30000  #background sample size from each group
bg=data %>%
  group_by(group) %>%
  sample_n(bg.sample)

bg.coords=cbind(x=bg$row,y=1) %>% as.matrix()
bg.grp=bg$group

# Confirm all quads have some data
bg %>%
  group_by(group) %>%
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

grps=unique(data$group)

#In the variables, hole refers to TEI, slope to slope, rough to roughness,
#coral_new, sponge_new, and octocoral_new to benthic cover of corals, sponges and octocorals

model_vars=c("hole_5","hole_10","hole_20","hole_50","hole_100",
             "slope_5","slope_10",
             "rough_5","rough_10","rough_20","rough_50","rough_100",
             "coral_new","sponge_new","octocoral_new",
             "sand","ground_new", "rock")

# Check missing data and removed those variables that have NAs:
nas_all_ocr <- cbind.data.frame(group=occ_ocr.grp,
                                raster::extract(rdata[[model_vars]],occ_ocr,na.rm=F))
nas_all_scr <- cbind.data.frame(group=occ_scr.grp,
                                raster::extract(rdata[[model_vars]],occ_scr,na.rm=F))

# count by var
nas_all_ocr %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  t() # I removed: hole_20,hole_50, all slopes but 5, rough_20 and rough_50

nas_all_scr %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  t() # I removed: hole_20,hole_50, all slopes but 5, rough_20 and rough_50

# count by quad
nas_all_ocr %>%
  rowwise() %>%
  mutate(na_vars = sum(is.na(c_across())),  # I loose 13 ocr recruits with the selected predictors:
         na_any=na_vars>0) %>%
  group_by(group) %>%
  summarise(n_presence=n(),
            n_na=sum(na_any),
            n_data=n_presence-n_na
  )


nas_all_scr %>%
  rowwise() %>%
  mutate(na_vars = sum(is.na(c_across())), # I loose 8 scr recruits with the selected predictors:
         na_any=na_vars>0) %>%
  group_by(group) %>%
  summarise(n_presence=n(),
            n_na=sum(na_any),
            n_data=n_presence-n_na
  )


names(rdata[[model_vars]])


#prepare the data for modelling suitability (ocr and scr):

occ_ocr=cbind(x=data$row[data$pres_ocr==1],y=1) %>%  as.matrix()

occ_scr=cbind(x=data$row[data$pres_scr==1],y=1) %>%  as.matrix()


#1. Put data for each of your species into an enmtools.species object:
#Create the empty object:

ocr <- enmtools.species()

ocr$presence.points <- occ_ocr
ocr$background.points <- bg.coords
ocr$species.name <- "ocr"

scr <- enmtools.species()

scr$presence.points <- occ_scr
scr$background.points <- bg.coords
scr$species.name <- "scr"


#Format the object:
#Formatting the object:
ocr <- check.species (ocr)

scr <- check.species (scr)
