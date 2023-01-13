###This is the final script for the models:
#####Qauds grouped in 8 groups
#####30000 bc points/group selected + all presences
#####Variables selected:
        #a)First, those that did not generate Nas in the presences
        #b)Second: Test for variable correlation.
        #c)Third: From pair of correlated (p> 0.6) select one:
            #I selected Rough_10, Rough_100 and Hole_10 and Rough_100.
#####Model fit with 70% of presences for training and 30% for testing.
#####Model selection testing performance with different Reg.Mul.(from 1 to 4 in sep 0.5)
       # I selected for ocr, the Reg. Mul = 1.5. And all features but thereshold.
       #rts.reps =200
       #Sampling bias using Bohl and Raes test: but it seems the model performs well):
#selected predictors based on var. importance
# (I removed sand, rough_5;20 adn 50 and hole_5;20;and 50):

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
#library(SDMtune)
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
         row=1:n())%>%
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
# this is done to faciliate using ENMeval predict functions on rasters
rdata=data %>%
  dplyr::select(-transect,-quad) %>%
  df2stack()

## Define groups
### This could be by quad or groups of quads
### These groups are used in the custom k-folding in ENMEval
table(data$transect_id)

# data$group=data$quad_id # this is an option, but some quads have very few presences which makes evaluation impossible
# anything defined in 'group' will be used for cross validation.
data$group=data$transect_id

# make sure all points are in a group
if(nrow(filter(data,group==0))>0) stop("some points are not in a group")

## Extract the information needed to fit with ENMeval
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


###########Variable importance without considering NAs########

#1 Variable importance, THEN removing correlated variables:

vars_ocr_all=c("hole_5","hole_10","hole_20","hole_50","hole_100",
               "slope_5","slope_10",
               "rough_5","rough_10","rough_20","rough_50","rough_100",
               "coral_new","sponge_new","octocoral_new",
               "sand","ground_new", "rock")

env_all=rdata[[vars_ocr_all]]

env_all <- check.env(env_all)

#A) train the model to select variables:

ocr_model_vi_all <- enmtools.maxent (ocr,
                                     env_all,
                                     test.prop = 0,
                                     nback = 30000, env.nback = 30000,
                                     overwrite = FALSE,
                                     rts.reps = 0, bg.source = "env",
                                     verbose = FALSE, clamp = TRUE)

enmtools.vip(ocr_model_vi_all, nsim = 100, method = "permute") #I removed <<< important variables
                                                                #using permutation importance.

#B) Correlation analysis:

library(reshape2)

bg <- prepareSWD(species = "ocr", a = bg.coords, env_all)

ocr_corrheatmap <- plotCor(bg, method ="spearman", cor_th = 0.6) #plot correlation heatmap with corr. threshold = 0.6

corVar(bg, method = "spearman", cor_th = 0.6) #pairs of corr. variables


#####Octo recruits models############
#I already previously checked reg.mul from 0.5 to 3.5 and selected the model with best performance bsed on AUC test nd Bohl test

#Model3 reg. mul = 1.5:THIS HAS THE BEST PERFORMANCE:

model_vars=c("hole_10","hole_100",
             "slope_10",
             "rough_10","rough_100",
             "coral_new","sponge_new", "octocoral_new",
             "ground_new","rock", "sand")

env2=rdata[[model_vars]]

env2new <- check.env(env2)

ocr_model3_new <- enmtools.maxent (ocr,
                                   env2new,
                                   test.prop = 0.3,
                                   nback = 30000, env.nback = 30000,
                                   overwrite = FALSE,
                                   rts.reps = 100, bg.source = "env",
                                   verbose = FALSE, clamp = TRUE,
                                   args=c('-b','1.5', "linear=TRUE", "product=TRUE", "hinge=TRUE",
                                          "threshold=FALSE", "quadratic=TRUE"))


#save(ocr_model3_new, file= "/projects/academic/adamw/am298/coral_microhabitat/ENMtools/Ecog_Models/ocr_final_new/ocr_model3_new.rda")

load("/projects/academic/adamw/am298/coral_microhabitat/ENMtools/Ecog_Models/ocr_final_new/ocr_model3_new.rda")

#Variable contributon:
ocr_ConVar <- enmtools.vip(ocr_model3_new, nsim = 100, method = "permute") #I removed <<< important variables


plot(ocr_model3_new$test.evaluation, 'auc')
plot(ocr_model3_new$test.evaluation, 'OR')
plot(ocr_model3_new$test.evaluation, 'kappa')
plot(ocr_model3_new$test.evaluation, 'TPR')
boxplot(ocr_model3_new$test.evaluation)
density(ocr_model3_new$test.evaluation)


##### Bohl Tests ####################################################################

# check 7 models and model 3 is the one with the highes AUC adn the best performance
#test 1:

#test 3: THIS IS THE WINNEEEER!!!!  ##########################

test.pres <- ocr_model3_new$test.data

test.bg <- ocr_model3_new$analysis.df %>%
  filter(presence == 0) %>%
  dplyr::select(Longitude, Latitude)

bohl.test <- function(thismodel){
  dismo::evaluate(test.pres, test.bg, thismodel, env2new)
}

null.dist <-sapply(ocr_model3_new$rts.test$rts.models,
                   FUN = function(x) bohl.test(x$model)@auc)

null.dist <- c(ocr_model3_new$env.test.evaluation@auc, null.dist)
names(null.dist)[1] <- "empirical"

Bohltest3_env_new <- qplot(null.dist, geom = "histogram", fill = "density", alpha = 0.5) +
  geom_vline(xintercept = null.dist["empirical"], linetype = "longdash") +
  xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("AUC") +
  ggtitle(paste("Model performance in E-space on test data")) +
  theme(plot.title = element_text(hjust = 0.5))

null.dist <- c(ocr_model3_new$test.evaluation@auc, null.dist)
names(null.dist)[1] <- "empirical"

Bohltest3_geo_new <- qplot(null.dist, geom = "histogram", fill = "density", alpha = 0.5) +
  geom_vline(xintercept = null.dist["empirical"], linetype = "longdash") +
  xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("AUC") +
  ggtitle(paste("Model performance in G-space on test data")) +
  theme(plot.title = element_text(hjust = 0.5))


#html to maxent model with the best performance:
ocr_model3_new$model

#Variable contribution:
plot(ocr_model3_new$model)
plot(scr_model3_new$model)

# visualize the model with the best fit:
#Include the model with the best fot tin the scripts above to get also the response curves

visualize.enm(ocr_model3_new, env2new, layers = c("hole_100", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("ground_new", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("slope_10", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("hole_10", "rough_10"), plot.test.data =T)

visualize.enm(ocr_model3_new, env2new, layers = c("octocoral_new", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("coral_new", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("sponge_new", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("rock", "rough_10"), plot.test.data =T)


#marginal responses curves:

ocrmod3_rc_new <- ocr_model3_new$response.plots

#A plot that compares the environmental distribution of presence points, background
#points, and the set of supplied environmental layers.

PCAocr <- threespace.plot(ocr_model3_new, env2new, maxpts = NA)


##############################################################################################################

############### MERGED  with ENMeval_updates script ###################################################################

# Don't use the rasters in the results object - it doesn't align with the original data
# instead use the prediction function like this:
ocr_pred_best <- raster::predict(rdata, ocr_model3_new$model,  type = "logistic", clamp = T)

# Plot cloglog response curve for a continuous environmental variable????

data$ocr_suit <- values(ocr_pred_best) #values for suitability


### Mesh files
## Build file list
meshfiles=data.frame(
  mesh_path=list.files(file.path("/projects/academic/adamw/projects/meshSDM/output/data/quad"),
                       pattern=".rds",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("[.].*$","",fname))%>%
  dplyr::select(-fname)%>%
  tibble::as_tibble()

#head(meshfiles)


# put predictions back on a particular landscape:
quad="ect14l"  #choose which quad
quad="eut16l"
quad="eut27l"
quad="eut345r"

quad="ect25l"
quad="ect28l"#choose which quad
#quad="ect210r"
mesh=meshfiles$mesh_path[meshfiles$quad==quad] %>% readRDS()

# then join back with predict_results by fid and quad to be sure everything lines up)
mesh_predict=left_join(mesh$data, # pull data from mesh object
                       dplyr::select(data,quad,fid, ocr_suit), # keep only these columns for merging
                       by=c("quad","fid")) %>%
  droplevels()%>%
  arrange(fid)# %>%
#  mutate(bio=(coral+sponge+ocr+scr)>0.2,
#         suitability_nobio=ifelse(bio,0,scr_suit))

#  mutate(bio=(coral+sponge+ocr+scr)>0.2,
#         suitability_nobio=ifelse(bio,0,scr_suit))


# plot the predicted suitability on the mesh

#library(plotly)

mesh %>%
  #  meshbase(clean_tol=0.4,adjust_z=0.1,edge_tol=0.01) %>%
  plotmesh(
    mesh_predict$ocr_suit, # the column to use to color mesh
    #    mesh_predict$slope_10, # the column to use to color mesh
    title="Suitability",showlegend = TRUE)


###########################################################################################################

#########  SCR   ##########################################
#Same approach as with ocr (same best Reg. Mul):

#1 Variable importance, THEN removing correlated variables:

vars_scr_all=c("hole_5","hole_10","hole_20","hole_50","hole_100",
               "slope_5","slope_10",
               "rough_5","rough_10","rough_20","rough_50","rough_100",
               "coral_new","sponge_new","octocoral_new",
               "sand","ground_new", "rock")

env_all=rdata[[vars_scr_all]]

env_all_new <- check.env(env_all)

#A) train the model to select variables:

scr_model_vi_all <- enmtools.maxent (scr,
                                     env_all_new,
                                     test.prop = 0,
                                     nback = 30000, env.nback = 30000,
                                     overwrite = FALSE,
                                     rts.reps = 0, bg.source = "env",
                                     verbose = FALSE, clamp = TRUE)

scr_vi <- enmtools.vip(scr_model_vi_all, nsim = 100, method = "permute") #I removed variables <<<< importance


plot(scr_model_vi_all$model)
scr_model_vi_all$model

#B) Correlation analysis:

library(reshape2)

bg <- prepareSWD(species = "scr", a = bg.coords, env_all)

scr_corrheatmap <- plotCor(bg, method ="spearman", cor_th = 0.6) #plot correlation heatmap with corr. threshold = 0.6

corVar(bg, method = "spearman", cor_th = 0.6) #pairs of corr. variables


################################################################################
# Selected aribles bellow:

model_vars=c("hole_10","hole_100",
             "slope_10",
             "rough_10","rough_100",
             "coral_new","sponge_new", "octocoral_new",
             "ground_new","rock", "sand")

env2=rdata[[model_vars]]

env2new <- check.env(env2)


scr_model3_new <- enmtools.maxent (scr,
                                   env2new,
                                   test.prop = 0.3,
                                   nback = 30000, env.nback = 30000,
                                   overwrite = FALSE,
                                   rts.reps = 100, bg.source = "env",
                                   verbose = FALSE, clamp = TRUE,
                                   args=c('-b','1.5', "linear=TRUE", "product=TRUE", "hinge=TRUE",
                                          "threshold=FALSE", "quadratic=TRUE"))

#save(scr_model3_new,
#file= "/projects/academic/adamw/am298/coral_microhabitat/ENMtools/Ecog_Models/scr_final_new/scr_model3_new.rda")

load("/projects/academic/adamw/am298/coral_microhabitat/ENMtools/Ecog_Models/scr_final_new/scr_model3_new.rda")

#Variable contributon:
scr_ConVar <- enmtools.vip(scr_model3_new, nsim = 100, method = "permute") #I removed <<< important variables


#####Bohl Test##########
###Model 1 and 3 are very similar, but visually model 1 is better
# Here I check the p values to compare performance between mod 1 and mod 3:

#test Mod4:

##### Bohl Tests ##########

#test 3:

test.pres <- scr_model3_new$test.data

test.bg <- scr_model3_new$analysis.df %>%
  filter(presence == 0) %>%
  dplyr::select(Longitude, Latitude)

bohl.test <- function(thismodel){
  dismo::evaluate(test.pres, test.bg, thismodel, env2new)
}

null.dist <-sapply(scr_model3_new$rts.test$rts.models,
                   FUN = function(x) bohl.test(x$model)@auc)

null.dist <- c(scr_model3_new$env.test.evaluation@auc, null.dist)
names(null.dist)[1] <- "empirical"

Bohltest3_scr_env_new <- qplot(null.dist, geom = "histogram", fill = "density", alpha = 0.5) +
  geom_vline(xintercept = null.dist["empirical"], linetype = "longdash") +
  xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("AUC") +
  ggtitle(paste("Model performance in E-space on test data")) +
  theme(plot.title = element_text(hjust = 0.5))

null.dist <- c(scr_model3_new$test.evaluation@auc, null.dist)
names(null.dist)[1] <- "empirical"

Bohltest3_scr_geo_new <- qplot(null.dist, geom = "histogram", fill = "density", alpha = 0.5) +
  geom_vline(xintercept = null.dist["empirical"], linetype = "longdash") +
  xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("AUC") +
  ggtitle(paste("Model performance in G-space on test data")) +
  theme(plot.title = element_text(hjust = 0.5))


#html to maxent model with the best performance:
scr_model3_new$model

#Variable contribution:
plot(scr_model3_new$model)


#marginal responses curves:
scr_model3_new$response.plots

#Visualize predictors in E-space

A1 <- visualize.enm(scr_model3_new, env2new, layers = c("slope_10","rough_10"),
                               plot.test.data =T)

 A2 <- visualize.enm(scr_model3_new, env2new, layers = c("hole_10","rough_10"),
                               plot.test.data =T)

A3 <- visualize.enm(scr_model3_new, env2new, layers = c( "hole_100","rough_10"),
              plot.test.data =T)

A4 <- visualize.enm(scr_model3_new, env2new, layers = c( "coral_new","rough_10"),
              plot.test.data =T)

A5 <- visualize.enm(scr_model3_new, env2new, layers = c( "sponge_new","rough_10"),
              plot.test.data =T)

A6 <- visualize.enm(scr_model3_new, env2new, layers = c( "octocoral_new","rough_10"),
              plot.test.data =T)


#A plot that compares the environmental distribution of presence points, background
#points, and the set of supplied environmental layers.

PCAscr <- threespace.plot(scr_model3_new, env2new, maxpts = NA)


####MERGE with ENMeval_updates script##########

# Don't use the rasters in the results object - it doesn't align with the original data
# instead use the prediction function like this:

scr_pred_best <- raster::predict(rdata, scr_model3_new$model,  type = "logistic",
                                 clamp = T)

# Plot cloglog response curve for a continuous environmental variable????

data$scr_suit <-values(scr_pred_best) #values for suitability


### Mesh files
## Build file list
meshfiles=data.frame(
  mesh_path=list.files(file.path("/projects/academic/adamw/projects/meshSDM/output/data/quad"),
                       pattern=".rds",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("[.].*$","",fname))%>%
  dplyr::select(-fname)%>%
  tibble::as_tibble()

head(meshfiles)


# put predictions back on a particular landscape:
quad="ect14l"  #choose which quad
quad="eut16l"  #choose which quad
#quad="eut27l"
quad="eut345r"
quad="ect25l"
quad="ect28l"

mesh=meshfiles$mesh_path[meshfiles$quad==quad] %>% readRDS()

# then join back with predict_results by fid and quad to be sure everything lines up)
mesh_predict=left_join(mesh$data, # pull data from mesh object
                       dplyr::select(data,quad,fid, scr_suit), # keep only these columns for merging
                       by=c("quad","fid")) %>%
  droplevels()%>%
  arrange(fid)#%>%
#mutate(bio=(coral+sponge+ocr+scr)>0.2,
   #suitability_nobio=ifelse(bio,0,scr_suit))

# plot the predicted suitability on the mesh

#library(plotly)

mesh %>%
  #  meshbase(clean_tol=0.4,adjust_z=0.1,edge_tol=0.01) %>%
  plotmesh(
    mesh_predict$scr_suit, # the column to use to color mesh
    #    mesh_predict$slope_10, # the column to use to color mesh
    title="Suitability",showlegend = TRUE)

#####################################################################
####################################################


# calculate DIFFERENCES BETWEEN TAXA:
data$niche_dif=values(ocr_pred_best-scr_pred_best)


### Mesh files
## Build file list
meshfiles=data.frame(
  mesh_path=list.files(file.path("/projects/academic/adamw/projects/meshSDM/output/data/quad"),
                       pattern=".rds",recursive = T, full=T),
  stringsAsFactors = F)%>%
  mutate(
    fname=basename(mesh_path),
    quad=sub("[.].*$","",fname))%>%
  dplyr::select(-fname)%>%
  tibble::as_tibble()

head(meshfiles)


# put predictions back on a particular landscape:
quad="ect14l"  #choose which quad
quad="eut16l"  #choose which quad
#quad="eut27l"
quad="eut345r"
quad="ect25l"
quad="ect28l"

mesh=meshfiles$mesh_path[meshfiles$quad==quad] %>% readRDS()

# then join back with predict_results by fid and quad to be sure everything lines up)
mesh_predict=left_join(mesh$data, # pull data from mesh object
                       dplyr::select(data,quad,fid,niche_dif), # keep only these columns for merging
                       by=c("quad","fid")) %>%
 droplevels()%>%
 arrange(fid)#%>%
#mutate(bio=(coral+sponge+ocr+scr)>0.2,
#suitability_nobio=ifelse(bio,0,niche_dif))

# plot the predicted suitability on the mesh

library(plotly)

mesh %>%
  #  meshbase(clean_tol=0.4,adjust_z=0.1,edge_tol=0.01) %>%
  plotmesh(
    mesh_predict$niche_dif, #visible, #scr_suit, etc...# the column to use to color mesh
    #    mesh_predict$slope_10, # the column to use to color mesh
    title="Suitability",showlegend = TRUE)

head(mesh_predict)


## Compare ocr and scr
ggplot(data,aes(x=scr_suit,y=ocr_suit))+
  geom_hex(bins=100)+
  scale_fill_viridis_c(trans="log10")

ggplot(data,aes(x=scr_suit,y=ocr_suit))+
  geom_density(y=ocr_suit)+
  scale_x_log10()

ggplot(data,aes(x=niche_dif,y=hole_10))+
  geom_hex(bins=100)+
  scale_fill_viridis_c(trans="log10")

ggplot(data,aes(x=niche_dif,y=hole_10))+
  geom_density()+
  scale_x_log10()

ggplot(data,aes(color=as.factor(pres_ocr),x=ocr_suit))+
  geom_density()+
  scale_x_log10()

ggplot(data,aes(color=as.factor(pres_ocr),x=niche_dif))+
  geom_density()+
  scale_x_log10()

ggplot(data,aes(color=as.factor(pres_ocr),x=visible))+
  geom_density()+
  scale_x_log10()


## An example plot:


ggplot(data,aes(x=as.factor(pres_ocr),y=scr_suit))+
  geom_boxplot()+
  scale_y_log10()+
  xlab("OCR Presence")

ggplot(data,aes(x=as.factor(pres_ocr),y=ocr_suit))+
  geom_boxplot()+
  scale_y_log10()+
  xlab("OCR Presence")


ggplot(data,aes(x=as.factor(scr_suit),y=ocr_suit))+
  geom_boxplot()+
  scale_y_log10()+
  xlab("OCR Presence")

#####################Prepare data for violin plots#################

minidataocr <- data %>%
  dplyr::select (ocr_suit, pres_ocr)%>%
  rename(suitscores = ocr_suit , datatype = pres_ocr) %>%
  mutate(rec ="Octocoral")

minidataocr$datatype <- as.factor(minidataocr$datatype)
  minidataocr$datatype <- recode_factor(minidataocr$datatype, "1" = "pres",
                                      "0" = "bcg")

minidatascr <- data %>%
  dplyr::select (scr_suit, pres_scr)%>%
  rename(suitscores = scr_suit , datatype = pres_scr) %>%
  mutate(rec ="Scleractinian")

minidatascr$datatype <- as.factor(minidatascr$datatype)
  minidatascr$datatype <- recode_factor(minidatascr$datatype, "1" = "pres",
                                      "0" = "bcg")

recscore <- merge(minidataocr, minidatascr, all.x = TRUE, all.y = TRUE) %>%
  na.omit()




#vIOLIN PLOT COMPARING SUITABILITY FOR EACH TAXA #############################

rec <- c("Octocoral", "Scleractinian")
names(rec) <- c("ocr", "scr")

Suitbcg <- recscore %>%
  filter (datatype =="bcg")%>%
  ggplot(aes(x=suitscores, y=rec, fill=rec)) +
  geom_violin(width=1) +
  geom_boxplot(width=0.08, color="gainsboro", alpha=0.2) +
  #scale_x_log10()+
  scale_fill_manual(values = c("grey","navy"),
                    breaks = c("Octocoral", "Scleractinian")) +
  #scale_color_viridis_d(option = "inferno") +
  theme_linedraw()+
  labs(x ="Suitability scores", y ="Taxa")+
  theme(legend.position = "none",
        text = element_text(size = 14))

library(ggpubr)

##### Test for normality

# Shapiro-Wilk normality test:
my_data<- recscore %>%
  filter (datatype =="bcg")%>%
  sample_(5000)#maximum sample size for Shapiro

head(my_data)
with(my_data, shapiro.test(suitscores[rec == "Octocoral"]))# p << 0.05 NOT NORMAL
with(my_data, shapiro.test(suitscores[rec == "Scleractinian"])) #  p << 0.05 NOT NORMAL

#Samples not normal distributed we use non parametric two-samples Wilcoxon rank test (Mann-Withey U):

my_data<- recscore %>%
  filter (datatype =="bcg") #all samples

#IS the median suitability scores different between taxa?

wilcox.test(suitscores ~ rec, data = my_data,   #p-value < 2.2e-16
            exact = TRUE, conf.int = TRUE, conf.level = 0.95)


#IS the median suitability lower for octos than for scler?

wilcox.test(suitscores ~ rec, data = my_data,
            exact = FALSE, alternative = "less")





# Data in two numeric vectors
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)
# Create a data frame

my_data <- data.frame(
  group = rep(c("Man","Woman"), each = 9),
  weight = c( men_weight,women_weight)
)


library("ggpubr")

ggboxplot(my_data, x = "group", y = "weight",
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

wilcox.test(weight ~ group, data = my_data,
            exact = FALSE, alternative = "less")















##########  HYPOTHESIS TESTING  ##################

#Breath: This function measures the spatial heterogeneity of the distribution of suitability scores
#from an ENM. It returns Levins’ two metrics of niche breadth.

G_breadth_ocr <- raster.breadth(ocr_model3_new) #in G-space
G_breadth_scr <- raster.breadth(scr_model3_new)

# env.breadth() Calculates breadth of a model in E-space using latin hypercube sampling


env.breadth(ocr_model3_new, env2new, tolerance = 1e-04, max.reps = 300, chunk.size = 1e+06)
env.breadth(scr_model3_new, env2new, tolerance = 1e-04, max.reps = 300, chunk.size = 1e+06)

#######Visualize the overlap???

visualize.overlap(
  ocr_model3_new,
  scr_model3_new,
  env2new,
  nbins = 100,
  layers= c("rough_10","hole_10"),
  plot.points = TRUE)

########### PCA ##################

#install.packages("FactoMineR")
#install.packages("factoextra")

library (FactoMineR)
library(factoextra)


env2new_ocr <- data %>%
            dplyr::select (pres_ocr,
                           rough_10, rough_100,
                           hole_10, hole_100,
                           slope_10,
                           coral_new, octocoral_new, sponge_new,
                           ground_new, rock, sand) %>%
                           filter(pres_ocr ==1) %>%
                           mutate (data="Octocoral") %>%
                           dplyr::select(-pres_ocr)


env2new_scr <- data %>%
            dplyr::select (pres_scr,
                 rough_10, rough_100,
                 hole_10, hole_100,
                 slope_10,
                 coral_new, octocoral_new, sponge_new,
                 ground_new, rock, sand)  %>%
                 filter(pres_scr ==1) %>%
                 mutate (data="Scleractinian") %>%
                 dplyr::select(-pres_scr)


env2new_bc <- data %>%
  dplyr::select (pres_scr,pres_ocr,
                 rough_10, rough_100,
                 hole_10, hole_100,
                 slope_10,
                 coral_new, octocoral_new, sponge_new,
                 ground_new, rock, sand)  %>%
  filter(pres_scr==0 & pres_ocr ==0) %>%
  mutate (data="bc") %>%
  na.omit() %>%
  slice_sample(n = 100000) %>%
  dplyr::select(-pres_scr, -pres_ocr)


env2newrecs <- merge(env2new_ocr, env2new_scr, all.x = TRUE, all.y = TRUE) %>%
  na.omit()

str(env2newrecs)

env2newrecs <- env2newrecs %>%
  rename(slope = slope_10 , calc.rock = ground_new,
         coral = coral_new,
         octocoral=octocoral_new,
         sponge = sponge_new,
         ig.rock =rock)

env2newrecs$data <- as.factor(env2newrecs$data)


env2newrecs2 <- merge(env2newrecs, env2new_bc, all.x = TRUE, all.y = TRUE) %>%
  rename(slope = slope_10 , calc.rock = ground_new,
         coral = coral_new,
         octocoral=octocoral_new,
         sponge = sponge_new,
         ig.rock =rock)

env2newrecs2$data <- as.factor(env2newrecs2$data)

summary(env2newrecs2)

#rec.pca <- PCA(env2newrecs2[,-12], graph = FALSE)

rec.pca <- prcomp(env2newrecs[,-12],  scale = TRUE)

rec.pca <- PCA(env2newrecs[,-12], graph =F)

fviz_eig(rec.pca)

summary(rec.pca)

#Results for Variables:

eig.val <- get_eigenvalue(rec.pca)

# Results for Variables
rec.var <- get_pca_var(rec.pca)
rec.var$coord          # Coordinates
rec.var$contrib        # Contributions to the PCs
rec.var$cos2           # Quality of representation

# Results for individuals
res.ind <- get_pca_ind(rec.var)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation



fviz_pca_ind(rec.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = env2newrecs$data, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             #addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "convex",
             legend.title = "Taxa",
             mean.point = FALSE
)



library(viridis)
library(paletteer)

fviz_pca_biplot(rec.pca,
                axes = c(1, 2),
                # Individuals
                geom.ind = "point",
                fill.ind = env2newrecs$data,
                alpha.ind = 0.5,
                pointshape = 21,
                pointsize = 2.5,
                col.ind ="black",
               palette = c("grey","navy"),
               label="var", #habillage=env2newrecs2$data,
               addEllipses=TRUE, ellipse.level=0.95,
                # Color variable by groups
                col.var = factor(c("Topographic relief", "Topographic relief", "Topographic relief",
                                   "Topographic relief","Topographic relief",
                                   "Benthic cover", "Benthic cover","Benthic cover",
                                   "Substrate type", "Substrate type","Substrate type")),
                legend.title = list(fill = "Taxa", color = "Env. Predictor"),
                repel = TRUE )+
     # Indiviual fill color
  ggpubr::color_palette(c("Black", "dark red", "coral"))

####in 3D######

library(rgl)
rec.pca <- prcomp(env2newrecs[,-12],  scale = TRUE)

pc <- princomp(env2newrecs[,-12], scores=TRUE)

plot3d(pc$scores[,1:3])

###################################################################################################################################

#measuring similarity between ENMs.
#These include Schoener’s D (Schoener 1968), I (Warren et al. 2008),
#and the Spearman rank correlation coefficient between two rasters.
#This function measures similarity in the geographic distribution of suitability scores from two
#ENMs. It returns thre metrics, I, D and Spearman rank correlation.
# These metrics are described in Warren et al. 2008.

raster.overlap(ocr_model3_new, scr_model3_new)


#While D and I are commonly used in the ENM literature, they may tend to overestimate similarity
#between ENMs when many grid cells are of similar values
#(e.g., when two species prefer different habitat but the region contains
#a great deal of habitat that is unsuitable for both).
#env.overlap Calculates overlap between models in environment space using latin hypercube sampling

env.overlap(ocr_model3_new, scr_model3_new,
            env2new,
            tolerance = 0.001,
            max.reps = 1000,
            cor.method = "spearman",
            chunk.size = 1e+06,
            verbose = FALSE)


#Ecospat tests:
# simplified interface to the niche identity and background tests
#that were developed by Broennimann et al. (2012).
#These tests do not rely on ENMs, instead using kernel density smoothing
#to estimate density of the species in environment space.
#Ecospat also uses the density of the available environment to correct
#for availability when measuring overlaps,
#so that overlaps are not strictly driven by availability of combinations of environmental variables.

#These tests only work with two environmental axes,
#so they are often done with the top two PC axes of a set of environments.
#Note that if you provide more than two layers to the enmtools.ecospat function,
#it will performa a PCA analysis on the provided layers and measure overlaps
#on the first two axes of that PCA space.
#Here’s an equivalency/identity test:


enmtools.ecospat.id(ocr, scr, env2new[[c=()]], nreps=100, nback=20000)


  #Hypothesis testing: Niche identity or equivalency test
#To run an identity test, we need to decide what type of models we will build,
#how many replicates we will run, and (in the case of GLM and GAM) a model formula to use for empirical models
#and the Monte Carlo replicates.
#The resulting object contains the replicate models, p values, and plots of the results.
#Typically identity tests are run with at least 99 replicates


identity.test <- identity.test(species.1 = ocr, species.2 = scr, env = env2new, nback = 20000,
              bg.source = "points",
              type = "mx", nreps = 100)


#Background test (symmetric) NOT WORTHY TO DO ASSYMETRIC:
#These test compare the empirical overlap to the overlap expected when points
#are drawn randomly from the background of both species
#(species.1 background vs. species.2 background)
#keeping sample sizes for each species constant



bg.bc.sym = background.test(species.1 = ocr,
                             species.2 = scr,
                             env = env2new,
                             type = "mx",
                             nreps = 100,
                             test.type = "symmetric")

bg.bc.sym



