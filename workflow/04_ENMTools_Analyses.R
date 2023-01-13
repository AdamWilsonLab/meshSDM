
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


###########Variable importance without considering NAs########

#1 Variable importance, THEN removing correlated variables:

vars_ocr_all=c("hole_5","hole_10","hole_20","hole_50","hole_100",
               "slope_5","slope_10",
               "rough_5","rough_10","rough_20","rough_50","rough_100",
               "coral_new","sponge_new","octocoral_new",
               "sand","ground_new", "rock")

env_all=rdata[[vars_ocr_all]]

env_all <- check.env(env_all)

#A) train the model with all variables:

ocr_model_vi_all <- enmtools.maxent (ocr,
                                     env_all,
                                     test.prop = 0,
                                     nback = 30000, env.nback = 30000,
                                     overwrite = FALSE,
                                     rts.reps = 0, bg.source = "env",
                                     verbose = FALSE, clamp = TRUE)

##Variable importance permutation method (Supplementary Figure 2A in Martinez-Quintana et al., 2023):
enmtools.vip(ocr_model_vi_all, nsim = 100, method = "permute") #Remove not important variables

#B) Correlation analysis

library(reshape2)

bg <- prepareSWD(species = "ocr", a = bg.coords, env_all)

##Supplementary Figure 2A in Martínez-Quintana et al., 2023):

ocr_corrheatmap <- plotCor(bg, method ="spearman", cor_th = 0.6) #plot correlation heatmap with corr. threshold = 0.6

corVar(bg, method = "spearman", cor_th = 0.6) #pairs of corr. variables


#####Octocoral recruits models######################################################################################

#After removing the not important variables I checked the performance of 7 models with 7 regularization multipliers
#(ranging from RM= 0.5 to 3.5)
#selected the model with the best performance based on AUC and Bohl tests (see the code below)

#Model3 with Reg. Mul = 1.5 is the model that performed best:

#Most important variables to be included in model fitting:

model_vars=c("hole_10","hole_100",
             "slope_10",
             "rough_10","rough_100",
             "coral_new","sponge_new", "octocoral_new",
             "ground_new","rock", "sand")

env2=rdata[[model_vars]]

env2new <- check.env(env2) #predictors

ocr_model3_new <- enmtools.maxent (ocr,
                                   env2new,
                                   test.prop = 0.3,# train with 70% of presences
                                   nback = 30000, env.nback = 30000,
                                   overwrite = FALSE,
                                   rts.reps = 100, bg.source = "env",
                                   verbose = FALSE, clamp = TRUE,
                                   args=c('-b','1.5', "linear=TRUE", "product=TRUE", "hinge=TRUE",
                                          "threshold=FALSE", "quadratic=TRUE"))

#you can save the results of the model and load it:

#save(ocr_model3_new, file= "/projects/academic/adamw/am298/coral_microhabitat/ENMtools/Ecog_Models/ocr_final_new/ocr_model3_new.rda")

#load("/projects/academic/adamw/am298/coral_microhabitat/ENMtools/Ecog_Models/ocr_final_new/ocr_model3_new.rda")

#Variable contribution (Figure 3A in Martínez-Quintana et al., 2023):
ocr_ConVar <- enmtools.vip(ocr_model3_new, nsim = 100, method = "permute") #remove not important variables

#Several model evaluation tests:
plot(ocr_model3_new$test.evaluation, 'auc')# method selected

#plot(ocr_model3_new$test.evaluation, 'OR')
#plot(ocr_model3_new$test.evaluation, 'kappa')
#plot(ocr_model3_new$test.evaluation, 'TPR')
#boxplot(ocr_model3_new$test.evaluation)
#density(ocr_model3_new$test.evaluation)


##### Bohl Tests for octocoral recruits ####################################################################

# I checked 7 models (with Regularization multipliers from 0.5, 1, 1.5, 2, 2.5, 3, and 3.5
#and the model #3 is the one with the highest AUC and the best performance based on the Bohl test below.

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

##Supplementary figure 4A in Martínez-Quintana et al., 2023):

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

##Visualize pair of predictors in E-space (example):

visualize.enm(ocr_model3_new, env2new, layers = c("hole_100", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("ground_new", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("slope_10", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("hole_10", "rough_10"), plot.test.data =T)

visualize.enm(ocr_model3_new, env2new, layers = c("octocoral_new", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("coral_new", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("sponge_new", "rough_10"), plot.test.data =T)
visualize.enm(ocr_model3_new, env2new, layers = c("rock", "rough_10"), plot.test.data =T)


#marginal responses curves used in Figure 3B in (Martínez-Quintana et al., 2023):

ocrmod3_rc_new <- ocr_model3_new$response.plots


##Create raster for model projection:

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
quad="eut16l"#choose one quadrat(it can be anyone)
mesh=meshfiles$mesh_path[meshfiles$quad==quad] %>% readRDS()

# then join back with predict_results by fid and quadrat to be sure everything lines up:
mesh_predict=left_join(mesh$data, # pull data from mesh object
                       dplyr::select(data,quad,fid, ocr_suit), # keep only these columns for merging
                       by=c("quad","fid")) %>%
  droplevels()%>%
  arrange(fid)

library(plotly)

#plot predictions on the mesh of the selected quadrat (Figure 4C and 4D in Martínez-Quintana et al., 2023):
mesh %>%
  plotmesh(
    mesh_predict$ocr_suit, # the column to use to color mesh
    #    mesh_predict$slope_10, # the column to use to color mesh
    title="Suitability",showlegend = TRUE) #NOte: This line does not run


###########################################################################################################

#########  SDM for Scleractinian recruits (scr)   ##########################################
#Same approach as with octocoral recruits (ocr)

#1 Variable importance, THEN removing correlated variables:

vars_scr_all=c("hole_5","hole_10","hole_20","hole_50","hole_100",
               "slope_5","slope_10",
               "rough_5","rough_10","rough_20","rough_50","rough_100",
               "coral_new","sponge_new","octocoral_new",
               "sand","ground_new", "rock")

env_all=rdata[[vars_scr_all]]

env_all_new <- check.env(env_all)

#A) train the model to selected variables:

scr_model_vi_all <- enmtools.maxent (scr,
                                     env_all_new,
                                     test.prop = 0,
                                     nback = 30000, env.nback = 30000,
                                     overwrite = FALSE,
                                     rts.reps = 0, bg.source = "env",
                                     verbose = FALSE, clamp = TRUE)

##Variable importance permutation method (Supplementary Figure 2B in Martinez-Quintana et al., 2023):
scr_vi <- enmtools.vip(scr_model_vi_all, nsim = 100, method = "permute") #remove not important variables


plot(scr_model_vi_all$model)
scr_model_vi_all$model

#B) Correlation analysis:

library(reshape2)

bg <- prepareSWD(species = "scr", a = bg.coords, env_all)

scr_corrheatmap <- plotCor(bg, method ="spearman", cor_th = 0.6) #plot correlation heatmap with corr. threshold = 0.6

corVar(bg, method = "spearman", cor_th = 0.6) #pairs of corr. variables


################################################################################
# Variables selected for model fitting:

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

#load("/projects/academic/adamw/am298/coral_microhabitat/ENMtools/Ecog_Models/scr_final_new/scr_model3_new.rda")

##Variable importance permutation method (Figure 3C in Martinez-Quintana et al., 2023):
scr_ConVar <- enmtools.vip(scr_model3_new, nsim = 100, method = "permute") #Removed not important variables


#####Bohl Test##########
# Here I check the p-values to compare performance between model 1 and model 3,
##but model 3 has a better performance:

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

##Supplementary Figure 4B in Martinez-Quintana et al., 2023:
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
##Figure 3D in Martinez-Quintana et al., 2023):
scr_model3_new$response.plots

#Visualize pair of predictors in E-space (example):

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
#points, and the set of supplied environmental layers (Example):

PCAscr <- threespace.plot(scr_model3_new, env2new, maxpts = NA)

##Create raster for model projection:

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
quad="eut16l"  #choose which quad (it can be anyone)
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
