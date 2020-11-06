library(tidyverse)
library(Morpho)
library(Rvcg)
library(scales)
library(viridis)
devtools::load_all(".")
library(raster)
library(sf)

## make up a sample dataset to illustrate how it works
## You will want to use real data here instead of this x object.

n=10
groups=2
x=data.frame(x=1:n,
             y=1, # always 1 to make it 1D
             group=rep(1:5,each=n/5,len=n),
             pres=rbinom(n,size=1,p=0.1),
             v1=rnorm(n),
             v2=rnorm(n),
             v3=rnorm(n))

### Convert to raster stack for integration with ENMeval
x2=df2stack(x, verbose=T)

# use as.data.frame to convert back to a data.frame.
x3 = as.data.frame(x2)

# confirm the converted version is just like the original
expect_equal(x,x3)

# plot(x2)  # if desired

### load the combined data
data<-readRDS("output/data/datawide.rds") %>%
  mutate(visible=as.numeric(visible),
         pres_ocr=ifelse(is.na(pres_ocr),0,1),
         pres_scr=ifelse(is.na(pres_scr),0,1),
         rock=rock+rock_igneous,
         quad=as.numeric(as.factor(quad)),
         row=1:n()
         ) %>%
  na.omit()

## EDA quads
data %>% group_by(quad) %>%
  summarize(n=n(),n_pres=sum(pres_ocr)) %>% View()

# reduce size of dataset for testing?
subsample=F
if(subsample){
data2=bind_rows(
  sample_n(filter(data,pres_ocr==0),1e6),  # keep only a sample of 0s
  filter(data,pres_ocr==1)) %>%  # keep all presences
  mutate(row=1:n())  #reset the row counter

data=data2
}

rdata=df2stack(data)

# save/load the raster object if desired
if(F){
  save(rdata,file="output/data/dataraster.rds")
  load("output/data/dataraster.rds")
}

## Extact the information needed to fit with ENMeval
##

occ=cbind(x=data$row[data$pres_ocr==1],y=1) %>%  as.matrix()
occ.grp=data$quad[data$pres_ocr==1]

bg.sample=10000  #background sample size
bg=data %>%
  sample_n(bg.sample)

bg.coords=cbind(x=bg$row,y=1) %>% as.matrix()
bg.grp=bg$quad

##############################################################
## ENMeval
library(ENMeval)

model_vars=c("hole_5","hole_10","hole_100","slope_10","rough_5","rough_10", "rough_20","coral","sponge","rock","sand")

names(rdata[[model_vars]])

eval1 <- ENMevaluate(occ,
                     env=rdata[[model_vars]], #select whatever layers you want to include
#                     occ.grp = occ.grp,
                     bg.coords = bg.coords,
#                     bg.grp = bg.grp,
#                     method='user',
                     method='randomkfold',
                    kfolds=5,
                    fc="LHQ",
                    rasterPreds=T)

hist(eval1@results$train.AUC)
eval1@results

data$suitability=values(eval1@predictions$LHQ_0.5)

ggplot(data,aes(x=as.factor(pres_ocr),y=suitability))+
  geom_boxplot()+
  scale_y_log10()+
  xlab("OCR Presence")

ggplot(data,aes(color=as.factor(pres_ocr),x=suitability))+
  geom_density()+
  scale_x_log10()


#convert back to dataframe to link with geometry.
#eval1_pred=as.data.frame(eval1@predictions)







