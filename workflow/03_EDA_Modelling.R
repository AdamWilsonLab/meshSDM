library(tidyverse)
library(Morpho)
library(Rvcg)
library(scales)
library(viridis)

### load the combined data
datal<-readRDS("output/datalong.rds")
dataw<-readRDS("output/datawide.rds")


############
datal %>%
  filter(var%in%c("rough","hole","gcs","aspect","slope","visible")) %>%
  ggplot(aes(x=as.factor(pres_ocr),y=value))+
  geom_violin()+
  facet_grid(var~scale,scales="free")


datal %>%
  filter(var%in%c("rough","hole","gcs","aspect","slope","visible")) %>%
  ggplot(aes(y=value,x=quad))+
  geom_violin()+
  facet_wrap(~var,scales="free")


dataw %>% filter(!is.na(pres_ocr))%>% group_by(visible) %>% summarize(n=n())

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
  as.tbl()


# visualize sampled quads
#mesh=readRDS("rawdata/20190415/output/ect27l_100.rds")
#plotmesh(mesh,mesh$data$mesh_curve)


## plot classification
#class=as.vector(unlist(apply(mesh$data[,c("coral","ground","octocoral","other","rock","sand","scr","ocr","sponge")],1,which.max)))
#plotmesh(mesh,class)

#plotmesh(mesh,visible)

# percent missing by variable

summary_nas <- datal%>%group_by(var,scale)%>%summarize(n=n(),na=sum(is.na(value)),p=na/n)


##############################################################
## Maxent
library(maxnet)

# number of background values to use (total across all quads)
# could modify to have some number in each quad to ensure it's balanced
n_background=10000

model_vars=c("hole_5","hole_10","hole_100","rough_5","rough_10")
pres_vars=c("pres_scr","pres_ocr","pres")

dataw<-dataw %>%
  mutate(pres=!is.na(pres_ocr)) %>%
  select(-pres_scr,-pres_ocr)

model_data=bind_rows(
  filter(dataw,pres==1) %>% # keep all presences
    na.omit(),
  filter(dataw,pres==0)%>%
    na.omit()%>%
    sample_n(n_background))%>% # use n_background samples
select(one_of(c("pres",model_vars)))

table(model_data$pres)

# fit maxent
p=model_data$pres
data=select(model_data,model_vars) %>% as.data.frame()

#data[is.na(data)]=0

m1=maxnet(p,data,maxnet.formula(p, data, classes="lq"))

plot(m1,type="logistic")

## Quad level summaries

# Make predictions for all quads
predict_data<-
  dataw %>%
  select(quad,fid,model_vars) %>%
  na.omit()

predict_suitability=predict(m1,newdata=predict_data,type="logistic",clamp=F) %>% as.vector()

# then join back by quad and fid to be sure everything lines up)
predict_results=left_join(dataw,bind_cols(predict_data,suitability=predict_suitability),by=c("quad","fid"))


# suitability by quad
ggplot(predict_results, aes(x=quad,y=hole_10))+
  geom_boxplot()+
  coord_flip()

ggplot(predict_results, aes(x=quad,y=suitability))+
  geom_boxplot()+
  coord_flip()


# put predictions back on a particular landscape:
quad="eut49r"  #choose which quad

mesh=meshfiles$mesh_path[meshfiles$quad==quad] %>% readRDS()

# then join back with predict_results by fid and quad to be sure everything lines up)
mesh_predict=left_join(mesh$data, # pull data from mesh object
                       select(predict_results,quad,fid,suitability), # keep only these columns for merging
                       by=c("quad","fid")) %>% arrange(fid)

# plot the predicted suitability on the mesh
plotmesh(mesh,
         mesh_predict$suitability, # the column to use to color mesh
         title="Suitability")


########################
## Exploring other mesh plots
if(F) {
  #fading from points to mesh
  xlim=c(-.1,.1)
  ylim=c(-.2,-.3)
  plot3d(mesh,type="shade",alpha=rescale_mid(mesh$vb[1,],c(0,1),mid=.01),
         xlim=xlim,ylim=ylim,aspect=F,smooth=F,forceClipRegion=T,add=F)
  wire3d(mesh,type="wire",alpha=rescale_mid(mesh$vb[1,],c(0,1),mid=.8),
         aspect=F,smooth=F,add=T)
  #  points3d(st_coordinates(pts_occ),col="blue",size=10) #classified points
  points3d(pts[,c("x","y","z")],size=.05,col="darkblue",
           alpha=rescale_mid(pts$x,c(1,0),mid=.01),add=T)   #points3d(mesh$data[,c("x","y","z")],size=.1) #matched face for each

  summary_table=data.frame(
    quad=f$quad,
    points=nrow(mesh$data),
    occurrences=sum(mesh$data$pres_adj),
    n_faces=ncol(mesh$vb))

  print(summary_table)

  return(summary_table)

}

##########################
## Summaries
#vcgArea(mesh)/(0.25*.25)

