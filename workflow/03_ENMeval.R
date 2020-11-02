library(tidyverse)
library(Morpho)
library(Rvcg)
library(scales)
library(viridis)
devtools::load_all(".")

### load the combined data
#dataw<-readRDS("output/data/datawide.rds")


### Convert to raster stack for integration with ENMeval
library(raster)
library(sf)

n=10000
groups=5

x=data.frame(x=1:n,
             y=1, # always 1 to make it 1D
             group=rep(1:5,each=n/5,len=n),
             pres=rbinom(n,size=1,p=0.1),
             v1=rnorm(n),
             v2=rnorm(n),
             v3=rnorm(n))

x2=df2stack(x)

#plot(x2)
#x[x$pres==1,]

occ=coordinates(x2)[x$pres==1,]
occ.grp=x$y[x$pres==1]

bg.sample=1000  #background sample size
bg=x %>%
  sample_n(bg.sample)

bg.coords=bg[,c("x","y")] %>% as.matrix()
bg.grp=bg$group

#plot(x2[[4]])
#points(occ)
# test extraction
extract(x2,bg.coords) %>% as.data.frame() %>% arrange(x)
occ
##############################################################
## ENMeval
library(ENMeval)


eval1 <- ENMevaluate(occ,
                     env=x2[[5:7]],
                     bg.coords,
                     occ.grp = occ.grp,
                     bg.coords = bg.coords,
                     bg.grp = bg.grp,
                     method='user',
                     fc="L",
                     categoricals=NULL,
                     RMvalues=rep(3,3))

