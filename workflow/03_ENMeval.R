library(tidyverse)
library(Morpho)
library(Rvcg)
library(scales)
library(viridis)
devtools::load_all(".")
library(raster)
library(sf)

### load the combined data
#dataw<-readRDS("output/data/datawide.rds")


### Convert to raster stack for integration with ENMeval

n=1000
groups=5

x=data.frame(x=1:n,
             y=1, # always 1 to make it 1D
             group=rep(1:5,each=n/5,len=n),
             pres=rbinom(n,size=1,p=0.1),
             v1=rnorm(n),
             v2=rnorm(n),
             v3=rnorm(n))

x2=df2stack(x)

# use as.data.frame to convert back to a data.frame.
expect_equal(x,as.data.frame(x2))

#plot(x2)
#x[x$pres==1,]

## Extact the information needed to fit with ENMeval
##
occ=coordinates(x2)[x$pres==1,]
occ.grp=x$y[x$pres==1]

bg.sample=100  #background sample size
bg=x %>%
  sample_n(bg.sample)

bg.coords=bg[,c("x","y")] %>% as.matrix()
bg.grp=bg$group

#plot(x2[[4]])
#points(occ)
# test extraction by comparing with the oringal dataset
extract(x2,bg.coords) %>% as.data.frame() %>% arrange(x)
occ


##############################################################
## ENMeval
library(ENMeval)

envvars=5:7
names(x2[[envvars]])

eval1 <- ENMevaluate(occ,
                     # env=x2[[envvars]], #select whatever layers you want to include here
                     # bg.coords,
                     # occ.grp = occ.grp,
                     # bg.coords = bg.coords,
                     # bg.grp = bg.grp,
                     # method='user',
                     method='jackknife',
                    fc="L")


#convert back to dataframe to link with geometry.
eval1_pred=as.data.frame(eval1@predictions)








