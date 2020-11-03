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








