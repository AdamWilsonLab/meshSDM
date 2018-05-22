library(Rvcg)
require(Morpho)
library(dplyr)

logp=function(x) sign(x) * log(abs(x) + 1)


mod=vcgPlyRead("data/Ect13r.ply", updateNormals = TRUE)

modc=vcgClean(mod, sel = 0, tol = 0, silent = FALSE, iterate = FALSE)

meshDist(modc,modc,tol=0.1,from=-20,to=20)

mod2=vcgSmooth(modc,type="taubin",mu=1,lambda = 1)
mod3=vcgSmooth(modc,type="taubin",mu=2.5)

meshDist(mod2,mod2,tol=0.1,from=-20,to=20)
meshDist(mod3,mod3,tol=0.1,from=-20,to=20)


curv <- vcgCurve(mod3)
summary(logp(curv$gaussvb))
meshDist(mod3,distvec=logp(curv$gaussvb),tol=0.1,from=-20,to=20)

plotNormals(mod3,long = .001,lwd = .01,col = "darkred")
