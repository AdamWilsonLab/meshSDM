library(Rvcg)
library(tidyverse)
library(plotly)
library(scales)

data(humface)

# load some mesh
tmesh <- rgl::icosahedron3d()
tmesh <- humface

# try to create an attribute table for each face
# I want this to have one row per face with various attributes (e.g. x barycenter coordinate)
faces=data.frame(
  fid=1:ncol(tmesh$it),                    # face index
  vcgBary(tmesh))%>%                       # add barycenter coordinates for each face
  select(fid=fid,x=X1,y=X2,z=X3)%>%        # rename columns
  mutate(border=vcgBorder(tmesh)$borderit) # add other variables from vcg functions


# now plot humface with colors from the faces table
# following code here:  https://laustep.github.io/stlahblog/posts/plotly_trisurf.html

#specify attribute to use to color the mesh:
colorvec=faces$z

facecolor = colour_ramp(
  brewer_pal(palette="RdBu")(9)
)(rescale(x=faces$z))

# and plot it:
plot_ly(
  x = tmesh$vb[1,], y = tmesh$vb[2,], z = tmesh$vb[3,],
  i = tmesh$it[1,]-1, j = tmesh$it[2,]-1, k = tmesh$it[3,]-1,
  facecolor = facecolor,
  type = "mesh3d",
) %>% layout(scene = list(aspectmode = "data"))


# why don't the colors align with the x position?
# shouldn't the vcgBary() return coordinates in order of 1:ncol(tmesh$it)??  What is the correct order?
