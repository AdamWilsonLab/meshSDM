#' @import Rvcg
#' @import tidyverse

mesh_adj<-function(fids,mesh,type=c("intersect","border")){
  # fids - will return neighbors of these fids
    adj_list=vcgVFadj(mesh)
    names(adj_list)=1:nverts(mesh)
    adj_unlist=unlist(adj_list,use.names = T)
    adj=data.frame(vid=as.numeric(names(adj_unlist)),
                 fid=adj_unlist,stringsAsFactors = F)
    adj_x=adj%>%
      filter(fid%in%fids)

    # get vertex IDs for faces
    vids=as.vector(unique(mesh$it[,fids]))
    # get faces associated with these vertices
    adj_faces=Morpho::getFaces(mesh,vids)

    if("border"%in%type) ind1=!adj_faces%in%fids
      else(ind1=NA)
    if("intersect"%in%type) ind2=adj_faces%in%fids
      else(ind2=NA)

    # return the index id and requested faces
    return(data.frame(fid=na.omit(adj_faces[ind1|ind2])))
}


#dists=meshDist(as.matrix(pts[,c("x","y","z")]),mesh)
