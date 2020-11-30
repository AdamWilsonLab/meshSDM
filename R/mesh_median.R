#' @import tidyverse

mesh_median <- function (fids, data, return=c("values","index"),...){
  # confirm the fid column exists
    if(!"fid" %in% names(data)) stop("data does not include an 'fid' column")
  # subset the faces data to include only faces for this fid
    fdata <- data%>%
    filter(fid%in%fids)%>%
    na.omit()

    # Calculate the multivariate median of the dataset
    fdata_median <-
      fdata%>%
      select(-fid)%>%  # don't include the fid in the distance calculation
      select_if(is_numeric)%>% #drop non-numeric values
      depth::med(method="Spatial",approx = T, ...)


  # calculate distance from each face to the median and
  # select the row number of the face closest to the median
  fdata_dist_index <-
    as.matrix(dist(rbind(fdata_median$median,fdata)))[-1,1]%>%
    which.min()%>%as.numeric()

  if(return=="values") return(as.data.frame(fdata[fdata_dist_index,]))
  if(return=="index") return(data.frame(fid=fdata$fid[fdata_dist_index]))

}
