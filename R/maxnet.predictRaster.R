if(F){
  env=rdata
  mod=eval_ocr@models[[1]]
  type="logistic"
  clamp=T
}


maxnet.predictRaster <- function (mod, env, type, clamp)
{
  env.n <- raster::nlayers(env)
  env.pts <- raster::rasterToPoints(env)
  env.pts$id=1:nrow(env.pts) #grid cell id before dropping NAs
  origNrow <- nrow(env.pts)
  env.pts <- na.omit(env.pts)
  naOmitNrow <- nrow(env.pts)
  rowDiff <- origNrow - naOmitNrow
  if (rowDiff > 0) {
    message(paste("\n", rowDiff, "grid cells found with at least one NA value: these cells were excluded from raster predictions."))
  }
  mxnet.p <- cbind(env.pts$id,
                   predict(mod, env, type = type, clamp = clamp))
  results=raster(env) #create new empty raster with identical dimensions to env

  env.pts <- cbind( as.numeric(mxnet.p))

  mxnet.p <- raster::rasterFromXYZ(env.pts[, c(1, 2, env.n +
                                                 3)], res = raster::res(env))
  return(mxnet.p)
}


maxnet.predictRaster_old <- function (mod, env, type, clamp)
{
  env.n <- raster::nlayers(env)
  env.pts <- raster::rasterToPoints(env)
  origNrow <- nrow(env.pts)
  env.pts <- na.omit(env.pts)
  naOmitNrow <- nrow(env.pts)
  rowDiff <- origNrow - naOmitNrow
  if (rowDiff > 0) {
    message(paste("\n", rowDiff, "grid cells found with at least one NA value: these cells were excluded from raster predictions."))
  }
  mxnet.p <- predict(mod, env.pts, type = type, clamp = clamp)
  env.pts <- cbind(env.pts, as.numeric(mxnet.p))
  mxnet.p <- raster::rasterFromXYZ(env.pts[, c(1, 2, env.n +
                                                 3)], res = raster::res(env))
  return(mxnet.p)
}
