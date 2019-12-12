# Dividir raster con los polígonos de Objeto SpatialPolygon

rasterSplitSPMosaic <-
  function(rst, grd,
           grd.att="ID",
           path=".", outDir=paste(path, "out", sep="/"),
           ncores=parallel::detectCores()/2,
           postfix="",
           showMsg=TRUE
  ) {
    projCRS<-raster::crs(rst)
    raster::projection(grd)<-projCRS

    rst.extent<-raster::extent(rst)
    polygonlist<-list()

    # Identificar los polígonos que tienen intersección
    for(block in grd@data[[grd.att]]){
      # Seleccionar el polígono
      grd.p<-grd[grd@data[[grd.att]]==block,]
      grd.pextent<-raster::extent(grd.p)

      # Verificar que existe intersección
      if( grd.pextent@xmax>rst.extent@xmin
          & grd.pextent@xmin<rst.extent@xmax
          & grd.pextent@ymax>rst.extent@ymin
          & grd.pextent@ymin<rst.extent@ymax
      ) {
        polygonlist[[block]]<-grd.p
      }
    }
    if(showMsg) message(paste(length(polygonlist),"polígonos encontrados"))

    cl<-parallel::makeCluster(ncores)
    rst.slist<-parallel::parLapply(cl, polygonlist, fun=rasterSplitCropMosaic, rst=rst, att=grd.att, outDir=outDir, postfix=postfix)
    parallel::stopCluster(cl)
    return(rst.slist)
  }

rasterSplitCropMosaic <-
  function(X, rst, att, outDir=".", postfix=""){
    block<-levels(factor(X@data[[att]]))
    fname<-paste(outDir, paste(paste(block, postfix, sep=""), "tif", sep="."), sep="/")
    if(file.exists(fname)){
      message(paste("Crop", fname))
      rst<-raster::crop(rst, X)
      rst.old<-raster::raster(fname)
      rst.mosaic<-raster::mosaic(rst, rst.old, fun=mean, tolerance=0.2)
      raster::writeRaster(rst.mosaic, filename=fname, overwrite=TRUE)
    } else {
      raster::crop(rst, X, filename=fname)
    }
  }
