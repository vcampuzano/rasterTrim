brickSplit <-
  function(gpath, gname, rfilename, gatt="ID", path=".", outDir=paste(path, "out", sep="/"), ncores=parallel::detectCores()/2, showMsg=TRUE ){
    grd<-rgdal::readOGR(gpath, gname)
    rst<-raster::brick(filename)
    rasterSplitSP(rst, grd, grd.att=gatt, path=path, outDir=outDir, ncores=ncores, showMsg=showMsg)
  }
