fakeRegions <- function(n=30,pts=cbind(runif(n),runif(n)), boundary = pts[chull(pts),], clipmode=0){
  ## make a fake geography
  ##
  ## either specify n for points in a unit square, or pts
  ##

  ## needed for polygon clipping
  require(gpclib)

  ## needed for voronoi tiling
  require(tripack)

  ## compute the tiling
  tt = voronoi.mosaic(pts[,1],pts[,2])

  ## convert the tripack tiling structure into something we can index into
  nt = length(tt$tri$x)
  segs = list()
  vs = list()
  for(i in 1:nt){
    verts = voronoi.findvertices(i,tt)
    if(length(verts)>0){
      vs[[i]]=verts

      for(ifirst in 1:length(vs[[i]])){
        isecond = ifirst + 1
        if(isecond > length(vs[[i]])){
          isecond = 1
        }
        tagF = paste(vs[[i]][ifirst],",",vs[[i]][isecond],sep="")
        tagB = paste(vs[[i]][isecond],",",vs[[i]][ifirst],sep="")
        if(is.null(segs[[tagF]]) & is.null(segs[[tagB]])){
          segs[[tagF]] = recurSplit(
                tt$x[verts[ifirst]],
                tt$y[verts[ifirst]],
                tt$x[verts[isecond]],
                tt$y[verts[isecond]],
                3
                )                
        }        
      }
    }
  }

  ## segs is now a list of wiggly segments indexed by "node-id,node-id" strings. 
  ## vs is the list of node vectors for each polygon
  
  mf = list(segs = segs, vs = vs)

  boundary = as(boundary,"gpc.poly")
  polys = list() ; ip = 1
  for(i in 1:length(mf$vs)){
    ## .makeLoop returns the segments in order to make a loop
    poly = .makeLoop(mf,i)
    if(length(poly)>0){
      poly = as(poly,"gpc.poly")
      ap = area.poly(poly)
      ## mode 0 - return the polygon
      if(clipmode == 0){
        polys[[ip]]=poly ; ip = ip + 1
      }
      if(clipmode == 3){
        polyb = intersect(poly,boundary)
        if(!(area.poly(polyb)<ap)){
          polys[[ip]] = poly;ip = ip+1
        }
      }
      if(clipmode == 1){
        polyb = intersect(poly,boundary)
        if(area.poly(polyb)>0){
          polys[[ip]]=poly;ip = ip + 1
        }
      }
      if(clipmode == 2){
        polyb = intersect(poly,boundary)
        polys[[ip]] = polyb;ip = ip+1
      }
    }
  }    

  xx = unlist(lapply(polys,get.bbox))

  xb = range(xx[substr(names(xx),1,1)=="x"])
  yb = range(xx[substr(names(xx),1,1)=="y"])
  
  return(list(polys = polys, bounds=boundary, points=pts, box = cbind(xb,yb)))
  
}
