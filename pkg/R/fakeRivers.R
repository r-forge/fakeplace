fakeRivers <- function(n=30,pts=cbind(runif(n),runif(n))){

  require(igraph)

  require(tripack)

  tt = voronoi.mosaic(pts[,1],pts[,2])

  nt = length(tt$tri$x)
  edges = list();ip=1
  segs=list()
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
        FTnodes = c(vs[[i]][ifirst], vs[[i]][isecond])
        Fnode=min(FTnodes)
        Tnode=max(FTnodes)
#        cat(ifirst,"->",isecond,"\n",sep="")
        tagF = paste(Fnode-1,",",Tnode-1,sep="")
        tagB = paste(Tnode-1,",",Fnode-1,sep="")
        if(is.null(segs[[tagF]]) & is.null(segs[[tagB]])){
          edges[[ip]]=c(Fnode-1,Tnode-1);ip=ip+1
          segs[[tagF]]=recurSplit(
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
  edges = do.call(rbind,edges)
  row.names(edges) <- paste(edges[,1],",",edges[,2],sep="")
  edges = edges[!duplicated(row.names(edges)),]
  eg = graph.empty()
  eg = add.vertices(eg,max(edges)+1)
  for(i in 1:nrow(edges)){
    dx = tt$x[edges[i,1]+1]-tt$x[edges[i,2]+1]
    dy = tt$y[edges[i,1]+1]-tt$y[edges[i,2]+1]
    d2 = dx^2 + dy^2
    eg = add.edges(eg,edges[i,],attr=list(weight=d2*d2*d2))
  }
  #  edges = graph.edgelist(edges,directed=FALSE)
  edges = eg
  tree = minimum.spanning.tree(edges,algorithm="prim")
  treelist = get.edgelist(tree)
  treeI = paste(treelist[,1],",",treelist[,2],sep="")
  treeSegs = segs[treeI]
  return(treeSegs)
}
