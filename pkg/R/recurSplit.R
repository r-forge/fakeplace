recurSplit <- function(x0,y0,x1,y1,n,...){

  segs = matrix(c(x0,y0,x1,y1,0),ncol=5)
  for(i in 1:n){
    unSplit = (1:nrow(segs))[segs[,5]==0]
    
    for(i in unSplit){
      nmid = splitSegMid(segs[i,1],segs[i,2],segs[i,3],segs[i,4])
      segs[i,5]=1
      segs=rbind(segs,
        c(segs[i,1],segs[i,2],nmid[1],nmid[2],0),
        c(nmid[1],nmid[2],segs[i,3],segs[i,4],0)
        )
    }
    
  }
  keep = segs[segs[,5]==0,]

  return(rbind(keep[,1:2],keep[nrow(keep),3:4]))
}
