.makeLoop <- function(mf,i){

  li = mf$vs[[i]]
  if(length(li)==0){
    return(NULL)
  }

  coords=list()

  for(ii in 1:length(li)){
    ifirst = li[ii]
    if(ii == length(li)){
      isecond=li[1]
    }else{
      isecond = li[ii+1]
    }
    tagF = paste(ifirst,",",isecond,sep="")
    tagB = paste(isecond,",",ifirst,sep="")

    if(length(mf$segs[[tagF]])>0){
      coords[[ii]] = mf$segs[[tagF]]
    }else{
      nodes = mf$segs[[tagB]]
      coords[[ii]] = nodes[nrow(nodes):1,]
    }
  }
  return(do.call("rbind",coords))
}
