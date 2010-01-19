splitSegMid <- function(x0,y0,x1,y1,f=0.1){
  ## choose a point at the midpoint with gaussian noise
  ## of sigma f * segment length

  xm = mean(c(x0,x1))
  ym = mean(c(y0,y1))
  sd = f*sqrt((x1-x0)^2 + (y1-y0)^2)
  th = runif(1,0,2*pi)
  return(c(xm+sd*sin(th),ym+sd*cos(th)))
  
}
