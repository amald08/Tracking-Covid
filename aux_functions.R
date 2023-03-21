library(tidyverse)
minmaxnorm <- function(x){
  return((x- min(x)) /(max(x)-min(x)))
}
accum_hits<-function(x){
  x<-x$interest_over_time
  x$hits[which(x$hits =="<1" | x$hits =="< 1" )]<-0
  x<-spread(x, hits, key = "keyword")
  x<-as.data.frame(apply(x[,6:dim(x)[2]],2,as.numeric))
  X<-rowSums(x)
  return(X)
}

MM<-function(x,q){
  y<-NULL
  for(j in 1:q)
  {
    y[j]=x[1]
  }
  for( i in (q+1):(length(x)-q))
  {
    y[i]=mean(x[(i-q):(i+q)])
  }
  for(j in (length(x)-q+1):length(x))
  {
    y[j]=x[length(x)-q]
  }
  return(y)
}

normalize = function(x){
  return(x/max(x))
}
