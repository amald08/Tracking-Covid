library(gtrendsR)
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

normalize = function(x){
  return(x/max(x))
}

signal = function(words, time, place = "MX"){
  trends = gtrends(words, geo = place, onlyInterest = T,               
                   time = time, hl = "es")
  trend<-accum_hits(trends)
  return(trend)
}

signals = function(words, times, place = "MX"){
  x = NULL
  for( i in 1:length(times)){
    y = signal(words, times[i], place = place)
    x = c(x,y)
  }
  return(x)
}
# Data without deaths or cases, we have to add them later, remove date

tidy_data = function(df, date_ini, norms = T, a = 4, b = 3){
  
  roll = a + b
  Y = data.frame(apply(df, 2,rollmean, k = roll))
  
  if(norms == T){
    Y<-data.frame(apply(Y,2,normalize))
    names(Y) = names(df)
    Y$Fecha = seq(as.Date(date_ini) + (roll-1)/2,
                  length.out = dim(Y)[1], 
                  by = "days")
  }
  else{
    names(Y) = names(df)
    Y$Fecha = seq(as.Date(date_ini) + (roll-1)/2,
                  length.out = dim(Y)[1],
                  by = "days")
  }
  return(Y)
}

countries_data = function(names, url){
  df = read.csv(url)
  df = df[names]
  df[is.na(df)] = 0
  return(df)
}


