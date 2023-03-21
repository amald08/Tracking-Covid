rm(list = ls())
library(rjson)
url<-"https://covid.ourworldindata.org/data/owid-covid-data.json"
datos<-fromJSON(file = url)

df<-as.data.frame(datos)

