rm(list = ls())

# current_path = rstudioapi::getActiveDocumentContext()$path 
# setwd(dirname(current_path))

library(gtrendsR) # para bajar los datos
library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(plotly)
library(zoo)
library(scales)
source("aux_functions.R")

# Data Cases
url<-'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases.csv'

datos<-read.csv(url)
# Parameters for the roll mean, a = b + 1
a = 4
b = 3

mx<-datos$Mexico

inicio = Sys.Date()-200 # la api no jala más de 200 días de datos
tiempo<-c("today 3-m", "today 12-m", "today+5-y", "2015-09-30 2020-04-28",
          "2020-02-24 2020-05-01", paste(inicio, Sys.Date()))
#################################
######### GOOGLE TRENDS #########
#################################

######### TRATAMIENTO
btrat<-gtrends(c("oxígeno","precio oxígeno","respirador",
                 "medicina covid", "tanque de oxígeno"),
               geo = "MX", onlyInterest = T,               
               time = tiempo[6], hl = "es")#$interest_over_time
tratamiento<-accum_hits(btrat)
# plot(btrat)
######### PRUEBAS
bprueba<-gtrends(c("test covid","test coronavirus","prueba covid",
                   "pruebas covid", "prueba coronavirus"),
                 geo = "MX", onlyInterest = T,               
                 time = tiempo[6], hl = "es")#$interest_over_time
pruebas<-accum_hits(bprueba)
# plot(bprueba)
######### Medicamentos
bmedicamentos<-gtrends(c("dexametasona precio",
                         "ivermectina precio",
                         "dexametasona",
                         "ivermectina",
                         "medicina covid"),
                       geo = "MX", onlyInterest = T,               
                       time = tiempo[6], hl = "es")#$interest_over_time
medicamentos<-accum_hits(bmedicamentos)


# Attention to this length

Mx<-mx[(length(mx)-200):(length(mx)-201 + length(medicamentos))]

pcov = as.numeric(subset(bprueba$interest_over_time,
                         (keyword =="prueba covid"))$hits)

pcov = normalize(pcov + as.numeric(subset(bprueba$interest_over_time,
                                          (keyword =="pruebas covid"))$hits))*100


df = cbind(pcov,as.numeric(subset(btrat$interest_over_time, 
                                  (keyword =="oxígeno"))$hits),
           as.numeric(subset(bmedicamentos$interest_over_time, 
                             (keyword == "ivermectina"))$hits),
           Mx/max(Mx)*100)
df = data.frame(df)

colnames(df) = c("Pruebas Covid", "Oxígeno",
                 "Ivermectina", "N Casos")

df$Fecha = seq(as.Date(inicio),length.out = dim(df)[1], by = "days")

DF<-df %>%
  gather(key = "Label", value = "Score",-Fecha)

p_fin_raw = ggplot(DF, aes( x = Fecha, y = Score, group = Label, 
                            col = Label)) + 
  geom_line() +
  theme_classic() +
  ggtitle("Señales de búsquedas en Google") +
  labs(x = "Fecha", y = " ", col = " ")
# ggplotly(p_fin_raw)

roll = a + b

df_2 = data.frame(apply(df[,1:4], 2,rollmean, k = roll))


df_2$Fecha = seq(as.Date(inicio) + (roll-1)/2,
                 length.out = dim(df)[1]-roll +1, 
                 by = "days")
names(df_2) = names(df)

DF_2<-df_2%>%
  gather(key = "Label", value = "Score", -Fecha)

p_final = ggplot(DF_2, aes( x = Fecha, y = Score, group = Label, col = Label)) + 
  geom_line() +
  theme_classic() +
  ggtitle(" ") +
  labs(x = "Fecha", y = " ", col = " ") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = .8)) +
  scale_x_date(breaks = breaks_pretty(10))
## Apply roll mean
ggsave('casos_score_google.png', dpi = 800)

