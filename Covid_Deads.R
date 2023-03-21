rm(list = ls())

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
# print( getwd() ) to check if in the correct path

library(gtrendsR)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(plotly)
library(zoo)
library(scales)
source("aux_functions.R")

# Data Dead
urldead<-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"
datos<-read.csv(urldead)

a = 4
b = 3

mx<-datos$Mexico
it<-datos$Italy
us<-datos$United.States
ger<-datos$Germany
bra<-datos$Brazil
ind<-datos$India
indo<-datos$Indonesia
isr<-datos$Israel

fecha<-seq(as.Date("2020-01-22"), by = "days", 
           length.out = length(mx))

Y<-data.frame(fecha,mx,
              it,
              us,
              ger,
              bra,
              ind,
              indo,
              isr)

names(Y)<-c("Fecha","MX","IT","US","GER","BRA","IND","INDO","ISR")
Y[is.na(Y)] = 0

Y_1<-data.frame(Y$Fecha, Y$MX/max(Y$MX), 
                Y$US/max(Y$US), Y$IT/max(Y$IT),
                Y$GER/max(Y$GER),Y$BRA/max(Y$BRA),
                Y$IND/max(Y$IND),
                Y$INDO/max(Y$INDO),
                Y$ISR/max(Y$ISR))
names(Y_1)<-names(Y)

x<-Y%>%
  gather(key = "País", value = "Casos",-Fecha)

x_1<-Y_1%>%
  gather(key = "País", value = "Casos",-Fecha)


Fecha = fecha[a:(length(fecha)-b)]

roll = a + b
Z<-data.frame(Fecha,rollmean(Y[,2:9],roll))
names(Z)<-c("Fecha","MX","IT","US","GER","BRA","IND","INDO","ISR")

Z_1<-data.frame(Fecha,apply(Z[,2:9],2,normalize))
names(Z_1)<-names(Z)

p_roll = ggplot(Z, aes(x = 1:(length(fecha)-roll + 1), y = MX, 
                       col = "México")) + 
  geom_line() +
  geom_line(aes(y = US, col = "EUA")) +
  geom_line(aes(y = IT, col = "Italia")) +
  geom_line(aes(y = GER, col = "Alemania")) +
  geom_line(aes(y = BRA, col = "Brasil")) + 
  geom_line(aes(y = IND, col = "India")) +
  geom_line(aes(y = INDO, col = "Indonesia")) +
  geom_line(aes(y = ISR, col = "Israel")) +
  theme_classic() +
  labs(x = "Días a partir del 22/01/2020", 
       y = " ", 
       caption = "Datos: JHU (John Hopkins University),
       media móvil de 7 días.") +
  theme(axis.text.y = element_blank(),legend.title = 
          element_blank()) +
  ggtitle("Casos confirmados")

ggsave('covid_deads.png', dpi = 800)
#####

p_roll_norm = ggplot(Z_1, aes(x = 1:(length(fecha)-(roll-1)), y = MX, 
                              col = "México")) + 
  geom_line() +
  geom_line(aes(y = US, col = "EUA")) +
  geom_line(aes(y = IT, col = "Italia")) +
  geom_line(aes(y = GER, col = "Alemania")) +
  geom_line(aes(y = BRA, col = "Brasil")) +
  geom_line(aes(y = IND, col = "India")) +
  geom_line(aes(y = INDO, col = "Indonesia")) +
  geom_line(aes(y = ISR, col = "Israel")) +
  theme_classic() +
  labs(x = "Días a partir del 22/01/2020", 
       y = " ", 
       caption = "Datos: JHU (John Hopkins University),
       media móvil de 7 días.") +
  theme(axis.text.y = element_blank(), 
        legend.title = element_blank()) +
  ggtitle("Tendencias de casos confirmados")

ggsave('covid_deads_norm.png', dpi = 800)


inicio = Sys.Date()-200
tiempo<-c("today 3-m", "today 12-m", "today+5-y", "2015-09-30 2020-04-28",
          "2020-02-24 2020-05-01", paste(inicio, Sys.Date()))

box<-gtrends(c("oxígeno",
               "precio oxígeno",
               "tanque de oxígeno"),
               geo = "MX", onlyInterest = T,               
               time = tiempo[6], hl = "es")#$interest_over_time
oxigeno<-accum_hits(box)

bhosp<-gtrends(c("hospital covid",
                 "hospital cerca"),
               geo = "MX", onlyInterest = T,
               time = tiempo[6], hl = "es")#$interest_over_time
hospital<-accum_hits(bhosp)

bcorazon<-gtrends(c("cardiologo",
                 "dolor de corazon",
                 "infarto",
                 "miocarditis"),
               geo = "MX", onlyInterest = T,
               time = tiempo[6], hl = "es")#$interest_over_time
corazon<-accum_hits(bcorazon)

Mx<-mx[(length(mx)-200):(length(mx)-201 + length(oxigeno))]
X<-cbind(Mx, corazon, oxigeno, hospital)
X<-as.data.frame(X)
names(X)<-c("Muertes en México", "Corazón", "Oxígeno", "Hospital")

Y<-as.data.frame(apply(X,2,minmaxnorm))
Y$Fecha<-seq(as.Date(inicio),length.out = dim(Y)[1], by = "days")

# Y_1<-Y %>%
#   gather(key = "Query", value = "Hits",-Date)

roll = a + b

df_2 = data.frame(apply(Y[,1:4], 2,rollmean, k = roll))
df_2$Fecha = seq(as.Date(inicio) + (roll-1)/2,
                 length.out = dim(Y)[1]-roll +1, 
                 by = "days")
names(df_2) = names(Y)

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

ggsave('covid_deads_google.png', dpi = 800)

