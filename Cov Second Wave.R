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

# Data Cases
url_mx<-'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases.csv'

datos_mx<-read.csv(url_mx)

# Parameters for the roll mean, a = b + 1
a = 4
b = 3

mx<-datos_mx$Mexico
it<-datos_mx$Italy
us<-datos_mx$United.States
ger<-datos_mx$Germany
bra<-datos_mx$Brazil
ind<-datos_mx$India
indo<-datos_mx$Indonesia
isr<-datos_mx$Israel

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
names(Y_1)<-c("Fecha","MX","US","IT","GER","BRA","IND", "INDO", "ISR")

x<-Y%>%
  gather(key = "País", value = "Casos",-Fecha)
# x$Casos[which(is.na(x$Casos)==T)]<-0

x_1<-Y_1%>%
  gather(key = "País", value = "Casos",-Fecha)
# x_1$Casos[which(is.na(x_1$Casos)==T)]<-0

p_conf = ggplot(x, aes(x = Fecha, y = Casos, col = País)) +
  geom_line() +
  theme_classic() +
  labs(caption = "Datos: ECDC (European Center for Disease Prevention and Control).") +
  theme(axis.text.y = element_blank(), 
        legend.title = element_blank()) +
  ggtitle("Casos confirmados") 

p_conf_norm = ggplot(x_1, aes(x = Fecha, y = Casos, col = País)) +
  geom_line() +
  theme_classic() +
  ggtitle("Casos confirmados normalizados")

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

ggsave('covid_casos_conf.png', dpi = 800)
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

ggsave('covid_casos_conf_roll.png', dpi = 800)

# plot(fecha, mx, type = "l", col = "blue", lwd = 2, 
#      main = "Casos Coronavirus M?xico", ylab = "No.Casos", xlab = "" )

# news<-read.csv("Corona News 2505.csv")[27:81,]
inicio = Sys.Date()-200
tiempo<-c("today 3-m", "today 12-m", "today+5-y", paste(inicio-300,inicio-202),
          paste(inicio-201,inicio-1), paste(inicio, Sys.Date()))
# tiempo[6]
# Symptoms
btos<-gtrends(c("tos","tos seca","remedios para la tos", "como curar la tos",
                "toz"), geo = "MX", onlyInterest = T,               
              time = tiempo[6], hl = "es")#$interest_over_time
tos<-accum_hits(btos)
# plot(btos)
############## FIEBRE
bfiebre<-gtrends(c("fiebre","calentura","temperatura"), geo = "MX", onlyInterest = T,               
                 time = tiempo[6],hl = "es")#$interest_over_time
fiebre<-accum_hits(bfiebre)

# plot(bfiebre)
############## DOLOR DE CABEZA
bdcabeza<-gtrends(c("dolor de cabeza","migra?a","me duele la cabeza",
                    "quitar dolor de cabeza"), geo = "MX", onlyInterest = T,               
                  time = tiempo[6],hl = "es")#$interest_over_time
dcabeza<-accum_hits(bdcabeza)
# plot(bdcabeza)
############## FATIGA
bfatiga<-gtrends(c("fatiga","cansancio","agotamiento", "estoy muy cansado",
                   "cuerpo pesado"), geo = "MX", onlyInterest = T,               
                 time = tiempo[6],hl = "es")#$interest_over_time
fatiga<-accum_hits(bfatiga)
# plot(bfatiga)
############## GARGANTA INFLAMADA
bgarganta<-gtrends(c("garganta inflamada","dolor de garganta",
                     "tos con flemas", "me duele la garganta",
                     "flemas"), geo = "MX", onlyInterest = T,               
                   time = tiempo[6], hl = "es")#$interest_over_time
garganta<-accum_hits(bgarganta)
# plot(bgarganta)
############## DOLOR MUSCULAR
bdmusc<-gtrends(c("me duele el cuerpo","cuerpo cortado","reumas",
                  "dolor muscular"),
                geo = "MX", onlyInterest = T,               
                time = tiempo[6], hl = "es")#$interest_over_time
dmusc<-accum_hits(bdmusc)
# plot(bdmusc)
############## ANOSMIA
banosmia<-gtrends(c("anosmia","no puedo oler","perdida del olfato",
                    "no huelo nada", "nada huele"),
                  geo = "MX", onlyInterest = T,               
                  time = tiempo[6], hl = "es")#$interest_over_time
anosmia<-accum_hits(banosmia)
# plot(banosmia)
############## COVID
bcovid<-gtrends(c("coronavirus","covid19","covid-19","covid","covid 19"),
                geo = "MX", onlyInterest = T,               
                time = tiempo[6], hl = "es")#$interest_over_time
covid<-accum_hits(bcovid)
# plot(bcovid)
######### DIF RESPIRAR
bdifresp<-gtrends(c("dificultad para respirar",
                    "no puedo respirar", "dificil respirar"
                    ), geo = "MX", onlyInterest = T,
                  time = tiempo[6], hl = "es")#$interest_over_time
difresp<-accum_hits(bdifresp)
# plot(bdifresp)
######### SINTOMAS
bsintomas<-gtrends(c("sintomas coronavirus","sintomas covid"),
                   geo = "MX", onlyInterest = T,               
                   time = tiempo[6], hl = "es")#$interest_over_time
sintomas<-accum_hits(bsintomas)
# plot(bsintomas)
# View(bsintomas$interest_over_time)
######### CURA
bcura<-gtrends(c("cura coronavirus","cura covid", "hospital covid",
                 "covid hospital"),
               geo = "MX", onlyInterest = T,               
               time = tiempo[6], hl = "es")#$interest_over_time
cura<-accum_hits(bcura)
# plot(bcura)
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
# plot(bmedicamentos)
######### Hospitales
bhosp<-gtrends(c("hospital covid",
                 "hospital cerca",
                 "hospital lleno"),
               geo = "MX", onlyInterest = T,
               time = tiempo[6], hl = "es")#$interest_over_time
hospital<-accum_hits(bhosp)
#plot(bhosp)

X<-cbind(tos, fiebre, dcabeza, fatiga, garganta, dmusc, anosmia,
         difresp, sintomas, covid)
names(X)<-c("Tos","Fiebre","D_Cabeza","Fatiga","Garganta","D_Musc",
            "Anosmia","Dif_Resp","Sintomas", "Covid")
X<-as.data.frame(X)

Y<-as.data.frame(apply(X,2,minmaxnorm))
Y$Date<-seq(as.Date(inicio),length.out = dim(Y)[1], by = "days")

Y_1<-Y %>%
  gather(key = "Query", value = "Hits",-Date)
p5<-ggplot(Y_1, aes(x = Date ,y = Hits, group = Query)) + 
  geom_line(aes(col = Query, linetype = Query))
# ggplotly(p5)

Y<-as.data.frame(apply(X,2,minmaxnorm))
Y_2<-apply(Y, 1, sum)
# Change for the correct number of searches

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

# Apply roll mean
ggsave('casos_score_google.png', dpi = 800)

