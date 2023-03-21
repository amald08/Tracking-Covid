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
source("Covid_Third_Aux.R")

url_cases<-'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases.csv' 
url_dead<-'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv'
url_vaccines<-'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv'

countries = c('Mexico', 'Italy', 'United.States', 
         'Germany', 'Brazil', 'Indonesia', 'Israel', 'Netherlands', 'Belgium')

start_date = "2020-01-22"

# Confirmed Cases

Y = countries_data(countries, url_cases)
X = tidy_data(Y, start_date)
X_2 = tidy_data(Y, start_date, norms = F)

Z<-X %>%
  gather(key = "País", value = "Cases",-Fecha)

Z_1<-X_2 %>%
  gather(key = 'País', value = 'Cases',-Fecha)

p_1 = ggplot(Z, aes( x = Fecha, y = Cases, group = País, col = País)) +
  geom_line() +
  theme_classic() +
  labs(x = "Días a partir del 22/01/2020", 
       y = " ", 
       caption = "Datos: JHU (John Hopkins University),
       media móvil de 7 días.") +
  theme(axis.text.y = element_blank(),legend.title = 
          element_blank()) +
  ggtitle("Casos Confirmados Normalizados")
ggsave(paste0('ConfirmadosNorm',Sys.Date(),'.png'), dpi = 800)

p_2 = ggplot(Z_1, aes( x = Fecha, y = Cases, group = País, col = País)) +
  geom_line() +
  theme_classic() +
  labs(x = "Días a partir del 22/01/2020", 
       y = " ", 
       caption = "Datos: JHU (John Hopkins University),
       media móvil de 7 días.") +
  theme(axis.text.y = element_blank(),legend.title = 
          element_blank()) +
  ggtitle("Casos Confirmados")
ggsave(paste0('Confirmados',Sys.Date(),'.png'), dpi = 800)
# Confirmed Dead

Y = countries_data(countries, url_dead)
X = tidy_data(Y, start_date)
X_2 = tidy_data(Y, start_date, norms = F)

Z<-X %>%
  gather(key = "País", value = "Cases",-Fecha)

Z_1<-X_2 %>%
  gather(key = 'País', value = 'Cases',-Fecha)

p_3 = ggplot(Z, aes( x = Fecha, y = Cases, group = País, col = País)) +
  geom_line() +
  theme_classic() +
  labs(x = "Días a partir del 22/01/2020", 
       y = " ", 
       caption = "Datos: JHU (John Hopkins University),
       media móvil de 7 días.") +
  theme(axis.text.y = element_blank(),legend.title = 
          element_blank()) +
  ggtitle("Muertes Confirmadas Normalizadas")
ggsave(paste0('MuertesNorm',Sys.Date(),'.png'), dpi = 800)

p_4 = ggplot(Z_1, aes( x = Fecha, y = Cases, group = País, col = País)) +
  geom_line() +
  theme_classic() +
  labs(x = "Días a partir del 22/01/2020", 
       y = " ", 
       caption = "Datos: JHU (John Hopkins University),
       media móvil de 7 días.") +
  theme(axis.text.y = element_blank(),legend.title = 
          element_blank()) +
  ggtitle("Muertes Confirmadas")
ggsave(paste0('Muertes',Sys.Date(),'.png'), dpi = 800)
# Google Trends

inicio = Sys.Date()-200
tiempo<-c(paste(start_date, inicio-404),paste(inicio-403,inicio-202),
          paste(inicio-201,inicio-1), paste(inicio, Sys.Date()))

w_oxigen = c('oxígeno', 'precio oxígeno', 'tanque de oxígeno',
             'comprar oxígeno')
w_hosp = c('hospital cerca', 'hospital covid')
w_heart = c("cardiologo", "dolor de corazon", "infarto", "miocarditis")
w_tos = c("tos", "tos seca", "remedios para la tos", "como curar la tos")
w_fiebre = c("fiebre","calentura","temperatura")
w_headache = c("dolor de cabeza", "migraña", "me duele la cabeza",
               "quitar dolor de cabeza")
w_fatiga = c("fatiga", "cansancio", "agotamiento", "estoy muy cansado",
             "cuerpo pesado")
w_garganta = c("garganta inflamada","dolor de garganta",
               "tos con flemas", "me duele la garganta",
               "flemas")
w_dolmusc = c("me duele el cuerpo","cuerpo cortado","reumas",
              "dolor muscular")
w_anosmia = c("anosmia","no puedo oler","perdida del olfato",
              "no huelo nada", "nada huele")
w_covid = c("coronavirus","covid19","covid-19","covid","covid 19")
w_difresp = c("dificultad para respirar", "no puedo respirar", "dificil respirar")
w_sint = c("sintomas coronavirus","sintomas covid")
w_cura = c("cura coronavirus","cura covid", "hospital covid",
           "covid hospital")
w_test = c("test covid","test coronavirus","prueba covid",
           "pruebas covid", "prueba coronavirus")
w_iver = c("ivermectina precio", "ivermectina", "medicina covid")
w_meds = c("zinc", "vitamina d", "donde comprar zinc", 'comprar vitamina D',
           'que es la vitamina d')

# Mexico Data

cases_mex = countries_data('Mexico', url_cases)
dead_mex = countries_data('Mexico', url_dead)

# Google and Confirmed Cases

X = data.frame(Oxígeno = signals(w_oxigen, tiempo),
               Ivermectina = signals(w_iver, tiempo),
               Tests = signals(w_test, tiempo),
               Remedios = signals(w_meds, tiempo),
               Casos = cases_mex$Mexico[1:(length(signals(w_oxigen, tiempo)))])

X = tidy_data(X,start_date)

Z<-X %>%
  gather(key = "Query", value = "Hits",-Fecha)

p_5 = ggplot(Z, aes(x = Fecha, y = Hits, group = Query, col = Query)) +
  geom_line() +
  theme_classic() +
  labs(x = "Días a partir del 22/01/2020", 
       y = " ", 
       caption = "Datos: Google Trends y JHU,
       media móvil de 7 días.") +
  theme(axis.text.y = element_blank(),legend.title = 
          element_blank()) +
  ggtitle("Casos Confirmados y señales de búsquedas")
ggsave(paste0('SeñalesConfirmados',Sys.Date(),'.png'), dpi = 800)

# Google and Deads

X = data.frame(Oxigen = signals(w_oxigen, tiempo),
               # Heart = signals(w_heart, tiempo),
               Hospital = signals(w_hosp, tiempo),
               Deads = dead_mex$Mexico[1:(length(signals(w_oxigen, tiempo)))])

X = tidy_data(X,start_date)

Z<-X %>%
  gather(key = "Query", value = "Hits",-Fecha)

p_6 = ggplot(Z, aes(x = Fecha, y = Hits, group = Query, col = Query)) +
  geom_line() +
  theme_classic() +
  labs(x = "Días a partir del 22/01/2020", 
       y = " ", 
       caption = "Datos: Google Trends y JHU,
       media móvil de 7 días.") +
  theme(axis.text.y = element_blank(),legend.title = 
          element_blank()) +
  ggtitle("Muertes y señales de búsquedas")
ggsave(paste0('SeñalesMuertos',Sys.Date(),'.png'), dpi = 800)

plots = list(CConfNorm = p_1, CConf = p_2, MConfNorm = p_3, MConf = p_4,
             GoogleConf = p_5, GoogleDead = p_6)

# d_dead = diff(dead_mex$Mexico)
# d_cases = diff(cases_mex$Mexico)
# 
# plot(d_dead[d_dead>0], type = 'l')
# lines(d_cases[d_cases>0], type = 'l', col = 'red')
# 
# plot(abs(d_dead), type = 'l')
# lines(abs(d_cases), type = 'l', col = 'red')
# 
# plot(minmaxnorm(d_dead[d_dead>0]))
# lines(minmaxnorm(d_cases[d_cases>0]), col = 'red')
# 
# plot(minmaxnorm(abs(d_dead)), type = 'l')
# lines(minmaxnorm(abs(d_cases)), col = 'red')

cociente = data.frame(Casos = cases_mex$Mexico,
                      Muertes = dead_mex$Mexico,
                      Tasa = dead_mex$Mexico/cases_mex$Mexico)

cociente = tidy_data(cociente, start_date, norms = F)
cociente$Tasa = cociente$Muertes/cociente$Casos

p_7 = ggplot(cociente, aes(y = Tasa, x = Fecha)) +
  geom_line(aes(col = 3)) +
  theme_classic() +
  labs(x = "Días a partir del 22/01/2020", 
       y = 'Muertes/Casos confirmados', 
       caption = "Datos: Google Trends y JHU,
       media móvil de 7 días.") +
  theme(legend.title = 
          element_blank(), legend.position = 'none') +
  ggtitle("Tasa de muertes vs casos confirmados")
ggsave(paste0('TasaMuerte',Sys.Date(),'.png'), dpi = 800)
  

