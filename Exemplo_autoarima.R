
#### Trabalho - Análise de Séries Temporais - COVID

library(tseries)
library(forecast)
library(lubridate) 
library(readxl)
library(dplyr)
setwd("C:/FGV/Analise de Series Temporais/Aula02")
df_covid <- read_excel("Dados_estados.xlsx")

######## - SC - ###############
df_covid_sc <- df_covid %>% select(Casos_Confirmados, Mortes_Confirmadas) %>% filter(df_covid$UF == 'SC')
data <- ts(df_covid_sc, start = decimal_date(ymd("2020-03-12")),frequency = 365) 
plot(data)

# Casos Confirmados
casos_confirmados <- ts(df_covid_sc$Casos_Confirmados, start = decimal_date(ymd("2020-03-12")), 
           frequency = 365) 
mod_casos_confirmados=auto.arima(casos_confirmados)
mod_casos_confirmados

fut= forecast(mod_casos_confirmados, 30) 
fut
plot(fut)

# ARIMA(2,2,2)
# X(t) = 1.2347 x(t-1) - 0.7444 x(t-2) - 1.6900 ep(t-1) + 0,9310 ep(t-2) 

# MORTES
mortes_confirmadas <- ts(df_covid_sc$Mortes_Confirmadas, start = decimal_date(ymd("2020-03-12")), 
                        frequency = 365) 
mod_mortes_confirmadas=auto.arima(mortes_confirmadas)
mod_mortes_confirmadas

fut= forecast(mod_mortes_confirmadas, 30) 
fut
plot(fut)

# ARIMA(2,2,4)
# X(t) = -0.3361 x(t-1) + 0.4308 x(t-2) - 0.5979 ep(t-1) - 0.6500 ep(t-2) + 0.1646 ep(t-3) + 0.4607 ep(t-4)


########### - PR - ###############

df_covid_pr <- df_covid %>% select(Casos_Confirmados, Mortes_Confirmadas) %>% filter(df_covid$UF == 'PR')
data <- ts(df_covid_pr, start = decimal_date(ymd("2020-03-12")),frequency = 365) 
plot(data)

# Casos Confirmados
casos_confirmados <- ts(df_covid_pr$Casos_Confirmados, start = decimal_date(ymd("2020-03-12")), 
                        frequency = 365) 
mod_casos_confirmados=auto.arima(casos_confirmados)
mod_casos_confirmados

fut= forecast(mod_casos_confirmados, 30) 
fut
plot(fut)

# ARIMA(0,2,1)
# X(t) = -0,7026 ep(t-1) 

# MORTES
mortes_confirmadas <- ts(df_covid_pr$Mortes_Confirmadas, start = decimal_date(ymd("2020-03-12")), 
                         frequency = 365) 
mod_mortes_confirmadas=auto.arima(mortes_confirmadas)
mod_mortes_confirmadas

fut= forecast(mod_mortes_confirmadas, 30) 
fut
plot(fut)

# ARIMA(3,2,2)
# X(t) = -0.7571 x(t-1) - 0.0781 x(t-2) - 0.4934 x(t-3) - 1.5043 ep(t-1) + 0.7113 ep(t-2)

########### - RS - ###############

df_covid_rs <- df_covid %>% select(Casos_Confirmados, Mortes_Confirmadas) %>% filter(df_covid$UF == 'RS')
data <- ts(df_covid_rs, start = decimal_date(ymd("2020-03-10")),frequency = 365) 
plot(data)

# Casos Confirmados
casos_confirmados <- ts(df_covid_rs$Casos_Confirmados, start = decimal_date(ymd("2020-03-10")), 
                        frequency = 365) 
mod_casos_confirmados=auto.arima(casos_confirmados)
mod_casos_confirmados

fut= forecast(mod_casos_confirmados, 30) 
fut
plot(fut)

# ARIMA(2,2,1)
# X(t) = 0.4239 x(t-1) - 0.3692 x(t-2) -0,8513 ep(t-1) 

# MORTES
mortes_confirmadas <- ts(df_covid_rs$Mortes_Confirmadas, start = decimal_date(ymd("2020-03-18")), 
                         frequency = 365) 
mod_mortes_confirmadas=auto.arima(mortes_confirmadas)
mod_mortes_confirmadas

fut= forecast(mod_mortes_confirmadas, 30) 
fut
plot(fut)

# ARIMA(0,2,1)
# X(t) = - 0.7552 ep(t-1)
