# Livrarias
library(nycflights13)   # Importacao da livraria que contem os BDs
library(dplyr)         # Manipulacao dos Bancos de Dados
library(lubridate)      # Manipulacao das Datas


# Visualizacao dos BDs
View(flights)
View(airports)
View(airlines)
View(planes)
View(weather)


# --------------------------------------------------------------------------------------------------------------------
# Tratativa dos Dados da Base Principal
# Siglas dos aeroportos
# John F Kennedy Intl   = JFK
# La Guardia            = LGA
# Newark Liberty Intl   = EWR

df_voos <- flights %>% filter(origin == 'JFK' ||
                              origin == 'LGA' ||
                              origin == 'EWR')


# Quantidade Total de Voos por Aeroporto
qtde_voos_total_JFK <- df_voos %>% filter(origin == 'JFK') %>% count(); qtde_voos_total_JFK <- as.numeric(qtde_voos_total_JFK[1,1])
qtde_voos_total_LGA <- df_voos %>% filter(origin == 'LGA') %>% count(); qtde_voos_total_LGA <- as.numeric(qtde_voos_total_LGA[1,1])
qtde_voos_total_EWR <- df_voos %>% filter(origin == 'EWR') %>% count(); qtde_voos_total_EWR <- as.numeric(qtde_voos_total_EWR[1,1])

class(qtde_voos_total_JFK)

# Quantidade de Voos Atrasados por Aeroporto de Origem
qtde_voos_atra_JFK <- df_voos %>% filter(dep_delay > 0, origin == 'JFK') %>% count(); qtde_voos_atra_JFK <- as.numeric(qtde_voos_atra_JFK[1,1])
qtde_voos_atra_LGA <- df_voos %>% filter(dep_delay > 0, origin == 'LGA') %>% count(); qtde_voos_atra_LGA <- as.numeric(qtde_voos_atra_LGA[1,1])
qtde_voos_atra_EWR <- df_voos %>% filter(dep_delay > 0, origin == 'EWR') %>% count(); qtde_voos_atra_EWR <- as.numeric(qtde_voos_atra_EWR[1,1])

# Percentual de Voos Atrasados
perc_atraso_JFK <- (qtde_voos_atra_JFK / qtde_voos_total_JFK) * 100
perc_atraso_LGA <- (qtde_voos_atra_LGA / qtde_voos_total_LGA) * 100
perc_atraso_EWR <- (qtde_voos_atra_EWR / qtde_voos_total_EWR) * 100

# Criacao da Tabela para Visualizacao Simplificada
tabela_atraso <- data.frame(aeroportos = c('JFK','LGA','EWR'),
                            qtde_total_voos = c(qtde_voos_total_JFK, qtde_voos_total_LGA, qtde_voos_total_EWR),
                            qtde_atraso_voos =c(qtde_voos_atra_JFK, qtde_voos_atra_LGA, qtde_voos_atra_EWR),
                            perc_atraso = c(perc_atraso_JFK, perc_atraso_LGA,perc_atraso_EWR))

View(tabela_atraso)
