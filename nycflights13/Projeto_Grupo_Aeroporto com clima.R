# Livrarias
library(nycflights13)   # Importacao da livraria que contem os BDs
library(dplyr)         # Manipulacao dos Bancos de Dados
library(lubridate)      # Manipulacao das Datas
library(summarytools)

# Visualizacao dos BDs
View(airports)
View(airlines)
View(planes)
View(weather)
View(flights)

# --------------------------------------------------------------------------------------------------------------------
# Tratativa dos Dados da Base Principal
# Siglas dos aeroportos
# John F Kennedy Intl   = JFK
# La Guardia            = LGA
# Newark Liberty Intl   = EWR

df_voos <- flights %>% filter(origin == 'JFK' ||
                              origin == 'LGA' ||
                              origin == 'EWR',
                              is.na(dep_delay) != TRUE,
                              is.na(dep_time) != TRUE,
                              is.na(arr_time) != TRUE,
                              is.na(arr_delay) != TRUE,
                              is.na(tailnum) != TRUE,
                              is.na(air_time) != TRUE)
                              
###### -- conta NAs
colSums(is.na(df_voos))


###### ----- Categorizar voos atrasados INCLUSÃO tipo ---------

df_voos <- df_voos %>% mutate(tipo = ifelse(dep_delay <= 15, "on time", "delayed"))

View(df_voos)



# --------------------------------------------------------------------------------------------------------------------
# 2ª QUESTÃO | Finalizdo
# Quantidade Total de Voos por Aeroporto
qtde_voos_total_JFK <- df_voos %>% filter(origin == 'JFK') %>% count(); qtde_voos_total_JFK <- as.numeric(qtde_voos_total_JFK[1,1])
qtde_voos_total_LGA <- df_voos %>% filter(origin == 'LGA') %>% count(); qtde_voos_total_LGA <- as.numeric(qtde_voos_total_LGA[1,1])
qtde_voos_total_EWR <- df_voos %>% filter(origin == 'EWR') %>% count(); qtde_voos_total_EWR <- as.numeric(qtde_voos_total_EWR[1,1])

class(qtde_voos_total_JFK)

# Quantidade de Voos Atrasados por Aeroporto de Origem
qtde_voos_atra_JFK <- df_voos %>% filter(dep_delay > 15, origin == 'JFK') %>% count(); qtde_voos_atra_JFK <- as.numeric(qtde_voos_atra_JFK[1,1])
qtde_voos_atra_LGA <- df_voos %>% filter(dep_delay > 15, origin == 'LGA') %>% count(); qtde_voos_atra_LGA <- as.numeric(qtde_voos_atra_LGA[1,1])
qtde_voos_atra_EWR <- df_voos %>% filter(dep_delay > 15, origin == 'EWR') %>% count(); qtde_voos_atra_EWR <- as.numeric(qtde_voos_atra_EWR[1,1])

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

# --------------------------------------------------------------------------------------------------------------------
# 3ª QUESTÃO | 
df_voos_atrasados <- df_voos

# Sem tratativa dos outliers
boxplot(df_voos_atrasados$dep_delay,
        horizontal = T,
        main = 'Análise Univariada | Distribuição de Atrasos', cex.main = 1.0,
        xlab = 'Total',
        ylab = 'Atraso em Minutos', cex.axis = 0.8,
        col = 'gray', border = 'black',
        outline = TRUE)

# Com tratativa dos outliers
boxplot(df_voos_atrasados$dep_delay,
        horizontal = F,
        main = 'Análise Univariada | Distribuição de Atrasos', cex.main = 1.0,
        xlab = 'Total',
        ylab = 'Atraso em Minutos', cex.axis = 0.8,
        col = 'gray', border = 'black',
        outline = FALSE)

# Sem tratativa dos outliers por Aeroporto
boxplot(dep_delay ~ origin,
        data = df_voos_atrasados,
        horizontal = F,
        main = 'Análise Bivariada | Distribuição de Atrasos', cex.main = 1.0,
        xlab = 'Total',
        ylab = 'Atraso em Minutos', cex.axis = 0.8,
        col = c('chocolate', 'forestgreen', 'tomato'), border = 'black',
        outline = TRUE)

# Com tratativa dos outliers por Aeroporto
boxplot(dep_delay ~ origin,
        data = df_voos_atrasados,
        horizontal = F,
        main = 'Análise Bivariada | Distribuição de Atrasos', cex.main = 1.0,
        xlab = 'Total',
        ylab = 'Atraso em Minutos', cex.axis = 0.8,
        col = c('chocolate', 'forestgreen', 'tomato'), border = 'black',
        outline = FALSE)

# Indicadores por Aeroporto
df_voos_atrasados %>% group_by(origin) %>% summarise(count = n(),
                                                     mean = mean(dep_delay, na.rm = TRUE),
                                                     sd = sd(dep_delay, na.rm = TRUE),
                                                     var = var(dep_delay, na.rm = TRUE))

# Verificação se a médias são semelhantes por meio do teste ANOVA (base com outliers)
one_way_anova_aeroporto <- aov(dep_delay ~ origin, data = df_voos_atrasados)
summary(one_way_anova_aeroporto)

# Teste T por Aeroporto
jfk <- df_voos_atrasados %>% filter(origin == 'JFK') %>% select(dep_delay)
ewr <- df_voos_atrasados %>% filter(origin == 'EWR') %>% select(dep_delay)
lga <- df_voos_atrasados %>% filter(origin == 'LGA') %>% select(dep_delay)

jfk <- as.vector(jfk[,'dep_delay'])
ewr <- as.vector(ewr[,'dep_delay'])
lga <- as.vector(lga[,'dep_delay'])

t.test(jfk, ewr, alternative = 'two.sided')
t.test(jfk, lga, alternative = 'two.sided')
t.test(lga, ewr, alternative = 'two.sided')

# --------------------------------------------------------------------------------------------------------------------
#Acrescimo da tabela de compania areas
df_voos_atrasados <- df_voos_atrasados %>% left_join(airlines, by = 'carrier')

unique(df_voos_atrasados$name)

# Sem tratativa dos outliers por Compania
par(mar=c(10,3,2,1))
boxplot(dep_delay ~ name,
        data = df_voos_atrasados,
        horizontal = F,
        main = 'Análise Bivariada | Distribuição de Atrasos', cex.main = 1.0,
        xlab = '',
        ylab = 'Atraso em Minutos', cex.axis = 0.8, las = 2,
        col = c('chocolate', 'forestgreen', 'tomato'), border = 'black',
        outline = TRUE)

# Com tratativa dos outliers por Compania
par(mar=c(10,3,2,1))
boxplot(dep_delay ~ name,
        data = df_voos_atrasados,
        horizontal = F,
        main = 'Análise Bivariada | Distribuição de Atrasos', cex.main = 1.0,
        xlab = '',
        ylab = 'Atraso em Minutos', cex.axis = 0.8, las = 2,
        col = c('chocolate', 'forestgreen', 'tomato'), border = 'black',
        outline = FALSE)

# Indicadores por Compania
df_voos_atrasados %>% group_by(name) %>% summarise(count = n(),
                                                   mean = mean(dep_delay, na.rm = TRUE),
                                                   sd = sd(dep_delay, na.rm = TRUE),
                                                   var = var(dep_delay, na.rm = TRUE))

one_way_anova_compania <- aov(dep_delay ~ name, data = df_voos_atrasados)
summary(one_way_anova_compania)

# --------------------------------------------------------------------------------------------------------------------
# 4ª QUESTÃO
# Tratativa de Data do Voos_Total
df_voos$time_hour <- ymd_hms(df_voos$time_hour)
df_voos <- df_voos %>% mutate(dia = day(time_hour),
                              ano = year(time_hour),
                              dia_semana = wday(time_hour, label = T))

vetor_total_dia <- df_voos %>% select(dia_semana) %>% group_by(dia_semana) %>% summarise(n())
vetor_total_dia <- as.vector(vetor_total_dia[,'dia_semana'])


# Tratativa de Data do Voos em Atraso
df_voos_atrasados$time_hour <- ymd_hms(df_voos_atrasados$time_hour)
df_voos_atrasados <- df_voos_atrasados %>% mutate(dia = day(time_hour),
                                                  mes = month(time_hour),
                                                  ano = year(time_hour),
                                                  dia_semana = wday(time_hour, label = T))



# --------------------------------------------------------------------------------------------------------------------
# ANALISE DE CORTE DE ATRASOS DE DECOLAGEM
# Análise dos atrasos de decolagem
bd <- df_voos %>% filter(dep_delay > 0) %>% group_by(dep_delay) %>% summarise(count = n(),
                                                                        mean = mean(dep_delay, na.rm = TRUE),
                                                                        sd = sd(dep_delay, na.rm = TRUE),
                                                                        var = var(dep_delay, na.rm = TRUE))
View(bd)

bp_total <- bd %>% summarise(sum(count))
bp <- bd %>% filter(dep_delay < 15) %>% summarise(sum(count))
bp_total <- as.numeric(bp_total[1,1])
bp <- as.numeric(bp[1,1])
bp; bp_total

percentual_desprezado <- (bp / bp_total) * 100
percentual_desprezado

View(bp)

boxplot(dep_delay ~ origin, data = df_atrasos,
        main = 'Análise Bivariada | Atraso por Aeroporto', cex.main = 1.0,
        xlab = 'Aeroporto',
        ylab = 'Atraso', cex.axis = 0.8,
        ylim = c(0, 1350), col = 'gray', border = 'black')

df_atras <- df_voos %>% filter(origin == 'JFK' ||
                                 origin == 'LGA' ||
                                origin == 'EWR',
                                dep_delay > 0)

summary(df_atras$dep_delay)      # 15 minutos de média de atraso na decolagem
# --------------------------------------------------------------------------------------------------------------------

#Avaliação do clima

df_clima <- df_voos %>% inner_join (weather, by = c('origin','time_hour'))
View (df_clima)

#Avaliação de atraso por temperatura

df_temp<- df_clima %>% filter(temp != 'NA',
                              dep_delay < 180,
                              dewp != 'NA',
                              humid != 'NA',
                              precip != 'NA',
                              visib != 'NA',
                              pressure != 'NA',
                              wind_dir !='NA',
                              wind_speed != 'NA')

View(df_temp)
                             

temp_ontime <- df_temp %>% filter(tipo == 'on time') %>% select(temp)
temp_late <- df_temp %>% filter(tipo == 'delayed') %>% select(temp)

t.test(temp_late, temp_ontime, alternative = 'less', var.equal = FALSE)

boxplot(temp ~ tipo, data = df_temp,
        main = 'Temperatura e atraso', cex.main = 1.0,
        xlab = 'Voos',
        ylab = 'atraso', cex.axis = 0.8,
        horizontal = F,
        col = c('green','brown'), border = 'gray29')

reg <- lm(dep_delay ~ temp, data = df_temp)
summary(reg)

plot(df_temp$temp, df_temp$dep_delay,
     main = 'Temperatura e Atraso', cex.main = 1.0,
     xlab = 'Temperatura',
     ylab = 'Dep_delay', cex.axis = 0.8,
     pch = 19, cex = 0.6,
     add = T, col = 'red', lwd = 2, lty = 2)

# Avaliação de dewp (não seu o que é) e atraso

dewp_ontime <- df_temp %>% filter(tipo == 'on time') %>% select(dewp)
dewp_late <- df_temp %>% filter(tipo == 'delayed') %>% select(dewp)

t.test(dewp_late, dewp_ontime, alternative = 'greater', var.equal = FALSE)

boxplot(dewp ~ tipo, data = df_temp,
        main = 'Dewp e atraso', cex.main = 1.0,
        xlab = 'Voos',
        ylab = 'DEWP', cex.axis = 0.8,
        horizontal = F,
        col = c('green','brown'), border = 'gray29')

# Avaliação de humidade e atraso


humid_ontime <- df_temp %>% filter(tipo == 'on time') %>% select(humid)
humid_late <- df_temp %>% filter(tipo == 'delayed') %>% select(humid)

t.test(humid_late, humid_ontime, alternative = 'greater', var.equal = FALSE)

boxplot(humid ~ tipo, data = df_temp,
        main = 'Humidade e atraso', cex.main = 1.0,
        xlab = 'Voos',
        ylab = 'Precipitação', cex.axis = 0.8,
        horizontal = F,
        col = c('green','brown'), border = 'gray29')

#Avaliação de rajada de vento e Atraso


df_wind<- df_temp %>% filter(wind_gust != 'NA')

windgust_ontime <- df_wind %>% filter(tipo == 'on time') %>% select(wind_gust)
windgust_late <- df_wind %>% filter(tipo == 'delayed') %>% select(wind_gust)

t.test(windgust_late, windgust_ontime, alternative = 'greater', var.equal = FALSE)

boxplot(wind_gust ~ tipo, data = df_wind,
        main = 'Rajadas de vento e atraso', cex.main = 1.0,
        xlab = 'Voos',
        ylab = 'Rajadas de vento', cex.axis = 0.8,
        horizontal = F,
        col = c('green','brown'), border = 'gray29')

# Avaliação de direção do vento e atraso

windspeed_ontime <- df_temp %>% filter(tipo == 'on time') %>% select(wind_speed)
windspeed_late <- df_temp %>% filter(tipo == 'delayed') %>% select(wind_speed)


t.test(windspeed_late, windspeed_ontime, alternative = 'less', var.equal = FALSE)

boxplot(wind_speed ~ tipo, data = df_wind,
        main = 'Velocidade do vento e atraso', cex.main = 1.0,
        xlab = 'Voos',
        ylab = 'Velocidade de vento', cex.axis = 0.8,
        horizontal = F,
        col = c('green','brown'), border = 'gray29')

# Avaliação de preciptação e atraso

precip_ontime <- df_temp %>% filter(tipo == 'on time') %>% select(precip)
precip_late <- df_temp %>% filter(tipo == 'delayed') %>% select(precip)

t.test(precip_late, precip_ontime, alternative = 'less', var.equal = FALSE)

boxplot(precip ~ tipo, data = df_temp,
        main = 'Precipitação e atraso', cex.main = 1.0,
        xlab = 'Voos',
        ylab = 'Precipitação', cex.axis = 0.8,
        horizontal = F,
        col = c('green','brown'), border = 'gray29',
        outline= TRUE)

# Avaliação pressão e atraso

press_ontime <- df_temp %>% filter(tipo == 'on time') %>% select(pressure)
press_late <- df_temp %>% filter(tipo == 'delayed') %>% select(pressure)

t.test(press_late, press_ontime, alternative = 'greater', var.equal = FALSE)

boxplot(pressure ~ tipo, data = df_temp,
        main = 'Pressão e atraso', cex.main = 1.0,
        xlab = 'Voos',
        ylab = 'Pressão', cex.axis = 0.8,
        horizontal = F,
        col = c('green','brown'), border = 'gray29',
        outline= TRUE)

# Avliação da visibilidade e atraso

visi_ontime <- df_temp %>% filter(tipo == 'on time') %>% select(visib)
visi_late <- df_temp %>% filter(tipo == 'delayed') %>% select(visib)

t.test(visi_late, visi_ontime, alternative = 'greater', var.equal = FALSE)

boxplot(visib ~ tipo, data = df_temp,
        main = 'Visibilidade e atraso', cex.main = 1.0,
        xlab = 'Voos',
        ylab = 'Pressão', cex.axis = 0.8,
        horizontal = F,
        col = c('green','brown'), border = 'gray29',
        outline= TRUE)

#---------------------------------------------------------------------------------
