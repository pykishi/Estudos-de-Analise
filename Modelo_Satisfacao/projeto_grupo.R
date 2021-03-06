# xx Projeto em Grupo xx

library(readxl)
library(dplyr)
library(summarytools)
library(lubridate)
library(caret)
library(Metrics)
library(rpart)
library(rattle)
library(rpart.plot)
library(car)
library(randomForest)


setwd('C:/Users/guilherme.n.nalin/Documents/MBA/Analise_preditiva/Projeto_Grupo')

grp <- T_GRP_Passenger_satisfaction

# Remover as colunas que correspondem a pesquisa feita ap�s o v�o
# Removemos ID

grp <- grp[,-(9:22)]
grp <- grp[,2:10]
View(grp)


# Convers�o dos campos qualitativos em factor

grp %>% group_by(Satisfaction) %>% summarise(n())

grp <- grp %>% mutate(Gender = factor(Gender, levels = c("Female","Male"), ordered = FALSE))

grp <- grp %>% mutate(`Customer Type` = factor(`Customer Type`, levels = c("disloyal Customer","Loyal Customer"), ordered = FALSE))

grp <- grp %>% mutate(`Type of Travel` = factor(`Type of Travel`, levels = c("Business travel","Personal Travel"), ordered = FALSE))

grp <- grp %>% mutate(Class = factor(Class, levels = c("Business","Eco","Eco Plus"), ordered = FALSE))

grp <- grp %>% mutate(Satisfaction = factor(Satisfaction, levels = c("neutral or dissatisfied","satisfied"), ordered = FALSE))

#Arrumando os nomes das colunas
grp <- rename(grp, Customer_type = `Customer Type`)
grp <- rename(grp, Travel_type = `Type of Travel`)
grp <- rename(grp, Flight_distance = `Flight Distance`)
grp <- rename(grp, Dep_delay = `Departure Delay in Minutes`)
grp <- rename(grp, Arrival_delay = `Arrival Delay in Minutes`)


# Tem linhas com "Arrival Delay in Minutes" = NA
grp <- grp %>% filter(is.na(Arrival_delay) == FALSE)


# Cria��o da base Dummy

#Vari�veis Explicativas
grp_x <- grp[,-1]

#Vari�vel Resposta
grp_y <- grp[,1]

# Cria��o da base dummy

grp_x_dummy <- dummyVars(' ~ .', data = grp_x, sep = '_', fullRank = T)
grp_x_dummy_df <- as.data.frame(predict(grp_x_dummy, newdata = grp_x))

# Base final dummificada

grp_dummy <- bind_cols(grp_x_dummy_df, grp_y)


str(grp_dummy)








# Mais de 50% da base corresponde a clientes da classe executiva WTF


# Tem crian�as de 7,8,9 na base. Faz sentido?
boxplot(grp$Age,
        main = 'Distribui��o Idade', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Idade', #, ylab = 'Chamados',
        cex.axis = 1.2, ylim = c(0,120), col = c('blue'))

# Tem ouliers (1.293 linhas com flight distance superior a 99%)
boxplot(grp$Flight_distance,
        main = 'Distribui��o Dist�ncia de V�o', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Dist�ncia (km)', #, ylab = 'Chamados',
        cex.axis = 1.2, col = c('blue'), ylim = c(0,4000))

grp %>% filter(`Flight Distance` > 3884) %>% summarise(n())

quantile(grp$Flight_distance,0.95)

# Atraso de partida
boxplot(Dep_delay ~ Satisfaction, data = grp,
        main = 'Distribui��o Atraso de partida', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Satisfa��o', ylab = 'ATraso(min)',
        cex.axis = 1.2, col = c('blue','darkorange'))

quantile(grp$Dep_delay, 0.95)


# Atraso de chegada
boxplot(Arrival_delay ~ Satisfaction, data = grp,
        main = 'Distribui��o Atraso de chegada', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Satisfa��o', ylab = 'ATraso(min)',
        cex.axis = 1.2, col = c('blue','darkorange'), ylim = c(0,100))

quantile(grp$Arrival_delay, 0.95)

view(grp)



