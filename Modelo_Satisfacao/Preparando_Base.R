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


setwd('C:/FGV/Analise Preditiva/Trabalho/Grupo')
grp = read_excel("T_GRP_Passenger satisfaction.xlsx")

######## FEATURE ENGINNERING ############

# Convers?o dos campos qualitativos em factor

#Arrumando os nomes das colunas
grp <- rename(grp, Flight_Distance = `Flight Distance`)
grp <- rename(grp, Dep_Delay = `Departure Delay in Minutes`)
grp <- rename(grp, Arr_Delay = `Arrival Delay in Minutes`)

#Arrumando os valores
grp <- grp %>% mutate(Gender = ifelse(Gender == "Female", "F", "M"))
grp$Gender <- as.factor(grp$Gender)

grp <- grp %>% mutate(Satisfaction = ifelse(Satisfaction == "satisfied", "Yes", "No"))
grp$Satisfaction <- as.factor(grp$Satisfaction)

grp <- grp %>% mutate(Customer = ifelse(`Customer Type` == "disloyal Customer","Disloyal","Loyal"))
grp$Customer <- as.factor(grp$Customer)

grp <- grp %>% mutate(Travel = ifelse(`Type of Travel` == "Business travel", "Business", "Personal"))
grp$Travel <- as.factor(grp$Travel)

grp <- grp %>% mutate(Class = case_when(Class == "Eco Plus" ~ "Plus",
                                         Class == "Eco" ~ "Eco",
                                         Class == "Business" ~ "Bus"))
grp$Class <- as.factor(grp$Class)

# Remover as colunas que correspondem a pesquisa feita ap?s o v?o
# Removemos ID
grp <- grp %>% select(Gender, Age, Flight_Distance, Dep_Delay, Arr_Delay, Satisfaction, Customer, Travel, Class)

# Verifica existencia de NAs
colSums(is.na(grp))
# Tem linhas com "Arrival Delay in Minutes" = NA
grp <- grp %>% filter(is.na(Arr_Delay) == FALSE)


# Cria??o da base Dummy

#Vari?veis Explicativas
grp_x <- grp[,-6]

#Vari?vel Resposta
grp_y <- grp[,6]

# Cria??o da base dummy
grp_x_dummy <- dummyVars(' ~ .', data = grp_x, sep = '_', fullRank = T)
grp_x_dummy_df <- as.data.frame(predict(grp_x_dummy, newdata = grp_x))

# Base final dummificada
grp_dummy <- bind_cols(grp_x_dummy_df, grp_y)


str(grp_dummy)


###############################



########### ANALISE VARIAVEIS ######################
# Mais de 50% da base corresponde a clientes da classe executiva WTF


# Tem crian?as de 7,8,9 na base. Faz sentido?
boxplot(grp$Age,
        main = 'Distribui??o Idade', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Idade', #, ylab = 'Chamados',
        cex.axis = 1.2, ylim = c(0,120), col = c('blue'))

# Tem ouliers (1.293 linhas com flight distance superior a 99%)
boxplot(grp$Flight_distance,
        main = 'Distribui??o Dist?ncia de V?o', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Dist?ncia (km)', #, ylab = 'Chamados',
        cex.axis = 1.2, col = c('blue'), ylim = c(0,4000))

grp %>% filter(`Flight Distance` > 3884) %>% summarise(n())

quantile(grp$Flight_distance,0.95)

# Atraso de partida
boxplot(Dep_delay ~ Satisfaction, data = grp,
        main = 'Distribui??o Atraso de partida', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Satisfa??o', ylab = 'ATraso(min)',
        cex.axis = 1.2, col = c('blue','darkorange'))

quantile(grp$Dep_delay, 0.95)


# Atraso de chegada
boxplot(Arrival_delay ~ Satisfaction, data = grp,
        main = 'Distribui??o Atraso de chegada', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Satisfa??o', ylab = 'ATraso(min)',
        cex.axis = 1.2, col = c('blue','darkorange'), ylim = c(0,100))

quantile(grp$Arrival_delay, 0.95)

view(grp)
############################################


