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
library(hmeasure)
library(pROC)
library(e1071)


setwd('C:/Users/guilherme.n.nalin/OneDrive - Accenture/Documents/MBA/Analise_preditiva/Projeto_Grupo')
grp = read_excel("T_GRP_Passenger satisfaction.xlsx")
grp_backup = read_excel("T_GRP_Passenger satisfaction.xlsx")

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
grp <- grp %>% select(id, Gender, Age, Flight_Distance, Dep_Delay, Arr_Delay, Satisfaction, Customer, Travel, Class)

# Verifica existencia de NAs
colSums(is.na(grp))
# Tem linhas com "Arrival Delay in Minutes" = NA
grp <- grp %>% filter(is.na(Arr_Delay) == FALSE)


# Cria??o da base Dummy

#Vari?veis Explicativas
grp_x <- grp[,-7]

#Vari?vel Resposta
grp_y <- grp[,7]

# Cria??o da base dummy
grp_x_dummy <- dummyVars(' ~ .', data = grp_x, sep = '_', fullRank = T)
grp_x_dummy_df <- as.data.frame(predict(grp_x_dummy, newdata = grp_x))

# Base final dummificada
grp_dummy <- bind_cols(grp_x_dummy_df, grp_y)





#--------------------------------------------------------------------------------#

# Modelo com Random Forest


set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN <- createDataPartition(grp_dummy$Satisfaction, p = 0.7, list = F)
TRAIN_SET_ID <- grp_dummy[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TRAIN_SET <- TRAIN_SET_ID[,-1]

TEST_SET_ID  <- grp_dummy[-INDEX_TRAIN,] # base de teste: 30%
TEST_SET <- TEST_SET_ID[,-1]


summary(TRAIN_SET$Satisfaction);summary(TEST_SET$Satisfaction)
prop.table(table(TRAIN_SET$Satisfaction));prop.table(table(TEST_SET$Satisfaction))


grp_forest <- randomForest(Satisfaction ~ .,
                           data = TRAIN_SET_roc,
                           importance = T,
                           mtry       = 4,
                           nodesize   = 20, 
                           ntree      = 100)



grp_forest
plot(grp_forest, main = 'Out-of-bag error') #vamos podar com 100 ?rvores
legend("topright", c('Out-of-bag',"1","0"), lty=1, col=c("black","green","red"))

## Agora vamos testar os outros par?metros, para tentar melhorar a performance
## Melhor performance encontrada com mtry = 4 e nodesize = 20

grp_forest <- randomForest(Satisfaction ~ .,
                           data = TRAIN_SET,
                           importance = T,
                           mtry       = 4,
                           nodesize   = 20, 
                           ntree      = 100)


# Probabilidade de classifica?ao de Satisfaction
Y_PROB_TRAIN <- predict(grp_forest, type = 'prob')[,2]
Y_PROB_TEST  <- predict(grp_forest, newdata = TEST_SET, type = 'regression')[,2]

# Avaliando performance do modelo

HMeasure(TRAIN_SET$Satisfaction,Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST)$metrics

View(head(Y_PROB_TRAIN))


## Plotando os resultados para an?lise

# Distribuicao do score
graphics.off()
par(mfrow = c(1,2))

# df auxiliar
AUX <- data.frame(Y_PROB = Y_PROB_TEST,
                  Y_OBS  = TEST_SET$Satisfaction)

boxplot(Y_PROB ~ Y_OBS, data = AUX,
        main = 'Boxplot Probabilidades', cex.main = 1.2, cex.axis = 1, 
        xlab = 'Probabilidades', ylab = 'Varia??o Target',
        ylim = c(0,1), horizontal = T,
        col = c('tomato','forestgreen'), border = 'gray20')
hist(AUX$Y_PROB, breaks = 12, xlim = c(0,1),
     main = 'Histograma de probabilidades', cex.main = 1.2, 
     xlab = 'PROBABILITIES', ylab = 'FREQUENCIA (#)', cex.axis = 1,  
     col = 'blue', border = 'brown')

graphics.off()

#--------------------------------------------------------------------------------#
# 7) Curva ROC


ROC1 <- roc(TRAIN_SET$Satisfaction,Y_PROB_TRAIN)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(TEST_SET$Satisfaction,Y_PROB_TEST)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5, main = "Curva ROC",
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'))

