# xx Projeto em Grupo xx
install.packages("rms")
install.packages("hmeasure")
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


setwd('C:/Users/lucas/FGV/03 - An?lise Preditiva/Projeto Grupo')
grp = read_excel("T_GRP_Passenger satisfaction.xlsx")
grp_FULL = read_excel("T_GRP_Passenger satisfaction.xlsx")


######## FEATURE ENGINNERING ############

# Convers?o dos campos qualitativos em factor

#Arrumando os nomes das colunas
grp <- rename(grp, Flight_Distance = 'Flight Distance')
grp <- rename(grp, Dep_Delay = 'Departure Delay in Minutes')
grp <- rename(grp, Arr_Delay = 'Arrival Delay in Minutes')

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

# Remover as colunas que correspondem a pesquisa feita apos o voo
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



#--------------------------------------------------------------------------------#
# 1) Divisao da base de modelagem em treino e teste com amostragem

set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN <- createDataPartition(grp_dummy$Satisfaction, p = 0.7, list = F)
TRAIN_SET <- grp_dummy[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- grp_dummy[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$Satisfaction);summary(TEST_SET$Satisfaction)
prop.table(table(TRAIN_SET$Satisfaction));prop.table(table(TEST_SET$Satisfaction))

#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo de regressao logistica

MDL_FIT <- glm(Satisfaction ~ ., data= TRAIN_SET_roc, family = binomial(link='logit'))
MDL_FIT

# Determinando os coeficientes das vari?veis explicativas
summary(MDL_FIT)

# Avaliando multicolinearidade via library car
library(car)
vif(MDL_FIT)

# Tratando a correla??o
TRAIN_SET_BKP <- TRAIN_SET_roc
TRAIN_SET_reg <- TRAIN_SET_roc[,-5]
TRAIN_SET_reg <- TRAIN_SET_roc

MDL_FIT <- glm(Satisfaction ~ ., data= TRAIN_SET_roc, family = binomial(link='logit'))
summary(MDL_FIT)

vif(MDL_FIT)

# O calculo nao encontrou correlacao

#--------------------------------------------------------------------------------#
# 3) Refinando o ajuste atraves do processo stepwise
library(MASS)
MDL_FIT.STEP <- stepAIC(MDL_FIT,direction = 'both', trace = TRUE)

MDL_FIT.STEP

# Determinando os coeficientes das vari?veis explicativas
summary(MDL_FIT.STEP)

#--------------------------------------------------------------------------------#
# 4) Realizando as predicoes

# Probabilidade Satisfaction pela regressao full
Y_PROB_TRAIN_reg <- predict(MDL_FIT, type = 'response') 
Y_PROB_TEST_reg  <- predict(MDL_FIT, newdata = TEST_SET, type = 'response')

head(Y_PROB_TRAIN)

# Probabilidade Satisfaction pela regressao  stepwise
Y_PROB_TRAIN_reg <- predict(MDL_FIT.STEP, type = 'response')  
Y_PROB_TEST_reg  <- predict(MDL_FIT.STEP, newdata = TEST_SET_roc, type = 'response')

# [EXTRA] Verificando a aderencia do ajuste logistico (teste Spiegelhalter)
library(rms)
val.prob(Y_PROB_TRAIN,ifelse(TRAIN_SET$Satisfaction == 'Yes',1,0), smooth = F)[c('S:z','S:p')]
# p valor > 5%, nao podemos rejeitar a hipotese nula

#--------------------------------------------------------------------------------#
# 5) Avaliando a performance dos modelos e existencia de overfitting

# Regressao full
library(hmeasure) 
HMeasure(TRAIN_SET$Satisfaction,Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST)$metrics

# Regressao com stepwise
HMeasure(TRAIN_SET$Satisfaction, Y_PROB_TRAIN.STEP)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST.STEP)$metrics

# Os resultados sao muito parecidos, porem a regressao com stepwise resultou em
# um modelo "mais enxuto", com mesma performance

# Modelo final
# Os resultados sao muito parecidos, porem a regressao com stepwise resultou em
# um modelo "mais enxuto", i.e. com menos vari?veis e com mesma performance
MDL_FINAL <- MDL_FIT.STEP

#--------------------------------------------------------------------------------#
# 6) Importancia das variaveis (Modelo final)

#https://cran.r-project.org/web/packages/dominanceanalysis/vignettes/da-logistic-regression.html
anova(MDL_FINAL, test= "Chisq")

#--------------------------------------------------------------------------------#

?cbind

# df auxiliar
AUX <- data.frame(Y_PROB = Y_PROB_TEST.STEP,
                  Y_OBS  = TEST_SET$Satisfaction)

boxplot(Y_PROB ~ Y_OBS, data = AUX,
        main = 'Boxplot probs', cex.main = 1.2, cex.axis = 1.2, 
        xlab = 'PROBABILITIES', ylab = 'Target',
        ylim = c(0,1), horizontal = T,
        col = c('darkorange','darkorange4'), border = 'gray20')
hist(AUX$Y_PROB, breaks = 20, xlim = c(0,1),
     main = 'Histogram probs', cex.main = 1.2, 
     xlab = 'PROBABILITIES', ylab = 'FREQUENCIA (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

graphics.off()

#--------------------------------------------------------------------------------#
# 8) Curva ROC

library(pROC)
ROC1 <- roc(TRAIN_SET$Satisfaction,Y_PROB_TRAIN.STEP)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(TEST_SET$Satisfaction,Y_PROB_TEST.STEP)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'))













########### ANALISE VARIAVEIS ######################
# Mais de 50% da base corresponde a clientes da classe executiva WTF


# Tem crian?as de 7,8,9 na base. Faz sentido?
boxplot(grp$Age,
        main = 'Distribui??o Idade', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Idade', #, ylab = 'Chamados',
        cex.axis = 1.2, ylim = c(0,120), col = c('blue'))

# Tem ouliers (1.293 linhas com flight distance superior a 99%)
boxplot(grp$Flight_Distance,
        main = 'Distribui??o Dist?ncia de Voo', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Dist?ncia (km)', #, ylab = 'Chamados',
        cex.axis = 1.2, col = c('blue'), ylim = c(0,4000))

#grp %>% filter(`Flight_Distance` > 3884) %>% summarise(n())

quantile(grp$Flight_Distance,0.95)

# Atraso de partida
boxplot(Dep_Delay ~ Satisfaction, data = grp,
        main = 'Distribui??o Atraso de partida', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Satisfa??o', ylab = 'ATraso(min)',
        cex.axis = 1.2, col = c('blue','darkorange'))

quantile(grp$Dep_Delay, 0.95)


# Atraso de chegada
boxplot(Arr_Delay ~ Satisfaction, data = grp,
        main = 'Distribui??o Atraso de chegada', cex.main = 1.0,
        cex.names = 0.8, xlab = 'Satisfa??o', ylab = 'ATraso(min)',
        cex.axis = 1.2, col = c('blue','darkorange'), ylim = c(0,100))

quantile(grp$Arr_Delay, 0.95)

view(grp)
############################################


