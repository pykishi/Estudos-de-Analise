##################################################
###-----------  TRABALHO EM GRUPO  ------------###
###-------------Analise Preditiva------------ ####
##################################################

########## ENSEMBLE METHODS - ADABOOST ###########

#install.packages("adabag")
library(adabag)
library(caret)


# 1) Divisao da base de modelagem em treino e teste com amostragem

set.seed(123) # garantindo reprodutibilidade da amostra

### SEPARANDO AMOSTRA DE 2% ####
INDEX_TRAIN <- createDataPartition(grp_dummy$Satisfaction, p = 0.02, list = F)
amostra <- grp_dummy[INDEX_TRAIN, ] # base de desenvolvimento: 2%

set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN <- createDataPartition(amostra$Satisfaction, p = 0.7, list = F)
TRAIN_SET <- amostra[INDEX_TRAIN, ] # base de desenvolvimento: 10%
TEST_SET  <- amostra[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$Satisfaction);summary(TEST_SET$Satisfaction)
prop.table(table(TRAIN_SET$Satisfaction));prop.table(table(TEST_SET$Satisfaction))
################################################
#--------------------------------------------------------------------------------#

############ PROCESSAMENTO PARALELO ##################
library(foreach)
library(doParallel)

#####Checa quantos núcleos existem#####
ncl<-detectCores()
ncl <- ncl - 1 ### Não colocar todos pra rodar

#Registra os clusters a serem utilizados
cl <- makeCluster(ncl)
registerDoParallel(cl)

#Inicia a contagem do tempo
ptm <- proc.time()

tree_depth = c(1,2)  #profundidade da arvore 
n_trees = seq(from = 10, 
              to = 100, #até n arvores
              by = 10) 

adaBoost_grid <- 
  foreach(t = tree_depth) %:% 
  foreach(n = n_trees) %dopar% {

    library(adabag)
    MDL_FIT <- boosting(Satisfaction ~ ., 
                        data      = TRAIN_SET, 
                        mfinal    = n, 
                        coeflearn = "Breiman", 
                        control   = rpart.control(maxdepth = t))
    
    }

#Para de contar o tempo
proc.time() - ptm

#Stop clusters
stopCluster(cl)

##### - COMO PEGAR DADOS -----------------------
floresta2 <- adaBoost_grid[[2]]
mod <- floresta2[[10]]

#metrica
MDL_FIT <- mod
Y_PROB_TRAIN <- predict.boosting(MDL_FIT, TRAIN_SET)$prob[,2]
Y_PROB_TEST  <- predict.boosting(MDL_FIT, TEST_SET)$prob[,2]
HMeasure(TRAIN_SET$Satisfaction,Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST)$metrics


################################################

#--------------------------------------------------------------------------------#
# 3) Realizando as predicoes

# Probabilidade de classificaçao de CHURN

Y_PROB_TRAIN <- predict.boosting(MDL_FIT, TRAIN_SET)$prob[,2]
Y_PROB_TEST  <- predict.boosting(MDL_FIT, TEST_SET)$prob[,2]

head(Y_PROB_TRAIN) # perceba que geraram duas colunas com probs (soma 1)

#--------------------------------------------------------------------------------#
# 4) Avaliando a performance dos modelos e existencia de overfitting

library(hmeasure) 
HMeasure(TRAIN_SET$Satisfaction,Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST)$metrics

# sinais de overfitting entre as amostras de treino e teste? 
MDL_FINAL <- MDL_FIT

#--------------------------------------------------------------------------------#
# 5) Importancia das variaveis (Modelo final)

# nativo do boosting
importanceplot(MDL_FINAL)

# outra forma
MDL_FINAL$importance[order(MDL_FINAL$importance,decreasing = T)]

#--------------------------------------------------------------------------------#
# 6) Inspecao dos valores previstos vs observados (modelo final)

# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)

# Label observado
Y_OBS <- TEST_SET$Satisfaction
levels(Y_OBS)

# Label previsto usando: 
#       se PROB > 50% -> 1 (Yes)
#       se PROB > 30% -> 1 (Yes)
Y_CLAS1 <- factor(ifelse(Y_PROB_TEST > 0.5,1,0),
                  levels = c(0,1),
                  labels = c('No','Yes')) 
Y_CLAS2 <- factor(ifelse(Y_PROB_TEST > 0.5,1,0),
                  levels = c(0,1),
                  labels = c('No','Yes'))

confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Yes')
confusionMatrix(data = Y_CLAS2, reference = Y_OBS, positive = 'Yes')

# Distribuicao do score
graphics.off()
par(mfrow = c(1,2))

# df auxiliar
AUX <- data.frame(Y_PROB = Y_PROB_TEST,
                  Y_OBS  = TEST_SET$Satisfaction)

boxplot(Y_PROB ~ Y_OBS, data = AUX,
        main = 'Boxplot probs', cex.main = 1.2, cex.axis = 1.2, 
        xlab = 'PROBABILITIES', ylab = 'TARGET',
        ylim = c(0,1), horizontal = T,
        col = c('darkorange','darkorange4'), border = 'gray20')
hist(AUX$Y_PROB, breaks = 12, xlim = c(0,1),
     main = 'Histogram probs', cex.main = 1.2, 
     xlab = 'PROBABILITIES', ylab = 'FREQUENCIA (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

graphics.off()

#--------------------------------------------------------------------------------#
# 7) Curva ROC

library(pROC)
ROC1 <- roc(TRAIN_SET$Satisfaction,Y_PROB_TRAIN)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(TEST_SET$Satisfaction,Y_PROB_TEST)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'))

#--------------------------------------------------------------------------------#


