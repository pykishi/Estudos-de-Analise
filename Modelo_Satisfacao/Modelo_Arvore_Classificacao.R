# Bibliotecas
library(caret)
library(Metrics)
library(dplyr)
library(rpart)
library(rattle)
library(hmeasure)
library(pROC)

# Definicao do Diretorio de Trabalho
setwd('Documents/MBA_Big_Data_Data_Analytics_FGV/Análise_Preditiva/Trabalho_Grupo')

# Abrindo o Arquivo Tratado com as Dummies Variables
DATA <- read.csv('clean_passenger_satisfaction.csv', stringsAsFactors = T)

# Metodo para Garantir a Reprodutibilidade do Modelo
set.seed(123)

# Aplicando a Metodologia Holdout (modelo final com 70/30)
INDEX_TRAIN <- createDataPartition(grp_dummy$Satisfaction, p = 0.7, list = F)
TRAIN_SET_roc <- grp_dummy[INDEX_TRAIN, ]
TEST_SET_roc <- grp_dummy[-INDEX_TRAIN, ]

# Verificação da Distribuicao nas Amostras
prop.table(table(TRAIN_SET$Satisfaction)); prop.table(table(TEST_SET$Satisfaction))

# Aplicacao do Modelo de Arvore de Classificacao (minbucket = 20)
MDL_FIT <- rpart(Satisfaction ~.,
                 data = TRAIN_SET_roc,
                 method = 'class',
                 control = rpart.control(minbucket = 20, cp = -1))

# Output do Modelo
MDL_FIT
summary(MDL_FIT)

# Avaliacao da Poda da Arvore
printcp(MDL_FIT)
plotcp(MDL_FIT)

# Realizacao da Poda Manual do Modelo (cp = 7.1095e-04)
MDL_FIT.PRUNE <- prune(MDL_FIT, cp = 7.1095e-04)

# Output do Modelo com a Poda
MDL_FIT.PRUNE
summary(MDL_FIT.PRUNE)

# Plot do Modelo Podado - Verificar outros parâmetros para plotar a árvore
fancyRpartPlot(MDL_FIT.PRUNE,
               main = 'Árvore de Classificação',
               palettes = c("Reds", "Greens"),
               cex = 0.6)

# Aplicando a Modelagem as Bases de Treino e Teste
Y_PROB_TRAIN_arv <- predict(MDL_FIT.PRUNE, type = 'prob')[, 2]
Y_PROB_TEST_arv <- predict(MDL_FIT.PRUNE, newdata = TEST_SET_roc, type = 'prob')[, 2]

# Verificacao dos Resultados do Modelo e Possibilidade Overfitting
HMeasure(TRAIN_SET$Satisfaction, Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST)$metrics

# Verificacao da Importancia das Variaveis
round(MDL_FIT.PRUNE$variable.importance, 3)

# Definicoa do Modelo Final
MDL_FINAL <- MDL_FIT.PRUNE

# Observacao dos Desvios entre Previsao e Observado
Y_OBS_TRAIN <- TRAIN_SET$Satisfaction
Y_OBS_TEST <- TEST_SET$Satisfaction

Y_CLASS_TRAIN <- factor(if_else(Y_PROB_TRAIN > 0.5, 1, 0),
                        levels = c(0, 1),
                        labels = c('No', 'Yes'))
Y_CLASS_TEST <- factor(if_else(Y_PROB_TEST > 0.5, 1, 0),
                    levels = c(0, 1),
                    labels = c('No', 'Yes'))

confusionMatrix(data = Y_CLASS_TRAIN, reference = Y_OBS_TRAIN, positive = 'Yes')
confusionMatrix(data = Y_CLASS_TEST, reference = Y_OBS_TEST, positive = 'Yes')

# Verificacao Grafica de Distribuicao do Score
AUX <- data.frame(Y_PROB = Y_PROB_TEST,
                  Y_OBS = TEST_SET$Satisfaction)

par(mfrow = c(1,2))
boxplot(Y_PROB ~ Y_OBS, data = AUX,
        main = 'Probabilidade', cex.main = 1.2, cex.axis = 1.0, 
        xlab = 'Probabilidade', ylab = 'Target',
        ylim = c(0,1), horizontal = T,
        col = c('indianred3', 'darkolivegreen4'), border = 'gray30')
hist(AUX$Y_PROB, breaks = 12, xlim = c(0,1),
     main = 'Histograma de Prob.', cex.main = 1.2, 
     xlab = 'Probabilidade', ylab = 'Frequência (#)', cex.axis = 1.0,  
     col = 'grey40', border = 'white')

# Curva ROC
ROC_TRAIN <- roc(TRAIN_SET$Satisfaction, Y_PROB_TRAIN)
Y1 <- ROC_TRAIN$sensitivities
X1 <- 1 - ROC_TRAIN$specificities

ROC_TEST <- roc(TEST_SET$Satisfaction, Y_PROB_TEST)
Y2 <- ROC_TEST$sensitivities
X2 <- 1 - ROC_TEST$specificities

par(mfrow = c(1,1))
plot(X1, Y1, type = 'n', cex.axis = 1.2, cex = 0.5,
     main = 'Curva ROC', cex.main = 1.5,
     xlab = '1 - Especificidade', ylab = 'Sensitividade')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'), cex = 0.6)

##### RESUMO DO MODELO - TREINO #####
# SENSITIVIDADE = 78,52%
# ESPECIFICIDADE = 81,86%
# AUC = 85,30%
# GINI = 70,59%

##### RESUMO DO MODELO - TESTE #####
# SENSITIVIDADE = 78,62%
# ESPECIFICIDADE = 81,95%
# AUC = 85,38%
# GINI = 70,77%

# Limpeza do Ambiente R
graphics.off()
rm(list = ls())
cat('\014')
