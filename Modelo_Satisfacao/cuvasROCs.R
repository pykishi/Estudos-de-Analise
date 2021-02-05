# Curva ROC - todos os modelos

Y_PROB_TRAIN
Y_PROB_TEST

library(pROC)

ROC_arv <- roc(TEST_SET_roc$Satisfaction, Y_PROB_TEST_arv)
Y1 <- ROC_arv$sensitivities
X1 <- 1 - ROC_arv$specificities

ROC_reg <- roc(TEST_SET_roc$Satisfaction,Y_PROB_TEST_reg)
Y3   <- ROC_reg$sensitivities
X3   <- 1 - ROC_reg$specificities

ROC2 <- roc(TEST_SET_roc$Satisfaction,Y_PROB_TEST)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

ROC_rf <- roc(TEST_SET_roc$Satisfaction, Y_PROB_TEST_rf)
Y4 <- ROC_rf$sensitivities
X4 <- 1 - ROC_rf$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'green3') 
lines(X3, Y3, lwd = 3, lty = 1, col = 'gray')
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
lines(X4, Y4, lwd = 3, lty = 1, col = 'tomato3') 
abline(0, 1, lty = 2)
legend('bottomright',c('Random Forest','Árvore Decisão','Regressão','AdaBoost'), lty = 1, col = c('tomato3','green3','gray','cyan3'))
