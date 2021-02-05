#-- *Complementando o trabalho individual:
#
# - Quanto os formandos podem esperar de salário após a formatura?
# - Existem variáveis que tenham um efeito importante no valor do salário dos formandos? 
#   Lembre-se que aqui terão que cobrir o que foi passado em curso (analise exploratória, descritiva e testes de hipótese)
#
#   Ao analisar os dados apresentados a seguir leve em conta: valor discrepante e valores válidos de salário.
#
#   Que recomendações você daria para Marie sobre a matrícula no programa de MBA?
#  
#  Qualquer dúvida, me procurem. 
#----------------------------------------------


library(dplyr)
library(readxl)
library(summarytools)
library(car)

#--Importando dados
df_salario = read_excel("T_IND_Case_MBA Starting Salaries.xlsx")

#--Verificando dados 
str(df_salario)
View(df_salario)

#--Categorizando dados
df_salario$Sexo <- factor(df_salario$sex, labels = c("Male","Female"))
df_salario$Ranking <- factor(df_salario$quarter, labels = c("1º","2º","3º","4º"))
df_salario$Lingua <- factor(df_salario$frstlang, labels = c("English","Outros"))
df_salario$Satisfacao <- factor(df_salario$satis, labels = c("Pessimo","Ruim",
                                                            "Moderado","Médio",
                                                            "Bom","Muito Bom",
                                                            "Excelente"))
#--Verificando dados categorizados
view(dfSummary(df_salario))

#-- Verificado que não tem valores missing, porém...
colSums(is.na(df_salario))


#-- Transformando 998 e 999 em missing
df_salario[df_salario$salary %in% c(998,999), "salary"] <- NA
df_salario[df_salario$satis == 998, "satis"] <- NA
salario <- df_salario %>% filter(between(salary,1,162000))
summary(salario$salary)


#-- Verificado observações com NA
colSums(is.na(salario))
salario %>% count(is.na(salary))


#-- Salário incial - U$ 1000,00

boxplot(salario$salary, 
        main = 'Análise Univariada - Distribuição Salário', cex.main = 1.0,
        xlab = 'Salário (anual)', cex.axis = 1.2,
        horizontal = T,
        col = 'royalblue', border = 'blue3',
        outline = TRUE)


#-- Salário x Sexo = é diferente, é maior

boxplot(salary ~ Sexo, data = salario,
        horizontal = F, 
        main = 'Análise Bivariada - Salário vs Sexo', cex.main = 1.0,
        xlab = '',
        ylab = 'Salário (anual)', cex.axis = 1.0,
        ylim = c(60000, 165000), col = c('dodgerblue', 'pink', border = 'gray'),
        outline = TRUE)

sal_sex <- salario %>% group_by(Sexo) %>% summarise(qtde = n(),
                                                       mean = mean(salary, na.rm = TRUE),
                                                       median = median(salary, na.rm = TRUE),
                                                       sd = sd(salary, na.rm = TRUE),
                                                       var = var(salary, na.rm = TRUE))

fmale <- salario %>% select(salary, Sexo) %>% filter(Sexo == 'Male') 
male <- fmale %>% select(salary)

ffemale <- salario %>% select(salary, Sexo) %>% filter(Sexo == 'Female') 
female <- ffemale %>% select(salary)

### Testando que masculino > feminino
t.test(male, female, var.equal = T, alternative = 'greater')


#-- Analise Idade

hist(salario$age,
     main = 'Análise Univariada - Histograma Idade', cex.main = 1.0,
     xlab = 'Idade',
     ylab = 'Frequência', cex.main = 1.0,
     ylim = c(0,100), col = 'dodgerblue', border = 'dodgerblue4')
     

plot(salario$age, salario$salary, 
     main = 'Análise Bivariada - Dispersão', cex.main = 1.0,
     xlab = 'Idade',
     ylab = 'Salário (anual)', cex.axis = 1.0,
     pch = 19, cex = 0.8,
     ylim = c(1000,250000),
     col = 'dodgerblue')


scatterplot(salary ~ age | Sexo,
            data = salario, smooth = F,
            main = '                   Análise Bivariada - Dispersão', cex.main = 1.0,
            xlab = 'Idade',
            ylab = 'Salário (anual)', cex.axis = 1.0,
            pch = c(19,17), cex = 1.0,
            col = c('blue','deeppink'))

summary(lm(salary ~ age, data = salario))
###sim exsite relação linear de salario x age

#-- Salario x GMAT_to

scatterplot(salary ~ gmat_tot | Sexo,
            data = salario, smooth = F,
            main = '               Análise Bivariada - Dispersão', cex.main = 1.0,
            xlab = 'GMAT',
            ylab = 'Salário (anual)', cex.axis = 1.0,
            pch = c(19,17), cex = 1.0,
            col = c('blue','deeppink'))

summary(lm(salary ~ gmat_tot, data = salario))
## não existe relação

## -- Grau de Satisfação Muito Boa ----
view(ctable(x = salario$Sexo,
            y = salario$Satisfacao, prop = "r"))

barplot(table(salario$Satisfacao),
        main = 'Análise Univariada - Satisfação', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = '', cex.axis = 1.0,
        ylim = c(0,50), col = 'steelblue3', border = 'steelblue4')
        

## -- Salário x Idioma
#amostra insuficiente da vairavel idioma

## -- Salario x Nota MBA

salario$Nota <- as.numeric(salario$s_avg + salario$f_avg)

scatterplot(salary ~ Nota | Sexo,
            data = salario, smooth = F,
            main = '               Análise Bivariada - Dispersão', cex.main = 1.0,
            xlab = 'Nota',
            ylab = 'Salário (anual)', cex.axis = 1.0,
            pch = c(19,17), cex = 1.0,
            col = c('blue','deeppink'))

summary(lm(salary ~ Nota, data = salario))
## não existe relação

# - Nota x Sexo
boxplot(Nota ~ Sexo, data = salario,
        horizontal = F, 
        main = 'Análise Bivariada - Nota vs Sexo', cex.main = 1.0,
        xlab = '',
        ylab = 'Nota MBA', cex.axis = 1.0,
        col = c('dodgerblue', 'pink', border = 'gray'),
        outline = TRUE)

df_salario <- df_salario %>% filter(between(salary,1,162000))
df_salario$Nota <- as.numeric(df_salario$s_avg + df_salario$f_avg)


nmale <- salario %>% select(Nota, Sexo) %>% filter(Sexo == 'Male') 
male <- nmale %>% select(Nota)

nfemale <- salario %>% select(Nota, Sexo) %>% filter(Sexo == 'Female') 
female <- nfemale %>% select(Nota)


### Testando que média feminino > masculino
t.test(female, male, var.equal = T, alternative = 'greater')


#-- Nota x Lingua

boxplot(Nota ~ Lingua, data = salario,
        horizontal = F, 
        main = 'Análise Bivariada - Nota vs Lingua', cex.main = 1.0,
        xlab = '',
        ylab = 'Nota MBA', cex.axis = 1.0,
        col = c('dodgerblue', 'pink', border = 'gray'),
        outline = TRUE)

lmale <- salario %>% select(Nota, Lingua) %>% filter(Lingua == 'English') 
english <- lmale %>% select(Nota)

lfemale <- salario %>% select(Nota, Lingua) %>% filter(Lingua == 'Outros') 
outros <- lfemale %>% select(Nota)


### Testando que média feminino > masculino
t.test(english, outros, var.equal = T, alternative = 'greater')

