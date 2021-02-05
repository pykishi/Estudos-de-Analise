
library(nycflights13)   # Importacao da livraria que contem os BDs
library(dplyr)         # Manipulacao dos Bancos de Dados
library(lubridate)      # Manipulacao das Datas
library(summarytools)
library(car)

######
######### Para usar a mesma base de FLIGHTS #############
df_voos <- flights %>% filter(origin == 'JFK' ||
                                origin == 'LGA' ||
                                origin == 'EWR')

#-- Retirando NAs
df_voos <- na.omit(df_voos)

colSums(is.na(df_voos)) 

#-- Categorizar voos atrasados INCLUSÃO tipo ---------

df_voos <- df_voos %>% mutate(tipo = ifelse(dep_delay <= 15, "on time", "delayed"))

a <- flights %>% filter(carrier == 'AA') 
b <- planes %>% filter(tailnum == 'N3DUAA')
c <- flights %>% left_join(planes, by = 'tailnum') %>% filter(carrier == 'AA')

################ PLANES ###################

############-- Observando Planes --##########

view(dfSummary(airlines))

#-- Verifica NAs
colSums(is.na(planes))


#-- Quantidade por Engine ----
barplot(table(planes$engine),
        main = 'Análise Univariada - Plane', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = 'Quantidade', cex.axis = 1.0,
        ylim = c(0,3000), col = 'blue', border = 'blue4')

#-- Quantidade por Fabricante ----
barplot(table(planes$manufacturer),
        main = 'Análise Univariada - Plane', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = 'Quantidade', cex.axis = 1.0,
        ylim = c(0,2000), col = 'blue', border = 'blue4')

d <- df_plane %>% group_by(manufacturer) %>% count()

#-- Quantidade por Modelo ----
barplot(table(planes$model),
        main = 'Análise Univariada - Plane', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = 'Quantidade', cex.axis = 1.0,
        ylim = c(0,2000), col = 'blue', border = 'blue4')

#-- Quantidade por Motor ----
barplot(table(planes$engines),
        main = 'Análise Univariada - Plane', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = 'Quantidade', cex.axis = 1.0,
        ylim = c(0,2000), col = 'blue', border = 'blue4')

#-- Quantidade por Motor e tipo motor ----

tbl <- prop.table(table(planes$engines, planes$engine), margin = 2)
barplot(tbl,
        main = 'Análise Bivariada - Quantidade de Motor', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = 'Frequência', cex.axis = 1.0,
        ylim = c(0,1), col = c('tomato','chartreuse4','blue','pink'), border='gray20')
legend("top", legend = row.names(tbl), fill = c('tomato', 'chartreuse4','blue','pink'))

#-- Tabelas cruzada ----------- 
view(ctable(x = planes$engine,
            y = planes$engines, prop = "r"))

view(ctable(x = planes$engine,
            y = planes$seats, prop = "r"))


################## FLIGHTS X PLANES ####################

#-- carregando em outro df sem NAs de year e speed
##df_plane <- na.omit(planes)

df_plane <- planes

#-- Carrega fligths COM planes 
df_plane <- df_voos %>% inner_join(df_plane, by = 'tailnum')

view(dfSummary(df_plane))

#-- Verifica NAs
colSums(is.na(df_plane))
 
#-- Criação campos: Idade,
df_plane <- df_plane %>% mutate(Idade = year.x - year.y) 

#-- Barras - Voos por
barplot(table(df_plane$model))
barplot(table(df_plane$manufacturer))
barplot(table(df_plane$engines))
barplot(table(df_plane$engine))


########## ENGINE ##########

#-- Verifica NAs
colSums(is.na(df_plane))

#-- Idade por Engine
#-- Retira NAs
df_plane_idade <- df_plane %>% filter(Idade != 'NA')
df_plane_engine <- df_plane %>% filter(engine != 'NA')

barplot(table(df_plane_idade$Idade))
barplot(table(df_plane_idade$engine))

#-- Idade Média
idade_engine <- df_plane_idade %>% group_by(tipo, engine) %>% summarise(qt = n(),
                                                   minimo = min(Idade),
                                                   maximo = max(Idade),
                                                   prim_quartil = quantile(Idade, 0.25),
                                                   mediana = median(Idade),
                                                   media = mean(Idade),
                                                   sd = sd(Idade),
                                                   var = var(Idade))
                                                        
boxplot(Idade ~ engine, data = df_plane_idade,
        horizontal = F, 
        main = 'Análise Bivariada - Idade vs Engine', cex.main = 1.0,
        xlab = '',
        ylab = 'Idade', cex.axis = 1.0,
        ylim = c(0, 65), 
        col = c('dodgerblue', 'pink', 'chartreuse4', 'tomato', 'cyan', 'chocolate', border = 'gray'),
        outline = F)


#-- On Time e Deplayed por Engine ------------
tbl <- prop.table(table(df_plane_engine$tipo, df_plane_engine$engine), margin = 2)
barplot(tbl,
        main = 'Análise Bivariada - Voos Atrasados por Tipo de Motor', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = 'Frequencia (%)', cex.axis = 1.0,
        ylim = c(0,1), col = c('tomato','chartreuse4'), border='gray20')
legend("top", legend = row.names(tbl), fill = c('tomato', 'chartreuse4'))


############ Atraso por Engine --##############
df_atraso_engine <- df_plane_engine %>% filter(tipo == 'delayed')
df_ontime_engine <- df_plane_engine %>% filter(tipo == 'on time')

boxplot(dep_delay ~ engine, data = df_atraso_engine,
        horizontal = F, 
        main = 'Análise Bivariada - Atraso', cex.main = 1.0,
        xlab = '',
        ylab = 'Frequência', cex.axis = 1.0,
        ylim = c(0, 220), 
        col = c('dodgerblue', 'pink', 'chartreuse4', 'tomato', 'cyan', 'chocolate', border = 'gray'),
        outline = F)

tb_atraso_engine <- df_atraso_engine %>% group_by(engine) %>% summarise(qt = n(),
                                                           minimo = min(dep_delay),
                                                           maximo = max(dep_delay),
                                                           prim_quartil = quantile(dep_delay, 0.25),
                                                           mediana = median(dep_delay),
                                                           media = mean(dep_delay),
                                                           sd = sd(dep_delay),
                                                           var = var(dep_delay))

#-- Confirma que as médias são bem diferentes
one_way_anova_compania <- aov(dep_delay ~ engine, data = df_atraso_engine)
summary(one_way_anova_compania)


################## FECHADO ###################

################ ANÁLISE BUGADA ################
### Voos x Idade - Dispersão
#scatterplot(dep_delay ~ Idade | tipo,
#            data = df_plane_idade, smooth = F,
#            main = 'Análise Bivariada - Dispersão', cex.main = 1.0,
#            xlab = 'Idade',
#            ylab = 'Frequência', cex.axis = 1.0,
#            pch = c(19,17), cex = 1.0,
#            col = c('blue','deeppink'))
#
#summary(lm(dep_delay ~ Idade, data = df_plane_idade))
#-- sem relação ---

##############################################

########## FABRICANTE ##########

#-- Verifica NAs
colSums(is.na(df_plane))

df_plane_fabricante <- df_plane %>% filter(manufacturer != 'NA')
df_plane_fabrican <- df_plane %>% filter(Idade != 'NA')

colSums(is.na(df_plane_fabricante))

#-- Idade Média
idade_fabricante <- df_plane_fabricante %>% group_by(tipo,manufacturer) %>% summarise(qt = n(),
                                                                 minimo = min(Idade),
                                                                 maximo = max(Idade),
                                                                 prim_quartil = quantile(Idade, 0.25),
                                                                 mediana = median(Idade),
                                                                 media = mean(Idade),
                                                                 sd = sd(Idade),
                                                                 var = var(Idade))

boxplot(Idade ~ manufacturer, data = df_plane_fabricante,
        horizontal = F, 
        main = 'Análise Bivariada - Idade vs Fabricante', cex.main = 1.0,
        xlab = '',
        ylab = 'Idade', cex.axis = 1.0,
        ylim = c(0, 65), 
        col = c('dodgerblue', 'pink', 'chartreuse4', 'tomato', 'cyan', 'chocolate', border = 'gray'),
        outline = T)

#-- On Time e Deplayed por Fabricante ------------
tbl <- prop.table(table(df_plane_fabricante$tipo, df_plane_fabricante$manufacturer), margin = 2)
barplot(tbl,
        main = 'Análise Bivariada - Atrasos por Fabricante', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '', las = 2, cex.axis = 0.8,
        ylab = 'Frequência (%)', cex.axis = 1.0,
        ylim = c(0,1), col = c('tomato','chartreuse4'), border='gray20')
legend("top", legend = row.names(tbl), fill = c('tomato', 'chartreuse4'))


############ Atraso por Fabricante --##############
df_atraso_fabricante <- df_plane_fabricante %>% filter(tipo == 'delayed')

boxplot(dep_delay ~ manufacturer, data = df_atraso_fabricante,
        horizontal = F, 
        main = 'Análise Bivariada - Atraso por Fabricante', cex.main = 1.0,
        xlab = '',
        ylab = 'Atraso em minutos', cex.axis = 1.0,
        ylim = c(0, 220), 
        col = c('dodgerblue', 'pink', 'chartreuse4', 'tomato', 'cyan', 'chocolate', border = 'gray'),
        outline = T)

tb_atraso_fabricante <- df_atraso_fabricante %>% group_by(manufacturer) %>% summarise(qt = n(),
                                                                        minimo = min(dep_delay),
                                                                        maximo = max(dep_delay),
                                                                        prim_quartil = quantile(dep_delay, 0.25),
                                                                        mediana = median(dep_delay),
                                                                        media = mean(dep_delay),
                                                                        sd = sd(dep_delay),
                                                                        var = var(dep_delay))

#-- Confirma que as médias são bem diferentes
one_way_anova_compania <- aov(dep_delay ~ manufacturer, data = df_atraso_fabricante)
summary(one_way_anova_compania)

################## FECHADO ###################




########## QUANTIDADE MOTORES ###############

#-- Verifica NAs
colSums(is.na(df_plane))

df_plane_motores <- df_plane %>% filter(engines != 'NA')

############ Atraso por Qtde Motor --##############

#-- On Time e Deplayed por Motor ------------
tbl <- prop.table(table(df_plane_motores$tipo, df_plane_motores$engines), margin = 2)
barplot(tbl,
        main = 'Análise Bivariada - Atrasos por Motor', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '', 
        ylab = 'Frequência (%)', cex.axis = 1.0,
        ylim = c(0,1), col = c('tomato','chartreuse4'), border='gray20')
legend("top", legend = row.names(tbl), fill = c('tomato', 'chartreuse4'))


df_atraso_motores <- df_plane_motores %>% filter(tipo == 'delayed')

boxplot(dep_delay ~ engines, data = df_atraso_motores,
        horizontal = F, 
        main = 'Análise Bivariada - Atraso por Qtde Motor', cex.main = 1.0,
        xlab = '',
        ylab = 'Atraso em minutos', cex.axis = 1.0,
        ylim = c(0, 220), 
        col = c('dodgerblue', 'pink', 'chartreuse4', 'tomato', 'cyan', 'chocolate', border = 'gray'),
        outline = T)

tb_atraso_motores <- df_atraso_motores %>% group_by(engines) %>% summarise(qt = n(),
                                                                           minimo = min(dep_delay),
                                                                           maximo = max(dep_delay),
                                                                           prim_quartil = quantile(dep_delay, 0.25),
                                                                           mediana = median(dep_delay),
                                                                           media = mean(dep_delay),
                                                                           sd = sd(dep_delay),
                                                                           var = var(dep_delay))

#-- Confirma que as médias são iguais
one_way_anova_compania <- aov(dep_delay ~ engines, data = df_atraso_motores)
summary(one_way_anova_compania)

#############################################


################## MODELO ###################

#-- Verifica NAs
colSums(is.na(df_plane))

############ Atraso por Qtde Motor --##############
df_atraso_modelo <- df_plane %>% filter(tipo == 'delayed')

boxplot(dep_delay ~ model, data = df_atraso_modelo,
        horizontal = F, 
        main = 'Análise Bivariada - Atraso por Modelo', cex.main = 1.0,
        xlab = '',
        ylab = 'Atraso em minutos', cex.axis = 1.0,
        ylim = c(0, 1350), 
        col = c('dodgerblue', 'pink', 'chartreuse4', 'tomato', 'cyan', 'chocolate', border = 'gray'),
        outline = T)

boxplot(dep_delay ~ model, data = df_atraso_modelo,
        horizontal = F, 
        main = 'Análise Bivariada - Atraso por Modelo', cex.main = 1.0,
        xlab = '',
        ylab = 'Atraso em minutos', cex.axis = 1.0,
        ylim = c(0, 300), 
        col = c('dodgerblue', 'pink', 'chartreuse4', 'tomato', 'cyan', 'chocolate', border = 'gray'),
        outline = F)

tb_atraso_modelo <- df_atraso_modelo %>% group_by(manufacturer,model) %>% summarise(qt = n(),
                                                                           minimo = min(dep_delay),
                                                                           maximo = max(dep_delay),
                                                                           prim_quartil = quantile(dep_delay, 0.25),
                                                                           mediana = median(dep_delay),
                                                                           media = mean(dep_delay),
                                                                           sd = sd(dep_delay),
                                                                           var = var(dep_delay))

#-- Confirma que as médias são iguais
one_way_anova_compania <- aov(dep_delay ~ model, data = df_atraso_modelo)
summary(one_way_anova_compania)

###############################################

########### ATRASO PLANE ################

#-- Verifica NAs
colSums(is.na(df_plane))

tb_atraso_plane <- df_plane %>% filter(tipo == 'delayed') %>% group_by(manufacturer,model, engines, engine, Idade) %>% summarise(qt = n(),
                                                                           minimo = min(dep_delay),
                                                                           maximo = max(dep_delay),
                                                                           prim_quartil = quantile(dep_delay, 0.25),
                                                                           mediana = median(dep_delay),
                                                                           media = mean(dep_delay),
                                                                           sd = sd(dep_delay),
                                                                           var = var(dep_delay))

####### Engine TURBO FAN ##############

#-- Verifica NAs
colSums(is.na(df_atraso_modelo))

df_plane_turboFan <- df_atraso_modelo %>% filter(engine == 'Turbo-fan')

boxplot(dep_delay ~ manufacturer, data = df_plane_turboFan,
        horizontal = F, 
        main = 'Análise Bivariada - Atraso por Turbo-Fan', cex.main = 1.0,
        xlab = 'Fabricante',
        ylab = 'Atraso em minutos', cex.axis = 1.0,
        ylim = c(0, 350), 
        col = c('dodgerblue', 'pink', 'chartreuse4', 'tomato', 'cyan', 'chocolate', border = 'gray'),
        outline = F)

tb_plane_turboFan <- df_plane_turboFan %>% group_by(manufacturer, model) %>% summarise(qt = n(),
                                                                                minimo = min(dep_delay),
                                                                                maximo = max(dep_delay),
                                                                                prim_quartil = quantile(dep_delay, 0.25),
                                                                                mediana = median(dep_delay),
                                                                                media = mean(dep_delay),
                                                                                sd = sd(dep_delay),
                                                                                var = var(dep_delay))

#-- Fabricante GULFSTREAM AEROSPACE
qt <- df_plane %>% group_by(tipo) %>% summarise(qt = n(),
                                                minimo = min(dep_delay),
                                                maximo = max(dep_delay),
                                                prim_quartil = quantile(dep_delay, 0.25),
                                                mediana = median(dep_delay),
                                                media = mean(dep_delay),
                                                sd = sd(dep_delay),
                                                var = var(dep_delay))


df_atrasildo <- df_plane %>% filter(manufacturer == 'GULFSTREAM AEROSPACE')

boxplot(dep_delay ~ model, data = df_atrasildo,
        horizontal = F, 
        main = 'Análise Bivariada - Atraso por Turbo-Fan', cex.main = 1.0,
        xlab = 'Modelo',
        ylab = 'Atraso em minutos', cex.axis = 1.0,
        ylim = c(0, 200), 
        col = c('dodgerblue', 'pink', 'chartreuse4', 'tomato', 'cyan', 'chocolate', border = 'gray'),
        outline = T)

giv <- df_atrasildo %>% select(dep_delay, model) %>% filter(model == 'G-IV')
giv <- df_atrasildo %>% select(dep_delay)
g1159b <- df_atrasildo %>% select(dep_delay, model) %>% filter(model == 'G1159B')
g1159b <- df_atrasildo %>% select(dep_delay)

t.test(giv, g1159b, alternative = 'greater', var.equal = FALSE)


############ Thursday ##########

quinta <- df_atraso_modelo %>% filter(week_day == 'Thursday') %>% 
        group_by(manufacturer, model, engine, dep_delay) %>% 
        select(manufacturer, model, engine, dep_delay) %>%
        summarise(qt = n(),
                  minimo = min(dep_delay),
                  maximo = max(dep_delay),
                  prim_quartil = quantile(dep_delay, 0.25),
                  mediana = median(dep_delay),
                  media = mean(dep_delay),
                  sd = sd(dep_delay),
                  var = var(dep_delay))

########### SEATS > 100 ##############

df_plane_comercial <- df_plane %>% filter(seats > 100)
colSums(is.na(df_plane_comercial))

#-- On Time e Deplayed por Engine ------------
tbl <- prop.table(table(df_plane_comercial$tipo, df_plane_comercial$engine), margin = 2)
barplot(tbl,
        main = 'Análise Bivariada - Horário', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = 'Frequencia', cex.axis = 1.0,
        ylim = c(0,1), col = c('tomato','chartreuse4'), border='gray20')
legend("top", legend = row.names(tbl), fill = c('tomato', 'chartreuse4'))


df_atraso_comercial <- df_plane_comercial %>% filter(tipo == 'delayed')


boxplot(dep_delay ~ engine, data = df_atraso_comercial,
        horizontal = F, 
        main = 'Análise Bivariada - Atraso', cex.main = 1.0,
        xlab = '',
        ylab = 'Frequência', cex.axis = 1.0,
        ylim = c(0, 220), 
        col = c('dodgerblue', 'pink', 'chartreuse4', border = 'gray'),
        outline = T)

tb_atraso_comercial <- df_atraso_comercial %>% group_by(engine) %>% summarise(qt = n(),
                                                                        minimo = min(dep_delay),
                                                                        maximo = max(dep_delay),
                                                                        prim_quartil = quantile(dep_delay, 0.25),
                                                                        mediana = median(dep_delay),
                                                                        media = mean(dep_delay),
                                                                        sd = sd(dep_delay),
                                                                        var = var(dep_delay))

#-- Confirma que as médias são bem diferentes
one_way_anova_compania <- aov(dep_delay ~ engine, data = df_atraso_comercial)
summary(one_way_anova_compania)


#-- SEM ofensor
df_atraso_comercial_ofensor <- df_atraso_comercial %>% filter(engine != 'Reciprocating')

boxplot(dep_delay ~ engine, data = df_atraso_comercial_ofensor,
        horizontal = F, 
        main = 'Análise Bivariada - Atraso', cex.main = 1.0,
        xlab = '',
        ylab = 'Frequência', cex.axis = 1.0,
        ylim = c(0, 220), 
        col = c('dodgerblue', 'pink', 'chartreuse4', border = 'gray'),
        outline = F)

tb_atraso_comercial_ofensor <- df_atraso_comercial_ofensor %>% group_by(engine) %>% summarise(qt = n(),
                                                                              minimo = min(dep_delay),
                                                                              maximo = max(dep_delay),
                                                                              prim_quartil = quantile(dep_delay, 0.25),
                                                                              mediana = median(dep_delay),
                                                                              media = mean(dep_delay),
                                                                              sd = sd(dep_delay),
                                                                              var = var(dep_delay))

#-- Confirma que as médias são bem diferentes
one_way_anova_compania <- aov(dep_delay ~ engine, data = df_atraso_comercial_ofensor)
summary(one_way_anova_compania)

ofensor1 <- df_atraso_comercial_ofensor %>% filter(engine == 'Turbo-fan') %>% select(dep_delay)
ofensor2 <- df_atraso_comercial_ofensor %>% filter(engine == 'Turbo-jet') %>% select(dep_delay)

t.test(ofensor1, ofensor2, alternative = 'two.side', var.equal = F)

ofensor1 <- df_atraso_comercial_ofensor %>% filter(engine == 'Turbo-fan') %>% select(dep_delay)
ofensor2 <- df_atraso_comercial_ofensor %>% filter(engine == 'Turbo-jet') %>% select(dep_delay)

t.test(ofensor1, ofensor2, alternative = 'two.side', var.equal = F)

a <- df_plane %>% filter(engine != 'Turbo-jet')
a <- a %>% filter(seats > 100, tipo == 'ontime')

ontime <- df_plane %>% filter(engine != 'Reciprocating', tipo == 'on time', seats > 100)
on_ofensor1 <- ontime %>% filter(engine == 'Turbo-fan') %>% select(dep_delay)
on_ofensor2 <- ontime %>% filter(engine == 'Turbo-jet') %>% select(dep_delay)

t.test(ofensor1, on_ofensor1, alternative = 'great', var.equal = F)


cia <- df_voos %>% inner_join(airlines, by = 'carrier')
cia <- df_plane %>% inner_join(planes, by = 'tailnum')

############## DL x Modelo ################

df_cia <- df_voos %>% filter(carrier == 'DL', tipo == 'delayed') %>% 
        inner_join(planes, by = 'tailnum')

colSums(is.na(df_cia))

#-- tira voos cancelados???
df_cia_modelo <- df_cia %>% filter(arr_time != 'NA',)

colSums(is.na(df_cia_modelo))

boxplot(dep_delay ~ model, data = df_cia_modelo,
        horizontal = F, 
        main = 'Análise Bivariada - DL Modelo', cex.main = 1.0,
        xlab = '',
        ylab = 'Frequência (%)', cex.axis = 1.0,
        ylim = c(0, 300), 
        col = c('dodgerblue', 'pink', 'chartreuse4', border = 'gray'),
        outline = F)

tb_cia_modelo <- df_cia_modelo %>% group_by(model, manufacturer) %>% summarise(qt = n(),
                                                                               minimo = min(dep_delay),
                                                                               maximo = max(dep_delay),
                                                                               prim_quartil = quantile(dep_delay, 0.25),
                                                                               mediana = median(dep_delay),
                                                                               media = mean(dep_delay),
                                                                               sd = sd(dep_delay),
                                                                               var = var(dep_delay))


############## UA x Modelo ################

df_cia <- df_voos %>% filter(carrier == 'DL', tipo == 'delayed') %>% 
        inner_join(planes, by = 'tailnum')

colSums(is.na(df_cia))

a <- df_cia %>% filter(is.na(arr_time))
na <- df_voos %>% filter(flight == 1299, hour == 20, minute == 20)

#-- Retirando NAs????????
df_voos <- na.omit(df_xx)

colSums(is.na(df_cia))

#-- tira voos cancelados???
#df_cia_modelo <- df_cia %>% filter(arr_time != 'NA',)

par(mar = c(20,5,2,0))
boxplot(dep_delay ~ manufacturer, data = df_cia,
        horizontal = F, 
        main = 'Análise Bivariada - Modelo', cex.main = 1.0,
        xlab = '', las = 2,
        ylab = 'Frequência (%)', cex.axis = 1.0,
        col = c('dodgerblue', 'pink', 'chartreuse4', border = 'gray'),
        outline = F)

tb_cia <- df_cia %>% group_by(model, manufacturer) %>% summarise(qt = n(),minimo = min(dep_delay),
                                                                          maximo = max(dep_delay),
                                                                          prim_quartil = quantile(dep_delay, 0.25),
                                                                          mediana = median(dep_delay),
                                                                          media = mean(dep_delay),
                                                                          sd = sd(dep_delay),
                                                                          var = var(dep_delay))

one_way_anova_compania <- aov(dep_delay ~ manufacturer, data = df_cia)
summary(one_way_anova_compania)





