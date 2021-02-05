
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

########################################################
################ PLANES ###################
############-- Observando Planes --##########

view(dfSummary(planes))

#-- Verifica NAs
colSums(is.na(planes))

################## FLIGHTS X PLANES ####################

df_plane <- planes

#-- Carrega fligths COM planes 
df_plane <- df_voos %>% inner_join(df_plane, by = 'tailnum')


#-- Verifica NAs
colSums(is.na(df_plane))

############ Atraso por Idade --##############

#-- Criando variável idade
df_plane <- df_plane %>% mutate(Idade = year.x - year.y)

#-- Retira NAs
df_plane_idade <- df_plane %>% filter(Idade != 'NA')

### Voos x Idade - Dispersão
scatterplot(dep_delay ~ Idade | tipo,
            data = df_plane_idade, smooth = F,
            main = 'Análise Bivariada - Dispersão', cex.main = 1.0,
            xlab = 'Idade',
            ylab = 'Vôos atrasadosrm', cex.axis = 1.0,
            pch = c(19,17), cex = 1.0,
            col = c('blue','deeppink'))

summary(lm(dep_delay ~ Idade, data = df_plane_idade))
#-- sem relação nenhuma ---


########## Fabricante ##########

#-- Verifica NAs
colSums(is.na(df_plane))

df_plane_fabricante <- df_plane %>% filter(manufacturer != 'NA')

#-- On Time e Deplayed por Fabricante ------------
tbl <- prop.table(table(df_plane_fabricante$tipo, df_plane_fabricante$manufacturer), margin = 2)
barplot(tbl,
        main = 'Análise Bivariada - Horário', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = 'Frequência', cex.axis = 1.0,
        ylim = c(0,1), col = c('tomato','chartreuse4'), border='gray20')
legend("top", legend = row.names(tbl), fill = c('tomato', 'chartreuse4'))


#-- Atraso por Fabricante
df_atraso_fabricante <- df_plane_fabricante %>% filter(tipo == 'delayed')

boxplot(tipo ~ manufacturer, data = df_atraso_fabricante,
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


