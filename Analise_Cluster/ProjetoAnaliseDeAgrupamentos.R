########### TRABALHO EM GRUPO ############
## PROJETO ANÁLISE DE AGRUPAMENTOS 
install.packages("factoextra")
install.packages("gridExtra")
install.packages("corrplot")

library(readxl)
library(cluster)
library(fpc)
library(NbClust)
library(ggplot2)
library(factoextra)
library(gridExtra)
library(corrplot)
library(summarytools)

setwd('C:/FGV/Metodos Matriciais e Analise de Cluster/Cluster_Analysis')
df_empresas <- read_excel("minimarket.xlsx")


head(df_empresas)

#Estrutura
str(df_empresas)

#NAs
colSums(is.na(df_empresas))

##### 1. Analise Variaveis ####

#### zona ####
a = view(summary(df_empresas$zona))

table(df_empresas$zona)
class(df_empresas$zona)
#transformando chr em factor para calcular distâncias com daisy??
df_empresas$zona = as.factor(df_empresas$zona)

table(df_empresas$zona, useNA = "ifany")


barplot(table(df_empresas$zona),
        main = 'Zona', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = '', cex.axis = 1.0,
        ylim = c(0,50), col = 'steelblue3', border = 'steelblue4')


#### idade ####
#transformando chr em factor para calcular distâncias com daisy??
## factor ou ordered??
df_empresas$idade = as.ordered(df_empresas$idade) 

table(df_empresas$idade, useNA = "ifany")


barplot(table(df_empresas$idade),
        main = 'Idade', cex.main = 1.0,
        cex.names = 1.0,
        xlab = '',
        ylab = '', cex.axis = 1.0,
        ylim = c(0,50), col = 'steelblue3', border = 'steelblue4')

#### lucro ####
table(df_empresas$lucro, useNA = "ifany")
summary(df_empresas$lucro)

par(mfrow=c(1,2))
b=boxplot(df_empresas$lucro, main="Lucro", col = 4)
hist(df_empresas$lucro, main="Lucro", col = 4)

#### faturamento ####
table(df_empresas$faturamento, useNA = "ifany")
summary(df_empresas$faturamento)
par(mfrow=c(1,2))
b=boxplot(df_empresas$faturamento, main="Faturamento", col = 4)
hist(df_empresas$faturamento, main="Faturamento", col = 4)

#### metas ####
table(df_empresas$metas, useNA = "ifany")
summary(df_empresas$metas)
par(mfrow=c(1,2))
b=boxplot(df_empresas$metas, main="Metas", col = 4)
hist(df_empresas$metas, main="Metas", col = 4)

#### publ ####
table(df_empresas$publ, useNA = "ifany")
summary(df_empresas$publ)
par(mfrow=c(1,2))
b=boxplot(df_empresas$publ, main="Publ", col = 4)
hist(df_empresas$publ, main="Publ", col = 4)


##### 2. Matriz de Correlação ####
correl=cor(df_empresas[ , 4:7])
round(correl,digits=2) # 2 casos de alta correlação

par(mfrow=c(2,2))
corrplot(correl, method = "pie")
corrplot(correl, method = "circle")
corrplot(correl, method = "number")
corrplot.mixed(correl)

#### 3. Clusters Hierarquicos ####

#### caso 2: drivers: todas as vars quali e quanti (exceto empresa) ####
drivers <- df_empresas[,2:7]
names(drivers)


#### SEM PESO ####

#Matriz de Distancias #
drivers.dist=daisy(drivers)
# quando tenho vars qualitativas e quantitativas--> padroniza automaticamente
hc2=hclust(drivers.dist, method = "ward.D2")
plot(hc2,hang = -1) #dendrograma


#### COM PESO ####

#Atribuindo peso diferente para as variaveis quali
pesos=c(.5,.5,1,1,1,1)

drivers_peso.dist=daisy(drivers, weights = pesos)
hc3=hclust(drivers_peso.dist, method = "ward.D2")
plot(hc3,hang = -1) #dendrograma

##### 4. K-Medoid ####

#### caso 2: drivers: todas as vars quali e quanti (exceto empresa) ####
#SEM PESO
set.seed(123)  #por causa da partição inicial
kmd=pamk(drivers.dist, diss = T, k=2:6, criterion="ch", critout = T)
kmd$nc
df_empresas$kmd=kmd$pamobject$clustering
table(df_empresas$kmd)


# COM PESO
set.seed(123)  #por causa da partição inicial
kmd_peso=pamk(drivers_peso.dist, diss = T, k=2:6, criterion="ch", critout = T)
kmd_peso$nc
df_empresas$kmd_peso=kmd_peso$pamobject$clustering
table(df_empresas$kmd_peso)


##### COMPARANDO DENDOGRAMAS #####
par(mfrow=c(1,2))
plot(hc2, hang = -1)    # ward todas drivers - hierarquico
plot(hc3, hang = -1)    # ward com peso 

##### COMPARANDO Analise dos Clusters #####

# Tabelas com corte - K = ?
k = 2

df_empresas$hcl2=cutree(hc2, k) #quanti e quali - Ward
table(df_empresas$hcl2)

df_empresas$hcl3=cutree(hc3, k) #quanti e quali - Ward
table(df_empresas$hcl3)


set.seed(1987)  #por causa da partição inicial
kmedo=pamk(drivers.dist, diss = T, k=k, criterion="ch", critout = T)
#utiliza criterio silhouette para avaliar k
kmedo$nc
df_empresas$kmedo=kmedo$pamobject$clustering
table(df_empresas$kmedo)

set.seed(1987)  #por causa da partição inicial
kmedo_peso=pamk(drivers_peso.dist, diss = T, k=k, criterion="ch", critout = T)
#utiliza criterio silhouette para avaliar k
kmedo_peso$nc
df_empresas$kmedo_peso=kmedo_peso$pamobject$clustering
table(df_empresas$kmedo_peso)


#Hierarquico
#Quanti e Quali - ward - SEM PESO
par(mfrow=c(2,2))
boxplot(df_empresas$lucro ~ df_empresas$hcl2, main= 'Lucro', col=topo.colors(4))
boxplot(df_empresas$faturamento ~ df_empresas$hcl2, main= 'Faturamento', col=topo.colors(4))
boxplot(df_empresas$metas ~ df_empresas$hcl2, main= 'Metas', col=topo.colors(4))
boxplot(df_empresas$publ ~ df_empresas$hcl2, main= 'Publ', col=topo.colors(4))

#Quanti e Quali - ward - COM PESO
boxplot(df_empresas$lucro ~ df_empresas$hcl3, main= 'Lucro', col=topo.colors(4))
boxplot(df_empresas$faturamento ~ df_empresas$hcl3, main= 'Faturamento', col=topo.colors(4))
boxplot(df_empresas$metas ~ df_empresas$hcl3, main= 'Metas', col=topo.colors(4))
boxplot(df_empresas$publ ~ df_empresas$hcl3, main= 'Publ', col=topo.colors(4))


##K-medoid
#Quali e Quanti  sugerido - SEM PESO
boxplot(df_empresas$lucro ~ df_empresas$kmd, main= 'Lucro', col=topo.colors(4))
boxplot(df_empresas$faturamento ~ df_empresas$kmd, main= 'Faturamento', col=topo.colors(4))
boxplot(df_empresas$metas ~ df_empresas$kmd, main= 'Metas', col=topo.colors(4))
boxplot(df_empresas$publ ~ df_empresas$kmd, main= 'Publ', col=topo.colors(4))

#Quali e Quanti  sugerido - COM PESO
boxplot(df_empresas$lucro ~ df_empresas$kmd_peso, main= 'Lucro', col=topo.colors(4))
boxplot(df_empresas$faturamento ~ df_empresas$kmd_peso, main= 'Faturamento', col=topo.colors(4))
boxplot(df_empresas$metas ~ df_empresas$kmd_peso, main= 'Metas', col=topo.colors(4))
boxplot(df_empresas$publ ~ df_empresas$kmd_peso, main= 'Publ', col=topo.colors(4))

#Quali e Quanti setado k = 3 - SEM PESO
boxplot(df_empresas$lucro ~ df_empresas$kmedo, main= 'Lucro', col=topo.colors(4))
boxplot(df_empresas$faturamento ~ df_empresas$kmedo, main= 'Faturamento', col=topo.colors(4))
boxplot(df_empresas$metas ~ df_empresas$kmedo, main= 'Metas', col=topo.colors(4))
boxplot(df_empresas$publ ~ df_empresas$kmedo, main= 'Publ', col=topo.colors(4))

#Quali e Quanti setado k = 3 - COM PESO
boxplot(df_empresas$lucro ~ df_empresas$kmedo_peso, main= 'Lucro', col=rainbow(4))
boxplot(df_empresas$faturamento ~ df_empresas$kmedo_peso, main= 'Faturamento', col=rainbow(4))
boxplot(df_empresas$metas ~ df_empresas$kmedo_peso, main= 'Metas', col=rainbow(4))
boxplot(df_empresas$publ ~ df_empresas$kmedo_peso, main= 'Publ', col=rainbow(4))


#analisando as demais vars (descritivas)
#Quanti e Quali - ward - sem peso
table(df_empresas$zona, df_empresas$hcl2, dnn=c('zona', 'hcl'))
table(df_empresas$idade, df_empresas$hcl2, dnn=c('idade', 'hcl'))

#Quanti e Quali - ward - com peso
table(df_empresas$zona, df_empresas$hcl3, dnn=c('zona', 'hcl'))
table(df_empresas$idade, df_empresas$hcl3, dnn=c('idade', 'hcl'))

## k- medoid
#Quanti e quali - 123 - sem peso
table(df_empresas$zona, df_empresas$kmd, dnn=c('zona', 'hcl'))
table(df_empresas$idade, df_empresas$kmd, dnn=c('idade', 'hcl'))

#Quanti e quali - 123 - com peso
table(df_empresas$zona, df_empresas$kmd_peso, dnn=c('zona', 'hcl'))
table(df_empresas$idade, df_empresas$kmd_peso, dnn=c('idade', 'hcl'))

#Quanti e quali - 1987 - sem peso
table(df_empresas$zona, df_empresas$kmedo, dnn=c('zona', 'hcl'))
table(df_empresas$idade, df_empresas$kmedo, dnn=c('idade', 'hcl'))

#Quanti e quali - 1987 - com peso
table(df_empresas$zona, df_empresas$kmedo_peso, dnn=c('zona', 'hcl'))
table(df_empresas$idade, df_empresas$kmedo_peso, dnn=c('idade', 'hcl'))


# Tabelas de Clusters

table(df_empresas$hcl2)
table(df_empresas$hcl3)
table(df_empresas$kmd)
table(df_empresas$kmd_peso)
table(df_empresas$kmedo)
table(df_empresas$kmedo_peso)

## Correlação entre os resultados

#Hierarquico x Kmedoid - sem peso - 123
cluster.stats(drivers.dist, df_empresas$hcl2, df_empresas$kmd)$corrected.rand

#Hierarquico x Kmedoid - com peso - 123
cluster.stats(drivers_peso.dist, df_empresas$hcl3, df_empresas$kmd_peso)$corrected.rand

#Hierarquico x kmedoid - sem peso - 1987
cluster.stats(drivers.dist, df_empresas$hcl2, df_empresas$kmedo)$corrected.rand

#Hierarquico x kmedoid - com peso - 1987
cluster.stats(drivers_peso.dist, df_empresas$hcl3, df_empresas$kmedo_peso)$corrected.rand

#########Centroide ???
cluster.stats(drivers.dist, df_empresas$kmd )$within.cluster.ss
cluster.stats(drivers_peso.dist, df_empresas$kmd_peso )$within.cluster.ss
cluster.stats(drivers.dist, df_empresas$kmedo)$within.cluster.ss
cluster.stats(drivers_peso.dist, df_empresas$kmedo_peso)$within.cluster.ss


a=ggplot(data = df_empresas) + 
  aes(x = idade, y = zona , color = idade) +
  geom_point() +
  theme_minimal()


b=ggplot(data = df_empresas) + 
  aes(x = lucro, y = meta, color = idade) +
  geom_point() +
  theme_minimal()

ggplot(data = drivers.dist) + 
  aes(x = luro, y = meta, color = idade) +
  geom_point() +
  theme_minimal()

plot(a, b, ncol=2)

grafico <- fviz_cluster(hcl2, geom = "point", data = drivers.dist) + ggtitle("K = 3")

prop.table(df_empresas[,4:7])


