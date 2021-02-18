# Trabalho em Individual

setwd('C:/FGV/Metodos Matriciais e Analise de Cluster/Trabalho_final_CP')
getwd()

#Bibliotecas
library(dplyr)         # Manipulacao dos Bancos de Dados
library(summarytools)
library(readxl)
library(NbClust)
library(cluster)
library(fpc)


#Carregar base
autos <- read_excel("PTA T9  MMCA CARS32v1.xlsx")
View(autos)
names(autos)

cc <- autos[,3:10]
names(cc)

par(mfrow=c(3,3))
boxplot(cc$weight)
table(cc$weight)
table(cc$Length)
boxplot(cc$Braking)
table(cc$Cylinders)
boxplot(cc$Displacement)
boxplot(cc$City)
table(cc$City)
boxplot(cc$Highway)
boxplot(cc$GHG)

#Matriz de Correlação
options(scipen = 999)
round(cor(cc),3)

#Passo 1 - Determintar as CP
cp=prcomp(cc,scale. = T) # acha as CPs das variaveis padronizadas
cp$x
plot(cp, ylim=c(0,7), col=11, main="variancias das CP")
grid(col=4) #eixo vertical as variancias das CP

#Passo 2 - Quais são as regras para determinar com qtas CP ficar
abline(h=1, col=2, lwd=2) #variancia > 1
abline(h=.75, col=4, lwd=2) #variancia > 0.75

#Passo 3 - Como interpretar as CPs
summary(cp) #pelo summary é possivel identificar qual a perda Proporção Cumulativa
quatrocomp=cp$x[,1:4] #ela contem as 6 CPs; mas nos vamos trabalhar só com as 4 primeiras
quatrocomp

#calcular as correlações entre os X e as CP --> interpretar
round(cor(cc, quatrocomp),3)

#Verificar a representação gráfica
plot(cp$x[,3], cp$x[,4],type = "n", xlab = "PC1", ylab="PC2")
cores=ifelse(autos$Cylinders==4, "red",ifelse(autos$Cylinders==6,"blue","green"))
pixel=ifelse(autos$Cylinders==4, 11,19)
text(cp$x[,1], cp$x[,2], col=cores, labels = autos$Car )
grid(col=2)

biplot(cp, xlab="cp1",ylab="cp2",xlabs=autos$Car,cex=.5, scale=0)
abline(h=0,v=0,col="blue",lty=1)
grid(col="green")

#pesos
cp$rotation
#################################################################
# cluster com as vars quantitativas ...qq
names(cc)
cc.scale=scale(cc)
head(cc.scale)
dd=dist(cc.scale)
hco=hclust(dd, method = "ward.D2")
plot(hco, hang = -1)
abline(h=6, col=3)
autos$hcoriginal=cutree(hco, 4)

#cluster com as 4 CP
duas.scale= scale(quatrocomp) #padronizando
ddcp=dist(duas.scale) 
hcc=hclust(ddcp, method = "ward.D2")
plot(hcc, hang = -1)
autos$hccomp=cutree(hcc,4)
boxplot(autos$weight~autos$hcoriginal)

#cruzar os dois resultados observa-se que não tem diferença entre as variaveis originais com as CPs
table(autos$hcoriginal, autos$hccomp, dnn = c("originial", "princomp"))

############### RESPOSTAS

#1.a --> CP = 4
#1.b --> 0.111
#1.c --> 17 , 15
cp1 <- cp$x[,1:2]
cp1

cp1.scale=scale(cp1) #Padronizando
head(cp1.scale)

nk=NbClust(data=cp1.scale, min.nc = 2,max.nc = 8,method = "kmeans", index = "all" )

set.seed(11)
kmn=kmeans(cp1.scale,2,nstart=25)
cp1$kmn=kmn$cluster
kmn$size

#1.d --> 6.010, 1.067
class(cc)
dim(cc)
R <- cor(cc[-9])
R
round(R,3)
options(scipen = 999)
L=eigen(R)
round(L$values,3)

#1.e --> 9.777
Rinv <- round(solve(R),3)
diag(Rinv)


#1.f --> Sim. Pois R.x = λ, onde x e λ são paralelos.
#Ou seja, R %*% x = kx, sendo k = 0.4530836 (autovalor de R).

x=c(-0.17413241, -0.36753180,  0.01375612, -0.68214295, -0.52446332,  -0.57510432, 
    -0.79494180,  0.32461643)
sum(x)

resultado <- R %*% x
autovalor <- resultado / x #0.4530836
round(autovalor, 3)

d <- x/R #dá os valores da diagonal principal
diag(d)

xt = 0.4530836 %*% x
t(xt);resultado


#2.a -->  5.625
cc <- autos[,3:8]
cc <- cc[,-5]
names(cc)

correl=cor(cc)
round(correl,digits=2) 

cc.scale=scale(cc) #padroniza os dados -subtrai média  divide pelo dp
head(cc.scale,4)
cc.dist=dist(cc.scale, method = "manhattan")  
round(cc.dist,3)

#2.b -->20  12
drivers.dist=dist(cc.scale)
hc=hclust(drivers.dist, method = "ward.D2")
plot(hc,hang = -1) #dendrograma
autos$hcl=cutree(hc, 2)
table(autos$hcl)

#2.c 
a <- select(autos, Car, CODE, hcl,weight) %>% filter(autos$hcl == 2)

#2.d
boxplot(autos$weight~autos$hcl, main= 'Weight', col=topo.colors(4))
summary(a$weight)

#2.e
set.seed(11)
kmd=pamk(drivers.dist, diss = T, k=3, criterion="ch", critout = T)
kmd$nc
autos$kmd=kmd$pamobject$clustering
table(autos$kmd)

boxplot(autos$weight~autos$kmd, main= 'Weight', col=topo.colors(5))

cluster.stats(drivers.dist, autos$hcl, autos$kmd)$corrected.rand
