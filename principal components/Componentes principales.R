install.packages("cluster")
install.packages("car")
install.packages("caret")
install.packages("gplots")
install.packages("gmodels")
install.packages("ggplot2")
install.packages("biotools")
install.packages("WRS2")
install.packages("MVN")
install.packages("psych")
install.packages("mvtnorm")
library(mvtnorm)
library(psych)
library(WRS2)
library(biotools)
library(cluster)
library(car)
library(MVN)
library(gplots)
library(gmodels)
library(ggplot2)
library(caret)
#Elimina todos los objetos en memoria
rm(list = ls())
ls()
#IMPORTANDO BASE DATOS
base<-read.csv("J:/CICLO 2018-II/ESTAD  MULTI/TerceraMulti_Parte2/BASE.csv")
d1<-base[-1]# sacando los nombres de las ciudades
summary(d1)

d2 <- d1[-1] # sin la variable TMR
#View(d1)
dim(d1)

#ESTANDARIZACIÓN 
d3<-scale(d1[,c(-1)])
d3 <- as.data.frame(d3)
View(d3)

#ANALISIS EXPLORATORIO
#Distribuciones despues de estandarizar
par(mfrow=c(2,6))

hist(d3$TMR, col = "green",main = "TMR",xlab ="" )  
hist(d3$SMIN, col="red",main = "SMIN",xlab ="" )
hist(d3$SMEAN, col="blue",main = "SMEAN",xlab ="")
hist(d3$SMAX, col="green",main = "SMAX",xlab ="" )
hist(d3$PMIN, col = "red",main = "PMIN",xlab ="")  
hist(d3$PMEAN, col="blue",main = "PMEAN",xlab ="")
hist(d3$PMAX, col="green",main = "PMAX",xlab ="")
hist(d3$PM2, col = "red",main = "PM2",xlab ="" )  
hist(d3$PERWH, col="blue",main = "PERWH",xlab ="" )
hist(d3$NONPOOR, col="green",main = "NONPOOR",xlab ="" )
hist(d3$GE65, col = "red",main = "GE65",xlab ="")  
hist(d3$LPOP, col="blue",main = "LPOP",xlab ="")

par(mfrow=c(1,1))

library(TeachingDemos)
#???dimnames(d1)[[1]] <- c("Providence","Jackson1","Johnstown","Jersey City","Huntington","Des Moines","Denver","Reading","Toledo","Fresno","Memphis","York","Milwaukee","Savannah","Omaha","Topeka","Columbus","Beaumont","Winston","Detroit","EI Paso","Macon","Rockford","Jackson","Fall River","Boston","Dayton","Charlotte","Miami","Bridgeport","Sioux Falls","Chicago","South Bend","Norfolk","Cleveland","Austin","Knoxville","Indianapolis","Nashville","Seattle","Dallas","Mobile","Phoenix","Augusta","Youngstown","Chattanooga","Galveston","Fort Worth","Flint","Charleston","New Haven","Portland","St. Louis","Atlantic City","New Orleans","Las Vegas","Little River","San Francisco","Raleigh","Oklahoma City","Worcester","Gary","Pittsburgh","Waco","Manchester","Terre Haute","Allentown","Richmond","Houston","Newark","Birmingham","Shreveport","Columbia","Brockton","Tampa","Lansing","Kansas City","Buffalo","San Bernadino","Spokane")
#faces(d1)

# Analizando las correlaciones (libreria psych)

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="blue", ...)
}

pairs.panels(d1[,-1]) # sin estandarizar
pairs.panels(d3)  #estandarizados 

# Calculando las componentes paso por paso

# 1. calculamos la correlación
library(corrplot)
r=cor(d3)
#View(r)
corrplot(r, order = "hclust", tl.col='black', tl.cex=1)
# las q tienen mayor correlacion son
#smax y smean
#pmin y pmean
#pmean y pmax
#perwh y nonpoor

# 2. calculamos los autovectores y autovalores de r

valores<-eigen(r)
#valores caracteristicos
View(valores$values)
#aqui se puede distinguir cuatro eigenvalores mayores a la unidad

#vectores caracterísiticos
View(valores$vectors)


#3. Grafico de sedimentación, para calcular el nº de componentes
dev.off()
autovalores<-valores[[1]]
ncomp<-seq(1:11)

ggplot(data.frame(autovalores,ncomp),aes(x=ncomp,y=autovalores))+geom_line(color="red")+
  geom_point(color="blue")+
  labs(x="Number of components",y="variance",title="Sedimentation graph")

# Selecionando solo las componentes valores caracteristicos > 1
# Matriz de componentes (solo 4 componentes).
variables<-names(d3)
autovectores<-valores[[2]][,c(1,2,3,4)]
autovectores<-cbind(variables,as.data.frame(autovectores))
names(autovectores)=c("variables","CP1","CP2","CP3","CP4")
autovectores

# Analisis de componentes principales[da a conocer la variabilidad aportada]
pcal <- princomp(d3,scores = TRUE,cor = TRUE)
summary(pcal)
pcal


# Matriz de cargas de componentes principales
loadings(pcal)
pcal$scores
pcal$loadings
# Graficos de autovalores
plot(pcal)
screeplot(pcal, type = "line", main = "Scree Plot")

# Las puntuaciones se obtienen mediante la orden
pcal$scores[,1:4]
plot(pcal$scores[,1:4])

par(mfrow=c(2,3))

par(pty="s")
plot(pcal$scores[,1],pcal$scores[,2],
     ylim=range(pcal$scores[,1]),
     xlab="PC1",ylab="PC2",type="n",lwd=2)
text(pcal$scores[,1],pcal$scores[,2],
     labels=abbreviate(row.names(d1)),cex=0.7,lwd=2)

par(pty="s")
plot(pcal$scores[,1],pcal$scores[,3],
     ylim=range(pcal$scores[,1]),
     xlab="PC1",ylab="PC3",type="n",lwd=2)
text(pcal$scores[,1],pcal$scores[,3],
     labels=abbreviate(row.names(d1)),cex=0.7,lwd=2)

par(pty="s")
plot(pcal$scores[,1],pcal$scores[,4],
     ylim=range(pcal$scores[,1]),
     xlab="PC1",ylab="PC4",type="n",lwd=2)
text(pcal$scores[,1],pcal$scores[,4],
     labels=abbreviate(row.names(d1)),cex=0.7,lwd=2)

par(pty="s")
plot(pcal$scores[,2],pcal$scores[,3],
     ylim=range(pcal$scores[,2]),
     xlab="PC2",ylab="PC3",type="n",lwd=2)
text(pcal$scores[,2],pcal$scores[,3],
     labels=abbreviate(row.names(d1)),cex=0.7,lwd=2)

par(pty="s")
plot(pcal$scores[,2],pcal$scores[,4],
     ylim=range(pcal$scores[,2]),
     xlab="PC2",ylab="PC4",type="n",lwd=2)
text(pcal$scores[,2],pcal$scores[,4],
     labels=abbreviate(row.names(d1)),cex=0.7,lwd=2)

par(pty="s")
plot(pcal$scores[,3],pcal$scores[,4],
     ylim=range(pcal$scores[,1]),
     xlab="PC3",ylab="PC4",type="n",lwd=2)
text(pcal$scores[,3],pcal$scores[,4],
     labels=abbreviate(row.names(d1)),cex=0.7,lwd=2)

par(mfrow=c(1,1))


# Grafico de Scores de variables "Análisis dual: Variables y Individuo"
biplot(pcal)


# Scores finales para los 4 componntes 
scores_final<-pcal$scores[,c(1:4)]
View(base_final<-data.frame(base,scores_final))


pairs(base_final[-2],diag.panel=panel.hist)


#ANALISIS DE REGRESIÓN LINEAL MÚLTIPLE

str(base_final)
modelo<-lm(TMR~Comp.1+Comp.2+Comp.3+Comp.4,base_final)
modelo
summary(modelo)

#Comprobamos los algunos indicadores de multicolinealidad en esta regresion
# Mediante factor de inflacion de varianza (VIF)
library(car)
VIF<-vif(modelo)
VIF

# Mediante indice de tolerancia (TOL)
TOL<-1/VIF
TOL


########################################
#         ANALISIS FACTORIAL           #
########################################


install.packages("GPArotation")
install.packages("nFactors")
install.packages("gplots")
install.packages("RColorBrewer")
install.packages("semPlot")
library(GPArotation)
library(nFactors)
library(gplots)
library(RColorBrewer)
library(semPlot)

install.packages("grid")
install.packages("REdaS")
library(grid)
library(REdaS)

#Prueba de Esferidad de Bartlett
bart_spher(d3) #Prueba de multicolinealidad

#Prueba de Kayser - Mayer - Okin
KMOS(d3) #Prueba de adecuosidad de la muestra

# Analisis factorial considerando 2 factores con rotación varimax y guardar scores
# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(d3)) # get eigenvalues
ap <- parallel(subject=nrow(d3),var=ncol(d3),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# Factoriales sin rotacion

fa4 <- factanal(d3,factor =4,rotation = "none",scores = "regression")
fa4
# Factoriales con rotacion varimax
far4 <- factanal(d3,factor =4,rotation = "varimax",scores = "regression")
far4



#ANALISIS DE REGRESIÓN LINEAL MÚLTIPLE CON FACTORES
scores_final2<-far4$scores[,c(1:4)]
View(base_final2<-data.frame(base,scores_final2))

str(base_final2)
modelo2<-lm(TMR~Factor1+Factor2+Factor3+Factor4,base_final2)
modelo2
summary(modelo2)




Fac1 <- c(fa4$loadings[,"Factor1"])
Fac2 <- c(fa4$loadings[,"Factor2"])
Fac3 <- c(fa4$loadings[,"Factor3"])
Fac4 <- c(fa4$loadings[,"Factor4"])


#Cargas factoriales
Cargas <- data.frame(Fac1,Fac2,Fac3,Fac4)

# use regression scores
Scores <- data.frame(fa4$scores)
head(Scores)

pairs(Scores) #Grafico de scores

#Cración de una base de clasificación
Id <- DataCluster$Enc
MatrizCluster <- data.frame(Id,Scores)
attach(MatrizCluster)

##estas solo quedan como referencias-ya que deben ir en factorial ##

#Rotacion de ejes
#aplicando una rotación Varimax
rotación1<-varimax(pcal$scores)

variables2<-names(d3)
rot<-rotación1$rotmat[,c(1,2,3,4)]
rot<-cbind(variables2,as.data.frame(rot))
names(rot)=c("variables","CP1","CP2","CP3","CP4")
rot
#aplicando rotación promax
rotación2<-promax(pcal$scores)

variables3<-names(d3)
rot2<-rotación2$rotmat[,c(1,2,3,4)]
rot2<-cbind(variables3,as.data.frame(rot2))
names(rot2)=c("variables","CP1","CP2","CP3","CP4")
rot2

