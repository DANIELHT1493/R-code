###############################
###############################
## ANALISIS DISCRIMINANTE   ###
## ESTADISTICA MULTIVARIADA ###
###############################
###############################

# DANIEL HERNANDEZ TAPIA
# MICHAEL GUTIERREZ PEREDA

# Algunos paquetes necesarios

install.packages("magrittr")
install.packages("MVN")
install.packages("effsize")
install.packages("biotools", dependencies = T)
install.packages("biotools")
library(SpatialEpi)
library(biotools)
library(effsize)
library(magrittr)
library(MASS)
library(ggplot2)
library(scales)
library(gridExtra)
library(MVN)
library(mvtnorm)

#Importamos la base
base<-read.csv(file="C:/Users/TOSHIBA/Desktop/baseDiscreta.csv",header = T,sep = ",")
attach(base)
head(base)
str(base)
summary(base)
##################################
# Exploración gráfica de los datos
##################################

# a nivel individual
#####################
library(ggplot2)
library(ggpubr)

p2 <- ggplot(data = base, aes(x = Freshwater, fill = Specie)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = base, aes(x = Marine, fill = Specie)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1,p2, p3, nrow =3, common.legend = TRUE)

#A nivel individual,p arece que la longitud del anillo 
#de los salmones de agua dulce (freshwater)
# es la variable que mas se diferencia entre especies

# Predictor factor
#base$Gender <- as.factor(base$Gender)
#str(base)

# a nivel conjunto
####################
pairs(x = base[, c("Freshwater","Marine")],
      col = c("firebrick", "green3")[base$Specie], pch = 19)

#El par de variables Freshwater y Marine, es la que no separa bien
# a las dos especies


# a nivel conjunto 3D
####################
library(scatterplot3d)
scatterplot3d(base$Gender, base$Freshwater, base$Marine,
              color = c("firebrick", "green3")[base$Specie], pch = 19,
              grid = TRUE, xlab = "Gender", ylab = "Freshwater",
              zlab = "Marine", angle = 65, cex.axis = 0.6)
legend("topleft",
       bty = "n", cex = 0.8,
       title = "Specie",
       c("alaskan", "canadian"), fill = c("firebrick", "green3"))

# La representación de las tres variables de forma simultánea 
# parece indicar que las dos especies NO están bastante separadas 
# en el espacio 3D generado. 


##################################
# Prior probabilities
##################################
50/100 =0.5

##################################
# Homogeneidad de Varianza
##################################

#Distribución de los predictores de forma individual:
####################################################

# Representación mediante Histograma de cada variable para cada especie 
par(mfcol = c(2, 3))
for (k in 1:3) {
  j0 <- names(base)[k]
  #br0 <- seq(min(base[, k]), max(base[, k]), le = 11)
  x0 <- seq(min(base[, k]), max(base[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(base$Specie)[i]
    x <- base[base$Specie == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("Specie", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}


# Representación de cuantiles normales de cada variable para cada especie 

for (k in 1:3) {
  j0 <- names(base)[k]
  x0 <- seq(min(base[, k]), max(base[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(base$Specie)[i]
    x <- base[base$Specie == i0, j0]
    qqnorm(x, main = paste("Specie", i0, j0), pch = 19, col = i + 1)
    qqline(x)
  }
}


# Contraste de normalidad Shapiro-Wilk para cada variable en cada especie
library(reshape2)
library(knitr)
library(dplyr)
training_tidy <- melt(base, value.name = "valor")
kable(training_tidy %>% group_by(Specie, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))

#contraste de normalidad multivariada
#PRUEBA DE NORMALIDAD MULTIVARIADA MARDIA-HZ-ROYSTON
library(MVN)
par(mfrow=c(1,6))
resMardia<- mvn(base[,c(-1,-4)], mvnTest = "mardia", multivariatePlot = "qq", multivariateOutlierMethod = "quan" )
mvn(base[,c(-1,-4)], mvnTest = "mardia")
#PRUEBA DE NORMALIDAD MULTIVARIADA DE HENZE-ZIRKLER (HZ)
resHZ <- mvn(base[,c(-1,-4)], mvnTest = "hz", multivariatePlot = "qq", multivariateOutlierMethod = "quan" )
mvn(base[,c(-1,-4)], mvnTest = "hz")
#PRUEBA DE NORMALIDAD MULTIVARIADA DE ROYSTON
resR <- mvn(base[,c(-1,-4)], mvnTest = "royston", multivariatePlot = "qq", multivariateOutlierMethod = "quan" )
mvn(base[,c(-1,-4)], mvnTest = "royston")
par(mfrow=c(1,1))

#podemos concluir que tanto univariado como multivariado p > 0.5 la normalidad 
#se cumple

#Prueba Chi - Cuadrado
# Ho= Independencia variables
# Ha= No Independencia variables
#Variables no son significativas (Independencia)
library(stats)
chisq.test(base$Gender,base$Specie)
#cocluimos que siendo la p=1, no rechazamos la Ho es decir existe
#indpendecnia entre género y especie.


#Aplicando Independencia entre var.Cualitativa y Var. Cuantitativa
#Usamos prueva de Cohoen (d<-mientras mas cercano a "0" es independiente las variables)
#d=[0.2-0.5> es baja
#d=[0.5-0.8> es media
#d= mas>0.8 es alta
library(effsize)
cohen.d(base$Freshwater,base$Specie)
#d estimate: -2.28172 (large)
cohen.d(base$Marine,base$Specie)
#d estimate: 1.862049 (large)
#en ambos casos las variables resultaron ser significativas,
# es decir tuienen una lata dependencia con la especie.

# La función boxM() contratará de matrices de covarianza.
library(SpatialEpi)
library(biotools)
boxM(data = base[, c(2,3)], grouping = base[, 4])
#como p-value = 0.01349 > 0.01 se acepta la Ho, es decir
# las varianzas son iguales.


# Selección de muestra de entrenamiento (70%) y de Validación (30%)
library(caret)
#semilla del generador de números aleatorios de R
set.seed(123)
#particion
index <- createDataPartition(base$Specie, p=0.7, list=FALSE)
training <- base[ index, ]
testing <-  base[-index, ]
set.seed(123)
sample(1:10, 5)
View(training)
View(testing)
# Verificando la estructura de los datos particionados
100*prop.table(table(base$Specie))
100*prop.table(table(training$Specie))
100*prop.table(table(testing$Specie))

##################################
# Cálculo de la función discriminante
##################################
attach(training)
library(MASS)
modelo_lda <- lda(Specie ~ Freshwater + Marine , data = training)
modelo_lda

##################################
# Evaluación de los errores de clasificación
##################################

predicciones <- predict(object = modelo_lda, newdata = testing)
table(testing$Specie, predicciones$class, dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(testing$Specie != predicciones$class) * 100
paste("trainig_error =", trainig_error, "%")



#La siguiente imagen muestra la representación de las observaciones,
#coloreadas por la verdadera especie a la que pertenecen y 
#acompañadas por una etiqueta con la especie que ha predicho el LDA
library(scatterplot3d)

with(testing, {
  s3d <- scatterplot3d(Gender,Freshwater,Marine,
                       color = c("firebrick", "green3")[testing$Specie],
                       pch = 19, grid = TRUE, xlab = "Gender", ylab = "Freshwater",
                       zlab = "Marine", angle = 65, cex.axis = 0.6)
  
  s3d.coords <- s3d$xyz.convert(Gender,Freshwater,Marine)
  # convierte coordenadas 3D en proyecciones 2D
legend("topleft", 
         bty = "n", cex = 0.8,
         title = "Especie",
         c("alaskan", "canadia"), fill = c("firebrick", "green3"))
})





