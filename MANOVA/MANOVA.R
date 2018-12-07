################################
## APLICACION DEL MANOVA PARA ##
## EL CASO DE RECAIDAS POR    ##
##       ALCOHOLISMO          ##
################################

# INSTALACION DE PAQUETES
######################################
install.packages("cluster")
install.packages("car")
install.packages("MVN")
install.packages("caret")
install.packages("gplots")
install.packages("gmodels")
install.packages("ggplot2")
install.packages("biotools")
install.packages("WRS2")

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
######################################
rm(list = ls())
ls()
d1<-read.csv("J:/CICLO 2018-II/ESTAD  MULTI/TRABAJO TERCERA/michael multi/alcohol.csv") 
View(d1)
d1$Grupo<-as.factor(d1$Grupo)
levels(d1$Grupo)<-c("Longevidad (1)","Recientes (2)","Nuevos (3)")                          
#datos reales
View(d1)
str(d1)

#Estandarizando las Variables 
######################################
preProcValues1 <- preProcess(d1[c(-1,-2)],method=c("center","scale"))
preProcValues1
d2 <- predict(preProcValues1, d1[c(-1,-2)])
d2<-data.frame(d1$Grupo,d2)
names(d2)=c("Grupo","UM","ES","LV")
View(d2)

#Histogramas reales vs standarizados (para todos los grupos,toda la data )
######################################
par(mfrow=c(2,3))
hist(d1$UM, col = "green",main = "Estados de ánimo desagradables",xlab ="UM" )  
hist(d1$ES, col="red",main = "Estados eufóricos y situaciones relacionadas",xlab ="ES" )
hist(d1$LV, col="blue",main = "Área designada como vigilancia reducida",xlab ="LV" )
hist(d2$UM, col = "green",main = "Estados de ánimo desagradables",xlab ="UM")  
hist(d2$ES, col="red",main = "Estados eufóricos y situaciones relacionadas",xlab ="ES")
hist(d2$LV, col="blue",main = "Área designada como vigilancia reducida",xlab ="LV")
par(mfrow=c(1,1))

#Histogramas por grupos
#########################
# llamamos a la misma base anterior ordenada por grupo
dopg<-read.csv("J:/CICLO 2018-II/ESTAD  MULTI/TRABAJO TERCERA/michael multi/alcohol_2.csv") 
View(dopg)
dopg$Grupo<-as.factor(dopg$Grupo)
levels(dopg$Grupo)<-c("Longevidad (1)","Recientes (2)","Nuevos (3)") 
#Estandarizando las Variables de la base ordenada
######################################
preProcValues2 <- preProcess(dopg[c(-1,-2)],method=c("center","scale"))
dopg2 <- predict(preProcValues2, dopg[c(-1,-2)])
dopg2<-data.frame(dopg$Grupo,dopg2)
names(dopg2)=c("Grupo","UM","ES","LV")
View(dopg2)


#Grupo Longevos (1)
par(mfrow=c(2,3))
hist(dopg[1:126,]$UM, col = "green",main = "Estados de ánimo desagradables",xlab ="UM en Longevos (1)" )  
hist(dopg[1:126,]$ES, col="red",main = "Estados eufóricos y situaciones relacionadas",xlab ="ES en Longevos (1)" )
hist(dopg[1:126,]$LV, col="blue",main = "Área designada como vigilancia reducida",xlab ="LV en Longevos (1)" )
hist(dopg2[1:126,]$UM, col = "green",main = "Estados de ánimo desagradables",xlab ="UM en Longevos (1)")  
hist(dopg2[1:126,]$ES, col="red",main = "Estados eufóricos y situaciones relacionadas",xlab ="ES en Longevos (1)")
hist(dopg2[1:126,]$LV, col="blue",main = "Área designada como vigilancia reducida",xlab ="LV en Longevos (1)")
par(mfrow=c(1,1))

#Grupo Recientes (2)
par(mfrow=c(2,3))
hist(dopg[127:217,]$UM, col = "green",main = "Estados de ánimo desagradables",xlab ="UM en Recientes (2)" )  
hist(dopg[127:217,]$ES, col="red",main = "Estados eufóricos y situaciones relacionadas",xlab ="ES en Recientes (2)" )
hist(dopg[127:217,]$LV, col="blue",main = "Área designada como vigilancia reducida",xlab ="LV en Recientes (2)" )
hist(dopg2[127:217,]$UM, col = "green",main = "Estados de ánimo desagradables",xlab ="UM en Recientes (2)")  
hist(dopg2[127:217,]$ES, col="red",main = "Estados eufóricos y situaciones relacionadas",xlab ="ES en Recientes (2)")
hist(dopg2[127:217,]$LV, col="blue",main = "Área designada como vigilancia reducida",xlab ="LV en Recientes (2)")
par(mfrow=c(1,1))

#Grupo Nuevos (3)
par(mfrow=c(2,3))
hist(dopg[218:253,]$UM, col = "green",main = "Estados de ánimo desagradables",xlab ="UM en Nuevos (3)" )  
hist(dopg[218:253,]$ES, col="red",main = "Estados eufóricos y situaciones relacionadas",xlab ="ES en Nuevos (3))" )
hist(dopg[218:253,]$LV, col="blue",main = "Área designada como vigilancia reducida",xlab ="LV en Nuevos (3)" )
hist(dopg2[218:253,]$UM, col = "green",main = "Estados de ánimo desagradables",xlab ="UM en Nuevos (3)")  
hist(dopg2[218:253,]$ES, col="red",main = "Estados eufóricos y situaciones relacionadas",xlab ="ES en Nuevos (3)")
hist(dopg2[218:253,]$LV, col="blue",main = "Área designada como vigilancia reducida",xlab ="LV en Nuevos (3)")
par(mfrow=c(1,1))


#Distribucion de los grupos de Vulnerabilidad
######################################
porcentaje<-100*round(prop.table(table(d1$Grupo)),3)
plot<-plot(d1$Grupo,col="orange",ylab="Porcentaje %",main="Graficos Barras: Grupos de Vulnerabilidad",xlab="Grupos")
text(plot, porcentaje, labels=porcentaje, pos=3, offset=0.8)

#Análisis Componentes Principales          
######################################
comp1<-princomp(d2[,c(-1)],cor=T)
comp1
plot(comp1)
cor(d2[,c(-1)])
summary(comp1)                            
biplot(comp1)                                
#Puntajes de los compononentes -solo 2 componentes

a.cp<-comp1$scores[,1:2]
a.cp
plot(a.cp)

#Analisis de Conglomerados                  
######################################
ag1<-agnes(d2[,c(-1)], method = "single")
ag2<-agnes(d2[,c(-1)], method = "complete") #mayor coeficiente aglomerativo
ag3<-agnes(d2[,c(-1)], method = "average")
plot(ag2)
#Comparando metodo Complete vs Average
div_ag2<-cutree(ag2,3)  # tres son los grupos
div_ag3<-cutree(ag3,4)
#graficando los grupos
par(mfrow=c(1,2))
plot(a.cp,col=div_ag2)
plot(a.cp,col=div_ag3)
#Resumen los grupos por componentes
par(mfrow=c(1,2))
biplot(comp1)
plot(a.cp,col=div_ag2)
par(mfrow=c(1,1))

#Anàlisis bivariado 
######################################
par(mfrow=c(1,3))
# UM
promedio1<-round(tapply(d1$UM,d1$Grupo,mean),3) 
promedio1
bar1 <- barplot(promedio1,
                main="Gráfico de Barras: Puntaje promedio según grupo
                UM",
                xlab="Vulnerabilidad",
                ylab="Puntaje Promedio",
                col = c("blue","red","orange"),
                cex.axis = 1,                              
                ylim = c(0,25))                                                          # Densidad para relleno de la barras, por defecto 0
text(bar1, promedio1, labels=promedio1, pos=3, offset=0.8)

#ES
promedio2<-round(tapply(d1$ES,d1$Grupo,mean),3) 
promedio2
bar2 <- barplot(promedio2,
                main="Gráfico de Barras: Puntaje promedio según grupo
                ES",
                xlab="Vulnerabilidad",
                ylab="Puntaje Promedio",
                col = c("blue","red","orange"),
                cex.axis = 1,                              
                ylim = c(0,15))                                                          # Densidad para relleno de la barras, por defecto 0
text(bar2, promedio2, labels=promedio2, pos=3, offset=0.8)

#LV
promedio3<-round(tapply(d1$LV,d1$Grupo,mean),3) 
promedio3
bar3 <- barplot(promedio3,
                main="Gráfico de Barras: Puntaje promedio según grupo
                LV",
                xlab="Vulnerabilidad",
                ylab="Puntaje Promedio",
                col = c("blue","red","orange"),
                cex.axis = 1,                              
                ylim = c(0,8))                                                        # Densidad para relleno de la barras, por defecto 0
text(bar3, promedio3, labels=promedio3, pos=3, offset=0.8)
par(mfrow=c(1,1))


# Diagrama de Cajas de variables cuantitativas según Grupo
######################################
#UM
par(mfrow=c(1,3))
boxplot(d1$UM~ d1$Grupo, 
        main= "BoxPlot de UM  vs Grupo",
        xlab = "Cluster", 
        ylim = c(0,45),
        col = c("red","blue","green"))
#ES
boxplot(d1$ES~ d1$Grupo, 
        main= "BoxPlot de ES  vs Grupo",
        xlab = "Cluster", 
        ylim = c(0,24),
        col = c("red","blue","green"))
#LV
boxplot(d1$LV~ d1$Grupo, 
        main= "BoxPlot de LV  vs Grupo",
        xlab = "Cluster", 
        ylim = c(0,12),
        col = c("red","blue","green"))
par(mfrow=c(1,1))

# Resumen de variables cuantitativas
######################################
round(tapply(d1$UM,d1$Grupo,mean),3)
round(tapply(d1$ES,d1$Grupo,mean),3)
round(tapply(d1$LV,d1$Grupo,mean),3)


#PRUEBA DE NORMALIDAD MULTIVARIADA MARDIA-HZ-ROYSTON "TOTAL"
######################################
par(mfrow=c(1,6))
dnorm<-d1[,c(-1,-2)]
resMardia<- mvn(dnorm, mvnTest = "mardia", multivariatePlot = "qq", multivariateOutlierMethod = "quan" )
mvn(dnorm, mvnTest = "mardia")

#PRUEBA DE NORMALIDAD MULTIVARIADA DE HENZE-ZIRKLER (HZ)
resHZ <- mvn(dnorm, mvnTest = "hz", multivariatePlot = "qq", multivariateOutlierMethod = "quan" )
mvn(dnorm, mvnTest = "hz")

#PRUEBA DE NORMALIDAD MULTIVARIADA DE ROYSTON
resR <- mvn(dnorm, mvnTest = "royston", multivariatePlot = "qq", multivariateOutlierMethod = "quan" )
mvn(dnorm, mvnTest = "royston")
par(mfrow=c(1,1))


#PRUEBA DE NORMALIDAD MULTIVARIADA MARDIA-HZ-ROYSTON POR GRUPO
######################################
#Prueba con el grupo LONGEVIDAD (1)
par(mfrow=c(1,6))
dnorm1<-dopg[1:126,c(-1,-2)]
resMardia<- mvn(dnorm1, mvnTest = "mardia", multivariatePlot = "qq", 
                multivariateOutlierMethod = "quan")
mvn(dnorm1, mvnTest = "mardia")
#Prueba con el grupo RECIENTES (2)
dnorm2<-dopg[127:217,c(-1,-2)]
resMardia<- mvn(dnorm2, mvnTest = "mardia", multivariatePlot = "qq",
                multivariateOutlierMethod = "quan")
mvn(dnorm2, mvnTest = "mardia")
#Prueba con el grupo NUEVOS (3)
dnorm3<-dopg[218:253,c(-1,-2)]
resMardia<- mvn(dnorm3, mvnTest = "mardia", multivariatePlot = "qq",
                multivariateOutlierMethod = "quan")
mvn(dnorm3, mvnTest = "mardia")
par(mfrow=c(1,1))


#MEDIANTE FUNCION (MARDIA) PARA EL "TOTAL"
######################################
mardia1=function(dat)
{n=dim(dat)[1]; p=dim(dat)[2]
u=rep(1,n)
D=scale(dat,scale=F)
mx=u%*%as.matrix(dat)*(1/n); s=var(dat)
si=solve(s)
G=D%*%si%*%t(D)
sesgo=sum(G^3)/n^2
kurtosis=sum(diag(G)^2)/n
kur=p*(p+2)
chi.cal=n*sesgo/6; gl=kur*(p+1)/6
z.cal=(kurtosis-kur)/(8*kur/n)^0.5
p1=pchisq(chi.cal,gl); q1=1-p1
p2=pnorm(z.cal); q2=1-p2
if(p1<q1) pval1=2*p1 else pval1=2*q1
if(p2<q2) pval2=2*p2 else pval2=2*q2
ses.sal=cbind(sesgo,chi.cal,pval1)
kur.sal=cbind(kurtosis,z.cal,pval2)
list(media=mx,var_cov=s,sesgo=ses.sal,kurtosis=kur.sal)
}
mardia1(dnorm)
View(dnorm)

#MEDIANTE FUNCION (MARDIA) POR GRUPOS
######################################
#Prueba con el grupo LONGEVIDAD (1)
mardia1(dnorm1)
#Prueba con el grupo RECIENTES (2)
mardia1(dnorm2)
#Prueba con el grupo NUEVOS (3)
mardia1(dnorm3)


#Outliers-Datos Atípicos  TOTAL          
######################################
n=dim(dnorm)[1]; p=dim(dnorm)[2]
u=rep(1,n)
D=scale(dnorm,scale=F)
s=var(dnorm)
si=solve(s)
G=D%*%si%*%t(D)
dis_Maha<-diag(G)
dM<-data.frame(d2,dis_Maha)
dM2<-data.frame(d1,dis_Maha)
View(dM)


#Muestra las distancias mas grandes
dM$dis_Maha[dis_Maha>9.348]            #valor establecido del cuantil


#tratamiento de outliers, http://r-statistics.co/Outlier-Treatment-With-R.html



######################################
to<-read.csv("J:/CICLO 2018-II/ESTAD  MULTI/TRABAJO TERCERA/michael multi/alcohol_2.csv") 
View(to)

mod <- lm(Grupo ~ UM+ES+LV, data=to)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="analisis de influencia por D de Cooks")  # dustancia de Cooks
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # añadiendo punto de corte
text(x=1:length(cooksd)+1, y=cooksd, 
labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # añadiendo etiquetas

######################################

#Por D de Cook: pag 188 Introduccion del analisis de regresion lineal, Montgomery,3ra Edicion,COMPAÑÍA EDITORIAL CONTINENTAL
#-230,-232,-240,-247,-224,-250,-229,-231,-253,-249,-219,-222

#Por Mahalanobis, Paper Iván Amón Uribe, pag 14 Técnicas para detección de outliers multivariantes,Revista en TelecToémcunnicicaasc pioanreas de eItnefcocrimónát dicea ,o Vuotlli.e 3r,s Nmou. l5tivpa. r1i1a n- t2e5s,Medellín - Colombia. Enero - Junio de 2013, ISSN 2215-8200

#Por orden descendente de la distancia de Mahalanobis, tenemos:
#-210,-70,-128,-200,-133,-47,-223,-213,-1,-134,-32,-73,-253
#GRUPO 1:
#  70,133,223,213,116,209,106,84,140
#GRUPO2:
#  210,128,47,1,134,32,185,207,146,85,202,112,152,169
#GRUPO 3:
#  200,73,235,158,9,48,180

#Posibles outliers en general
# -230,-232,-240,-247,-224,-250,-229,-231,-253,-249,-219,-222,-210,-70,-128,-200,-133,-47,-223,-213,-1,-134,-32,-73,-253

library(openxlsx)
write.xlsx(dM, 'dM.xlsx')
#quitamos aquellos datos (registros) que posiblemente distorcionan normalidad
mardia1(dnorm[c(-38,-50,-126,-114,
                -76,-21,-70,-99,-29),])

#PRUEBA DE NORMALIDAD MULTIVARIADA MARDIA-HZ-ROYSTON POR GRUPO Y SIN OUTLIERS
######################################
Longevos_1<-read.csv("J:/CICLO 2018-II/ESTAD  MULTI/TRABAJO TERCERA/michael multi/Longevos_1.csv") 

#Prueba con el grupo LONGEVIDAD (1)

dnorm1<-Longevos_1[c(-38,-50,-126,-114,
                     -76,-21,-70,-99,-29),c(-1,-2)]
resMardia<- mvn(dnorm1, mvnTest = "mardia", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
mvn(dnorm1, mvnTest = "mardia")

# GRAFICOS MULTIVARIADO NORMALIDAD

normplot=function(dat)
{ n=dim(dat)[1]; p=dim(dat)[2]
u=rep(1,n)
mx=u%*%as.matrix(dat)*(1/n); vx=var(dat)
d2=mahalanobis(dat,mx,vx)
d2=sort(d2)
a=seq((1-1/2),(n-1/2),by=1)
b=a/n
quant=qchisq(b,p)
plot(quant,d2,xlab="quantil chi-cuadrado",
     ylab="distancia de Mahalanobis al cuadrado",
     main="Plot de normalidad multivariada")
abline(0,1,col=2)
list(media=mx,var_cov=vx)
}

normplot(dnorm)

#Realizamos la Prueva de Mardia 
mvn(dM[c(-70,-88,-249,-223,-140,-34,-131,-187,-54),c(-1,-5)], mvnTest = "mardia")

resMardia<- mvn(dM[c(-70,-88,-249,-223,-140,-34,-131,-187,-54),
                   c(-1,-5)],mvnTest = "mardia", 
                multivariatePlot = "qq", 
                multivariateOutlierMethod = "quan" )

#Prueba de Igualdad de Varianzas poblacionales
boxM(dM[c(-70,-88,-249,-223,-140,-34,-131,-187,-54
),2:4],dM[c(-70,-88,-249,-223,-140,-34,-131,-187,-54),1])
#Anàlisis MANOVA

MANOVA<-manova(cbind(UM,ES,LV)~Grupo,dM[c(-70,-88,-249,-223,-140,-34,-131,-187,-54),])
#Pruebas de la Igualdad de medias poblacionales
summary(MANOVA,test="Hotelling-Lawley")
summary(MANOVA,test="Roy")
summary(MANOVA,test="Pillai")
summary(MANOVA,test="Wilks")
summary.aov(MANOVA)



