library(pgirmess)
library(clinfun)
library(psych)
library(pastecs)
library(car)
library(ggplot2)

###########################
#PRUEBA 1: Mann-Whitney
###########################
villanueva<-c(8,9,7,8,9)
argamasilla<-c(6,5,6,4,5)
n<-length(argamasilla)
pueblo<-gl(2,n,length=2*n,labels=c("Argamasilla","Villanueva"),ordered=T)
opiniones<-c(argamasilla,villanueva)
datos<-data.frame(opiniones=opiniones, pueblo=pueblo)

#Exploración datos
#################
#Graficos
boxplot(opiniones ~ pueblo,data=datos)


#Estadísticos

analisis_descriptivo<-by(datos[, c("opiniones")], datos$pueblo, stat.desc, basic = FALSE,
                         norm = TRUE)
media_argamasilla<-analisis_descriptivo$Argamasilla[2]
media_villanueva<-analisis_descriptivo$Villanueva[2]

#Contraste Mann-Whitney
######################
#Test
prueba1Model<-wilcox.test(opiniones ~ pueblo, data = datos, exact = FALSE,correct= FALSE)
prueba1Model
p1_pvalor<-prueba1Model$p.value
W<-prueba1Model$statistic

#Tamaño del efecto
rFromWilcox<-function(wilcoxModel, N){
  z<- qnorm(wilcoxModel$p.value/2)
  r<- z/ sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
  return (r)
  
}

rFromWilcox(prueba1Model, 20)
p1_efecto<-rFromWilcox(prueba1Model, 20)

###################################
#PRUEBA 2: Rangos signados Wilcoxon
###################################

#Importación datos
##################
grupo<-gl(2,5,length=10,labels=c("Antes leer","Después leer"),ordered=T)
antes<-c(6,7,5,6,5)
despues<-c(9,9,7,8,8)

datos<-data.frame(grupo=grupo,opinion=c(antes,despues))

#Exploración datos
#################
#Graficos
boxplot(opinion~ grupo,data=datos)


#Estadísticos
by(datos[, c("opinion")], datos$grupo, stat.desc, basic = FALSE,
   norm = TRUE)


#Contraste Rangos Signados de Wilcoxon
######################
#Test

prueba2Model<-wilcox.test(antes, despues,paired=T,correct=F)
prueba2Model


rFromWilcox(prueba2Model, 20)

###########################
#PRUEBA 3: Kruskal-Wallis
###########################

#Importación datos
grupo<-gl(4, 4, labels = c("Criptana","Argmasilla","Villanueva","Toboso"))
opinion<-c(8,9,8,9,
           6,5,5,4,
           9,10,9,8,
           7,6,6,5)


datos<-data.frame(opinion, grupo)
datos$grupo<-factor(datos$grupo, levels = levels(datos$grupo)[c(1,2,3,4)])

#Graficos
ggplot(datos, aes(x = grupo, y = opinion, fill = grupo)) +
  geom_boxplot() +
  labs(
    title = "Boxplot opiniones expertos por pueblo",
    x = "Pueblos",
    y = "Opiniones"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Estadísticos
by(datos[, c("opinion")], datos$grupo, stat.desc, basic = F)                                

#Contraste
prueba3Model<-kruskal.test(opinion ~ grupo, data = datos)
prueba3Model

datos$Ranks<-rank(datos$opinion)

by(datos$Ranks, datos$grupo, mean)

#Test comparaciones Múltiples
cm<-kruskalmc(opinion ~ grupo, data = datos)
cm

row.names(cm$dif.com[cm$dif.com["stat.signif"]==TRUE,])
###########################
#PRUEBA 4: Friedman
###########################

#Importacion datos
##################

idealismo<-c(7,6,7,8)
ambiente<-c(6,7,6,5)
tradicion<-c(5,5,6,6)
probabilidad<-c(8,9,8,9)

datos<-data.frame(puntuaciones=c(idealismo, ambiente, tradicion, probabilidad),
                  criterio=gl(4,4,16,labels=c("Idealismo","Ambiente","Tradición","Probabilidad")))


#Exploración datos
#################
#Graficos
boxplot(puntuaciones~ criterio,data=datos)


#Estadísticos
by(datos[, c("puntuaciones")], datos$criterio, stat.desc, basic = FALSE,
   norm = TRUE)

#Test
#####
#Quitamos datos perdidos (si los hubiera).
#Los datos tienen que pasarse como matriz al test
datos<-matrix(c(idealismo, ambiente, tradicion, probabilidad),nrow=4,ncol=4)
datos <- as.matrix(na.omit(datos))

friedman.test(datos)

#Test comparaciones múltiples
cmf<-friedmanmc(datos) 

cmf

diferencias_f<-row.names(cmf$dif.com[cmf$dif.com["stat.signif"]==TRUE,])
