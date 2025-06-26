
library(clinfun)
library(psych)
library(pastecs)
library(car)


#Importación datos
##################

grupo<-gl(2,12,length=24,labels=c("Cafe","Te"),ordered=T)
bugs_cafe <- c(86,69,72,65,113,65,118,45,141,41,30,104)
bugs_te<-c(55,40,22,58,163,7,9,16,26,36,20,15)
datos<-data.frame(grupo=grupo,bugs=c(bugs_cafe,bugs_te))

#Exploración datos
#################
#Graficos
boxplot(bugs ~ grupo,data=datos)


#Estadísticos

by(datos[, c("bugs")], datos$grupo, stat.desc, basic = FALSE,
   norm = TRUE)

#Contraste normalidad
shapiro.test(bugs_cafe)
shapiro.test(bugs_te)

#Contraste homogeneidad varianza
leveneTest(datos$bugs, datos$grupo, center = mean)


#Contraste Mann-Whitney
######################
#Test
#newModel<-wilcox.test(outcome ~ predictor, data = dataFrame, paired = FALSE/TRUE)

bugsModel<-wilcox.test(bugs ~ grupo, data = datos, exact = FALSE,
                      correct= FALSE)
bugsModel


#Tamaño del efecto
rFromWilcox<-function(wilcoxModel, N){
  z<- qnorm(wilcoxModel$p.value/2)
  r<- z/ sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

rFromWilcox(bugsModel, 20)




