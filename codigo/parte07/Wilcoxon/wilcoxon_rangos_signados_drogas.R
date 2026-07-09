#####################
#Importacion de datos
#####################
droga<-gl(2,10,length=20,labels=c("Extasis","Alcohol"),
          ordered=T)
domingoBDI<-c(15,35,16,18,19,17,27,16,13,20,
              16,15,20,15,16,13,14,19,18,18)
miercolesBDI<-c(28,35,35,24,39,32,27,29,36,35,
                5,6,30,8,9,7,6,17,3,10)
drogaDatos<-data.frame(droga,domingoBDI,miercolesBDI)
alcoholDatos <- drogaDatos[drogaDatos$droga ==
                          "Alcohol",]
extasisDatos <- drogaDatos[drogaDatos$droga ==
                          "Extasis",]

#####################
#Exploracion de datos
#####################
#Graficos
boxplot(alcoholDatos[,2:3])
boxplot(extasisDatos[,2:3])

#Estadisticos
summary(alcoholDatos)
summary(extasisDatos)

#Contraste normalidad
shapiro.test(alcoholDatos$miercolesBDI)
shapiro.test(alcoholDatos$domingoBDI)
shapiro.test(extasisDatos$miercolesBDI)
shapiro.test(extasisDatos$domingoBDI)

######################################
#Contraste Rangos Signados de Wilcoxon
######################################
alcoholModelo<-wilcox.test(alcoholDatos$miercolesBDI,
                           alcoholDatos$domingoBDI,
                           paired=T,correct=F,exact=F)
alcoholModelo

extasisModelo<-wilcox.test(extasisDatos$miercolesBDI,
                           extasisDatos$domingoBDI,
                           paired=T,correct=F,exact=F)
extasisModelo

###################
#Tamanio del efecto
###################
rFromWilcox<-function(wilcoxModel, N){
  z<- qnorm(wilcoxModel$p.value/2)
  r<- z/ sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

rFromWilcox(alcoholModelo, 20)
rFromWilcox(extasisModelo, 20)
