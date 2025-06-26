library(clinfun)

#Importación datos
##################
grupo<-gl(2,9,length=18,labels=c("Sin Anillo","Con Anillo"),ordered=T)
con<-c(8, 5, 3, 3, 2, 2 ,2 ,1, 1)
sin<-c(11,  5 , 4,  3 , 6 , 3 , 1 , 1 , 3)

datos<-data.frame(grupo=grupo,tiempo=c(sin,con))

#Exploración datos
#################
#Graficos
boxplot(tiempo~ grupo,data=datos)


#Estadísticos
by(datos[, c("tiempo")], datos$grupo, stat.desc, basic = FALSE,
   norm = TRUE)
#Contraste normalidad
shapiro.test(sin)
shapiro.test(con)


#Contraste Rangos Signados de Wilcoxon
######################
#Test
#newModel<-wilcox.test(outcome ~ predictor, data = dataFrame, paired = FALSE/TRUE)

LoRModel<-wilcox.test(con, sin,paired=T,correct=F)
LoRModel




#Tamaño del efecto
rFromWilcox<-function(wilcoxModel, N){
  z<- qnorm(wilcoxModel$p.value/2)
  r<- z/ sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

rFromWilcox(LoRModel, 20)



