#Importacion datos
##################

mes0<-c(63.75,62.98,65.98,107.27,66.58,120.46,62.01,71.87,83.01,76.62)
mes1<-c(65.38,66.24,67.70,102.72,69.45,119.96,66.09,73.62,75.81,67.66)
mes2<-c(81.34,69.31,77.89,91.33,72.87,114.26,68.01,55.43,71.63,68.60)

datos<-data.frame(peso=c(mes0,mes1,mes2),
                  mes=gl(3,length(mes0),3*length(mes0),labels=c("Mes 0","Mes 1","Mes 2")))

dietData<-matrix(c(mes0,mes1,mes2),nrow=length(mes0),ncol=3)
#Quitamos datos perdidos (si los hubiera).
#Los datos tienen que pasarse como matriz al test
dietCompleteCases <- as.matrix(na.omit(dietData))

#Exploración datos
#################
#Graficos
boxplot(peso~ mes,data=datos)


#Estadísticos
by(datos[, c("peso")], datos$mes, stat.desc, basic = FALSE,
   norm = TRUE)

#Test
#####

friedman.test(as.matrix(dietData))

#Test comparaciones múltiples
friedmanmc(as.matrix(dietData)) 
