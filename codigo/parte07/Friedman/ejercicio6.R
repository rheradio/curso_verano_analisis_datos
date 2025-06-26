#Importacion datos
##################

r1<-c(3,9,6,6,5,4,5,6)
r2<-c(4,8,6,9,8,10,9,9)
r3<-c(2,7,6,7,6,6,8,7)

datos<-data.frame(esfuerzo=c(r1,r2,r3),
                  rutina=gl(3,length(r1),3*length(r2),labels=c("Rutina 1","Rutina 2","Rutina 3")))


#Exploración datos
#################
#Graficos
boxplot(esfuerzo~ rutina,data=datos)


#Estadísticos
by(datos[, c("esfuerzo")], datos$rutina, stat.desc, basic = FALSE,
   norm = TRUE)

#Test
#####
#Quitamos datos perdidos (si los hubiera).
#Los datos tienen que pasarse como matriz al test
datos<-matrix(c(r1,r2,r3),nrow=length(r1),ncol=3)
datos <- as.matrix(na.omit(datos))

friedman.test(datos)

#Test comparaciones múltiples
friedmanmc(datos) 
