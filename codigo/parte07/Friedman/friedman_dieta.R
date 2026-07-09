library(tidyverse)

##################
#Importacion datos
##################

mes0<-c(63.75,62.98,65.98,107.27,66.58,120.46,
        62.01,71.87,83.01,76.62)
mes1<-c(65.38,66.24,67.70,102.72,69.45,119.96,
        66.09,73.62,75.81,67.66)
mes2<-c(81.34,69.31,77.89,91.33,72.87,114.26,
        68.01,55.43,71.63,68.60)

datos<-data.frame(peso=c(mes0,mes1,mes2),
                  mes=gl(3,length(mes0),3*length(mes0),
                         labels=c("Mes 0","Mes 1","Mes 2")))

dietaDatos<-matrix(c(mes0,mes1,mes2),
                   nrow=length(mes0),ncol=3)
#Quitamos datos perdidos (si los hubiera).
#Los datos tienen que pasarse como matriz al test
dietaCompleta <- as.matrix(na.omit(dietaDatos))

#####################
#Exploracion de datos
#####################
#Graficos
boxplot(peso ~ mes,data=datos)

#Estadisticos
estadisticos <- datos %>%
  group_by(mes) %>%
  summarise(
    n        = n(),            
    media    = mean(peso),
    sd       = sd(peso),      
    mediana  = median(peso),
    min      = min(peso),
    max      = max(peso),
    .groups  = "drop"          
  )
print(estadisticos_dieta)

#################
#Test de Friedman
#################

friedman.test(as.matrix(dietaDatos))

#Test comparaciones múltiples
friedmanmc(as.matrix(dietaDatos)) 
