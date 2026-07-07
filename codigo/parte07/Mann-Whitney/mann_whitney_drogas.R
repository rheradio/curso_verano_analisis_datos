library(car)
library(tidyverse)

#####################
#Importacion de datos
#####################
droga<-gl(2,10,length=20,labels=c("Extasis","Alcohol"), 
          ordered=T)
domingoBDI<-c(15, 35, 16, 18, 19, 17, 27, 16, 13, 20,
             16, 15, 20, 15, 16, 13, 14, 19, 18, 18)
miercolesBDI<-c(28, 35, 35, 24, 39, 32, 27, 29, 36, 35, 
           5, 6, 30, 8, 9, 7, 6, 17, 3, 10)
drogaDatos<-data.frame(droga,domingoBDI,miercolesBDI)

#Graficos
boxplot(miercolesBDI ~ droga)
boxplot(domingoBDI ~ droga)

#Estadisticos
#by(drogaDatos[, c("domingoBDI", "miercolesBDI")], 
#   drogaDatos$droga, stat.desc, basic = FALSE, norm = TRUE)
#Estadisticos
estadisticos <- drogaDatos %>%
  pivot_longer(
    cols = c(domingoBDI, miercolesBDI), 
    names_to = "dia", values_to = "BDI"
  ) %>%
  group_by(droga, dia) %>%
  summarise(
    n       = n(),
    media   = mean(BDI),
    sd      = sd(BDI),
    mediana = median(BDI),
    min     = min(BDI),
    max     = max(BDI),
    .groups = "drop"
  )
estadisticos

#Contraste normalidad
shapiro.test(miercolesBDI)
shapiro.test(domingoBDI)

#Contraste homogeneidad varianza
leveneTest(drogaDatos$miercolesBDI, drogaDatos$droga, 
           center = median)
leveneTest(drogaDatos$domingoBDI, drogaDatos$droga, 
           center = median)

#######################
#Test Mann-Whitney
#######################
miercolesModelo<-wilcox.test(miercolesBDI ~ droga, 
                             data = drogaDatos, 
                             exact = FALSE, correct= FALSE)
miercolesModelo

domingoModelo<-wilcox.test(domingoBDI ~ droga, 
                           data = drogaDatos, 
                           exact = FALSE, correct= FALSE)
domingoModelo

##################
#Tamanio del efecto
##################
rFromWilcox<-function(wilcoxModelo, N){
  z<- qnorm(wilcoxModelo$p.value/2)
  r<- z/ sqrt(N)
  cat(wilcoxModelo$data.name, "Tamanio del efecto, r = ", r)
}

rFromWilcox(domingoModelo, 20)
rFromWilcox(miercolesModelo, 20)

