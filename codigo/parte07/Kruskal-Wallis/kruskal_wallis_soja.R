library(tidyverse)
library(car)
library(pgirmess)

#####################
#Importacion de datos
#####################
soja<-gl(4, 20, labels = c("1","2","3","4"))
esperma<-c(0.35,0.58,0.88,0.92,1.22,1.51,1.52,1.57,2.43,2.79,
           3.40,4.52,4.72,6.90,7.58,7.78,9.62,10.05,10.32,
           21.08,0.33,0.36,0.63,0.64,0.77,1.53,1.62,1.71,
           1.94,2.48,2.71,4.12,5.65,6.76,7.08,7.26,7.92,8.04,
           12.10,18.47,0.40,0.60,0.96,1.20,1.31,1.35,1.68,
           1.83,2.10,2.93,2.96,3.00,3.09,3.36,4.34,5.81,5.94,
           10.16,10.98,18.21,0.31,0.32,0.56,0.57,0.71,0.81,
           0.87,1.18,1.25,1.33,1.34,1.49,1.50,2.09,2.70,2.75,
           2.83,3.07,3.28,4.11)

sojaDatos<-data.frame(esperma, soja)
sojaDatos$soja<-factor(sojaDatos$soja, 
                       levels = levels(sojaDatos$soja)
                       [c(1,2,3,4)])
#########
#Graficos
#########
ggplot(sojaDatos, aes(x = soja, y = esperma, fill = soja)) +
  geom_boxplot() +
  labs(
    title = "Boxplot calidad de esperma",
    x = "Grupo soja",
    y = "Recuento de esperma"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Estadisticos
estadisticos <- sojaDatos %>%
  group_by(soja) %>%
  summarise(
    n        = n(),            
    media    = mean(esperma),
    sd       = sd(esperma),      
    mediana  = median(esperma),
    min      = min(esperma),
    max      = max(esperma),
    .groups  = "drop"   
  )
print(estadisticos)

#Contraste homogeneidad varianza
leveneTest(sojaDatos$esperma, 
           sojaDatos$soja, center = median)   

#Contraste normalidad
shapiro.test(sojaDatos[sojaDatos$soja ==
                         "1","esperma"])
shapiro.test(sojaDatos[sojaDatos$soja ==
                         "2","esperma"])  
shapiro.test(sojaDatos[sojaDatos$soja ==
                         "3","esperma"])
shapiro.test(sojaDatos[sojaDatos$soja ==
                         "4","esperma"])

#newModel<-kruskal.test(outcome ~ predictor, 
#                       data = dataFrame, 
#                       na.action = "an.action")

kruskalTest<-kruskal.test(esperma ~ soja, 
                          data = sojaDatos)
print(kruskalTest)

sojaDatos$Ranks<-rank(sojaDatos$esperma)

by(sojaDatos$Ranks, sojaDatos$soja, mean)

#Test comparaciones multiples
kruskalmc(esperma ~ soja, data = sojaDatos)

kruskalmc(esperma ~ soja, data = sojaDatos, 
          cont = 'two-tailed')
