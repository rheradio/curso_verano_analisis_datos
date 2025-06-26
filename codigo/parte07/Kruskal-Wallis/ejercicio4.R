library(ggplot2)
library(pgirmess)
#Importación datos
grupo<-gl(4, 5, labels = c("1","2","3","4"))
t1<-c(42,39,48,43,44)
t2<-c(45,46,45,39,43)
t3<-c(64,61,50,55,58)
t4<-c(56,55,62,59,60)

datos<-data.frame(grupo=grupo, tiempo=c(t1,t2,t3,t4))

#Graficos
ggplot(datos, aes(x = grupo, y = tiempo, fill = grupo)) +
  geom_boxplot() +
  labs(
    title = "Boxplot Tiempo Sumas",
    x = "Cantidad de Alcohol",
    y = "Tiempo"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Estadísticos
by(datos[, c("tiempo")], datos$grupo, stat.desc, basic = F)

#Contraste normalidad
shapiro.test(t1)
shapiro.test(t2)  
shapiro.test(t3)
shapiro.test(t4)


#Contraste homogeneidad varianza
leveneTest(datos$tiempo, datos$grupo, center = median)                                                                                                                         

#Contraste
#newModel<-kruskal.test(outcome ~ predictor, data = dataFrame, na.action =
#                         "an.action")

kruskalTest<-kruskal.test(tiempo ~ grupo, data = datos)
kruskalTest

datos$Ranks<-rank(datos$tiempo)

by(datos$Ranks, datos$grupo, mean)

#Test comparaciones Múltiples
kruskalmc(tiempo ~ grupo, data = datos)
kruskalmc(tiempo ~ grupo, data = datos,cont="two-tailed")
