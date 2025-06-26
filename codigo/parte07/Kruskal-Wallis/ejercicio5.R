library(ggplot2)
library(pgirmess)
#Importación datos
grupo<-gl(3, 5, labels = c("Jedi","Sith","No afiliados"))
g1<-c(14.0,15.2,13.8,14.7,14.9)
g2<-c(16.3,17.1,16.0,15.8,16.9)
g3<-c(9,10.2,8.5,9.8,9.6)


datos<-data.frame(grupo=grupo, tiempo=c(g1,g2,g3))

#Graficos
ggplot(datos, aes(x = grupo, y = tiempo, fill = grupo)) +
  geom_boxplot() +
  labs(
    title = "Boxplot Niveles Midiclorianos",
    x = "Usuarios",
    y = "Nivel midicloriano"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Estadísticos
by(datos[, c("tiempo")], datos$grupo, stat.desc, basic = F)

#Contraste normalidad
shapiro.test(g1)
shapiro.test(g2)  
shapiro.test(g3)



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
