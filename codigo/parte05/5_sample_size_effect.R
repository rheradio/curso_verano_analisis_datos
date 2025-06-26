#Efecto del tamaño de la muestra
x1 <- rnorm(10, 100, 10)
x2 <- x1 + rnorm(10, 0.1, 0.5)
t.test(x1, x2, paired=TRUE)

x3 <- rnorm(1000, 100, 10)
x4 <- x3 + rnorm(1000, 0.1, 0.5)
t.test(x3, x4, paired=TRUE)

#Tamaño del efecto
d.cohen <- mean(x1-x2)/sd(x1-x2)
# Usando el paquete effectsize
cohens_d(x1, x2, paired = T)
