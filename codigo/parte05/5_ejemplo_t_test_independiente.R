#Datos
madres.fumadoras <- c(0.081, 0.091, 0.099, 0.068, 0.099, 0.088, 0.133, 0.130, 0.101, 0.097, 0.084, 0.110)
madres.no.fumadoras <- c(0.108, 0.115, 0.092, 0.084, 0.070, 0.116, 0.098, 0.080, 0.111, 0.136)

# Levene's Test (paquete car)
grupos <-  factor(c(rep(1, length(madres.fumadoras)), rep(2, length(madres.no.fumadoras))))
leveneTest(c(madres.fumadoras, madres.no.fumadoras), grupos)

# t.test
t.test(madres.no.fumadoras, madres.fumadoras, var.equal = TRUE, alternative = "greater")

# d de Cohen (paquete effectsize)
cohens_d(madres.no.fumadoras, madres.fumadoras, pooled = T)
