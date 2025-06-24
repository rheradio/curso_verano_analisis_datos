#Datos
weight.pre <- c(81, 74, 75, 71, 80, 76, 72,
                75, 80, 75)
weight.post <- c(71, 75, 68, 72, 81, 72, 70,
                 73, 77, 75)

#Cálculo de la diferencia entre los pesos pre y post
differences <- weight.post-weight.pre

##### Cálculo manual ####
#Error estándar
se <- sd(differences)/sqrt(10)

# p-valor
t <- mean(differences)/se
p.value <- pt(t, df=9)

# CI
t <- qt(0.95, 9)
ci.low <- mean(differences) - t*se
ci.high <- mean(differences) + t*se

#### Cálculo con t.test ####
t.test.result <- t.test(weight.post, weight.pre, paired=TRUE, alternative="less")