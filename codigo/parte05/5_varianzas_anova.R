#Variabilidad de la varianza
variances <- rep(NA, 1000)
for (i in 1:1000) {
  sample1 <- rnorm(11, mean = 60, sd = 4)
  sample2 <- rnorm(11, mean = 60, sd = 4)
  sample3 <- rnorm(11, mean = 60, sd = 4)
  variances[i] <- var(c(mean(sample1), mean(sample2),
                        mean(sample3)))
}
plot(density(variances), main = "", xlab = "",
     ylab = "")
