# Probabilidad de 3 o más curaciones en 9 enfermos
pbinom(2, 9, 0.05, FALSE)
# Pr(X = 20)
dbinom(x = 20, size = 50, prob = 0.5)
# Pr(X <= 20)
pbinom(q = 20, size = 50, prob = 0.5)
# Pr(X >= 19)
pbinom(q = 19, size = 50, prob = 0.5, lower.tail = FALSE)
# Random sampling de binomial
rbinom(10, 4, 0.5)
# Con otros parámetros
rbinom(5, 1, 0.8)
# Contar cuantas veces se produce que los 5 niños tengan la boca sana
# Comentado para no hacer trampa
# sample <- rbinom(1000000, 5, 0.2)
# a <- table(sample)
#a
# Calcular directamente el p-valor para este caso
% dbinom(5, 5, 0.02)
# La probabilidad de recuperación espontánea es 5%. Vamos a generar enfermos
recovery.prob <- 0.05
number.of.patients <- 10
number .of. trials <- 50
results <- rbinom ( number.of.trials , number.of.patients , recovery.prob)
hist ( results , breaks =0: number.of.patients , freq = FALSE)
# Distribución de las medias de una normal
sample.means <- rep(NA, 1000)
for (i in 1:1000) {
  sample <- rnorm(40, mean = 60, sd = 4)
  sample.means[i] <- mean(sample)
}

mean.of.the.sample.means <- mean(sample.means)
sd.of.the.sample.means <- sd(sample.means)
# Distribución de las medias de una uniforme
sample.means <- rep(NA, 1000)
for (i in 1:1000) {
  sample <- runif(1000)
  sample.means[i] <- mean(sample)
}

mean.of.the.sample.means <- mean(sample.means)
sd.of.the.sample.means <- sd(sample.means)

hist(sample.means)
# Lo mismo con una exponencial
sample.means <- rep(NA, 1000)
for (i in 1:1000) {
  sample <- rexp(100, 1/60)
  sample.means[i] <- mean(sample)
}

hist(sample.means)
mean.of.the.sample.means <- mean(sample.means)
sd.of.the.sample.means <- sd(sample.means)
# El ejercicio para la distribución geometrica
# No hagas trampa
#p <- 0.2
#times <- 100000
#sample.means = rep(NA, times)
#n <- 50
#for (i in 1:times) {
#  sample <- rgeom(n, p)
#  sample.means[i] <- mean(sample)
#}
#print(c(mean(sample.means), (1-p)/p))
#print(c(sd(sample.means), sqrt((1-p)/((p^2)*n))))
# Integrando la distribucion normal
integrate(
  function(x) dnorm(x, mean = 0, sd = 1),
  0-1.96*1,
  0+1.96*1)
# Código para pintar las normales
plot(function(x) dnorm(x, mean = 50, sd = 3),
     xlim = c(40, 60))
segments(50-3*1.96, 0, 50-3*1.96, 0.2)
segments(50+3*1.96, 0, 50+3*1.96, 0.2)

# Cuanto cae dentro del intervalo de confianza?
into.the.ci <- 0
poblational.mean <- 60
poblational.sd <- 4
sample.size <- 150
for (i in 1:100) {
  sample <- rnorm(sample.size,
                  poblational.mean, poblational.sd)
  sample.mean <- mean(sample)
  low <- sample.mean - 1.96*(poblational.sd/sqrt(sample.size))
  high <- sample.mean + 1.96*(poblational.sd/sqrt(sample.size))
  print(c(low, high))
  if ( (poblational.mean >= low) && (poblational.mean <= high)) {
    into.the.ci <- into.the.ci + 1
  }
}
print(into.the.ci)
# Estimando sigma con el error estándar
sample.s <- rep(NA, 5000)
for (i in c(1:5000)) {
  sample <- runif(1000)
  s <- sqrt(
        sum((sample - mean(sample))^2)/(length(sample)-1)
      )
  sample.s[i] <- s
}
mean(sample.s)
hist(sample.s)
# Dibujando la t de Student
range <- seq(-4, 4, 0.01)
for (i in c(2, 5, 15, 20)) {
  plot(range, dnorm(range), lty = 1, col = gray(0.5),
       xlab = "", ylab = "", cex.axis = 1.5)
  lines(range, dt(range, df = i), lty = 2,
        lwd = 2)
  mtext(paste("df=", i), cex = 1.2)
}

# Otro ejercicio: Calculal el CI poblacional y muestral 
into.the.ci.poblational <- 0
into.the.ci.sample <- 0
p <- 0.2
n <- 50
times <- 1000
poblational.mean <- (1-p)/p
poblational.sd <- sqrt((1-p)/((p^2)*n))
for (i in 1:times) {
  sample <-  rgeom(n, p)
  sample.mean <- mean(sample)
  sample.sd <- sd(sample)
  sample.se <- sqrt(sum((sample - sample.mean)^2) / ((n - 1)*n))
  low.poblational <- sample.mean - 1.96*(poblational.sd)
  high.poblational <- sample.mean + 1.96*(poblational.sd)
  low.sample <- sample.mean - 1.96*(sample.se)
  high.sample <- sample.mean + 1.96*(sample.se)
  print(c(low.poblational, high.poblational, low.sample, high.sample))
  if ( (poblational.mean >= low.poblational) && (poblational.mean <= high.poblational)) {
    into.the.ci.poblational <- into.the.ci.poblational + 1
  }
  if ( (poblational.mean >= low.sample) && (sample.mean <= high.sample)) {
    into.the.ci.sample <- into.the.ci.sample + 1
  }
}
print(c(into.the.ci.poblational, into.the.ci.sample))

