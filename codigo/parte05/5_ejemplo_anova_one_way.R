library(multcomp)
library(psych)
data(cholesterol)
describeBy(cholesterol, cholesterol$trt)
boxplot(cholesterol$response ~ cholesterol$trt)

#Test ANOVA
aov.model <- aov(cholesterol$response ~
                   cholesterol$trt)
summary(aov.model)

#Tamaño efecto
eta_squared(aov.model)

#Potencia
eta <- eta_squared(aov.model)$Eta2
Cohen.f <- sqrt(eta^2/(1-eta^2))
pwr.anova.test(k=5, n=10, f=Cohen.f, sig.level=0.05)

pwr.anova.test(k=5, f=0.25, sig.level=0.05, power=0.9)

#Normalidad de residuos
aov.residuals <- residuals(object = aov.model)
shapiro.test(aov.residuals)

#Homogeneidad de varianzas
library(car)
leveneTest(cholesterol$response, cholesterol$trt)

#En caso de no homogeneidad de las varianzas
oneway.test(cholesterol$response ~ cholesterol$trt,
            var.equal=FALSE)

#Post-hoc Tests
TukeyHSD(aov.model)

one.time <- subset(cholesterol, trt=="1time")
two.times <- subset(cholesterol, trt=="2times")
t.test(one.time$response, two.times$response,
       var.equal = TRUE)

pairwise.t.test(cholesterol$response, cholesterol$trt, p.adjust.method = "bonferroni")
