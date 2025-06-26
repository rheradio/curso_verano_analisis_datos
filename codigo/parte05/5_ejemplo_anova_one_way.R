library(multcomp)
library(psych)
data(cholesterol)
describeBy(cholesterol, cholesterol$trt)
boxplot(cholesterol$response ~ cholesterol$trt)

#Test ANOVA
aov.model <- aov(cholesterol$response ~
                   cholesterol$trt)
summary(aov.model)

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

with(cholesterol, pairwise.t.test(response, trt,
                                  paired=F, p.adjust.method="bonferroni"))
