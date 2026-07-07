# Modificamos el dataset
data(cholesterol)
subjects <- factor(rep(1:10, 5))
cholesterol$subjects <- subjects

# ANOVA
aov.model <- aov(cholesterol$response ~ cholesterol$trt +
                   Error(cholesterol$subjects/cholesterol$trt))
summary(aov.model)

#Tamaño del efecto
eta.squared <- 1351.4/(1351.4+237.4)
eta_squared(aov.model, partial = TRUE)

#Post-hoc
pairwise.t.test(cholesterol$response, cholesterol$trt, paired = TRUE)
