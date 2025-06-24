# Modificamos el dataset
data(cholesterol)
subjects <- factor(rep(1:10, 5))
cholesterol$subjects <- subjects

# ANOVA
aov.model <- aov(cholesterol$response ~ cholesterol$trt +
                   Error(cholesterol$subjects/cholesterol$trt))
summary(aov.model)