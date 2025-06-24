library(phia)
# Lectura de datos
driving.data <- read.table("factorial_anova_data.txt")

# Modelo
fact.aov.model <- aov(errors ~ driving*conversation, driving.data)

# Normalidad de los residuos
fact.aov.residuals <- residuals(object = fact.aov.model)
shapiro.test(fact.aov.residuals)

# Homogeneidad de la varianza
leveneTest(driving.data$errors ~ driving.data$driving *
             driving.data$conversation)

# ANOVA
summary(fact.aov.model)

# creamos el modelo lineal que subyace al ANOVA
driving.data$conversation <- factor(driving.data$conversation)
driving.data$driving <- factor(driving.data$driving)
lm.model <- lm(errors ~ driving * conversation,
               data=driving.data)
# analizamos los efectos simples
testInteractions(lm.model, fixed="conversation",
                 across="driving")