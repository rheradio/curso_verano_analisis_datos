library(clinfun)

#Importación datos
##################
drug<-gl(2,10,length=20,labels=c("Éxtasis","Alcohol"),ordered=T)
sundayBDI<-c(15,35,16,18,19,17,27,16,13,20,16,15,20,15,16,13,14,19,18,18)
wedsBDI<-c(28,35,35,24,39,32,27,29,36,35,5,6,30,8,9,7,6,17,3,10)
drugData<-data.frame(drug,sundayBDI,wedsBDI)
alcoholData <- drugData[drugData$drug ==
                         "Alcohol",]
extasisData <- drugData[drugData$drug ==
                         "Éxtasis",]
#Exploración datos
#################
#Graficos
boxplot(alcoholData[,2:3])
boxplot(extasisData[,2:3])

#Estadísticos
summary(alcoholData)
summary(extasisData)
#Contraste normalidad
shapiro.test(alcoholData$wedsBDI)
shapiro.test(alcoholData$sundayBDI)
shapiro.test(extasisData$wedsBDI)
shapiro.test(extasisData$sundayBDI)

#Contraste Rangos Signados de Wilcoxon
######################
#Test
#newModel<-wilcox.test(outcome ~ predictor, data = dataFrame, paired = FALSE/TRUE)

alcoholModel<-wilcox.test(alcoholData$wedsBDI,alcoholData$sundayBDI,paired=T,correct=F)
alcoholModel

extasisModel<-wilcox.test(extasisData$wedsBDI,extasisData$sundayBDI,paired=T,correct=F)
extasisModel


#Tamaño del efecto
rFromWilcox<-function(wilcoxModel, N){
  z<- qnorm(wilcoxModel$p.value/2)
  r<- z/ sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

rFromWilcox(alcoholModel, 20)
rFromWilcox(extasisModel, 20)



