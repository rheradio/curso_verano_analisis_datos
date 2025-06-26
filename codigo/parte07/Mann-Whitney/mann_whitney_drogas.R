library (clinfun)
library (ggplot2)
library (pastecs)
library (pgirmess)
library(ggplot2)
#Importacion datos
##################
drug<-gl(2,10,length=20,labels=c("Extasis","Alcohol"),
         ordered=T)
sundayBDI<-c(15, 35, 16, 18, 19, 17, 27, 16, 13, 20,
             16, 15, 20, 15, 16, 13, 14, 19, 18, 18)
wedsBDI<-c(28, 35, 35, 24, 39, 32, 27, 29, 36, 35, 
           5, 6, 30, 8, 9, 7, 6, 17, 3, 10)
drugData<-data.frame(drug,sundayBDI,wedsBDI)

#Graficos
boxplot(wedsBDI ~ drug)
boxplot(sundayBDI ~ drug)

#Estadisticos

by(drugData[, c("sundayBDI", "wedsBDI")], drugData$drug, 
   stat.desc, basic = FALSE, norm = TRUE)

#Contraste normalidad
shapiro.test(wedsBDI)
shapiro.test(sundayBDI)

#Contraste homogeneidad varianza
leveneTest(drugData$wedsBDI, drugData$drug, center = mean)
leveneTest(drugData$sundayBDI, drugData$drug, center = mean)

#Contraste Mann-Whitney
######################
#Test
wedModel<-wilcox.test(wedsBDI ~ drug, data = drugData, 
                      exact = FALSE, correct= FALSE)
wedModel

sunModel<-wilcox.test(sundayBDI ~ drug, data = drugData, 
                      exact = FALSE,correct= FALSE)
sunModel

#Tamaño del efecto
rFromWilcox<-function(wilcoxModel, N){
  z<- qnorm(wilcoxModel$p.value/2)
  r<- z/ sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

rFromWilcox(sunModel, 20)
rFromWilcox(wedModel, 20)