library(factoextra)
library(scales)
library(ggplot2)
library(cluster)
library(fpc)

data.ever <- read.csv("../data/drug-test-ever.csv")
data.ever$X<-NULL

data.ever.scaled <- data.ever
data.ever.scaled$countofdrugs_ever <- rescale(data.ever.scaled$countofdrugs_ever)
data.ever.scaled$PersonalIncome <- rescale(data.ever.scaled$PersonalIncome)
data.ever.scaled$FamilyIncome <- rescale(data.ever.scaled$FamilyIncome)
data.ever.scaled$EmploymentStatus <- rescale(data.ever.scaled$EmploymentStatus)
data.ever.scaled$race_num <- rescale(data.ever.scaled$race_num)
data.ever.scaled$education <- rescale(data.ever.scaled$education)
data.ever.scaled$sex <- rescale(data.ever.scaled$sex)

set.seed(1)

submt <- kmeans(data.ever, centers=1)$betweenss
for(i in 2:10) submt[i] <- kmeans(data.ever, centers=i)$betweenss

plot(1:10, submt, type="b", xlab="nÃºmero de clusters", ylab="Diferencia entre grupos")
abline(v = 3, col="red", lty=2)
abline(v = 6, col="blue", lty=2)

submt <- kmeans(data.ever, centers=1)$withinss
for(i in 2:10) submt[i] <- kmeans(data.ever, centers=i)$withinss

plot(1:10, submt, type="b", xlab="nÃºmero de clusters", ylab="Diferencia dentro de grupos")
abline(v = 3, col="red", lty=2)
abline(v = 6, col="blue", lty=2)

clarafit <- clara(data.ever.scaled, 3, samples = 1000)
data.ever.3 <- data.ever
data.ever.3$cluster <- clarafit$clustering

df <- as.data.frame(clarafit$medoids)

print("En promedio, las personas presentan: ")
if(df[1,]$marij_ever>0){
  print("Uso de marihuana")
}
if(df[1,]$cocaine_ever>0){
  print("Uso de cocaína")
}
if(df[1,]$crack_ever>0){
  print("Uso de crack")
}
if(df[1,]$heroin_ever>0){
  print("Uso de heroína")
}
if(df[1,]$hallucinogen_ever>0){
  print("Uso de alucinógenos")
}
if(df[1,]$inhalant_ever>0){
  print("Uso de inhalantes")
}
if(df[1,]$meth_ever>0){
  print("Uso de metanfentamínas")
}
if(df[1,]$painrelieve_ever>0){
  print("Uso de calmantes de dolor")
}
if(df[1,]$tranq_ever>0){
  print("Uso de tranquilizantes")
}
if(df[1,]$stimulant_ever>0){
  print("Uso de estimulantes")
}
if(df[1,]$sedative_ever>0){
  print("Uso de sedantes")
}
if(df[1,]$countofdrugs_ever>0){
  print("Uso de sedantes")
}
cDrug <- df[1,]$countofdrugs_ever
if(cDrug<0.1){
  print("Uso de una sola droga entre las anteriormente mencionadas")
}else if(cDrug>0.1 && cDrug < 0.2){
  print("Uso de dos drogas diferentes entre las anteriormente mencionadas")
}else if(cDrug>0.2 && cDrug < 0.3){
  print("Uso de tres drogas diferentes entre las anteriormente mencionadas")
}else if(cDrug>0.3 && cDrug < 0.4){
  print("Uso de cuatro drogas diferentes entre las anteriormente mencionadas")
}else if(cDrug>0.4 && cDrug < 0.5){
  print("Uso de cinco drogas diferentes entre las anteriormente mencionadas")
}else if(cDrug>0.5 && cDrug < 0.6){
  print("Uso de seis drogas diferentes entre las anteriormente mencionadas")
}else if(cDrug>0.6 && cDrug < 0.7){
  print("Uso de siete drogas diferentes entre las anteriormente mencionadas")
}else if(cDrug>0.7 && cDrug < 0.8){
  print("Uso de ocho drogas diferentes entre las anteriormente mencionadas")
}else if(cDrug>0.8 && cDrug < 0.9){
  print("Uso de nueve drogas diferentes entre las anteriormente mencionadas")
}else if(cDrug==1){
  print("Uso de diez drogas diferentes entre las anteriormente mencionadas")
}
pIncome <- df[1,]$PersonalIncome
if(pIncome<0.1){
  print("Ingresos personales menores a $10,000")
}else if(pIncome>0.1 && pIncome < 0.3){
  print("Ingresos personales entre $10,000 - $19,999")
}else if(pIncome>0.3 && pIncome < 0.4){
  print("Ingresos personales entre $20,000 - $29,999")
}else if(pIncome>0.4 && pIncome < 0.6){
  print("Ingresos personales entre $30,000 - $39,999")
}else if(pIncome>0.6 && pIncome < 0.8){
  print("Ingresos personales entre $40,000 - $49,999")
}else if(pIncome>0.81 && pIncome < 0.9){
  print("Ingresos personales entre $50,000 - $74,999")
}else if(pIncome==1){
  print("Ingresos personales de más de $75,000")
}
pFamily <- df[1,]$FamilyIncome
if(pFamily<0.1){
  print("Ingresos familiares menores a $10,000")
}else if(pFamily>0.1 && pFamily < 0.3){
  print("Ingresos familiares entre $10,000 - $19,999")
}else if(pFamily>0.3 && pFamily < 0.4){
  print("Ingresos familiares entre $20,000 - $29,999")
}else if(pFamily>0.4 && pFamily < 0.6){
  print("Ingresos familiares entre $30,000 - $39,999")
}else if(pFamily>0.6 && pFamily < 0.8){
  print("Ingresos familiares entre $40,000 - $49,999")
}else if(pFamily>0.81 && pFamily < 0.9){
  print("Ingresos familiares entre $50,000 - $74,999")
}else if(pFamily==1){
  print("Ingresos familiares de más de $75,000")
}
pStat <- df[1,]$EmploymentStatus
if(pStat==0){
  print("Cuentan con un trabajo de tiempo completo")
}else if(pStat==0.5){
  print("Cuentan con un trabajo de medio tiempo")
}else if(pStat==1){
  print("Están sin trabajo")
}
pRace <- df[1,]$race_num
if(pRace<0.1){
  print("Personas de raza blanca")
}else if(pRace>0.1 && pRace < 0.3){
  print("Personas de raza afroamericana")
}else if(pRace>0.3 && pRace < 0.4){
  print("Personas de raza nativa americana")
}else if(pRace>0.4 && pRace < 0.6){
  print("Personas de raza hawaiana")
}else if(pRace>0.6 && pRace < 0.8){
  print("Personas de raza asiática")
}else if(pRace>0.81 && pRace < 0.9){
  print("Personas de raza mestiza")
}else if(pRace==1){
  print("Personas de raza hispana")
}
pEdu <- df[1,]$education
if(pEdu<0.1){
  print("Estudios menores a preparatoria")
}else if(pEdu>0.1 && pEdu < 0.4){
  print("Estudios hasta preparatoria")
}else if(pEdu>0.4 && pEdu < 0.9){
  print("Carrera sin terminar")
}else if(pEdu==1){
  print("Carrera terminada")
}
pSex <- df[1,]$sex
if(pSex==0){
  print("Por lo general son hombres")
}else if(pSexo==1){
  print("Por lo general son mujeres")
}

sort(unique(data.ever.scaled$sex))


sort(unique(data.ever.scaled$PersonalIncome))



clarafit <- clara(data.ever.scaled, 6, samples = 1000)
data.ever.6 <- data.ever
data.ever.6$cluster <- clarafit$clustering
