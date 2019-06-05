library(ggplot2)
library(factoextra)
library(scales)
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
abline(v = 6, col="blue", lty=2)

submt <- kmeans(data.ever, centers=1)$withinss
for(i in 2:10) submt[i] <- kmeans(data.ever, centers=i)$withinss

plot(1:10, submt, type="b", xlab="nÃºmero de clusters", ylab="Diferencia dentro de grupos")
abline(v = 6, col="blue", lty=2)

clarafit <- clara(data.ever.scaled, 6, samples = 1000)
df <- as.data.frame(clarafit$medoids)
dft <- as.data.frame(t(df))
colnames(dft)
names(dft)[names(dft) == "V1"] <- "Grupo 1"
names(dft)[names(dft) == "V2"] <- "Grupo 2"
names(dft)[names(dft) == "V3"] <- "Grupo 3"
names(dft)[names(dft) == "V4"] <- "Grupo 4"
names(dft)[names(dft) == "V5"] <- "Grupo 5"
names(dft)[names(dft) == "V6"] <- "Grupo 6"

for(i in 1:ncol(dft)){
  if(dft[1, i]==1){
    dft[1, i]<-"Si"
  }else{
    dft[1, i]<-"No"
  }
  if(dft[2, i]==1){
    dft[2, i]<-"Si"
  }else{
    dft[2, i]<-"No"
  }
  if(dft[3, i]==1){
    dft[3, i]<-"Si"
  }else{
    dft[3, i]<-"No"
  }
  if(dft[4, i]==1){
    dft[4, i]<-"Si"
  }else{
    dft[4, i]<-"No"
  }
  if(dft[5, i]==1){
    dft[5, i]<-"Si"
  }else{
    dft[5, i]<-"No"
  }
  if(dft[6, i]==1){
    dft[6, i]<-"Si"
  }else{
    dft[6, i]<-"No"
  }
  if(dft[7, i]==1){
    dft[7, i]<-"Si"
  }else{
    dft[7, i]<-"No"
  }
  if(dft[8, i]==1){
    dft[8, i]<-"Si"
  }else{
    dft[8, i]<-"No"
  }
  if(dft[9, i]==1){
    dft[9, i]<-"Si"
  }else{
    dft[9, i]<-"No"
  }
  if(dft[10, i]==1){
    dft[10, i]<-"Si"
  }else{
    dft[10, i]<-"No"
  }
  if(dft[11, i]==1){
    dft[11, i]<-"Si"
  }else{
    dft[11, i]<-"No"
  }
  if(as.numeric(dft[12, i])<0.1){
    dft[12, i] <- 1
  }else if(as.numeric(dft[12, i])>0.1 && as.numeric(dft[12, i]) < 0.2){
    dft[12, i] <- 2
  }else if(as.numeric(dft[12, i])>0.2 && as.numeric(dft[12, i]) < 0.3){
    dft[12, i] <- 3
  }else if(as.numeric(dft[12, i])>0.3 && as.numeric(dft[12, i]) < 0.4){
    dft[12, i] <- 4
  }else if(as.numeric(dft[12, i])>0.4 && as.numeric(dft[12, i]) < 0.5){
    dft[12, i] <- 5
  }else if(as.numeric(dft[12, i])>0.5 && as.numeric(dft[12, i]) < 0.6){
    dft[12, i] <- 6
  }else if(as.numeric(dft[12, i])>0.6 && as.numeric(dft[12, i]) < 0.7){
    dft[12, i] <- 7
  }else if(as.numeric(dft[12, i])>0.7 && as.numeric(dft[12, i]) < 0.8){
    dft[12, i] <- 8
  }else if(as.numeric(dft[12, i])>0.8 && as.numeric(dft[12, i]) < 0.9){
    dft[12, i] <- 9
  }else if(as.numeric(dft[12, i])==1){
    dft[12, i] <- 10
  }
  
  if(as.numeric(dft[13, i])<0.1){
    dft[13, i] <- "Menores a $10,000"
  }else if(as.numeric(dft[13, i])>0.1 && as.numeric(dft[13, i]) < 0.3){
    dft[13, i] <- "Entre $10,000 - $19,999"
  }else if(as.numeric(dft[13, i])>0.3 && as.numeric(dft[13, i]) < 0.4){
    dft[13, i] <- "Entre $20,000 - $29,999"
  }else if(as.numeric(dft[13, i])>0.4 && as.numeric(dft[13, i]) < 0.6){
    dft[13, i] <- "Entre $30,000 - $39,999"
  }else if(as.numeric(dft[13, i])>0.6 && as.numeric(dft[13, i]) < 0.8){
    dft[13, i] <- "Entre $40,000 - $49,999"
  }else if(as.numeric(dft[13, i])>0.81 && as.numeric(dft[13, i]) < 0.9){
    dft[13, i] <- "Entre $50,000 - $74,999"
  }else if(as.numeric(dft[13, i])==1){
    dft[13, i] <- "Más de $75,000"
  }
  
  if(as.numeric(dft[14, i])<0.1){
    dft[14, i] <- "Menores a $10,000"
  }else if(as.numeric(dft[14, i])>0.1 && as.numeric(dft[14, i]) < 0.3){
    dft[14, i] <- "Entre $10,000 - $19,999"
  }else if(as.numeric(dft[14, i])>0.3 && as.numeric(dft[14, i]) < 0.4){
    dft[14, i] <- "Entre $20,000 - $29,999"
  }else if(as.numeric(dft[14, i])>0.4 && as.numeric(dft[14, i]) < 0.6){
    dft[14, i] <- "Entre $30,000 - $39,999"
  }else if(as.numeric(dft[14, i])>0.6 && as.numeric(dft[14, i]) < 0.8){
    dft[14, i] <- "Entre $40,000 - $49,999"
  }else if(as.numeric(dft[14, i])>0.81 && as.numeric(dft[14, i]) < 0.9){
    dft[14, i] <- "Entre $50,000 - $74,999"
  }else if(as.numeric(dft[14, i])==1){
    dft[14, i] <- "Más de $75,000"
  }
  
  if(as.numeric(dft[15, i])==0){
    dft[15, i] <- "Tiempo completo"
  }else if(as.numeric(dft[15, i])==0.5){
    dft[15, i] <- "Medio tiempo"
  }else if(as.numeric(dft[15, i])==1){
    dft[15, i] <- "Desempleo"
  }
  
  if(as.numeric(dft[16, i])<0.1){
    dft[16, i] <- "Raza blanca"
  }else if(as.numeric(dft[16, i])>0.1 && as.numeric(dft[16, i]) < 0.3){
    dft[16, i] <- "Raza afroamericana"
  }else if(as.numeric(dft[16, i])>0.3 && as.numeric(dft[16, i]) < 0.4){
    dft[16, i] <- "Raza nativa americana"
  }else if(as.numeric(dft[16, i])>0.4 && as.numeric(dft[16, i]) < 0.6){
    dft[16, i] <- "Raza hawaiana"
  }else if(as.numeric(dft[16, i])>0.6 && as.numeric(dft[16, i]) < 0.8){
    dft[16, i] <- "Raza asiática"
  }else if(as.numeric(dft[16, i])>0.81 && as.numeric(dft[16, i]) < 0.9){
    dft[16, i] <- "Raza mestiza"
  }else if(as.numeric(dft[16, i])==1){
    dft[16, i] <- "Raza hispana"
  }
  
  if(as.numeric(dft[17, i])<0.1){
    dft[17, i] <- "Menores a preparatoria"
  }else if(as.numeric(dft[17, i])>0.1 && as.numeric(dft[17, i]) < 0.4){
    dft[17, i] <- "Solo preparatoria"
  }else if(as.numeric(dft[17, i])>0.4 && as.numeric(dft[17, i]) < 0.9){
    dft[17, i] <- "Carrera inconclusa"
  }else if(as.numeric(dft[17, i])==1){
    dft[17, i] <- "Carrera terminada"
  }
  
  if(as.numeric(dft[18, i])==0){
    dft[18, i] <- "Hombres"
  }else if(as.numeric(dft[18, i])==1){
    dft[18, i] <- "Mujeres"
  }
  
}

rownames(dft)[rownames(dft) == "marij_ever"] <- "Marihuana"
rownames(dft)[rownames(dft) == "cocaine_ever"] <- "Cocaína"
rownames(dft)[rownames(dft) == "crack_ever"] <- "Crack"
rownames(dft)[rownames(dft) == "heroin_ever"] <- "Heroína"
rownames(dft)[rownames(dft) == "hallucinogen_ever"] <- "Alucinógenos"
rownames(dft)[rownames(dft) == "inhalant_ever"] <- "Inhalantes"
rownames(dft)[rownames(dft) == "meth_ever"] <- "Metanfetamínas"
rownames(dft)[rownames(dft) == "painrelieve_ever"] <- "Calmantes de dolor"
rownames(dft)[rownames(dft) == "tranq_ever"] <- "Tranquilizantes"
rownames(dft)[rownames(dft) == "stimulant_ever"] <- "Estimulantes"
rownames(dft)[rownames(dft) == "sedative_ever"] <- "Sedantes"
rownames(dft)[rownames(dft) == "countofdrugs_ever"] <- "Cantidad de drogas probadas"
rownames(dft)[rownames(dft) == "PersonalIncome"] <- "Ingresos personales"
rownames(dft)[rownames(dft) == "FamilyIncome"] <- "Ingresos familiares"
rownames(dft)[rownames(dft) == "EmploymentStatus"] <- "Estado de empleo"
rownames(dft)[rownames(dft) == "race_num"] <- "Raza predominante"
rownames(dft)[rownames(dft) == "education"] <- "Máximo grado de estudios alcanzados"
rownames(dft)[rownames(dft) == "sex"] <- "Sexo predominante"

num <- 1
print("En promedio, las personas presentan: ")
if(df[num,]$marij_ever>0){
  print("Uso de marihuana")
}
if(df[num,]$cocaine_ever>0){
  print("Uso de cocaína")
}
if(df[num,]$crack_ever>0){
  print("Uso de crack")
}
if(df[num,]$heroin_ever>0){
  print("Uso de heroína")
}
if(df[num,]$hallucinogen_ever>0){
  print("Uso de alucinógenos")
}
if(df[num,]$inhalant_ever>0){
  print("Uso de inhalantes")
}
if(df[num,]$meth_ever>0){
  print("Uso de metanfentamínas")
}
if(df[num,]$painrelieve_ever>0){
  print("Uso de calmantes de dolor")
}
if(df[num,]$tranq_ever>0){
  print("Uso de tranquilizantes")
}
if(df[num,]$stimulant_ever>0){
  print("Uso de estimulantes")
}
if(df[num,]$sedative_ever>0){
  print("Uso de sedantes")
}
if(df[num,]$countofdrugs_ever>0){
  print("Uso de sedantes")
}
cDrug <- df[num,]$countofdrugs_ever
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
pIncome <- df[num,]$PersonalIncome
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
pFamily <- df[num,]$FamilyIncome
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
pStat <- df[num,]$EmploymentStatus
if(pStat==0){
  print("Cuentan con un trabajo de tiempo completo")
}else if(pStat==0.5){
  print("Cuentan con un trabajo de medio tiempo")
}else if(pStat==1){
  print("Están sin trabajo")
}
pRace <- df[num,]$race_num
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
pEdu <- df[num,]$education
if(pEdu<0.1){
  print("Estudios menores a preparatoria")
}else if(pEdu>0.1 && pEdu < 0.4){
  print("Estudios hasta preparatoria")
}else if(pEdu>0.4 && pEdu < 0.9){
  print("Carrera sin terminar")
}else if(pEdu==1){
  print("Carrera terminada")
}
pSex <- df[num,]$sex
if(pSex==0){
  print("Por lo general son hombres")
}else if(pSex==1){
  print("Por lo general son mujeres")
}


