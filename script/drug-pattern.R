library(factoextra)
library(scales)
library(ggplot2)
library(cluster)
library(fpc)
library(ggplot2)

## Alguna vez --------------------------------------------
#lECTURA DE INFORMACION
data.ever <- read.csv("../data/drug-test-ever.csv")
data.ever$X<-NULL

#Transformación de datos
data.ever.scaled <- data.ever
data.ever.scaled$countofdrugs_ever <- rescale(data.ever.scaled$countofdrugs_ever)
data.ever.scaled$PersonalIncome <- rescale(data.ever.scaled$PersonalIncome)
data.ever.scaled$FamilyIncome <- rescale(data.ever.scaled$FamilyIncome)
data.ever.scaled$EmploymentStatus <- rescale(data.ever.scaled$EmploymentStatus)
data.ever.scaled$race_num <- rescale(data.ever.scaled$race_num)
data.ever.scaled$education <- rescale(data.ever.scaled$education)
data.ever.scaled$sex <- rescale(data.ever.scaled$sex)
summary(data.ever.scaled)

#numero de clusters
set.seed(2019)
submt <- kmeans(data.ever, centers=1)$betweenss
for(i in 2:10) submt[i] <- kmeans(data.ever, centers=i)$betweenss
plot(1:10, submt, type="b", xlab="número de clusters", ylab="Diferencia entre grupos")
abline(v = 3, col="red", lty=2)
abline(v = 6, col="blue", lty=2)

submt <- kmeans(data.ever, centers=1)$withinss
for(i in 2:10) submt[i] <- kmeans(data.ever, centers=i)$withinss
plot(1:10, submt, type="b", xlab="número de clusters", ylab="Diferencia dentro de grupos")
abline(v = 3, col="red", lty=2)
abline(v = 6, col="blue", lty=2)

clarafit <- clara(data.ever.scaled, 3, samples = 1000)
clarafit$medoids
#km <- pam(data.ever.scaled, 3)
fviz_cluster(clarafit, data=data.ever, geom = "point",
             pointsize = 2, 
             main="Agrupamiento por análisis de componentes principales") + theme_bw()


#Creación de clusters
data.ever.km <- kmeans(data.ever.scaled, center=5)

data.ever.km$totss
data.ever.km$betweenss
data.ever.km$withinss
data.ever.km$tot.withinss

#Análisis de cluster
data.ever$cluster <- data.ever.km$cluster
data.ever.cl.1 <- data.ever[data.ever$cluster==1,]
data.ever.cl.2 <- data.ever[data.ever$cluster==2,]
data.ever.cl.3 <- data.ever[data.ever$cluster==3,]
data.ever.cl.4 <- data.ever[data.ever$cluster==4,]
data.ever.cl.5 <- data.ever[data.ever$cluster==5,]

#CL1
par(mfrow=c(2,4))
boxplot(data.ever.cl.1$marij_ever, main="Uso de mariguana", ylab="valor")
boxplot(data.ever.cl.1$cocaine_ever, main="Uso de cocaína", ylab="valor")
boxplot(data.ever.cl.1$crack_ever, main="Uso de crack", ylab="valor")
boxplot(data.ever.cl.1$heroin_ever, main="Uso de heroína", ylab="valor")
boxplot(data.ever.cl.1$hallucinogen_ever, main="Uso de alucinógenos", ylab="valor")
boxplot(data.ever.cl.1$inhalant_ever, main="Uso de inhalantes", ylab="valor")
boxplot(data.ever.cl.1$meth_ever, main="Uso de metanfetamina", ylab="valor")
boxplot(data.ever.cl.1$painrelieve_ever, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.ever.cl.1$tranq_ever, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.ever.cl.1$stimulant_ever, main="Uso de estimulantes", ylab="valor")
boxplot(data.ever.cl.1$sedative_ever, main="Uso de sedantes", ylab="valor")
boxplot(data.ever.cl.1$countofdrugs_ever, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.ever.cl.1$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.ever.cl.1$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.ever.cl.1$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.ever.cl.1$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.ever.cl.1$education, main="Grado de estudios", ylab="valor")
boxplot(data.ever.cl.1$sex, main="Sexo", ylab="valor")

#CL2
par(mfrow=c(2,4))
boxplot(data.ever.cl.2$marij_ever, main="Uso de mariguana", ylab="valor")
boxplot(data.ever.cl.2$cocaine_ever, main="Uso de cocaína", ylab="valor")
boxplot(data.ever.cl.2$crack_ever, main="Uso de crack", ylab="valor")
boxplot(data.ever.cl.2$heroin_ever, main="Uso de heroína", ylab="valor")
boxplot(data.ever.cl.2$hallucinogen_ever, main="Uso de alucinógenos", ylab="valor")
boxplot(data.ever.cl.2$inhalant_ever, main="Uso de inhalantes", ylab="valor")
boxplot(data.ever.cl.2$meth_ever, main="Uso de metanfetamina", ylab="valor")
boxplot(data.ever.cl.2$painrelieve_ever, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.ever.cl.2$tranq_ever, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.ever.cl.2$stimulant_ever, main="Uso de estimulantes", ylab="valor")
boxplot(data.ever.cl.2$sedative_ever, main="Uso de sedantes", ylab="valor")
boxplot(data.ever.cl.2$countofdrugs_ever, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.ever.cl.2$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.ever.cl.2$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.ever.cl.2$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.ever.cl.2$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.ever.cl.2$education, main="Grado de estudios", ylab="valor")
boxplot(data.ever.cl.2$sex, main="Sexo", ylab="valor")

#CL3
par(mfrow=c(2,4))
boxplot(data.ever.cl.3$marij_ever, main="Uso de mariguana", ylab="valor")
boxplot(data.ever.cl.3$cocaine_ever, main="Uso de cocaína", ylab="valor")
boxplot(data.ever.cl.3$crack_ever, main="Uso de crack", ylab="valor")
boxplot(data.ever.cl.3$heroin_ever, main="Uso de heroína", ylab="valor")
boxplot(data.ever.cl.3$hallucinogen_ever, main="Uso de alucinógenos", ylab="valor")
boxplot(data.ever.cl.3$inhalant_ever, main="Uso de inhalantes", ylab="valor")
boxplot(data.ever.cl.3$meth_ever, main="Uso de metanfetamina", ylab="valor")
boxplot(data.ever.cl.3$painrelieve_ever, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.ever.cl.3$tranq_ever, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.ever.cl.3$stimulant_ever, main="Uso de estimulantes", ylab="valor")
boxplot(data.ever.cl.3$sedative_ever, main="Uso de sedantes", ylab="valor")
boxplot(data.ever.cl.3$countofdrugs_ever, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.ever.cl.3$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.ever.cl.3$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.ever.cl.3$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.ever.cl.3$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.ever.cl.3$education, main="Grado de estudios", ylab="valor")
boxplot(data.ever.cl.3$sex, main="Sexo", ylab="valor")

#CL4
par(mfrow=c(2,4))
boxplot(data.ever.cl.4$marij_ever, main="Uso de mariguana", ylab="valor")
boxplot(data.ever.cl.4$cocaine_ever, main="Uso de cocaína", ylab="valor")
boxplot(data.ever.cl.4$crack_ever, main="Uso de crack", ylab="valor")
boxplot(data.ever.cl.4$heroin_ever, main="Uso de heroína", ylab="valor")
boxplot(data.ever.cl.4$hallucinogen_ever, main="Uso de alucinógenos", ylab="valor")
boxplot(data.ever.cl.4$inhalant_ever, main="Uso de inhalantes", ylab="valor")
boxplot(data.ever.cl.4$meth_ever, main="Uso de metanfetamina", ylab="valor")
boxplot(data.ever.cl.4$painrelieve_ever, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.ever.cl.4$tranq_ever, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.ever.cl.4$stimulant_ever, main="Uso de estimulantes", ylab="valor")
boxplot(data.ever.cl.4$sedative_ever, main="Uso de sedantes", ylab="valor")
boxplot(data.ever.cl.4$countofdrugs_ever, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.ever.cl.4$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.ever.cl.4$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.ever.cl.4$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.ever.cl.4$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.ever.cl.4$education, main="Grado de estudios", ylab="valor")
boxplot(data.ever.cl.4$sex, main="Sexo", ylab="valor")

#CL5
par(mfrow=c(2,4))
boxplot(data.ever.cl.5$marij_ever, main="Uso de mariguana", ylab="valor")
boxplot(data.ever.cl.5$cocaine_ever, main="Uso de cocaína", ylab="valor")
boxplot(data.ever.cl.5$crack_ever, main="Uso de crack", ylab="valor")
boxplot(data.ever.cl.5$heroin_ever, main="Uso de heroína", ylab="valor")
boxplot(data.ever.cl.5$hallucinogen_ever, main="Uso de alucinógenos", ylab="valor")
boxplot(data.ever.cl.5$inhalant_ever, main="Uso de inhalantes", ylab="valor")
boxplot(data.ever.cl.5$meth_ever, main="Uso de metanfetamina", ylab="valor")
boxplot(data.ever.cl.5$painrelieve_ever, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.ever.cl.5$tranq_ever, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.ever.cl.5$stimulant_ever, main="Uso de estimulantes", ylab="valor")
boxplot(data.ever.cl.5$sedative_ever, main="Uso de sedantes", ylab="valor")
boxplot(data.ever.cl.5$countofdrugs_ever, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.ever.cl.5$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.ever.cl.5$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.ever.cl.5$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.ever.cl.5$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.ever.cl.5$education, main="Grado de estudios", ylab="valor")
boxplot(data.ever.cl.5$sex, main="Sexo", ylab="valor")

#Inspección
#Componentes principales
fviz_cluster(data.ever.km, data=data.ever, geom = "point",
            pointsize = 2, 
            main="Agrupamiento por análisis de componentes principales") + theme_bw()
#Por sexo
fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("sex", "countofdrugs_ever"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Cantidad de drogas probadas",
             xlab="Sexo",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("sex", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Ingresos personales",
             xlab="Sexo",
             ylab="Ingresos personales")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("sex", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Ingresos familiares",
             xlab="Sexo",
             ylab="Ingresos familiares")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("sex", "EmploymentStatus"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Estado laboral",
             xlab="Sexo",
             ylab="Estado laboral")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("sex", "race_num"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Raza",
             xlab="Sexo",
             ylab="Raza")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("sex", "education"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Grado de estudios",
             xlab="Sexo",
             ylab="Grado de estudios")

#Por grado de estudios

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("education", "countofdrugs_ever"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Cantidad de drogas probadas",
             xlab="Grado de estudios",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("education", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Ingresos personales",
             xlab="Grado de estudios",
             ylab="Ingresos personales")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("education", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Ingresos familiares",
             xlab="Grado de estudios",
             ylab="Ingresos familiares")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("education", "EmploymentStatus"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Estado laboral",
             xlab="Grado de estudios",
             ylab="Estado laboral")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("education", "race_num"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Raza",
             xlab="Grado de estudios",
             ylab="Raza")

#Por raza

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("race_num", "countofdrugs_ever"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Cantidad de drogas probadas",
             xlab="Raza",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("race_num", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Ingresos Personales",
             xlab="Raza",
             ylab="Ingresos Personales")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("race_num", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Ingresos Familiares",
             xlab="Raza",
             ylab="Ingresos Familiares")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("race_num", "EmploymentStatus"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Estado laboral",
             xlab="Raza",
             ylab="Estado laboral")

#Por estado laboral

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("EmploymentStatus", "countofdrugs_ever"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Estado laboral vs Cantidad de drogas probadas",
             xlab="Estado laboral",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("EmploymentStatus", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Estado laboral vs Ingresos personales",
             xlab="Estado laboral",
             ylab="Ingresos personales")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("EmploymentStatus", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Estado laboral vs Ingresos familiares",
             xlab="Estado laboral",
             ylab="Ingresos familiares")

#Por ingresos familiares
fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("FamilyIncome", "countofdrugs_ever"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Ingresos familiares vs Cantidad de drogas probadas",
             xlab="Ingresos familiares",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("FamilyIncome", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Ingresos familiares vs Ingresos personales",
             xlab="Ingresos familiares",
             ylab="Ingresos personales")

#Por ingresos personales
fviz_cluster(data.ever.km, data=data.ever, geom = "point",
             choose.vars = c("PersonalIncome", "countofdrugs_ever"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Ingresos personales vs Cantidad de drogas probadas",
             xlab="Ingresos personales",
             ylab="Cantidad de drogas probadas")

graphics.off()
## Alguna vez fin --------------------------------------------

## Ultimo año inicio --------------------------------------------------
data.year <- read.csv("../data/drug-test-year.csv")
data.year$X<-NULL

#Transformación de datos
data.year.scaled <- data.year
data.year.scaled$countofdrugs_ever <- rescale(data.year.scaled$countofdrugs_ever)
data.year.scaled$PersonalIncome <- rescale(data.year.scaled$PersonalIncome)
data.year.scaled$FamilyIncome <- rescale(data.year.scaled$FamilyIncome)
data.year.scaled$EmploymentStatus <- rescale(data.year.scaled$EmploymentStatus)
data.year.scaled$race_num <- rescale(data.year.scaled$race_num)
data.year.scaled$education <- rescale(data.year.scaled$education)
data.year.scaled$sex <- rescale(data.year.scaled$sex)
summary(data.year.scaled)

#numero de clusters
set.seed(2019)
submt <- kmeans(data.year, centers=1)$betweenss
for(i in 2:10) submt[i] <- kmeans(data.year, centers=i)$betweenss
plot(1:10, submt, type="b", xlab="número de clusters", ylab="suma de cuadrados inter grupos")

#Creación de clusters
data.year.km <- kmeans(data.year.scaled, center=7)


data.year.km$totss
data.year.km$betweenss
data.year.km$withinss
data.year.km$tot.withinss

#Análisis de cluster
data.year$cluster <- data.year.km$cluster
data.year.cl.1 <- data.year[data.year$cluster==1,]
data.year.cl.2 <- data.year[data.year$cluster==2,]
data.year.cl.3 <- data.year[data.year$cluster==3,]
data.year.cl.4 <- data.year[data.year$cluster==4,]
data.year.cl.5 <- data.year[data.year$cluster==5,]
data.year.cl.6 <- data.year[data.year$cluster==6,]
data.year.cl.7 <- data.year[data.year$cluster==7,]

summary(data.year.cl.7)

#CL1
par(mfrow=c(2,4))
boxplot(data.year.cl.1$marij_year, main="Uso de mariguana", ylab="valor")
boxplot(data.year.cl.1$cocaine_year, main="Uso de cocaína", ylab="valor")
boxplot(data.year.cl.1$crack_year, main="Uso de crack", ylab="valor")
boxplot(data.year.cl.1$heroin_year, main="Uso de heroína", ylab="valor")
boxplot(data.year.cl.1$hallucinogen_year, main="Uso de alucinógenos", ylab="valor")
boxplot(data.year.cl.1$inhalant_year, main="Uso de inhalantes", ylab="valor")
boxplot(data.year.cl.1$meth_year, main="Uso de metanfetamina", ylab="valor")
boxplot(data.year.cl.1$painrelieve_year, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.year.cl.1$tranq_year, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.year.cl.1$stimulant_year, main="Uso de estimulantes", ylab="valor")
boxplot(data.year.cl.1$sedative_year, main="Uso de sedantes", ylab="valor")
boxplot(data.year.cl.1$countofdrugs_year, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.year.cl.1$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.year.cl.1$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.year.cl.1$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.year.cl.1$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.year.cl.1$education, main="Grado de estudios", ylab="valor")
boxplot(data.year.cl.1$sex, main="Sexo", ylab="valor")

#CL2
par(mfrow=c(2,4))
boxplot(data.year.cl.2$marij_year, main="Uso de mariguana", ylab="valor")
boxplot(data.year.cl.2$cocaine_year, main="Uso de cocaína", ylab="valor")
boxplot(data.year.cl.2$crack_year, main="Uso de crack", ylab="valor")
boxplot(data.year.cl.2$heroin_year, main="Uso de heroína", ylab="valor")
boxplot(data.year.cl.2$hallucinogen_year, main="Uso de alucinógenos", ylab="valor")
boxplot(data.year.cl.2$inhalant_year, main="Uso de inhalantes", ylab="valor")
boxplot(data.year.cl.2$meth_year, main="Uso de metanfetamina", ylab="valor")
boxplot(data.year.cl.2$painrelieve_year, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.year.cl.2$tranq_year, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.year.cl.2$stimulant_year, main="Uso de estimulantes", ylab="valor")
boxplot(data.year.cl.2$sedative_year, main="Uso de sedantes", ylab="valor")
boxplot(data.year.cl.2$countofdrugs_year, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.year.cl.2$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.year.cl.2$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.year.cl.2$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.year.cl.2$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.year.cl.2$education, main="Grado de estudios", ylab="valor")
boxplot(data.year.cl.2$sex, main="Sexo", ylab="valor")

#CL3
par(mfrow=c(2,4))
boxplot(data.year.cl.3$marij_year, main="Uso de mariguana", ylab="valor")
boxplot(data.year.cl.3$cocaine_year, main="Uso de cocaína", ylab="valor")
boxplot(data.year.cl.3$crack_year, main="Uso de crack", ylab="valor")
boxplot(data.year.cl.3$heroin_year, main="Uso de heroína", ylab="valor")
boxplot(data.year.cl.3$hallucinogen_year, main="Uso de alucinógenos", ylab="valor")
boxplot(data.year.cl.3$inhalant_year, main="Uso de inhalantes", ylab="valor")
boxplot(data.year.cl.3$meth_year, main="Uso de metanfetamina", ylab="valor")
boxplot(data.year.cl.3$painrelieve_year, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.year.cl.3$tranq_year, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.year.cl.3$stimulant_year, main="Uso de estimulantes", ylab="valor")
boxplot(data.year.cl.3$sedative_year, main="Uso de sedantes", ylab="valor")
boxplot(data.year.cl.3$countofdrugs_year, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.year.cl.3$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.year.cl.3$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.year.cl.3$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.year.cl.3$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.year.cl.3$education, main="Grado de estudios", ylab="valor")
boxplot(data.year.cl.3$sex, main="Sexo", ylab="valor")

#CL4
par(mfrow=c(2,4))
boxplot(data.year.cl.4$marij_year, main="Uso de mariguana", ylab="valor")
boxplot(data.year.cl.4$cocaine_year, main="Uso de cocaína", ylab="valor")
boxplot(data.year.cl.4$crack_year, main="Uso de crack", ylab="valor")
boxplot(data.year.cl.4$heroin_year, main="Uso de heroína", ylab="valor")
boxplot(data.year.cl.4$hallucinogen_year, main="Uso de alucinógenos", ylab="valor")
boxplot(data.year.cl.4$inhalant_year, main="Uso de inhalantes", ylab="valor")
boxplot(data.year.cl.4$meth_year, main="Uso de metanfetamina", ylab="valor")
boxplot(data.year.cl.4$painrelieve_year, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.year.cl.4$tranq_year, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.year.cl.4$stimulant_year, main="Uso de estimulantes", ylab="valor")
boxplot(data.year.cl.4$sedative_year, main="Uso de sedantes", ylab="valor")
boxplot(data.year.cl.4$countofdrugs_year, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.year.cl.4$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.year.cl.4$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.year.cl.4$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.year.cl.4$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.year.cl.4$education, main="Grado de estudios", ylab="valor")
boxplot(data.year.cl.4$sex, main="Sexo", ylab="valor")

#CL5
par(mfrow=c(2,4))
boxplot(data.year.cl.5$marij_year, main="Uso de mariguana", ylab="valor")
boxplot(data.year.cl.5$cocaine_year, main="Uso de cocaína", ylab="valor")
boxplot(data.year.cl.5$crack_year, main="Uso de crack", ylab="valor")
boxplot(data.year.cl.5$heroin_year, main="Uso de heroína", ylab="valor")
boxplot(data.year.cl.5$hallucinogen_year, main="Uso de alucinógenos", ylab="valor")
boxplot(data.year.cl.5$inhalant_year, main="Uso de inhalantes", ylab="valor")
boxplot(data.year.cl.5$meth_year, main="Uso de metanfetamina", ylab="valor")
boxplot(data.year.cl.5$painrelieve_year, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.year.cl.5$tranq_year, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.year.cl.5$stimulant_year, main="Uso de estimulantes", ylab="valor")
boxplot(data.year.cl.5$sedative_year, main="Uso de sedantes", ylab="valor")
boxplot(data.year.cl.5$countofdrugs_year, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.year.cl.5$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.year.cl.5$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.year.cl.5$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.year.cl.5$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.year.cl.5$education, main="Grado de estudios", ylab="valor")
boxplot(data.year.cl.5$sex, main="Sexo", ylab="valor")

#CL6

par(mfrow=c(2,4))
boxplot(data.year.cl.6$marij_year, main="Uso de mariguana", ylab="valor")
boxplot(data.year.cl.6$cocaine_year, main="Uso de cocaína", ylab="valor")
boxplot(data.year.cl.6$crack_year, main="Uso de crack", ylab="valor")
boxplot(data.year.cl.6$heroin_year, main="Uso de heroína", ylab="valor")
boxplot(data.year.cl.6$hallucinogen_year, main="Uso de alucinógenos", ylab="valor")
boxplot(data.year.cl.6$inhalant_year, main="Uso de inhalantes", ylab="valor")
boxplot(data.year.cl.6$meth_year, main="Uso de metanfetamina", ylab="valor")
boxplot(data.year.cl.6$painrelieve_year, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.year.cl.6$tranq_year, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.year.cl.6$stimulant_year, main="Uso de estimulantes", ylab="valor")
boxplot(data.year.cl.6$sedative_year, main="Uso de sedantes", ylab="valor")
boxplot(data.year.cl.6$countofdrugs_year, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.year.cl.6$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.year.cl.6$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.year.cl.6$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.year.cl.6$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.year.cl.6$education, main="Grado de estudios", ylab="valor")
boxplot(data.year.cl.6$sex, main="Sexo", ylab="valor")

#CL7

par(mfrow=c(2,4))
boxplot(data.year.cl.7$marij_year, main="Uso de mariguana", ylab="valor")
boxplot(data.year.cl.7$cocaine_year, main="Uso de cocaína", ylab="valor")
boxplot(data.year.cl.7$crack_year, main="Uso de crack", ylab="valor")
boxplot(data.year.cl.7$heroin_year, main="Uso de heroína", ylab="valor")
boxplot(data.year.cl.7$hallucinogen_year, main="Uso de alucinógenos", ylab="valor")
boxplot(data.year.cl.7$inhalant_year, main="Uso de inhalantes", ylab="valor")
boxplot(data.year.cl.7$meth_year, main="Uso de metanfetamina", ylab="valor")
boxplot(data.year.cl.7$painrelieve_year, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.year.cl.7$tranq_year, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.year.cl.7$stimulant_year, main="Uso de estimulantes", ylab="valor")
boxplot(data.year.cl.7$sedative_year, main="Uso de sedantes", ylab="valor")
boxplot(data.year.cl.7$countofdrugs_year, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.year.cl.7$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.year.cl.7$FamilyIncome, main="Ingresos familiares", ylab="valor")
boxplot(data.year.cl.7$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.year.cl.7$race_num, main="Raza", ylab="valor")

par(mfrow=c(1,2))
boxplot(data.year.cl.7$education, main="Grado de estudios", ylab="valor")
boxplot(data.year.cl.7$sex, main="Sexo", ylab="valor")

#Inspección
#Componentes principales
fviz_cluster(data.year.km, data=data.year, geom = "point",
             pointsize = 2, 
             main="Agrupamiento por análisis de componentes principales") + theme_bw()
#Por sexo
fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("sex", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Cantidad de drogas probadas",
             xlab="Sexo",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("sex", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Ingresos personales",
             xlab="Sexo",
             ylab="Ingresos personales")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("sex", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Ingresos familiares",
             xlab="Sexo",
             ylab="Ingresos familiares")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("sex", "EmploymentStatus"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Estado laboral",
             xlab="Sexo",
             ylab="Estado laboral")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("sex", "race_num"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Raza",
             xlab="Sexo",
             ylab="Raza")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("sex", "education"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Grado de estudios",
             xlab="Sexo",
             ylab="Grado de estudios")

#Por grado de estudios

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("education", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Cantidad de drogas probadas",
             xlab="Grado de estudios",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("education", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Ingresos personales",
             xlab="Grado de estudios",
             ylab="Ingresos personales")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("education", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Ingresos familiares",
             xlab="Grado de estudios",
             ylab="Ingresos familiares")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("education", "EmploymentStatus"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Estado laboral",
             xlab="Grado de estudios",
             ylab="Estado laboral")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("education", "race_num"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Raza",
             xlab="Grado de estudios",
             ylab="Raza")

#Por raza

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("race_num", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Cantidad de drogas probadas",
             xlab="Raza",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("race_num", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Ingresos Personales",
             xlab="Raza",
             ylab="Ingresos Personales")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("race_num", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Ingresos Familiares",
             xlab="Raza",
             ylab="Ingresos Familiares")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("race_num", "EmploymentStatus"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Estado laboral",
             xlab="Raza",
             ylab="Estado laboral")

#Por estado laboral

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("EmploymentStatus", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Estado laboral vs Cantidad de drogas probadas",
             xlab="Estado laboral",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("EmploymentStatus", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Estado laboral vs Ingresos personales",
             xlab="Estado laboral",
             ylab="Ingresos personales")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("EmploymentStatus", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Estado laboral vs Ingresos familiares",
             xlab="Estado laboral",
             ylab="Ingresos familiares")

#Por ingresos familiares
fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("FamilyIncome", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Ingresos familiares vs Cantidad de drogas probadas",
             xlab="Ingresos familiares",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("FamilyIncome", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Ingresos familiares vs Ingresos personales",
             xlab="Ingresos familiares",
             ylab="Ingresos personales")

#Por ingresos personales
fviz_cluster(data.year.km, data=data.year, geom = "point",
             choose.vars = c("PersonalIncome", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Ingresos personales vs Cantidad de drogas probadas",
             xlab="Ingresos personales",
             ylab="Cantidad de drogas probadas")
graphics.off()
## Ultimo año final --------------------------------------------------

## Ultimo mes inicio --------------------------------------------------
data.month <- read.csv("../data/drug-test-month.csv")
data.month$X<-NULL
summary(data.month)

#Transformación de datos
data.month.scaled <- data.month
data.month.scaled$countofdrugs_ever <- rescale(data.month.scaled$countofdrugs_ever)
data.month.scaled$PersonalIncome <- rescale(data.month.scaled$PersonalIncome)
data.month.scaled$FamilyIncome <- rescale(data.month.scaled$FamilyIncome)
data.month.scaled$EmploymentStatus <- rescale(data.month.scaled$EmploymentStatus)
data.month.scaled$race_num <- rescale(data.month.scaled$race_num)
data.month.scaled$education <- rescale(data.month.scaled$education)
data.month.scaled$sex <- rescale(data.month.scaled$sex)
summary(data.month.scaled)

#numero de clusters
set.seed(2019)
submt <- kmeans(data.month, centers=1)$betweenss
for(i in 2:10) submt[i] <- kmeans(data.month, centers=i)$betweenss
plot(1:10, submt, type="b", xlab="número de clusters", ylab="suma de cuadrados inter grupos")


#Creación de clusters
# 3, 4, 5, 6
data.month.km <- kmeans(data.month.scaled, center=6)

data.month.km$totss
data.month.km$betweenss
data.month.km$withinss
data.month.km$tot.withinss

#Análisis de cluster
data.month$cluster <- data.month.km$cluster
data.month.cl.1 <- data.month[data.month$cluster==1,]
data.month.cl.2 <- data.month[data.month$cluster==2,]
data.month.cl.3 <- data.month[data.month$cluster==3,]
data.month.cl.4 <- data.month[data.month$cluster==4,]
data.month.cl.5 <- data.month[data.month$cluster==5,]
data.month.cl.6 <- data.month[data.month$cluster==6,]

#CL1
par(mfrow=c(2,4))
boxplot(data.month.cl.1$marij_month, main="Uso de mariguana", ylab="valor")
boxplot(data.month.cl.1$cocaine_month, main="Uso de cocaína", ylab="valor")
boxplot(data.month.cl.1$crack_month, main="Uso de crack", ylab="valor")
boxplot(data.month.cl.1$heroin_month, main="Uso de heroína", ylab="valor")
boxplot(data.month.cl.1$hallucinogen_month, main="Uso de alucinógenos", ylab="valor")
boxplot(data.month.cl.1$inhalant_month, main="Uso de inhalantes", ylab="valor")
boxplot(data.month.cl.1$meth_month, main="Uso de metanfetamina", ylab="valor")
boxplot(data.month.cl.1$painrelieve_month, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.month.cl.1$tranq_month, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.month.cl.1$stimulant_month, main="Uso de estimulantes", ylab="valor")
boxplot(data.month.cl.1$sedative_month, main="Uso de sedantes", ylab="valor")
boxplot(data.month.cl.1$pharmamonth, main="Uso de drogas recetadas", ylab="valor")
boxplot(data.month.cl.1$illicitmonth_nomj, main="Uso de drogas ilicitas", ylab="valor")
boxplot(data.month.cl.1$countofdrugs_month, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.month.cl.1$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.month.cl.1$FamilyIncome, main="Ingresos familiares", ylab="valor")

par(mfrow=c(2,2))
boxplot(data.month.cl.1$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.month.cl.1$race_num, main="Raza", ylab="valor")
boxplot(data.month.cl.1$education, main="Grado de estudios", ylab="valor")
boxplot(data.month.cl.1$sex, main="Sexo", ylab="valor")

#CL2
par(mfrow=c(2,4))
boxplot(data.month.cl.2$marij_month, main="Uso de mariguana", ylab="valor")
boxplot(data.month.cl.2$cocaine_month, main="Uso de cocaína", ylab="valor")
boxplot(data.month.cl.2$crack_month, main="Uso de crack", ylab="valor")
boxplot(data.month.cl.2$heroin_month, main="Uso de heroína", ylab="valor")
boxplot(data.month.cl.2$hallucinogen_month, main="Uso de alucinógenos", ylab="valor")
boxplot(data.month.cl.2$inhalant_month, main="Uso de inhalantes", ylab="valor")
boxplot(data.month.cl.2$meth_month, main="Uso de metanfetamina", ylab="valor")
boxplot(data.month.cl.2$painrelieve_month, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.month.cl.2$tranq_month, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.month.cl.2$stimulant_month, main="Uso de estimulantes", ylab="valor")
boxplot(data.month.cl.2$sedative_month, main="Uso de sedantes", ylab="valor")
boxplot(data.month.cl.2$pharmamonth, main="Uso de drogas recetadas", ylab="valor")
boxplot(data.month.cl.2$illicitmonth_nomj, main="Uso de drogas ilicitas", ylab="valor")
boxplot(data.month.cl.2$countofdrugs_month, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.month.cl.2$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.month.cl.2$FamilyIncome, main="Ingresos familiares", ylab="valor")

par(mfrow=c(2,2))
boxplot(data.month.cl.2$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.month.cl.2$race_num, main="Raza", ylab="valor")
boxplot(data.month.cl.2$education, main="Grado de estudios", ylab="valor")
boxplot(data.month.cl.2$sex, main="Sexo", ylab="valor")

#CL3
par(mfrow=c(2,4))
boxplot(data.month.cl.3$marij_month, main="Uso de mariguana", ylab="valor")
boxplot(data.month.cl.3$cocaine_month, main="Uso de cocaína", ylab="valor")
boxplot(data.month.cl.3$crack_month, main="Uso de crack", ylab="valor")
boxplot(data.month.cl.3$heroin_month, main="Uso de heroína", ylab="valor")
boxplot(data.month.cl.3$hallucinogen_month, main="Uso de alucinógenos", ylab="valor")
boxplot(data.month.cl.3$inhalant_month, main="Uso de inhalantes", ylab="valor")
boxplot(data.month.cl.3$meth_month, main="Uso de metanfetamina", ylab="valor")
boxplot(data.month.cl.3$painrelieve_month, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.month.cl.3$tranq_month, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.month.cl.3$stimulant_month, main="Uso de estimulantes", ylab="valor")
boxplot(data.month.cl.3$sedative_month, main="Uso de sedantes", ylab="valor")
boxplot(data.month.cl.3$pharmamonth, main="Uso de drogas recetadas", ylab="valor")
boxplot(data.month.cl.3$illicitmonth_nomj, main="Uso de drogas ilicitas", ylab="valor")
boxplot(data.month.cl.3$countofdrugs_month, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.month.cl.3$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.month.cl.3$FamilyIncome, main="Ingresos familiares", ylab="valor")

par(mfrow=c(2,2))
boxplot(data.month.cl.3$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.month.cl.3$race_num, main="Raza", ylab="valor")
boxplot(data.month.cl.3$education, main="Grado de estudios", ylab="valor")
boxplot(data.month.cl.3$sex, main="Sexo", ylab="valor")

#CL4
par(mfrow=c(2,4))
boxplot(data.month.cl.4$marij_month, main="Uso de mariguana", ylab="valor")
boxplot(data.month.cl.4$cocaine_month, main="Uso de cocaína", ylab="valor")
boxplot(data.month.cl.4$crack_month, main="Uso de crack", ylab="valor")
boxplot(data.month.cl.4$heroin_month, main="Uso de heroína", ylab="valor")
boxplot(data.month.cl.4$hallucinogen_month, main="Uso de alucinógenos", ylab="valor")
boxplot(data.month.cl.4$inhalant_month, main="Uso de inhalantes", ylab="valor")
boxplot(data.month.cl.4$meth_month, main="Uso de metanfetamina", ylab="valor")
boxplot(data.month.cl.4$painrelieve_month, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.month.cl.4$tranq_month, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.month.cl.4$stimulant_month, main="Uso de estimulantes", ylab="valor")
boxplot(data.month.cl.4$sedative_month, main="Uso de sedantes", ylab="valor")
boxplot(data.month.cl.4$pharmamonth, main="Uso de drogas recetadas", ylab="valor")
boxplot(data.month.cl.4$illicitmonth_nomj, main="Uso de drogas ilicitas", ylab="valor")
boxplot(data.month.cl.4$countofdrugs_month, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.month.cl.4$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.month.cl.4$FamilyIncome, main="Ingresos familiares", ylab="valor")

par(mfrow=c(2,2))
boxplot(data.month.cl.4$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.month.cl.4$race_num, main="Raza", ylab="valor")
boxplot(data.month.cl.4$education, main="Grado de estudios", ylab="valor")
boxplot(data.month.cl.4$sex, main="Sexo", ylab="valor")

#CL5
par(mfrow=c(2,4))
boxplot(data.month.cl.5$marij_month, main="Uso de mariguana", ylab="valor")
boxplot(data.month.cl.5$cocaine_month, main="Uso de cocaína", ylab="valor")
boxplot(data.month.cl.5$crack_month, main="Uso de crack", ylab="valor")
boxplot(data.month.cl.5$heroin_month, main="Uso de heroína", ylab="valor")
boxplot(data.month.cl.5$hallucinogen_month, main="Uso de alucinógenos", ylab="valor")
boxplot(data.month.cl.5$inhalant_month, main="Uso de inhalantes", ylab="valor")
boxplot(data.month.cl.5$meth_month, main="Uso de metanfetamina", ylab="valor")
boxplot(data.month.cl.5$painrelieve_month, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.month.cl.5$tranq_month, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.month.cl.5$stimulant_month, main="Uso de estimulantes", ylab="valor")
boxplot(data.month.cl.5$sedative_month, main="Uso de sedantes", ylab="valor")
boxplot(data.month.cl.5$pharmamonth, main="Uso de drogas recetadas", ylab="valor")
boxplot(data.month.cl.5$illicitmonth_nomj, main="Uso de drogas ilicitas", ylab="valor")
boxplot(data.month.cl.5$countofdrugs_month, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.month.cl.5$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.month.cl.5$FamilyIncome, main="Ingresos familiares", ylab="valor")

par(mfrow=c(2,2))
boxplot(data.month.cl.5$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.month.cl.5$race_num, main="Raza", ylab="valor")
boxplot(data.month.cl.5$education, main="Grado de estudios", ylab="valor")
boxplot(data.month.cl.5$sex, main="Sexo", ylab="valor")

#CL6
par(mfrow=c(2,4))
boxplot(data.month.cl.6$marij_month, main="Uso de mariguana", ylab="valor")
boxplot(data.month.cl.6$cocaine_month, main="Uso de cocaína", ylab="valor")
boxplot(data.month.cl.6$crack_month, main="Uso de crack", ylab="valor")
boxplot(data.month.cl.6$heroin_month, main="Uso de heroína", ylab="valor")
boxplot(data.month.cl.6$hallucinogen_month, main="Uso de alucinógenos", ylab="valor")
boxplot(data.month.cl.6$inhalant_month, main="Uso de inhalantes", ylab="valor")
boxplot(data.month.cl.6$meth_month, main="Uso de metanfetamina", ylab="valor")
boxplot(data.month.cl.6$painrelieve_month, main="Uso de calmantes de dolor", ylab="valor")

par(mfrow=c(2,4))
boxplot(data.month.cl.6$tranq_month, main="Uso de tranquilizantes", ylab="valor")
boxplot(data.month.cl.6$stimulant_month, main="Uso de estimulantes", ylab="valor")
boxplot(data.month.cl.6$sedative_month, main="Uso de sedantes", ylab="valor")
boxplot(data.month.cl.6$pharmamonth, main="Uso de drogas recetadas", ylab="valor")
boxplot(data.month.cl.6$illicitmonth_nomj, main="Uso de drogas ilicitas", ylab="valor")
boxplot(data.month.cl.6$countofdrugs_month, main="Cantidad de drogas probadas", ylab="valor")
boxplot(data.month.cl.6$PersonalIncome, main="Ingresos personales", ylab="valor")
boxplot(data.month.cl.6$FamilyIncome, main="Ingresos familiares", ylab="valor")

par(mfrow=c(2,2))
boxplot(data.month.cl.6$EmploymentStatus, main="Estado laboral", ylab="valor")
boxplot(data.month.cl.6$race_num, main="Raza", ylab="valor")
boxplot(data.month.cl.6$education, main="Grado de estudios", ylab="valor")
boxplot(data.month.cl.6$sex, main="Sexo", ylab="valor")

#Componentes principales
fviz_cluster(data.month.km, data=data.month, geom = "point",
             pointsize = 2, 
             main="Agrupamiento por análisis de componentes principales") + theme_bw()
#Por sexo
fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("sex", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Cantidad de drogas probadas",
             xlab="Sexo",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("sex", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Ingresos personales",
             xlab="Sexo",
             ylab="Ingresos personales")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("sex", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Ingresos familiares",
             xlab="Sexo",
             ylab="Ingresos familiares")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("sex", "EmploymentStatus"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Estado laboral",
             xlab="Sexo",
             ylab="Estado laboral")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("sex", "race_num"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Raza",
             xlab="Sexo",
             ylab="Raza")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("sex", "education"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Agrupamiento de Sexo vs Grado de estudios",
             xlab="Sexo",
             ylab="Grado de estudios")

#Por grado de estudios

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("education", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Cantidad de drogas probadas",
             xlab="Grado de estudios",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("education", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Ingresos personales",
             xlab="Grado de estudios",
             ylab="Ingresos personales")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("education", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Ingresos familiares",
             xlab="Grado de estudios",
             ylab="Ingresos familiares")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("education", "EmploymentStatus"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Estado laboral",
             xlab="Grado de estudios",
             ylab="Estado laboral")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("education", "race_num"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Grado de estudios vs Raza",
             xlab="Grado de estudios",
             ylab="Raza")

#Por raza

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("race_num", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Cantidad de drogas probadas",
             xlab="Raza",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("race_num", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Ingresos Personales",
             xlab="Raza",
             ylab="Ingresos Personales")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("race_num", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Ingresos Familiares",
             xlab="Raza",
             ylab="Ingresos Familiares")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("race_num", "EmploymentStatus"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Raza vs Estado laboral",
             xlab="Raza",
             ylab="Estado laboral")

#Por estado laboral

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("EmploymentStatus", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Estado laboral vs Cantidad de drogas probadas",
             xlab="Estado laboral",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("EmploymentStatus", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Estado laboral vs Ingresos personales",
             xlab="Estado laboral",
             ylab="Ingresos personales")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("EmploymentStatus", "FamilyIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Estado laboral vs Ingresos familiares",
             xlab="Estado laboral",
             ylab="Ingresos familiares")

#Por ingresos familiares
fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("FamilyIncome", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Ingresos familiares vs Cantidad de drogas probadas",
             xlab="Ingresos familiares",
             ylab="Cantidad de drogas probadas")

fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("FamilyIncome", "PersonalIncome"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Ingresos familiares vs Ingresos personales",
             xlab="Ingresos familiares",
             ylab="Ingresos personales")

#Por ingresos personales
fviz_cluster(data.month.km, data=data.month, geom = "point",
             choose.vars = c("PersonalIncome", "countofdrugs_year"),
             ellipse = FALSE, pointsize = 2, stand = FALSE,
             main="Ingresos personales vs Cantidad de drogas probadas",
             xlab="Ingresos personales",
             ylab="Cantidad de drogas probadas")

graphics.off()
## Ultimo mes final --------------------------------------------------

