#Borramos la memoria
rm(list = ls())
#Cargamos la librería y datos.
library(caret)
library(ROSE)
datos_train <- read.csv("C:/Users/WinUser/Desktop/TESIS DE GRADO/VISUALIZACION Y PREPROCESAMIENTO DE LA BASE DE DATOS/MODELO LOGIT/base_entrenamiento_balanceada.csv", sep = ",")
dim(datos_train)
datos_train$dcronica <- as.factor(datos_train$dcronica)

#Creamos el modelo
mod_logit_b <- glm(dcronica~FBCP1_6.indígena. +FBCP1_7.separado+FBCH_17.no+FBCH_12+
                     FBCP1_1+FBCP1_5+FBCP1_8.Educación.Media.Bachillerato+FBCP1_8.Superior+FIPA_33.no.sabe...no.responde+
                     FSCE_3+FSCE_9.prematuro+FSCE_12+FSCN_5.más.grande+FSCN_5.pequeño+
                     FSCCN_21.19.23+FSCCN_21.31.35+FIEI_1.no+FIEI_1.si,data=datos_train,family=binomial(link="logit"))

#Guardamos el modelo. 
saveRDS(mod_logit_b, "mod_logit_b.rds")
rm(list = ls())
#Cargamos modelo.
model_logit <- readRDS("mod_logit_b.rds")
