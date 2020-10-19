#Borramos la memoria
rm(list = ls())
#Cargamos la librería y datos.
library(caret) 
library(tidyverse) 
library(MASS)
library(dummies)
library(VIM)
library(DMwR)
library(funModeling)
library(rpart)
library(rpart.plot)
library(ROSE) 
library(ROCR)
library(pROC)
set.seed(123456789)
train <- read.csv("C:/Users/WinUser/Desktop/TESIS DE GRADO/VISUALIZACION Y PREPROCESAMIENTO DE LA BASE DE DATOS/BALANCEO DE CLASES/balanced_entrenamiento.csv", sep = ",")
factor <- c("dcronica","id_viv","id_hogar","id_per","FBCP1_4","FBCP1_6","FBCP1_7","FBCH_17","FSCCN_23","FBCH_1","FBCH_2",
            "FBCH_3","FBCH_4","FBCH_5","FBCH_6","FBCH_7","FBCH_8","FBCH_9","FBCH_10","FBCH_13","FBCH_14","FBCH_15","FBCH_16",
            "FBCH_18","FBCP1_8","FIPA_4","FIPA_5","FIPA_6","FIPA_11","FIPA_12","FIPA_13","FIPA_15",
            "FIPA_17","FIPA_19","FIPA_20","FIPA_22","FIPA_24","FIPA_26","FIPA_28","FIPA_30","FIPA_32","FIPA_33","FIPA_34",
            "FIPA_35","FIPA_36","FBCP1_3","FSCE_1","FSCE_2","FSCE_5","FSCE_6","FSCE_7","FSCE_8","FSCE_9","FSCE_10","FSCE_14","FSCN_1",
            "FSCN_2","FSCN_5","FSCN_6","FSCN_7","FSCN_9","FSCN_11","FSCN_13","FSCCN_1","FSCCN_5","FSCCN_6","FSCCN_7",
            "FSCCN_21","FIEI_1","FIEI_4")
for(i in factor){
  train[,i] <- as.factor(train[,i])
}

train_d <- dplyr::select(train,-id_viv,-id_hogar,-id_per)
dummy <- dummyVars(~.-dcronica, data = train_d)
#Predecimos sobre el conjunto de datos
train_d_t <- as.data.frame(predict(dummy,train_d))
#Añadimos la variable dependiente
train_d_t$dcronica <- train_d$dcronica
nearZeroVar(train_d_t, saveMetrics = TRUE)
ntrain <- train_d_t %>%
  dplyr::select(-"FBCP1_6.afroecuatoriano/a         afrodescendiente?",-"FBCP1_6.blanco/a?",-"FBCP1_6.montuvio/a?",
                -"FBCP1_6.mulato/a?",-"FBCP1_6.negro/a?",-"FBCP1_6.otra, cuál? (especifique)",-"FBCP1_7.divorciado",-"FBCP1_7.unión de hecho",
                -"FBCP1_7.viudo",-"FBCH_1.otro, cuál?",-"FBCH_1.rio/mar",-"FBCH_2.choza",-"FBCH_2.covacha",
                -"FBCH_2.cuarto/s en casa de inquilinato",-"FBCH_2.otra, cuál?",-"FBCH_3.otro, cuál?",
                -"FBCH_3.palma/paja/hoja?",-"FBCH_4.adobe/tapia?",-"FBCH_4.bahareque (caña, carrizo revestido)?",
                -"FBCH_4.caña o estera?",
                -"FBCH_4.otra, cuál ?",-"FBCH_5.caña?",-"FBCH_5.mármol/marmeton?",-"FBCH_5.otro, cuál?",
                -"FBCH_5.tierra?",-"FBCH_6.carro repartidor/triciclo?",-"FBCH_6.otro, cuál?",-"FBCH_6.pila o llave pública?",
                -"FBCH_7.por tubería fuera del edificio, lote o terreno?",-"FBCH_8.letrina?",-"FBCH_9.empresa eléctrica pública?",
                -"FBCH_9.ninguno?",-"FBCH_9.planta eléctrica privada?",-"FBCH_9.vela, candil, mechero, gas?",
                -"FBCH_10.botan a la calle/ quebrada/ río?",-"FBCH_10.contratan el servicio?",-"FBCH_10.la entierran?",-"FBCH_10.otra, cuál ?",
                -"FBCH_13.no sabe",-"FBCH_13.otro tratamiento?",-"FBCH_14.electricidad? (inducción)",-"FBCH_14.no cocina",
                -"FBCH_18.anticresis y arriendo?",-"FBCH_18.otra, cuál?",-"FBCH_18.recibida por servicios?",-"FBCP1_8.Ninguno o Centro de Alfabetización",
                -"FIPA_13.no sabe / no responde",-"FIPA_15.no sabe / no responde",-"FIPA_17.no sabe / no responde",
                -"FIPA_20.no sabe / no responde",-"FIPA_22.no sabe / no responde",-"FIPA_26.no sabe / no responde",
                -"FIPA_30.no",-"FIPA_30.no sabe / no responde",-"FIPA_30.si",
                -"FIPA_34.no sabe / no responde",-"FIPA_35.no sabe / no responde",-"FBCP1_3.no quería tener hijos?",-"FSCE_1.no",
                -"FSCE_1.si",-"FSCE_2.consejo provincial/unidad municipal de salud",-"FSCE_2.fundación/ ong",
                -"FSCE_2.hospital ff.aa/policía",-"FSCE_2.junta de beneficencia",
                -"FSCE_2.otro, cuál?",-"FSCE_2.partera",-"FSCE_2.seguro social campesino",-"FSCE_5.no sabe / no responde",
                -"FSCE_6.consejo provincial/unidad municipalde salud",-"FSCE_6.fundación/ ong",-"FSCE_6.hospital ff.aa/ policía",
                -"FSCE_6.junta de beneficencia",-"FSCE_6.otro, cuál?",-"FSCE_6.seguro social campesino",
                -"FSCE_7.aux. enfermería",-"FSCE_7.comadrona o partera",-"FSCE_7.enfermera",-"FSCE_7.familiar",
                -"FSCE_7.otro, cuál?",-"FSCE_7.usted misma",-"FSCE_9.no sabe",-"FSCE_9.posmaduro",-"FSCE_14.fundación/ ong",
                -"FSCE_14.hospital ff.aa/policía",-"FSCE_14.hospital/clínica/dispensario del iess",-"FSCE_14.junta de beneficencia",
                -"FSCE_14.no recuerda",-"FSCE_14.otro, cuál?",-"FSCE_14.partera",-"FSCE_14.seguro social campesino",-"FSCN_1.no",-"FSCN_1.si",
                -"FSCN_5.muy pequeño",-"FSCN_5.no sabe",-"FSCCN_1.no",-"FSCCN_1.si",-"FSCCN_5.no recuerda",-"FSCCN_6.consejo provincial/unidad municipal de salud",
                -"FSCCN_6.fundación/ ong",-"FSCCN_6.hospital ff.aa/policía",-"FSCCN_6.junta de beneficencia",-"FSCCN_6.no recuerda",
                -"FSCCN_6.otro, cuál?",-"FSCCN_6.seguro social campesino",-"FSCCN_7.no",-"FSCCN_7.si",-"FSCCN_21.36-42",
                -"FSCCN_21.43-47",-"FSCCN_21.48-59",-"FIEI_1.no sabe / no responde")

#Creamos el modelo
model_dcronica <- rpart(dcronica ~ .,
                        data = ntrain,
                        method = "class",
                        control = rpart.control(cp = 0.001))
printcp(model_dcronica)
model_dcronica$cptable[5,]
model_dcronica_opcp <- prune(model_dcronica, cp = 0.0046468) 

#Guardamos el modelo. 
saveRDS(model_dcronica_opcp, "model_tree.rds")
rm(list = ls())
#Cargamos modelo.
model_tree <- readRDS("model_tree.rds")
