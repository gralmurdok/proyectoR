library(shiny)
library(ggplot2)
library(DT)
library(tidyverse)
library(MASS)
library(dummies)
library(VIM)
library(DMwR)
library(rpart)
library(rpart.plot)
library(carData)
library(caret)
library(funModeling)
library(ROSE)
library(ROCR)
library(pROC)

ui <- fluidPage(
  theme = "formato.css",
  titlePanel(title = "Aplicación árboles de decisión",windowTitle = "Tree App"),
  sidebarLayout(
    sidebarPanel(width = 3, 
       wellPanel(
         radioButtons(inputId = "select", 
                      label = "Seleccione la base de datos con la que desea trabajar", 
                      choices = "Datos Externos",
                      selected = " ")),
       
    
       conditionalPanel(condition = "input.select =='Datos Externos'",
                        wellPanel(
                                  
                                  fileInput(inputId = "file",
                                            label = "Ingrese el archivo de su preferencia"),
                                  radioButtons(inputId = 'sep',
                                               label = 'Separador', 
                                               choices = c(Coma=',',Punto_y_coma=';',Tab='\t', Espacio=''), 
                                               selected = ','))),
       
              actionButton(inputId = "go", 
                    label = "Procesar",
                    icon =icon("play-circle")) 
       
    ),
    mainPanel( width = 9,
          
      tabsetPanel(
        tabPanel("Datos", dataTableOutput(outputId = "tabla")),
        tabPanel("Resultados", verbatimTextOutput(outputId = "res")),
        tabPanel("Grafica", plotOutput(outputId = "graph")),
        tabPanel("Predicciones", verbatimTextOutput(outputId = "pred"))
                
),
        
      )
    )
  )
  
