server <- function(input, output,session){
  
  set.seed(123456789)
  
  datapath <- paste(getwd(), "data", "base_entrenamiento.csv", sep="/")
  
  # aqui se esta cargando la base de datos de entrenamiento
  
  datareact <- reactive({
    input$go
    isolate(
      read.table(datapath, fileEncoding="latin1", sep=",", header = T )
    )
  })
  
  # datos externos
  
  datareactexternal <- reactive({
    input$go
    isolate(
      if(input$select == "Datos Externos"){
        dir <- input$file
        read.table(dir$datapath, fileEncoding="latin1", sep=input$sep, header = T )
      } else{
        get(input$data)
      })
  })
  
  
  #Datos
  output$tabla <- renderDataTable({

    datareact()
    
  })

  #resultados arbol de decision
  
  output$res <- renderPrint({
    
    input$go
        #Creamos el modelo
    isolate(
     readRDS("model_tree.rds")
    )
  })
  
  
  

# grafico del arbol de decision
  output$graph <- renderPlot({
    rpart.plot(readRDS("model_tree.rds"),extra = 1, type = 2, digits = 2)
      })

#Predicciones
  output$pred <- renderPrint({
      
    predict_model <- predict(readRDS("model_tree.rds"),newdata = datareactexternal(), type = "class")
    confusionMatrix(predict_model, datareactexternal$dcronica, positive = "1")
    
  })

  
}





