server <- function(input, output,session){
  
  is.nonum <- function(x){
    !is.numeric(x)
  }
  
  datapath <- paste(getwd(), "data", "base_entrenamiento.csv", sep="/")
  
  datareact <- reactive({
    input$go
    isolate(
      read.table(datapath, fileEncoding="Latin1", sep=",", header = T )
    )
  })
  
  #Datos
  output$tabla <- renderDataTable({
    

    datareact()
    
    
  })
  
trainData <- reactive({
  ind <- sample(2,nrow(datareact()), replace = T, prob = c(0.7,0.3))
  datareact()[ind==1,] #para entrenamiento
})

testData <- reactive({
  ind <- sample(2,nrow(datareact()), replace = T, prob = c(0.7,0.3))
  datareact()[ind==2,] #para test
})

   #resultados arbol de decision
  output$res <- renderPrint({
    
 input$go
    
    observe({
      updateSelectInput(session, "y",
                        choices = names(select_if(datareact(), is.nonum)))
    })
    

    
    #creamos el arbol de decision
    
   
    isolate(rpart(get(input$y) ~ ., method = "class", data = trainData()))

   
  


  })
# grafico del arbol de decision
  output$graph <- renderPlot({
    (rpart.plot(rpart(get(input$y) ~ ., method = "class", data = trainData()), extra = 4, digits = 2)) #extra = 4 probabilidad de observaciones por clase
  })

#Predicciones
  output$pred <- renderPrint({
    testPredRpart <- predict(rpart(get(input$y) ~ ., method = "class", data = trainData()),newdata = testData(), type = "class")
    testPredRpart
    #visualizemos la matriz de confusion
    #table(testPredRpart, testData()$get(input$y))
    confusionMatrix(testPredRpart,reference = da)
   # #calculamos el % de aciertos 
   # sum(testPredRpart==testData()$y)/length(testData()$y)*100
   
  })

  
}





