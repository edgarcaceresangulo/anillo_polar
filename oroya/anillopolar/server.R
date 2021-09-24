server <- shinyServer(function(input, output, server){
  
  autoInvalidate <- reactiveTimer(1000*60*60) # El periodo de actualización automática es de 1 hora
  
  parametros <- reactive({
    
    sw1 <- as.numeric(input$parametro) # Cambio de variable [sw1 = 1 = SO2]
    
    sw2 <- switch(as.numeric(input$periodo), "hour", "weekday", "season", "trend")
    
    date <- input$dateRange_SO2 # Rango de fechas
    
    date <- paste(day(date), month(date), year(date), sep = "-")
    f1 <- date[1] # Fecha inicial 
    f2 <- date[2] # Fecha final
    
    h1 <- input$hourRange_SO2[1]
    h2 <- input$hourRange_SO2[2]

    list(sw1 = sw1, sw2 = sw2, f1 = f1, f2 = f2, h1 = h1, h2 = h2)

  }) # Cierra parametros
  
  datos <- reactive({
    
    f1 <- parametros()$f1;  f2 <- parametros()$f2; sw2 <- parametros()$sw2;
    h1 <- parametros()$h1; h2 <- parametros()$h2
    
    autoInvalidate()
    #source("cargadatos.R")
    bd <- rangofecha(bd.1h, f1, f2, h1:h2); nom.bd <- c("bd.1h"); corte <- corte; cota <- cota
    
    list(bd = bd, nom.bd = nom.bd, corte = corte, cota = cota)  
    
  }) # Cierra datos
  
  dyplot <- function(){
    
    bd <- datos()$bd; sw2 <- parametros()$sw2
    source("SO2/anillopolar.R")
    anillopolar(bd, sw2, titulo)
    
  } # Cierra dyplot
  
  output$plot <- renderPlot({
    
    dyplot()
    
  }) # Cierra plot
  
  output$table <- DT::renderDataTable({
    
    bd <- datos()$bd; corte <- datos()$corte
    source("tabladedatos.R")
    tabladedatos(bd, corte)
    
  }) # Cierra datatable
  
  output$mensaje <- renderUI({
    
    opc2 <- as.numeric(input$periodo)
    
    switch(opc2, 
           { shiny::includeMarkdown("textomarkdown01.md") },
           { shiny::includeMarkdown("textomarkdown02.md") },
           { shiny::includeMarkdown("textomarkdown03.md") },
           { shiny::includeMarkdown("textomarkdown04.md") }) # Cierra switch
    
  }) # Cierra renderUI
  
  output$exportPlot <- downloadHandler(
    
    filename = function(file){
      paste(datos()$nom.bd, 
        "_SO2_", 
        Sys.Date(),
        ".png", 
        sep = "")
    },
    content = function(file){
      png(file)
      dyplot()
      dev.off()
    },
    contentType = "image/png"

  ) # Cierra exportPlot
  
  output$downloadData <- downloadHandler(
    
    filename = function(file){
      paste(datos()$nom.bd, 
        "_SO2_", 
        Sys.Date(), 
        ".xlsx", 
        sep = "")
    },
    content = function(file){
      write.xlsx2(x = as.data.frame(datos()$bd), file = file, sheetName = "datos", row.names = FALSE)
    }
    
  ) # Cierra downloadData
  
}) # Cierra shinyServer