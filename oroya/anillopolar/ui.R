ui <- shinyUI(function(req){ fluidPage(
  
  includeCSS("www/styles.css"),
  
  headerPanel("Relación del viento y parámetro por periodo"),
  
  fluidRow(
    
    column(4,
      
      wellPanel(
        
        selectInput(inputId = "parametro", label = "Parámetro:", 
          choices = list("SO2" = 1), 
          selected = 1, width = "50%"),
        
        selectInput(inputId = "periodo", label = "Periodo:",
                    choices = list("Horario" = 1, "Día de la semana" = 2, "Mensual" = 3, "Tendencia" = 4), 
                    selected = 1, width = "50%"),
        
        conditionalPanel(condition = "input.parametro == 1",
            dateRangeInput(inputId = "dateRange_SO2",
              label = "Rango de fechas:",
              start = cota-365, end = cota,
              min = "2018-01-01", max = Sys.Date(),
              format = "dd/mm/yyyy",
              language ="es",
              separator = "-")),
      
      sliderInput("hourRange_SO2",
              label = "Rango de horas: 0-23",
              min = 0, max = 23,
              value = c(0, 23)),

        tags$h5(strong("Descargar:")),
        downloadButton("downloadData", "Datos (.xlsx)"),
        downloadButton("exportPlot", "Imagen (.png)")
        
      ) # Cierra wellPanel      
      
    ), # Cierra column(3,
    
    column(8,
      
      tabsetPanel(type = "tabs", 
        
        tabPanel("Gráfica", br(), 
          
          shiny::plotOutput("plot", height = "400px", width = "100%"),  br(), uiOutput("mensaje")),
        
        tabPanel("Tabla de datos", br(), DT::dataTableOutput("table"))
        
      ) # Cierra tabsetpanel
      
    ) # Cierra column(9,
    
  ) # Cierra fluidRow
  
)}) # Cierra fluidPage y shinyUI