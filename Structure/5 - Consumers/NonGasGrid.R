require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

NonGasGridOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Proportion of households not on the gas grid by local authority (estimates)", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('NonGasGridSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('NonGasGrid.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("NonGasGridPlot")),
    imageOutput(ns("NonGasGridPlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("NonGasGridTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
      column(1,
             p("Next update:")),
      column(2,
             p("March 2019")),
      column(1, align = "right",
             p("Sources:")),
      column(
        8,
        align = "right",
        SourceLookup("BEISFinalConsump"),
        SourceLookup("ETElecGen"),
        SourceLookup("ESTRenHeat")
        
      )
    )
  )
}




###### Server ######
NonGasGrid <- function(input, output, session) {
  # output$NonGasGridPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/2 - Renewables/Electricity/NonGasGrid.csv",
  #       header = TRUE,
  #       sep = ",",
  #       na.strings = "-"
  #     )
  #
  #   YearLow <- as.numeric(min(RenEn$Year))
  #   YearHigh <- as.numeric(max(RenEn$Year +1))
  #
  #   dygraph(RenEn, main = "Renewable Energy Target") %>%
  #     dyAxis("y", label = "% Progress", valueRange = c(0,30)) %>%
  #     dyAxis("x", label = "Year", drawGrid = TRUE) %>%
  #     dyOptions(colors =  c("Green","Orange", "Blue")) %>%
  #     dyLegend(width = 170 ,
  #              labelsSeparateLines = TRUE ,
  #              show = "always") %>%
  #     dyOptions(
  #       stackedGraph = TRUE,
  #       axisLineColor = "white",
  #       gridLineColor = "white",
  #       includeZero = TRUE,
  #       fillAlpha = .65
  #     ) %>%
  #     #    dyRangeSelector() %>%
  #     dyCSS("Structure/2 - Renewables/Electricity/legend.css")
  #
  # })
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("NonGasGrid.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$NonGasGridSubtitle <- renderText({
    
    paste("Scotland, 2017")
  
    })
  
  output$NonGasGridPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/5 - Consumers/NonGasGridOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$NonGasGridTable = renderDataTable({
    
    NonGasGrid <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-gas grid by LA",
      skip = 13
    )[2:5]

    datatable(
      NonGasGrid,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Proportion of households not on the gas grid by local authority (estimates)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of households not on the gas grid by local authority (estimates)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of households not on the gas grid by local authority (estimates)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(4, 1) %>% 
      formatRound(2:3, 0)
  })
  
  output$NonGasGridTimeSeriesTable = renderDataTable({
    
    NonGasGrid <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier elec",
      skip = 13
    )
    
    names(NonGasGrid)[1] <- "Quarter"#
    
    NonGasGrid <- NonGasGrid[complete.cases(NonGasGrid),]
    
    NonGasGrid$Quarter <- as.Date(as.numeric(NonGasGrid$Quarter), origin = "1899-12-30")
    
    NonGasGrid$Quarter <- as.character(as.yearqtr(NonGasGrid$Quarter))
    
    
    datatable(
      NonGasGrid,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:9, 1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/NonGasGrid.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("NonGasGridTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("NonGasGridTimeSeriesTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$NonGasGrid.png <- downloadHandler(
    filename = "NonGasGrid.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/NonGasGridChart.png"), file) 
    }
  )
}
