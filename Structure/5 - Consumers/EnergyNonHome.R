require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnergyNonHomeOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Electricity",
        fluidRow(column(8,
                    h3("Proportion of domestic electricity customers on non-home supplier", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('ElecNonHomeSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecNonHome.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("ElecNonHomePlot")),
    imageOutput(ns("ElecNonHomePlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas",
             fluidRow(column(8,
                             h3("Proportion of domestic gas customers on non-home supplier", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('GasNonHomeSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasNonHome.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("GasNonHomePlot")),
             imageOutput(ns("GasNonHomePlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      tabPanel("Elec - Payment Methods",
    fluidRow(
    column(10, h3("Data - Proportion of domestic electricity customers on non-home supplier by payment method", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ElecNonHomeTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Elec - Time Series",
             fluidRow(
               column(10, h3("Data - Proportion of electricity customers not with their home supplier", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ElecNonHomeTimeSeriesTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas - Payment Methods",
             fluidRow(
               column(10, h3("Data - Proportion of domestic gas customers on non-home supplier by payment method", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasNonHomeTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas - Time Series",
             fluidRow(
               column(10, h3("Data - Proportion of gas customers not with their home supplier", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasNonHomeTimeSeriesTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
      column(2, p("Next update:")),
      column(2,
             DateLookup(c("BEISQuarterlyElecCustomers", "BEISQuarterlyGasCustomers"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISQuarterlyElecCustomers"),
        SourceLookup("BEISQuarterlyGasCustomers")
        
      )
    )
  )
}




###### Server ######
EnergyNonHome <- function(input, output, session) {
  # output$EnergyNonHomePlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/2 - Renewables/Electricity/EnergyNonHome.csv",
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
  
  print("EnergyNonHome.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$ElecNonHomeSubtitle <- renderText({
    
    ElecNonHome <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier elec",
      skip = 21
    )
    
    names(ElecNonHome)[1] <- "Quarter"#
    
    ElecNonHome <- ElecNonHome[complete.cases(ElecNonHome),]
    
    ElecNonHome$Quarter <- as.Date(as.numeric(ElecNonHome$Quarter), origin = "1899-12-30")
    
    ElecNonHome$Quarter <- as.character(as.yearqtr(ElecNonHome$Quarter))
    
    paste("Scotland,", min(ElecNonHome$Quarter),"-", max(ElecNonHome$Quarter))
  })
  
  output$ElecNonHomePlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/5 - Consumers/ElecNonHomeOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$ElecNonHomeTable = renderDataTable({
    
    ElecNonHome <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier elec",
      skip = 14,
      n_max = 4
    )
    
    ElecNonHome <- ElecNonHome[c(1,3,5,7,9)]

    names(ElecNonHome) <- c("Region",
                                  "Credit",
                                  "Direct Debit",
                                  "Prepayment",
                                  "All")
    
    datatable(
      ElecNonHome,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Proportion of domestic electricity customers on non-home supplier by payment method",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of domestic electricity customers on non-home supplier by payment method',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of domestic electricity customers on non-home supplier by payment method')
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
  
  output$ElecNonHomeTimeSeriesTable = renderDataTable({
    
    ElecNonHome <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier elec",
      skip = 21
    )
    
    names(ElecNonHome)[1] <- "Quarter"#
    
    ElecNonHome <- ElecNonHome[complete.cases(ElecNonHome),]
    
    ElecNonHome$Quarter <- as.Date(as.numeric(ElecNonHome$Quarter), origin = "1899-12-30")
    
    ElecNonHome$Quarter <- as.character(as.yearqtr(ElecNonHome$Quarter))
    
    
    datatable(
      ElecNonHome,
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
        title = "Proportion of domestic electricity customers on non-home supplier - Time Series",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of domestic electricity customers on non-home supplier -  Time Series',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of domestic electricity customers on non-home supplier -  Time Series')
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
                     paste(readtext("Structure/5 - Consumers/EnergyNonHome.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("ElecNonHomeTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ElecNonHomeTimeSeriesTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ElecNonHome.png <- downloadHandler(
    filename = "ElecNonHome.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/ElecNonHomeChart.png"), file) 
    }
  )
  
  
  output$GasNonHomeSubtitle <- renderText({
    
    GasNonHome <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier gas",
      skip = 21
    )
    
    names(GasNonHome)[1] <- "Quarter"#
    
    GasNonHome <- GasNonHome[complete.cases(GasNonHome),]
    
    GasNonHome$Quarter <- as.character(as.yearqtr(GasNonHome$Quarter))
    
    paste("Scotland,", min(GasNonHome$Quarter),"-", max(GasNonHome$Quarter))
  })
  
  output$GasNonHomePlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    writePNG(readPNG("Structure/5 - Consumers/GasNonHomeOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$GasNonHome.png <- downloadHandler(
    filename = "GasNonHome.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/GasNonHomeChart.png"), file) 
    }
  )
  
  output$GasNonHomeTable = renderDataTable({
    
    GasNonHome <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier gas",
      skip = 14,
      n_max = 4
    )
    GasNonHome <- GasNonHome[c(1,3,5,7,9)]
    
    names(GasNonHome) <- c("Region",
                           "Credit",
                           "Direct Debit",
                           "Prepayment",
                           "All")
    
    datatable(
      GasNonHome,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Proportion of domestic gas customers on non-home supplier by payment method",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of domestic gas customers on non-home supplier by payment method',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of domestic gas customers on non-home supplier by payment method')
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
  
  output$GasNonHomeTimeSeriesTable = renderDataTable({
    
    GasNonHome <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier gas",
      skip = 21
    )
    
    names(GasNonHome)[1] <- "Quarter"#
    
    GasNonHome <- GasNonHome[complete.cases(GasNonHome),]
    
    GasNonHome$Quarter <- as.character(as.yearqtr(GasNonHome$Quarter))
    
    
    datatable(
      GasNonHome,
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
        title = "Proportion of domestic gas customers on non-home supplier - Time series",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of domestic gas customers on non-home supplier - Time series',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of domestic gas customers on non-home supplier - Time series')
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
  
  observeEvent(input$ToggleTable3, {
    toggle("GasNonHomeTable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("GasNonHomeTimeSeriesTable")
  })
}
