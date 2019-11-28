require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ElecNonHomeOutput <- function(id) {
  ns <- NS(id)
  tagList(
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
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      tabPanel("Payment Methods",
    fluidRow(
    column(10, h3("Data - Payment Methods", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ElecNonHomeTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Time Series",
             fluidRow(
               column(10, h3("Data - Time Series", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ElecNonHomeTimeSeriesTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
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
ElecNonHome <- function(input, output, session) {
  # output$ElecNonHomePlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/2 - Renewables/Electricity/ElecNonHome.csv",
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
  
  print("ElecNonHome.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$ElecNonHomeSubtitle <- renderText({
    
    RenEn <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Renewable energy target",
      col_names = FALSE,
      skip = 21,
      n_max = 23
    )
    RenEn <- as.data.frame(t(RenEn))
    RenEn <- RenEn[, c(1, 6, 12, 18, 23)]
    RenEn <- tail(RenEn,-5)
    names(RenEn) <-
      c("Year", "Electricity", "Heat", "Transport", "Renewables")
    RenEn[, c(1, 2, 3, 4, 5)] %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenEn[which(RenEn$Year != max(RenEn$Year)),][2:4] <- 0
    
    paste("Scotland,", min(RenEn$Year),"-", max(RenEn$Year))
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

    names(ElecNonHome) <- c("Region",
                                 "Credit - Home Supplier", "Credit - Non-Home Supplier",
                                 "Direct Debit - Home Supplier", "Direct Debit - Non-Home Supplier",
                                 "Prepayment - Home Supplier", "Prepayment - Non-Home Supplier",
                                 "All - Home Supplier", "All - Non-Home Supplier")
    
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
        title = "Proportion of domestic electricity customers on non-home supplier",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of domestic electricity customers on non-home supplier',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of domestic electricity customers on non-home supplier')
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
        title = "Proportion of domestic electricity customers on non-home supplier",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of domestic electricity customers on non-home supplier',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of domestic electricity customers on non-home supplier')
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
                     paste(readtext("Structure/5 - Consumers/ElecNonHome.txt")[2])
                     
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
      writePNG(readPNG("Structure/4 - Energy Efficiency/ElecNonHomeChart.png"), file) 
    }
  )
}
