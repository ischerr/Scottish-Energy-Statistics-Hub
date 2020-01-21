require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnSupplySwitchOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Proportion of customers who have switched energy supplier by local authority", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('EnSupplySwitchSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnSupplySwitch.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("EnSupplySwitchPlot")),
    imageOutput(ns("EnSupplySwitchPlot"), height = "500px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data - Proportion of customers who have switched energy supplier by local authority 2018", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnSupplySwitchTable"))%>% withSpinner(color="#68c3ea"))),
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
EnSupplySwitch <- function(input, output, session) {
  # output$EnSupplySwitchPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/2 - Renewables/Electricity/EnSupplySwitch.csv",
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
  
  print("EnSupplySwitch.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$EnSupplySwitchSubtitle <- renderText({
    
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
    
    paste("Scotland, 2018")
  })
  
  output$EnSupplySwitchPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/5 - Consumers/EnSupplySwitchOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$EnSupplySwitchTable = renderDataTable({
    
    EnSupplySwitch <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Switching by LA",
      skip = 13
    )[2:4]

    datatable(
      EnSupplySwitch,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Proportion of customers who have switched energy supplier by local authority, 2018",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of customers who have switched energy supplier by local authority, 2018',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of customers who have switched energy supplier by local authority, 2018')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(3), 1) 
  })
  
  output$EnSupplySwitchTimeSeriesTable = renderDataTable({
    
    EnSupplySwitch <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier elec",
      skip = 13
    )
    
    names(EnSupplySwitch)[1] <- "Quarter"#
    
    EnSupplySwitch <- EnSupplySwitch[complete.cases(EnSupplySwitch),]
    
    EnSupplySwitch$Quarter <- as.Date(as.numeric(EnSupplySwitch$Quarter), origin = "1899-12-30")
    
    EnSupplySwitch$Quarter <- as.character(as.yearqtr(EnSupplySwitch$Quarter))
    
    
    datatable(
      EnSupplySwitch,
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
                     paste(readtext("Structure/5 - Consumers/EnSupplySwitch.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("EnSupplySwitchTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EnSupplySwitchTimeSeriesTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnSupplySwitch.png <- downloadHandler(
    filename = "EnSupplySwitch.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/EnSupplySwitchChart.png"), file) 
    }
  )
}
