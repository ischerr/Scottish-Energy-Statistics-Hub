require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

SmartMetersOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Smart meter installations", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('SmartMetersSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SmartMeters.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("SmartMetersPlot")),
    imageOutput(ns("SmartMetersPlot"), height = "500px")%>% withSpinner(color="#68c3ea"),
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
      column(12, dataTableOutput(ns("SmartMetersTable"))%>% withSpinner(color="#68c3ea"))),
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
SmartMeters <- function(input, output, session) {
  # output$SmartMetersPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/2 - Renewables/Electricity/SmartMeters.csv",
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
  
  print("SmartMeters.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$SmartMetersSubtitle <- renderText({
    
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
  
  output$SmartMetersPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/5 - Consumers/SmartMetersOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$SmartMetersTable = renderDataTable({
    
    SmartMeters <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Smart meter installations",
      skip = 14
    )
    
    names(SmartMeters) <- c("Date", "North Scotland - Installations (Cumulative)", "North Scotland - Proportion of smart meters", "South Scotland - Installations (Cumulative)", "South Scotland - Proportion of smart meters")

    SmartMeters$Date <- format(SmartMeters$Date, "%b %Y")
    
    datatable(
      SmartMeters[nrow(SmartMeters):1,],
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
      formatPercentage(c(3,5), 1) 
  })
  
  output$SmartMetersTimeSeriesTable = renderDataTable({
    
    SmartMeters <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier elec",
      skip = 13
    )
    
    names(SmartMeters)[1] <- "Quarter"#
    
    SmartMeters <- SmartMeters[complete.cases(SmartMeters),]
    
    SmartMeters$Quarter <- as.Date(as.numeric(SmartMeters$Quarter), origin = "1899-12-30")
    
    SmartMeters$Quarter <- as.character(as.yearqtr(SmartMeters$Quarter))
    
    
    datatable(
      SmartMeters,
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
                     paste(readtext("Structure/5 - Consumers/SmartMeters.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("SmartMetersTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("SmartMetersTimeSeriesTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$SmartMeters.png <- downloadHandler(
    filename = "SmartMeters.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/SmartMetersChart.png"), file) 
    }
  )
}
