require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenElecLAOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Share of renewable energy in gross final consumption", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenElecLASubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecLA.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenElecLAPlot")),
    imageOutput(ns("RenElecLAPlot"), height = "700px")%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RenElecLATable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
RenElecLA <- function(input, output, session) {
  # output$RenElecLAPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/2 - Renewables/Electricity/RenElecLA.csv",
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
  
  print("RenElecLA.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RenElecLASubtitle <- renderText({
    
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
  
  output$RenElecLAPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/2 - Renewables/Electricity/RenElecLAOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$RenElecLATable = renderDataTable({
    
    RenElecLA <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump by LA",
      skip = 13,
      n_max = 33
    )

    names(RenElecLA)[1:2] <- c("Geography Code", "Local Authority")
    datatable(
      RenElecLA[c(2,1,3:6)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'asc')),
        title = "Total final energy consumption by consuming sector (GWh), by local authority in Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Total final energy consumption by consuming sector (GWh), by local authority in Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Total final energy consumption by consuming sector (GWh), by local authority in Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(3:6, 0)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenElecLA.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("RenElecLATable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenElecLA.png <- downloadHandler(
    filename = "RenElecLA.png",
    content = function(file) {
      writePNG(readPNG("Structure/2 - Renewables/Electricity/RenElecLAChart.png"), file) 
    }
  )
}
