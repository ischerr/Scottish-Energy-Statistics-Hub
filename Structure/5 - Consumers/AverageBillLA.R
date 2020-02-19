require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

AverageBillLAOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Average annual energy bill prices", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('AverageBillLASubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('AverageBillLA.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("AverageBillLAPlot")),
    imageOutput(ns("AverageBillLAPlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
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
      column(12, dataTableOutput(ns("AverageBillLATable"))%>% withSpinner(color="#68c3ea"))),
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
        SourceLookup("BEISUnitElec"),
        SourceLookup("BEISSubNatElec"),
        SourceLookup("BEISUnitGas"),
        SourceLookup("BEISSubNatGas")

        
      )
    )
  )
}




###### Server ######
AverageBillLA <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("AverageBillLA.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$AverageBillLASubtitle <- renderText({
    
    paste("Scotland, 2017")
  
    })
  
  output$AverageBillLAPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/5 - Consumers/AverageBillLAOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$AverageBillLATable = renderDataTable({
    
    AverageBillLA <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Average Bill",
      skip = 22
    )[c(2,1,4,5,6)]

    names(AverageBillLA) <- c("Local Authority", "Code", "Average Electricity Bill", "Average Gas Bill", "Average Total Bill")
    
    datatable(
      AverageBillLA,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Average annual energy bill prices (\u00A3)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average annual energy bill prices (\u00A3)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average annual energy bill prices (\u00A3)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2, 1) %>% 
      formatCurrency(3:5, currency = "\u00A3")
  })
  
  output$AverageBillLATimeSeriesTable = renderDataTable({
    
    AverageBillLA <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier elec",
      skip = 13
    )
    
    names(AverageBillLA)[1] <- "Quarter"#
    
    AverageBillLA <- AverageBillLA[complete.cases(AverageBillLA),]
    
    AverageBillLA$Quarter <- as.Date(as.numeric(AverageBillLA$Quarter), origin = "1899-12-30")
    
    AverageBillLA$Quarter <- as.character(as.yearqtr(AverageBillLA$Quarter))
    
    
    datatable(
      AverageBillLA,
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
                     paste(readtext("Structure/5 - Consumers/AverageBillLA.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("AverageBillLATable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("AverageBillLATimeSeriesTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$AverageBillLA.png <- downloadHandler(
    filename = "AverageBillLA.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/AverageBillLAChart.png"), file) 
    }
  )
}
