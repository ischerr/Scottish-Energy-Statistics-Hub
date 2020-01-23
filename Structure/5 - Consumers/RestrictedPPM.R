require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RestrictedPPMOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
    tabPanel("Restricted",
    fluidRow(column(8,
                    h3("Restricted meters by local authority", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('RestrictedMeterSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RestrictedMeter.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("RestrictedMeterPlot")),
    imageOutput(ns("RestrictedMeterPlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Prepayment",
             fluidRow(column(8,
                             h3("Prepayment meters by local authority", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('PPMMeterSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('PPMMeter.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("PPMMeterPlot")),
             imageOutput(ns("PPMMeterPlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      tabPanel("Restricted",
    fluidRow(
    column(10, h3("Data - Restricted meters by Local Authority", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RestrictedMeterTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Prepayment",
             fluidRow(
               column(10, h3("Data - Prepayment meters by Local Authority", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("PrepaymentMeterTable"))%>% withSpinner(color="#68c3ea"))),
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
RestrictedPPM <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RestrictedPPM.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RestrictedMeterSubtitle <- renderText({
    
    paste("Scotland, 2017")
  
    })
  
  output$RestrictedMeterPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/5 - Consumers/RestrictedMeterOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$RestrictedMeterTable = renderDataTable({
    
    RestrictedMeter <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Restricted and PPM",
      skip = 15
    )[c(1,2,4,5)]
    
    names(RestrictedMeter) <- c("LA Code", "Local Authority", "Restricted meters (Economy 7/10)", "Proportion of total meters")

    datatable(
      RestrictedMeter[c(2,1,3,4)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Restricted meters by Local Authority",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Restricted meters by Local Authority',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Restricted meters by Local Authority')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(4, 1) %>% 
      formatRound(3, 0)
  })
  
  output$PrepaymentMeterTable = renderDataTable({
    
    RestrictedMeter <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Restricted and PPM",
      skip = 15
    )[c(1,2,6,7)]
    
    names(RestrictedMeter) <- c("LA Code", "Local Authority", "Pre-payment Meters", "Proportion of total meters")
    
    datatable(
      RestrictedMeter[c(2,1,3,4)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Restricted meters by Local Authority",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Restricted meters by Local Authority',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Restricted meters by Local Authority')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(4, 1) %>% 
      formatRound(3, 0)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/RestrictedPPM.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("RestrictedMeterTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("PrepaymentMeterTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RestrictedMeter.png <- downloadHandler(
    filename = "RestrictedMeter.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/RestrictedMeterChart.png"), file) 
    }
  )
  
  output$PPMMeterSubtitle <- renderText({
    
    paste("Scotland, 2017")
    
  })
  
  output$PPMMeterPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    writePNG(readPNG("Structure/5 - Consumers/PPMMeterOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$PPMMeter.png <- downloadHandler(
    filename = "PPMMeter.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/PPMMeterChart.png"), file) 
    }
  )
}
