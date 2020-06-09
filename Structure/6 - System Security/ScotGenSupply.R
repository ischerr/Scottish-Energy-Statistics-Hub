require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ScotGenSupplyOutput <- function(id) {
  ns <- NS(id)
  tagList(
      fluidRow(column(8,
                      h3("Scottish electricity generation and supply", style = "color: #5d8be1;  font-weight:bold"),
                      selectInput(ns("YearSelect"), "Year:", rev(unique(GenSupplyReadable$Year)), selected = max(unique(GenSupplyReadable$Year)), multiple = FALSE,
                                  selectize = TRUE, width = NULL, size = NULL)
      ),
      column(
        4, style = 'padding:15px;',
        downloadButton(ns('ScotGenSupply.png'), 'Download Graph', style="float:right")
      )),
      
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
               #dygraphOutput(ns("ScotGenSupplyPlot")),
              visNetworkOutput(ns("ScotGenSupplyPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
               p("* TTL = Transfers, Transformation and Losses"),
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
      column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
      column(10, h3("Data - Supply (ktoe)", style = "color: #5d8be1;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Tables", style = "float:right; "))
    ),
    fluidRow(
      column(12, DTOutput(ns("ScotGenSupplyTable1"))%>% withSpinner(color="#5d8be1"))),
        tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             p("March 2019")),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISElecGen"),
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("HMRCTrade"),
        SourceLookup("BEISDUKESBalance"),
        SourceLookup("SGCommodityBalance"),
        SourceLookup("BEISImportExport")
        
      )
    )
  )
}




###### Server ######
ScotGenSupply <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ScotGenSupply.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$ScotGenSupplySubtitle <- renderText({
    
    paste("Scotland, 2018")
  })
  
  GenSupplyReadable <- read_delim("Processed Data/Output/Renewable Generation/GenSupplyReadable.txt", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
  
  
  RenSupplyGen <- read_excel("Structure/6 - System Security/RenSupplyGen.xlsx")


  output$ScotGenSupplyPlot <- renderVisNetwork({
    

  

  Country <- "Scotland"
  
  Year <- as.numeric(input$YearSelect)
  
  
  GenSupplyReadableProcessed <- GenSupplyReadable[which(GenSupplyReadable$Country == Country),]
  
  GenSupplyReadableProcessed <- GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$Year == Year),]
  
  RenSupplyGen[c(3,6)]
  
  RenSupplyGen$title <- 0
  
  RenSupplyGen$title[1] <- GenSupplyReadableProcessed$`Total generated`
  
  RenSupplyGen$title[2] <- -(GenSupplyReadableProcessed$`Electricity transferred to England (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Northern Ireland (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Europe (net of receipts)`)
  
  RenSupplyGen$title[3] <- RenSupplyGen$title[1] + RenSupplyGen$title[2]
  
  RenSupplyGen$title[4] <- -GenSupplyReadableProcessed$`Transfers from other generators to public supply`
  
  RenSupplyGen$title[5] <- -GenSupplyReadableProcessed$`Consumption by autogenerators`
  
  RenSupplyGen$title[6] <- -(GenSupplyReadableProcessed$`Own use by Other generators`+GenSupplyReadableProcessed$`Used in pumping at pumped storage and other own use by MPPs`)
  
  RenSupplyGen$title[7] <- -(GenSupplyReadableProcessed$`Transmission losses` + GenSupplyReadableProcessed$`Distribution losses and theft`)
  
  RenSupplyGen$title[8] <- sum(RenSupplyGen$title[c(1,4, 5, 6)])
  
  RenSupplyGen$title[10] <- sum(RenSupplyGen$title[c(3,6:7)])
  
  RenSupplyGen$title[9] <- sum(RenSupplyGen$title[c(10,5)])
  
  RenSupplyGen$size <- (abs(RenSupplyGen$title) / 50000) * 75
    
    nodes <- RenSupplyGen
    
    edges <- as.data.frame(read_excel("Structure/6 - System Security/RenSupplyGen.xlsx", 
                                      sheet = "Links"))
    
    nodes$label <- str_wrap(nodes$label, 20)
    nodes$title <- paste(format(as.numeric(nodes$title), big.mark = ",", trim = TRUE), "GWh")
    
    visNetwork(nodes, edges, height = "100%", width = "100%") %>% 
      visEdges(arrows = "to") %>% 
      visHierarchicalLayout(direction = "LR", levelSeparation = 250) %>% visPhysics(hierarchicalRepulsion = c(nodeDistance = 180)) %>% 
      visOptions(highlightNearest = list(enabled =TRUE,  hover = T)) %>%
      visNodes(font = c(
        face = "century gothic",
        size = 20
      ),
      labelHighlightBold = TRUE
      ) %>% 
      visEdges(font = c(
        face = "century gothic",
        size = 20
      ),
      labelHighlightBold = TRUE
      ) %>% 
      visExport("Chart", type  = "png")
    
    
    
    
  })
  
  
  output$ScotGenSupplyTable1 = renderDT({
    
    ScotGenSupply <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy balance",
      skip = 29,
      n_max = 11
    )
    names(ScotGenSupply)[1] <- ""
    
    ScotGenSupply <- tail(ScotGenSupply, -1)
    
    ScotGenSupply[2:10] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    ScotGenSupply[1] <- c( 
      "Indigenous production", 
      "Imports", "...Rest of world", 
      "...Rest of UK", "Exports", 
      "...Rest of world", 
      "...Rest of UK", 
      "Marine bunkers", 
      "Stock change", 
      "Primary supply"
    )
    
    datatable(
      ScotGenSupply,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Aggregate energy balance (thousand tonnes of oil equivalent)",
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Aggregate energy balance (thousand tonnes of oil equivalent)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Aggregate energy balance (thousand tonnes of oil equivalent)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%     
      formatRound(2:10, 0) %>% 
      formatStyle(1,
                  target = 'row',
                  backgroundColor = styleEqual(c('Primary Supply'), c('#bdbdbd')))
    
  })
  

  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/6 - System Security/ScotGenSupply.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ScotGenSupply.png <- downloadHandler(
    filename = "ScotGenSupply.png",
    content = function(file) {
      writePNG(readPNG("Structure/6 - System Security/ScotGenSupply.png"), file) 
    }
  )
  
  

  observeEvent(input$ToggleTable1, {
    toggle("ScotGenSupplyTable1")
  })
  

  
  
}
                                                                                                                                                     