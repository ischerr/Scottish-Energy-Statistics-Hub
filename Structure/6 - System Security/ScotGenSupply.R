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
        downloadButton(ns('ScotGenSupply.png'), paste('Download',max(unique(GenSupplyReadable$Year)), 'Graph'), style="float:right")
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
      column(10, h3("Data - Scottish electricity generation and supply (GWh)", style = "color: #5d8be1;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, DTOutput(ns("ScotGenSupplyTable1"))%>% withSpinner(color="#5d8be1"))),
        tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISElecGen"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISElecGen")
        
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
    
    GenSupplyReadableProcessed <- GenSupplyReadable[which(GenSupplyReadable$Country == "Scotland"),]
    
    GenSupplyReadableProcessed$`Total Generation` <- GenSupplyReadableProcessed$`Total generated`
    
    GenSupplyReadableProcessed$`Net Exports` <- -(GenSupplyReadableProcessed$`Electricity transferred to England (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Northern Ireland (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Europe (net of receipts)`)
    
    GenSupplyReadableProcessed$`Gross Electricity Consumption` <- GenSupplyReadableProcessed$`Total Generation` + GenSupplyReadableProcessed$`Net Exports`
    
    GenSupplyReadableProcessed$`Transfers from other generators` <- -GenSupplyReadableProcessed$`Transfers from other generators to public supply`
    
    GenSupplyReadableProcessed$`Consumption by Autogenerators` <- -GenSupplyReadableProcessed$`Consumption by autogenerators`
    
    GenSupplyReadableProcessed$`Own Use` <- -(GenSupplyReadableProcessed$`Own use by Other generators`+GenSupplyReadableProcessed$`Used in pumping at pumped storage and other own use by MPPs`)
    
    GenSupplyReadableProcessed$`Losses` <- -(GenSupplyReadableProcessed$`Transmission losses` + GenSupplyReadableProcessed$`Distribution losses and theft`)
    
    GenSupplyReadableProcessed$`Electricity Supplied` <- GenSupplyReadableProcessed$`Total Generation` + GenSupplyReadableProcessed$`Transfers from other generators` + GenSupplyReadableProcessed$`Consumption by autogenerators` + GenSupplyReadableProcessed$`Own Use`
    
    GenSupplyReadableProcessed$`Consumption from Public Supply` <-  GenSupplyReadableProcessed$`Gross Electricity Consumption` + GenSupplyReadableProcessed$`Own Use` + GenSupplyReadableProcessed$`Losses`
    
    GenSupplyReadableProcessed$`Total Electricity Consumption` <- GenSupplyReadableProcessed$`Consumption by autogenerators` + GenSupplyReadableProcessed$`Consumption from Public Supply`
    
    datatable(
      GenSupplyReadableProcessed[c(1,21:28,30,29)],
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
        title = "Scottish electricity generation and supply (GWh)",
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish electricity generation and supply (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish electricity generation and supply (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%     
      formatRound(2:11, 0)
    
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
                                                                                                                                                     