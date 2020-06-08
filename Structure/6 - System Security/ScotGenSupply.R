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
    tabsetPanel(
      tabPanel("Hey",
      fluidRow(column(8,
                      h3("Proportion of electricity generation by fuel", style = "color: #5d8be1;  font-weight:bold"),
                      selectInput(ns("YearSelect"), "Year:", rev(unique(GenSupplyReadable$Year)), selected = max(unique(GenSupplyReadable$Year)), multiple = FALSE,
                                  selectize = TRUE, width = NULL, size = NULL)
      ),
      column(
        4, style = 'padding:15px;',
        downloadButton(ns('ElecGenFuel.png'), 'Download Graph', style="float:right")
      )),
      
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
               #dygraphOutput(ns("ScotGenSupplyPlot")),
              visNetworkOutput(ns("ScotGenSupplyPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
               p("* TTL = Transfers, Transformation and Losses"),
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
      tabPanel("Simplified flow chart",
               fluidRow(column(8,
                               h3("Simplified energy flow chart", style = "color: #5d8be1;  font-weight:bold"),
                               h4(textOutput(ns('SimplifiedFlowSubtitle')), style = "color: #5d8be1;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('SimplifiedFlow.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
               #dygraphOutput(ns("ScotGenSupplyPlot")),
               
               fluidRow(
                 plotlyOutput(ns("SimplifiedFlowPlot1"))%>% withSpinner(color="#5d8be1")),
               fluidRow(
                 plotlyOutput(ns("SimplifiedFlowPlot2"))%>% withSpinner(color="#5d8be1")),
               fluidRow(
                 plotlyOutput(ns("SimplifiedFlowPlot3"))%>% withSpinner(color="#5d8be1"))
               ,
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
    fluidRow(
      column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    tabsetPanel(
      tabPanel("Energy balance data",
    fluidRow(
      column(8, h3("Data - Supply (ktoe)", style = "color: #5d8be1;  font-weight:bold")),
      column(2, style = "padding:15px",  downloadButton(ns('ScotGenSupplyData.xlsx'), 'Download Full Data', style="float:right")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Tables", style = "float:right; "))
    ),
    fluidRow(
      column(12, DTOutput(ns("ScotGenSupplyTable1"))%>% withSpinner(color="#5d8be1"))),
    fluidRow(
      column(10, h3("Data - Transfers and Transformation (ktoe)", style = "color: #5d8be1;  font-weight:bold"))
    ),
    fluidRow(
      column(12, DTOutput(ns("ScotGenSupplyTable2"))%>% withSpinner(color="#5d8be1"))),
    fluidRow(
      column(10, h3("Data - Consumption (ktoe)", style = "color: #5d8be1;  font-weight:bold"))
    ),
    fluidRow(
      column(12, DTOutput(ns("ScotGenSupplyTable3"))%>% withSpinner(color="#5d8be1")))),
    tabPanel("Energy flow data",
             
             fluidRow(
               column(10, h3("Data - Indigenous production & imports", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Tables", style = "float:right; "))
             ),
             fluidRow(
               column(12, DTOutput(ns("EnFlowTable1"))%>% withSpinner(color="#5d8be1"))),
    fluidRow(
      column(10, h3("Data - Outputs", style = "color: #5d8be1;  font-weight:bold"))),
    fluidRow(
      column(12, DTOutput(ns("EnFlowTable2"))%>% withSpinner(color="#5d8be1"))),
    
    fluidRow(
      column(10, h3("Data - Exports and losses", style = "color: #5d8be1;  font-weight:bold"))),
    fluidRow(
      column(12, DTOutput(ns("EnFlowTable3"))%>% withSpinner(color="#5d8be1"))),
    
  fluidRow(
    column(10, h3("Data - Final consumption", style = "color: #5d8be1;  font-weight:bold")),
  ),
  fluidRow(
    column(12, DTOutput(ns("EnFlowTable4"))%>% withSpinner(color="#5d8be1"))),
  
  )),
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
      )
    
    
    
    
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
  
  output$ScotGenSupplyTable2 = renderDT({
    
    ScotGenSupply <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy balance",
      skip = 29,
      n_max = 19
    )
    names(ScotGenSupply)[1] <- ""
    
    ScotGenSupply <- tail(ScotGenSupply, -12)
    
    ScotGenSupply[2:10] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    ScotGenSupply[1] <- c( 
      "Primary Demand",
      "Transfers",
      "Transformation",
      "...Electricity generation",
      "...Petroleum refineries",
      "...Manufactured fuel & other",
      "Energy industry use and distribution"
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
                  backgroundColor = styleEqual(c('Primary Demand'), c('#bdbdbd'))
      )
    
  })
  
  output$ScotGenSupplyTable3 = renderDT({
    
    ScotGenSupply <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy balance",
      skip = 29,
      n_max = 25
    )
    names(ScotGenSupply)[1] <- ""
    
    ScotGenSupply <- tail(ScotGenSupply, -19)
    
    ScotGenSupply[2:10] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    ScotGenSupply[1] <- c( 
      "Final Consumption",
      "Non-energy Use",
      "Industry",
      "Domestic",
      "Transport",
      "Other"
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
                  backgroundColor = styleEqual(c('Final Consumption'), c('#bdbdbd'))
      )
    
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/1 - Whole System/ScotGenSupply.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ScotGenSupply.png <- downloadHandler(
    filename = "ScotGenSupply.png",
    content = function(file) {
      writePNG(readPNG("Structure/1 - Whole System/ScotGenSupply.png"), file) 
    }
  )
  
  
  output$ScotGenSupplyData.xlsx <- downloadHandler(
    filename = "ScotGenSupplyData.xlsx",
    content <- function(file) {
      file.copy("Structure/1 - Whole System/ScotGenSupplyData.xlsx", file)
    })  
  
  output$SimplifiedFlowSubtitle <- renderText({
    
    paste("Scotland, 2018")
  })
  
  output$SimplifiedFlow.png <- downloadHandler(
    filename = "SimplifiedFlow.png",
    content = function(file) {
      writePNG(readPNG("Structure/1 - Whole System/SimplifiedFlow.png"), file) 
    }
  ) 
  
  output$SimplifiedFlowPlot1 <- renderPlotly  ({
    
    Pie1 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie1 <- Pie1[12:13]
    
    names(Pie1) <- c("Label", "Value")
    
    Pie1 <- Pie1[complete.cases(Pie1),]
    
    Pie1$TextInfo <- Pie1$Value / sum(Pie1$Value)
    
    Pie2 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie2 <- Pie2[15:16]
    
    names(Pie2) <- c("Label", "Value")
    
    Pie2 <- Pie2[complete.cases(Pie2),]
    
    p1 <- plot_ly() %>% 
      add_pie(data = Pie2, 
              labels = ~Label, 
              values = ~Value, 
              textinfo = 'none',
              textposition = "inside",
              type = 'pie', 
              insidetextfont = list(color = "#FFFFFF",
                                    font = "bold"),
              hoverinfo = 'text',
              text = paste0(Pie2$Label,": ", format(round(Pie2$Value, digits = 0), big.mark = ","), " ktoe\n", percent((Pie2$Value)/ sum(Pie2$Value))),
              hole = 0.8, 
              sort = F,
              marker = list(colors = c("#262626", "#6f8a91"),
                            line = list(color = '#FFFFFF', width = 2)),
              rotation = 90 + ((Pie2$Value[2]/ sum(Pie2$Value) * 360)/2)
      )%>% 
      add_pie(data = Pie1, labels = ~Label, values = ~Value,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              domain = list(
                x = c(0.09, 0.91),
                y = c(0.1, 0.9)),
              marker = list(colors = c("#254061", "#376092", "#00aa88", "#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(Pie1$Label,": ", format(round(Pie1$Value, digits = 0), big.mark = ","), " ktoe\n", percent((Pie1$Value)/ sum(Pie1$Value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Indigenous production & imports</b>:", format(round(sum(Pie2$Value), digits = 0), big.mark = ","), "ktoe"),
          font = list(
            color = "#5d8be1"
          )
        ),
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h')
      ) 
    
    p1
    
  })
  output$SimplifiedFlowPlot2 <- renderPlotly  ({
    
    Pie2 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie2 <- Pie2[15:16]
    
    names(Pie2) <- c("Label", "Value")
    
    Pie2 <- Pie2[complete.cases(Pie2),]
    
    Pie3 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie3 <- Pie3[18:19]
    
    names(Pie3) <- c("Label", "Value")
    
    Pie3 <- Pie3[complete.cases(Pie3),]
    
    Pie3[3,1] <- "Industry & Distribution Losses"
    
    p2 <- plot_ly() %>% 
      add_pie(labels = c("Exports and Losses"), 
              values = c(1), 
              type = 'pie', 
              hole = 0.9,
              textinfo = 'none',
              textposition = "inside",
              type = 'pie', 
              insidetextfont = list(color = "#FFFFFF",
                                    font = "bold"),
              hoverinfo = 'none',
              marker = list(colors = c("#262626"),
                            line = list(color = '#FFFFFF', width = 2)),
              sort = F
      )%>% 
      add_pie(data = Pie3, labels = ~Label, values = ~Value,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              domain = list(
                x = c(0.05, 0.95),
                y = c(0.05, 0.95)),
              marker = list(colors = c("#4f6228",  "#948a54", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(Pie3$Label,": ", format(round(Pie3$Value, digits = 0), big.mark = ","), " ktoe\n", percent((Pie3$Value)/ sum(Pie3$Value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Exports and losses</b>:",format(round(Pie2$Value[1], digits = 0), big.mark = ","), "ktoe"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h')
      )
    p2
    
    
    
    
  })
  output$SimplifiedFlowPlot3 <- renderPlotly  ({
    Pie2 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie2 <- Pie2[15:16]
    
    names(Pie2) <- c("Label", "Value")
    
    Pie2 <- Pie2[complete.cases(Pie2),]
    
    Pie4 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie4 <- Pie4[21:22]
    
    names(Pie4) <- c("Label", "Value")
    
    Pie4 <- Pie4[complete.cases(Pie4),]
    
    p3 <- plot_ly() %>% 
      add_pie(labels = c("Final Consumption"), 
              values = c(1), 
              type = 'pie', 
              hole = 0.9,
              textinfo = 'none',
              textposition = "inside",
              type = 'pie', 
              insidetextfont = list(color = "#FFFFFF",
                                    font = "bold"),
              hoverinfo = 'none',
              marker = list(colors = c("#6f8a91"),
                            line = list(color = '#FFFFFF', width = 2)),
              sort = F
      )%>% 
      add_pie(data = Pie4, labels = ~Label, values = ~Value,
              textinfo = 'none',
              hoverinfo = 'text',
              insidetextfont = list(color = "#FFFFFF",
                                    font = "bold"),
              domain = list(
                x = c(0.05, 0.95),
                y = c(0.05, 0.95)),
              marker = list(colors = c("#77933c",  "#c3d69b", "#8eb4e3","#8064a2", "#345e90", "#403152"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(Pie4$Label,": ", format(round(Pie4$Value, digits = 0), big.mark = ","), " ktoe\n", percent((Pie4$Value)/ sum(Pie4$Value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Final consumption</b>:", format(round(Pie2$Value[2], digits = 0), big.mark = ","), "ktoe"),
          font = list(
            color = "#6f8a91"
          )
        ),
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h')
      )
    
    p3
    
    
  })
  
  observeEvent(input$ToggleTable1, {
    toggle("ScotGenSupplyTable1")
    toggle("ScotGenSupplyTable2")
    toggle("ScotGenSupplyTable3")
  })
  
  
  output$EnFlowTable1 = renderDT({
    
    ScotGenSupply <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "PieChart Working",
      skip = 1,
      n_max = 6
    )[12:13]
    
    names(ScotGenSupply) <- c("Fuel", "Percentage")
    
    ScotGenSupply$Percentage <- ScotGenSupply$Percentage/ sum(ScotGenSupply$Percentage)
    
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
        title = "Indigenous Production & Imports - Fuel Proportions",
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Indigenous Production & Imports - Fuel Proportions',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Indigenous Production & Imports - Fuel Proportions')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%     
      formatPercentage(2, 1)
      
    
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EnFlowTable1")
    toggle("EnFlowTable2")
    toggle("EnFlowTable3")
    toggle("EnFlowTable4")
  })
  
  output$EnFlowTable2 = renderDT({
    
    ScotGenSupply <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "PieChart Working",
      skip = 1,
      n_max = 2
    )[15:16]
    
    names(ScotGenSupply) <- c("Output", "Percentage")
    
    ScotGenSupply$Percentage <- ScotGenSupply$Percentage/ sum(ScotGenSupply$Percentage)
    
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
        title = "Indigenous Production & Imports - Outputs",
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Indigenous Production & Imports - Outputs',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Indigenous Production & Imports - Outputs')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%     
      formatPercentage(2, 1)
    
    
  })
  
  
  output$EnFlowTable3 = renderDT({
    
    ScotGenSupply <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "PieChart Working",
      skip = 1,
      n_max = 3
    )[18:19]
    
    names(ScotGenSupply) <- c("Output", "Percentage")
    
    ScotGenSupply$Percentage <- ScotGenSupply$Percentage/ sum(ScotGenSupply$Percentage)
    
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
        title = "Exports and losses",
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Exports and losses',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Exports and losses')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%     
      formatPercentage(2, 1)
    
    
  })
  
  
  output$EnFlowTable4 = renderDT({
    
    ScotGenSupply <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "PieChart Working",
      skip = 1,
      n_max = 6
    )[21:22]
    
    names(ScotGenSupply) <- c("Fuel", "Percentage")
    
    ScotGenSupply$Percentage <- ScotGenSupply$Percentage/ sum(ScotGenSupply$Percentage)
    
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
        title = "Final consumption",
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Final consumption',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Final consumption')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%     
      formatPercentage(2, 1)
    
    
  })
  
  
  
}
                                                                                                                                                     