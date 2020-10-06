require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



EnBalanceOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Balance",
               fluidRow(column(8,
                               h3("Scottish energy balance", style = "color: #1A5D38;  font-weight:bold"),
                               h4(textOutput(ns('EnBalanceSubtitle')), style = "color: #1A5D38;"),
                               selectInput(ns("UnitSelect"), "Unit:", BalanceMultipliers$Unit, selected = BalanceMultipliers$Unit[1], multiple = FALSE,
                             selectize = TRUE, width = NULL, size = NULL)
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('EnBalance.png'), 'Download Graph', style="float:right"),
                 
               )),
               
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
               #dygraphOutput(ns("EnBalancePlot")),
               plotlyOutput(ns("EnBalancePlot"), height = "900px")%>% withSpinner(color="#1A5D38"),
               p("* TTL = Transfers, Transformation and Losses"),
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
      tabPanel("Simplified flow chart",
               fluidRow(column(8,
                               h3("Simplified energy flow chart", style = "color: #1A5D38;  font-weight:bold"),
                               h4(textOutput(ns('SimplifiedFlowSubtitle')), style = "color: #1A5D38;"),
                               selectInput(ns("UnitSelect2"), "Unit:", BalanceMultipliers$Unit, selected = BalanceMultipliers$Unit[1], multiple = FALSE,
                                           selectize = TRUE, width = NULL, size = NULL)
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('SimplifiedFlow.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
               #dygraphOutput(ns("EnBalancePlot")),
               
               fluidRow(
                 plotlyOutput(ns("SimplifiedFlowPlot1"))%>% withSpinner(color="#1A5D38")),
               fluidRow(
                 plotlyOutput(ns("SimplifiedFlowPlot2"))%>% withSpinner(color="#1A5D38")),
               fluidRow(
                 plotlyOutput(ns("SimplifiedFlowPlot3"))%>% withSpinner(color="#1A5D38"))
               ,
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"))),
    fluidRow(
      column(10,h3("Commentary", style = "color: #1A5D38;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    tabsetPanel(
      tabPanel("Energy balance data",
    fluidRow(
      uiOutput(ns("DataTableBalanceSupplyText")),
      
      column(2, style = "padding:15px",  downloadButton(ns('EnBalanceData.xlsx'), 'Download Full Data', style="float:right")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Tables", style = "float:right; "))
    ),
    fluidRow(column(12,selectInput(ns("UnitSelect3"), "Unit:", BalanceMultipliers$Unit, selected = BalanceMultipliers$Unit[1], multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL))),
    fluidRow(
      column(12, DTOutput(ns("EnBalanceTable1"))%>% withSpinner(color="#1A5D38"))),
    fluidRow(
      uiOutput(ns("DataTableBalanceTransfersText")),
    ),
    fluidRow(
      column(12, DTOutput(ns("EnBalanceTable2"))%>% withSpinner(color="#1A5D38"))),
    fluidRow(
      uiOutput(ns("DataTableBalanceConsumptionText")),
    ),
    fluidRow(
      column(12, DTOutput(ns("EnBalanceTable3"))%>% withSpinner(color="#1A5D38")))),
    tabPanel("Energy flow data",
             
             fluidRow(
               column(10, h3("Data - Indigenous production & imports", style = "color: #1A5D38;  font-weight:bold")),
               
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Tables", style = "float:right; "))
             ),
             fluidRow(column(12,selectInput(ns("UnitSelect4"), "Unit:", BalanceMultipliers$Unit, selected = BalanceMultipliers$Unit[1], multiple = FALSE,
                                  selectize = TRUE, width = NULL, size = NULL))),
             fluidRow(
               column(12, DTOutput(ns("EnFlowTable1"))%>% withSpinner(color="#1A5D38"))),
    fluidRow(
      column(10, h3("Data - Outputs", style = "color: #1A5D38;  font-weight:bold"))),
    fluidRow(
      column(12, DTOutput(ns("EnFlowTable2"))%>% withSpinner(color="#1A5D38"))),
    
    fluidRow(
      column(10, h3("Data - Exports and losses", style = "color: #1A5D38;  font-weight:bold"))),
    fluidRow(
      column(12, DTOutput(ns("EnFlowTable3"))%>% withSpinner(color="#1A5D38"))),
    
  fluidRow(
    column(10, h3("Data - Final consumption", style = "color: #1A5D38;  font-weight:bold")),
  ),
  fluidRow(
    column(12, DTOutput(ns("EnFlowTable4"))%>% withSpinner(color="#1A5D38"))),
  
  )),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISElecGen", "BEISSubNatEnergy", "HMRCTrade", "BEISDUKESBalance", "SGCommodityBalance", "BEISImportExport"))),
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
EnBalance <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EnBalance.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  observe({
    BalanceDropdown$Unit <- input$UnitSelect
    })
  
  observe({
    BalanceDropdown$Unit <- input$UnitSelect2
  })
  
  observe({
    BalanceDropdown$Unit <- input$UnitSelect3
  })
  
  observe({
    BalanceDropdown$Unit <- input$UnitSelect4
  })
  
  observe(
    {
      updateSelectInput(session, 'UnitSelect', selected = BalanceDropdown$Unit)
      updateSelectInput(session, 'UnitSelect2', selected = BalanceDropdown$Unit)
      updateSelectInput(session, 'UnitSelect3', selected = BalanceDropdown$Unit)
      updateSelectInput(session, 'UnitSelect4', selected = BalanceDropdown$Unit)
    }
  )
  
  output$DataTableBalanceSupplyText <- renderUI({
    
    unit <- as.character(BalanceDropdown$Unit)
    
    column(8, h3(paste0("Data - Supply (", unit, ")"), style = "color: #1A5D38;  font-weight:bold"))
  })
  
  output$DataTableBalanceTransfersText <- renderUI({
    
    unit <- as.character(BalanceDropdown$Unit)
    
    column(10, h3(paste0("Data - Transfers and Transformation (", unit, ")"), style = "color: #1A5D38;  font-weight:bold"))
  })
  
  output$DataTableBalanceConsumptionText <- renderUI({
    
    unit <- as.character(BalanceDropdown$Unit)
    
    column(10, h3(paste0("Data - Consumption (", unit, ")"), style = "color: #1A5D38;  font-weight:bold"))
  })
  
  
  output$EnBalanceSubtitle <- renderText({
    
    paste("Scotland, 2018")
  })
  
  output$EnBalancePlot <- renderPlotly  ({
    
    EnergyLinks <- as.data.frame(read_excel("Structure/1 - Whole System/EnBalance.xlsx", 
                                            sheet = "links"))
    
    EnergyNodes <- as.data.frame(read_excel("Structure/1 - Whole System/EnBalance.xlsx", 
                                            sheet = "nodes"))
    
    unit <- as.character(BalanceDropdown$Unit)
    
    EnergyLinks$Value <- EnergyLinks$Value * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
    p <- plot_ly(
      type = "sankey",
      domain = list(
        x =  c(0,1),
        y =  c(0,1)
      ),
      orientation = "h",
      valueformat = ",.0f",
      valuesuffix = paste0(" ", unit),
      
      node = list(
        label = EnergyNodes$Name,
        pad = 10,
        thickness = 30,
        colour = "white",
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = EnergyLinks$Source,
        target = EnergyLinks$Target,
        value =  EnergyLinks$Value
      )
    ) %>% 
      layout(
        xaxis = list(showgrid = F, zeroline = F),
        yaxis = list(showgrid = F, zeroline = F),
        plot_bgcolor = 'rgba(22,5e,a8,FF)',
        paper_bgcolor = 'rgba(22,5e,a8,FF)'
      )
    
    p <- ggplotly(p)
    
    p
    
    
    
  })
  
  
  output$EnBalanceTable1 = renderDT({
    
    EnBalance <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy balance",
      skip = 29,
      n_max = 11
    )
    names(EnBalance)[1] <- ""
    
    EnBalance <- tail(EnBalance, -1)
    
    unit <- as.character(BalanceDropdown$Unit)
    
    EnBalance[2:10] %<>% lapply(function(x) as.numeric(as.character(x)) * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier)
    
    EnBalance[1] <- c( 
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
      EnBalance,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = paste0("Aggregate energy balance (", unit, ")"),
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = paste0("Aggregate energy balance (", unit, ")"),
            header = TRUE
          ),
          list(extend = 'csv',
               title = paste0("Aggregate energy balance (", unit, ")"))
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
  
  output$EnBalanceTable2 = renderDT({
    
    EnBalance <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy balance",
      skip = 29,
      n_max = 19
    )
    names(EnBalance)[1] <- ""
    
    EnBalance <- tail(EnBalance, -12)
    
    unit <- as.character(BalanceDropdown$Unit)
    
    EnBalance[2:10] %<>% lapply(function(x) as.numeric(as.character(x)) * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier)
    
    EnBalance[1] <- c( 
      "Primary Demand",
      "Transfers",
      "Transformation",
      "...Electricity generation",
      "...Petroleum refineries",
      "...Manufactured fuel & other",
      "Energy industry use and distribution"
    )
    
    datatable(
      EnBalance,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = paste0("Aggregate energy balance (", unit, ")"),
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = paste0("Aggregate energy balance (", unit, ")"),
            header = TRUE
          ),
          list(extend = 'csv',
               title = paste0("Aggregate energy balance (", unit, ")"))
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
  
  output$EnBalanceTable3 = renderDT({
    
    EnBalance <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy balance",
      skip = 29,
      n_max = 25
    )
    names(EnBalance)[1] <- ""
    
    EnBalance <- tail(EnBalance, -19)
    
    unit <- as.character(BalanceDropdown$Unit)
    
    EnBalance[2:10] %<>% lapply(function(x) as.numeric(as.character(x)) *   BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier )
    
    EnBalance[1] <- c( 
      "Final Consumption",
      "Non-energy Use",
      "Industry",
      "Domestic",
      "Transport",
      "Other"
    )
    
    datatable(
      EnBalance,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = paste0("Aggregate energy balance (", unit, ")"),
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = paste0("Aggregate energy balance (", unit, ")"),
            header = TRUE
          ),
          list(extend = 'csv',
               title = paste0("Aggregate energy balance (", unit, ")"))
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
                     paste(readtext("Structure/1 - Whole System/EnBalance.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnBalance.png <- downloadHandler(
    filename = function(filename){
      unit <- as.character(BalanceDropdown$Unit)
      filename <- paste0("EnBalance", unit, ".png")},
    content = function(file) {
      unit <- as.character(BalanceDropdown$Unit)
      writePNG(readPNG(paste0("Structure/1 - Whole System/EnBalance", unit, ".png")), file) 
    }
  )
  
  
  output$EnBalanceData.xlsx <- downloadHandler(
    filename = function(filename){
      unit <- as.character(BalanceDropdown$Unit)
      filename <- paste0("EnBalanceData", unit, ".xlsx")},
    content <- function(file) {
      unit <- as.character(BalanceDropdown$Unit)
      
      file.copy(paste0("Structure/1 - Whole System/EnBalanceData", unit, ".xlsx"), file)
    })  
  
  output$SimplifiedFlowSubtitle <- renderText({
    
    paste("Scotland, 2018")
  })
  
  output$SimplifiedFlow.png <- downloadHandler(
    filename = function(filename){
      unit <- as.character(BalanceDropdown$Unit)
      filename <- paste0("SimplifiedFlow", unit, ".png")},
    content = function(file) {
      unit <- as.character(BalanceDropdown$Unit)
      
      writePNG(readPNG(paste0("Structure/1 - Whole System/SimplifiedFlow", unit, ".png")), file) 
    }
  ) 
  
  output$SimplifiedFlowPlot1 <- renderPlotly  ({
    
    unit <- as.character(BalanceDropdown$Unit)
    
    Pie1 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie1 <- Pie1[12:13]
    
    names(Pie1) <- c("Label", "Value")
    
    Pie1 <- Pie1[complete.cases(Pie1),]
    
    Pie1$Value <- Pie1$Value * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
    Pie1$TextInfo <- Pie1$Value / sum(Pie1$Value)
    
    Pie2 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie2 <- Pie2[15:16]
    
    names(Pie2) <- c("Label", "Value")
    
    Pie2 <- Pie2[complete.cases(Pie2),]
    
    Pie2$Value <- Pie2$Value * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
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
              text = paste0(Pie2$Label,": ", format(round(Pie2$Value, digits = 0), big.mark = ","), " ", unit, "\n", percent((Pie2$Value)/ sum(Pie2$Value), .1)),
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
              text = paste0(Pie1$Label,": ", format(round(Pie1$Value, digits = 0), big.mark = ","), " ktoe\n", percent((Pie1$Value)/ sum(Pie1$Value), .1)),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Indigenous production & imports</b>:", format(round(sum(Pie2$Value), digits = 0), big.mark = ","), unit),
          font = list(
            color = "#1A5D38"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      ) 
    
    p1
    
  })
  output$SimplifiedFlowPlot2 <- renderPlotly  ({
    
    unit <- as.character(BalanceDropdown$Unit)
    
    Pie2 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie2 <- Pie2[15:16]
    
    names(Pie2) <- c("Label", "Value")
    
    Pie2 <- Pie2[complete.cases(Pie2),]
    
    Pie2$Value <- Pie2$Value * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
    Pie3 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie3 <- Pie3[18:19]
    
    names(Pie3) <- c("Label", "Value")
    
    Pie3 <- Pie3[complete.cases(Pie3),]
    
    Pie3[3,1] <- "Industry & Distribution Losses"
    
    Pie3$Value <- Pie3$Value * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
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
              text = paste0(Pie3$Label,": ", format(round(Pie3$Value, digits = 0), big.mark = ","), " ", unit,"\n", percent((Pie3$Value)/ sum(Pie3$Value), .1)),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Exports and losses</b>:",format(round(Pie2$Value[1], digits = 0), big.mark = ","), unit),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p2
    
    
    
    
  })
  output$SimplifiedFlowPlot3 <- renderPlotly  ({
    
    unit <- as.character(BalanceDropdown$Unit)
    
    
    Pie2 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie2 <- Pie2[15:16]
    
    names(Pie2) <- c("Label", "Value")
    
    Pie2 <- Pie2[complete.cases(Pie2),]
    
    Pie2$Value <- Pie2$Value * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
    Pie4 <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "PieChart Working", col_names = TRUE, 
                       skip = 1)
    Pie4 <- Pie4[21:22]
    
    names(Pie4) <- c("Label", "Value")
    
    Pie4 <- Pie4[complete.cases(Pie4),]
    
    Pie4$Value <- Pie4$Value * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
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
              text = paste0(Pie4$Label,": ", format(round(Pie4$Value, digits = 0), big.mark = ","), " ", unit, "\n", percent((Pie4$Value)/ sum(Pie4$Value), .1)),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Final consumption</b>:", format(round(Pie2$Value[2], digits = 0), big.mark = ","), unit),
          font = list(
            color = "#6f8a91"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    
    p3
    
    
  })
  
  observeEvent(input$ToggleTable1, {
    toggle("EnBalanceTable1")
    toggle("EnBalanceTable2")
    toggle("EnBalanceTable3")
  })
  
  
  output$EnFlowTable1 = renderDT({
    
    EnBalance <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "PieChart Working",
      skip = 1,
      n_max = 6
    )[12:13]
    
    unit <- as.character(BalanceDropdown$Unit)
    
    names(EnBalance) <- c("Fuel", "Volume")
    
    EnBalance$Percentage <- EnBalance$`Volume`/ sum(EnBalance$`Volume`)
    
    EnBalance$Volume <- EnBalance$Volume * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
    names(EnBalance)[2] <- paste0("Volume (", unit, ")")
    
    datatable(
      EnBalance,
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
      formatPercentage(3, 1) %>% 
      formatRound(2,0)
      
    
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EnFlowTable1")
    toggle("EnFlowTable2")
    toggle("EnFlowTable3")
    toggle("EnFlowTable4")
  })
  
  output$EnFlowTable2 = renderDT({
    
    EnBalance <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "PieChart Working",
      skip = 1,
      n_max = 2
    )[15:16]
    
    unit <- as.character(BalanceDropdown$Unit)
    
    names(EnBalance) <- c("Fuel", "Volume")
    
    EnBalance$Percentage <- EnBalance$`Volume`/ sum(EnBalance$`Volume`)
    
    EnBalance$Volume <- EnBalance$Volume * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
    names(EnBalance)[2] <- paste0("Volume (", unit, ")")
    
    datatable(
      EnBalance,
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
      formatPercentage(3, 1) %>% 
      formatRound(2,0)
    
    
  })
  
  
  output$EnFlowTable3 = renderDT({
    
    EnBalance <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "PieChart Working",
      skip = 1,
      n_max = 3
    )[18:19]
    
    unit <- as.character(BalanceDropdown$Unit)
    
    names(EnBalance) <- c("Fuel", "Volume")
    
    EnBalance$Percentage <- EnBalance$`Volume`/ sum(EnBalance$`Volume`)
    
    EnBalance$Volume <- EnBalance$Volume * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
    names(EnBalance)[2] <- paste0("Volume (", unit, ")")
    
    datatable(
      EnBalance,
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
      formatPercentage(3, 1) %>% 
      formatRound(2,0)
    
    
  })
  
  
  output$EnFlowTable4 = renderDT({
    
    EnBalance <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "PieChart Working",
      skip = 1,
      n_max = 6
    )[21:22]
    
    unit <- as.character(BalanceDropdown$Unit)
    
    names(EnBalance) <- c("Fuel", "Volume")
    
    EnBalance$Percentage <- EnBalance$`Volume`/ sum(EnBalance$`Volume`)
    
    EnBalance$Volume <- EnBalance$Volume * BalanceMultipliers[which(BalanceMultipliers$Unit == unit),]$Multiplier
    
    names(EnBalance)[2] <- paste0("Volume (", unit, ")")
    
    datatable(
      EnBalance,
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
      formatPercentage(3, 1) %>% 
      formatRound(2,0)
    
    
  })
  
  
  
}
                                                                                                                                                     