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
      tabPanel("Generation and Supply",
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
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
      tabPanel("Time Series",
               fluidRow(column(8,
                               h3("Scottish electricity generation and supply over time", style = "color: #5d8be1;  font-weight:bold"),
                               h4(textOutput(ns('ScotGenSupplyTimeSubtitle')), style = "color: #5d8be1;")
                               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('ScotGenSupplyTime.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
               #dygraphOutput(ns("ScotGenSupplyPlot")),
               plotlyOutput(ns("ScotGenSupplyTimePlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
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
  
  RenSupplyGen$title[2] <- (GenSupplyReadableProcessed$`Electricity transferred to England (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Northern Ireland (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Europe (net of receipts)`)
  
  RenSupplyGen$title[3] <- RenSupplyGen$title[1] - RenSupplyGen$title[2]
  
  RenSupplyGen$title[4] <- GenSupplyReadableProcessed$`Transfers from other generators to public supply`
  
  RenSupplyGen$title[5] <- GenSupplyReadableProcessed$`Consumption by autogenerators`
  
  RenSupplyGen$title[6] <- (GenSupplyReadableProcessed$`Own use by Other generators`+GenSupplyReadableProcessed$`Used in pumping at pumped storage and other own use by MPPs`)
  
  RenSupplyGen$title[7] <- (GenSupplyReadableProcessed$`Transmission losses` + GenSupplyReadableProcessed$`Distribution losses and theft`)
  
  RenSupplyGen$title[8] <- sum(RenSupplyGen$title[c(1)]) - sum(RenSupplyGen$title[c(4, 5, 6)])
  
  RenSupplyGen$title[10] <- RenSupplyGen$title[c(3)] - sum(RenSupplyGen$title[c(6:7)])
  
  RenSupplyGen$title[9] <-  RenSupplyGen$title[c(10)] - RenSupplyGen$title[c(5)]
  
  RenSupplyGen$size <- (abs(RenSupplyGen$title) / 50000) * 75
    
    nodes <- RenSupplyGen
    
    edges <- as.data.frame(read_excel("Structure/6 - System Security/RenSupplyGen.xlsx", 
                                      sheet = "Links"))
    
    nodes$label <- str_wrap(nodes$label, 20)
    nodes$title <- paste(format(round(as.numeric(nodes$title), 0), big.mark = ",", trim = TRUE), "GWh")
    
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
    
    GenSupplyReadableProcessed <- GenSupplyReadable[which(GenSupplyReadable$Country == "Scotland"),]
    
    GenSupplyReadableProcessed$`Total Generation` <- GenSupplyReadableProcessed$`Total generated`
    
    GenSupplyReadableProcessed$`Net Exports` <- (GenSupplyReadableProcessed$`Electricity transferred to England (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Northern Ireland (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Europe (net of receipts)`)
    
    GenSupplyReadableProcessed$`Gross Electricity Consumption` <- GenSupplyReadableProcessed$`Total Generation` - GenSupplyReadableProcessed$`Net Exports`
    
    GenSupplyReadableProcessed$`Transfers from other generators` <- GenSupplyReadableProcessed$`Transfers from other generators to public supply`
    
    GenSupplyReadableProcessed$`Consumption by Autogenerators` <- GenSupplyReadableProcessed$`Consumption by autogenerators`
    
    GenSupplyReadableProcessed$`Own Use` <- (GenSupplyReadableProcessed$`Own use by Other generators`+GenSupplyReadableProcessed$`Used in pumping at pumped storage and other own use by MPPs`)
    
    GenSupplyReadableProcessed$`Losses` <- (GenSupplyReadableProcessed$`Transmission losses` + GenSupplyReadableProcessed$`Distribution losses and theft`)
    
    GenSupplyReadableProcessed$`Electricity Supplied` <- GenSupplyReadableProcessed$`Total Generation` - GenSupplyReadableProcessed$`Transfers from other generators` - GenSupplyReadableProcessed$`Consumption by autogenerators` - GenSupplyReadableProcessed$`Own Use`
    
    GenSupplyReadableProcessed$`Total Electricity Consumption` <-  GenSupplyReadableProcessed$`Gross Electricity Consumption` - GenSupplyReadableProcessed$`Own Use` - GenSupplyReadableProcessed$`Losses`
    
    GenSupplyReadableProcessed$`Consumption from Public Supply` <-   GenSupplyReadableProcessed$`Total Electricity Consumption` - GenSupplyReadableProcessed$`Consumption by autogenerators`
    
    datatable(
      GenSupplyReadableProcessed[c(1,21:30)],
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
      formatRound(2:11, 0) %>% 
      formatStyle(c(2,4,9,10,11), fontWeight = "bold")
    
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
  

  output$ScotGenSupplyTimePlot <- renderPlotly({
    
    GenSupplyReadableProcessed <- GenSupplyReadable[which(GenSupplyReadable$Country == "Scotland"),]
    
    GenSupplyReadableProcessed$`Total Generation` <- GenSupplyReadableProcessed$`Total generated`
    
    GenSupplyReadableProcessed$`Net Exports` <- (GenSupplyReadableProcessed$`Electricity transferred to England (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Northern Ireland (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Europe (net of receipts)`)
    
    GenSupplyReadableProcessed$`Gross Electricity Consumption` <- GenSupplyReadableProcessed$`Total Generation` - GenSupplyReadableProcessed$`Net Exports`
    
    GenSupplyReadableProcessed$`Transfers from other generators` <- GenSupplyReadableProcessed$`Transfers from other generators to public supply`
    
    GenSupplyReadableProcessed$`Consumption by Autogenerators` <- GenSupplyReadableProcessed$`Consumption by autogenerators`
    
    GenSupplyReadableProcessed$`Own Use` <- (GenSupplyReadableProcessed$`Own use by Other generators`+GenSupplyReadableProcessed$`Used in pumping at pumped storage and other own use by MPPs`)
    
    GenSupplyReadableProcessed$`Losses` <- (GenSupplyReadableProcessed$`Transmission losses` + GenSupplyReadableProcessed$`Distribution losses and theft`)
    
    GenSupplyReadableProcessed$`Electricity Supplied` <- GenSupplyReadableProcessed$`Total Generation` - GenSupplyReadableProcessed$`Transfers from other generators` - GenSupplyReadableProcessed$`Consumption by autogenerators` - GenSupplyReadableProcessed$`Own Use`
    
    GenSupplyReadableProcessed$`Total Electricity Consumption` <-  GenSupplyReadableProcessed$`Gross Electricity Consumption` - GenSupplyReadableProcessed$`Own Use` - GenSupplyReadableProcessed$`Losses`
    
    GenSupplyReadableProcessed$`Consumption from Public Supply` <-   GenSupplyReadableProcessed$`Total Electricity Consumption` - GenSupplyReadableProcessed$`Consumption by autogenerators`
    
    GenSupplyReadableProcessed <- GenSupplyReadableProcessed[c(1,21:28,30,29)]
    
    ### variables
    ChartColours <- c("#5d8be1", "#253494", "#2c7fb8", "#41b6c4", "#a1dab4", "#74c476")
    sourcecaption = "Source: Elexon, National Grid"
    plottitle = "Proportion of time Scotland is capable of meeting\ndemand from Scottish generation"
    
    GenSupplyReadableProcessed$Year <- paste0("01/01/", GenSupplyReadableProcessed$Year)
    
    GenSupplyReadableProcessed$Year <- dmy(GenSupplyReadableProcessed$Year)
    
    
    
    p <-  plot_ly(GenSupplyReadableProcessed,x = ~ Year ) %>% 
      add_trace(data = GenSupplyReadableProcessed,
                x = ~ Year,
                y = ~ `Total Generation`,
                name = "Total Generation",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Total Generation: ",
                  format(round(GenSupplyReadableProcessed$`Total Generation`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(GenSupplyReadableProcessed$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Total Generation` > 0 | GenSupplyReadableProcessed$`Total Generation` < 0),], 1),
        x = ~ Year,
        y = ~ `Total Generation`,
        legendgroup = "1",
        name = "Total Generation",
        text = paste0(
          "Total Generation: ",
          format(round(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Total Generation` > 0 | GenSupplyReadableProcessed$`Total Generation` < 0),][-1,]$`Total Generation`, digits = 0), big.mark = ","),
          " GWh\nYear: ",
          format(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Total Generation` > 0 | GenSupplyReadableProcessed$`Total Generation` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = GenSupplyReadableProcessed,
                x = ~ Year,
                y = ~ `Gross Electricity Consumption`,
                name = "Gross Electricity Consumption",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Gross Electricity Consumption: ",
                  format(round(GenSupplyReadableProcessed$`Gross Electricity Consumption`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(GenSupplyReadableProcessed$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Gross Electricity Consumption` > 0 | GenSupplyReadableProcessed$`Gross Electricity Consumption` < 0),], 1),
        x = ~ Year,
        y = ~ `Gross Electricity Consumption`,
        legendgroup = "2",
        name = "Gross Electricity Consumption",
        text = paste0(
          "Gross Electricity Consumption: ",
          format(round(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Gross Electricity Consumption` > 0 | GenSupplyReadableProcessed$`Gross Electricity Consumption` < 0),][-1,]$`Gross Electricity Consumption`, digits = 0), big.mark = ","),
          " GWh\nYear: ",
          format(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Gross Electricity Consumption` > 0 | GenSupplyReadableProcessed$`Gross Electricity Consumption` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_trace(data = GenSupplyReadableProcessed,
                x = ~ Year,
                y = ~ `Electricity Supplied`,
                name = "Electricity Supplied",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Electricity Supplied: ",
                  format(round(GenSupplyReadableProcessed$`Electricity Supplied`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(GenSupplyReadableProcessed$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[5], dash = "none")
      ) %>% 
      add_trace(
        data = tail(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Electricity Supplied` > 0 | GenSupplyReadableProcessed$`Electricity Supplied` < 0),], 1),
        x = ~ Year,
        y = ~ `Electricity Supplied`,
        legendgroup = "3",
        name = "Electricity Supplied",
        text = paste0(
          "Electricity Supplied: ",
          format(round(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Electricity Supplied` > 0 | GenSupplyReadableProcessed$`Electricity Supplied` < 0),][-1,]$`Electricity Supplied`, digits = 0), big.mark = ","),
          " GWh\nYear: ",
          format(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Electricity Supplied` > 0 | GenSupplyReadableProcessed$`Electricity Supplied` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[5])
      ) %>% 
      add_trace(data = GenSupplyReadableProcessed,
                x = ~ Year,
                y = ~ `Total Electricity Consumption`,
                name = "Total Electricity Consumption",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "Total Electricity Consumption: ",
                  format(round(GenSupplyReadableProcessed$`Total Electricity Consumption`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(GenSupplyReadableProcessed$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[4], dash = "none")
      ) %>% 
      add_trace(
        data = tail(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Total Electricity Consumption` > 0 | GenSupplyReadableProcessed$`Total Electricity Consumption` < 0),], 1),
        x = ~ Year,
        y = ~ `Total Electricity Consumption`,
        legendgroup = "4",
        name = "Total Electricity Consumption",
        text = paste0(
          "Total Electricity Consumption: ",
          format(round(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Total Electricity Consumption` > 0 | GenSupplyReadableProcessed$`Total Electricity Consumption` < 0),][-1,]$`Total Electricity Consumption`, digits = 0), big.mark = ","),
          " GWh\nYear: ",
          format(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Total Electricity Consumption` > 0 | GenSupplyReadableProcessed$`Total Electricity Consumption` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[4])
      ) %>% 
      
      
      add_trace(data = GenSupplyReadableProcessed,
                x = ~ Year,
                y = ~ `Consumption from Public Supply`,
                name = "Consumption from Public Supply",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "5",
                text = paste0(
                  "Consumption from Public Supply: ",
                  format(round(GenSupplyReadableProcessed$`Consumption from Public Supply`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(GenSupplyReadableProcessed$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[6], dash = "none")
      ) %>% 
      add_trace(
        data = tail(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Consumption from Public Supply` > 0 | GenSupplyReadableProcessed$`Consumption from Public Supply` < 0),], 1),
        x = ~ Year,
        y = ~ `Consumption from Public Supply`,
        legendgroup = "5",
        name = "Consumption from Public Supply",
        text = paste0(
          "Consumption from Public Supply: ",
          format(round(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Consumption from Public Supply` > 0 | GenSupplyReadableProcessed$`Consumption from Public Supply` < 0),][-1,]$`Consumption from Public Supply`, digits = 0), big.mark = ","),
          " GWh\nYear: ",
          format(GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$`Consumption from Public Supply` > 0 | GenSupplyReadableProcessed$`Consumption from Public Supply` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[6])
      ) %>% 
      
      
      
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(GenSupplyReadableProcessed$Year)-100, max(GenSupplyReadableProcessed$Year)+100)),
        yaxis = list(
          title = "GWh",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
    
  })
  
  
  output$ScotGenSupplyTime.png <- downloadHandler(
    filename = "ScotGenSupplyTime.png",
    content = function(file) {
      
      
      
      GenSupplyReadableProcessed <- GenSupplyReadable[which(GenSupplyReadable$Country == "Scotland"),]
      
      GenSupplyReadableProcessed$`Total Generation` <- GenSupplyReadableProcessed$`Total generated`
      
      GenSupplyReadableProcessed$`Net Exports` <- (GenSupplyReadableProcessed$`Electricity transferred to England (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Northern Ireland (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Europe (net of receipts)`)
      
      GenSupplyReadableProcessed$`Gross Electricity Consumption` <- GenSupplyReadableProcessed$`Total Generation` - GenSupplyReadableProcessed$`Net Exports`
      
      GenSupplyReadableProcessed$`Transfers from other generators` <- GenSupplyReadableProcessed$`Transfers from other generators to public supply`
      
      GenSupplyReadableProcessed$`Consumption by Autogenerators` <- GenSupplyReadableProcessed$`Consumption by autogenerators`
      
      GenSupplyReadableProcessed$`Own Use` <- (GenSupplyReadableProcessed$`Own use by Other generators`+GenSupplyReadableProcessed$`Used in pumping at pumped storage and other own use by MPPs`)
      
      GenSupplyReadableProcessed$`Losses` <- (GenSupplyReadableProcessed$`Transmission losses` + GenSupplyReadableProcessed$`Distribution losses and theft`)
      
      GenSupplyReadableProcessed$`Electricity Supplied` <- GenSupplyReadableProcessed$`Total Generation` - GenSupplyReadableProcessed$`Transfers from other generators` - GenSupplyReadableProcessed$`Consumption by autogenerators` - GenSupplyReadableProcessed$`Own Use`
      
      GenSupplyReadableProcessed$`Total Electricity Consumption` <-  GenSupplyReadableProcessed$`Gross Electricity Consumption` - GenSupplyReadableProcessed$`Own Use` - GenSupplyReadableProcessed$`Losses`
      
      GenSupplyReadableProcessed$`Consumption from Public Supply` <-   GenSupplyReadableProcessed$`Total Electricity Consumption` - GenSupplyReadableProcessed$`Consumption by autogenerators`
      
      GenSupplyReadableProcessed <- GenSupplyReadableProcessed[c(1,21:28,30,29)]
      
      width = max(GenSupplyReadableProcessed$Year)- min(GenSupplyReadableProcessed$Year)
      ### variables
      ChartColours <- c("#5d8be1", "#253494", "#2c7fb8",  "#a1dab4","#41b6c4", "#74c476")
      sourcecaption = "Source: Elexon, National Grid"
      plottitle = "Scottish electricity generation and supply over time"
      
      #GenSupplyReadableProcessed$`Total Generation`Percentage <- PercentLabel(GenSupplyReadableProcessed$`Total Generation`)
      
      
      GenSupplyReadableProcessedChart <- GenSupplyReadableProcessed %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        geom_line(
          aes(y = `Total Generation`,
              label =  paste0(`Total Generation` * 100, "%")),
          colour = ChartColours[2],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - width*0.08,
            y = `Total Generation`,
            label = ifelse(Year == min(Year), paste0(format(round(`Total Generation`, digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            vjust = .5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + width*0.08,
            y = `Total Generation`,
            label = ifelse(Year == max(Year), paste0(format(round(`Total Generation`, digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GenSupplyReadableProcessed, 1),
          aes(x = Year,
              y = `Total Generation`,
              show_guide = FALSE),
          colour = ChartColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GenSupplyReadableProcessed$Year),
          y = mean(GenSupplyReadableProcessed$`Total Generation`),
          label = "Total Generation",
          hjust = 0.5,
          vjust = -2.9,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Gross Electricity Consumption`,
              label = paste0(`Gross Electricity Consumption` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - width*0.08,
            y = `Gross Electricity Consumption`,
            label = ifelse(Year == min(Year), paste0(format(round(`Gross Electricity Consumption`, digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            vjust = -.1,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + width*0.08,
            y = `Gross Electricity Consumption`,
            label = ifelse(Year == max(Year), paste0(format(round(`Gross Electricity Consumption`, digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            vjust = 1,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GenSupplyReadableProcessed, 1),
          aes(x = Year,
              y = `Gross Electricity Consumption`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GenSupplyReadableProcessed$Year),
          y = mean(GenSupplyReadableProcessed$`Gross Electricity Consumption`),
          label = "Gross Electricity\nConsumption",
          hjust = 0.5,
          vjust = 1.4,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Electricity Supplied`,
              label = paste0(`Electricity Supplied` * 100, "%")),
          colour = ChartColours[4],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - width*0.08,
            y = `Electricity Supplied`,
            label = ifelse(Year == min(Year), paste0(format(round(`Electricity Supplied`, digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            vjust = 0.95,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + width*0.08,
            y = `Electricity Supplied`,
            label = ifelse(Year == max(Year), paste0(format(round(`Electricity Supplied`,  digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GenSupplyReadableProcessed, 1),
          aes(x = Year,
              y = `Electricity Supplied`,
              
              show_guide = FALSE),
          size = 4,
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GenSupplyReadableProcessed$Year),
          y = mean(GenSupplyReadableProcessed$`Electricity Supplied`),
          label = "Electricity Supplied",
          hjust = 0.5,
          vjust = -3.1,
          colour = ChartColours[4],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Total Electricity Consumption`,
              label = paste0(`Total Electricity Consumption` * 100, "%")),
          colour = ChartColours[5],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - width*0.08,
            y = `Total Electricity Consumption`,
            label = ifelse(Year == min(Year), paste0(format(round(`Total Electricity Consumption`, digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + width*0.08,
            y = `Total Electricity Consumption`,
            label = ifelse(Year == max(Year), paste0(format(round(`Total Electricity Consumption`, digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            vjust = 1,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GenSupplyReadableProcessed, 1),
          aes(x = Year,
              y = `Total Electricity Consumption`,
              show_guide = FALSE),
          colour = ChartColours[5],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GenSupplyReadableProcessed$Year),
          y = mean(GenSupplyReadableProcessed$`Total Electricity Consumption`),
          label = "Total Electricity Consumption",
          hjust = 0.5,
          vjust = 4.8,
          colour = ChartColours[5],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_line(
          aes(y = `Consumption from Public Supply`,
              label = paste0(`Consumption from Public Supply` * 100, "%")),
          colour = ChartColours[6],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - width*0.08,
            y = `Consumption from Public Supply`,
            label = ifelse(Year == min(Year), paste0(format(round(`Consumption from Public Supply`, digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[6],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + width*0.08,
            y = `Consumption from Public Supply`,
            label = ifelse(Year == max(Year), paste0(format(round(`Consumption from Public Supply`, digits = 0), big.mark = ","), "\nGWh"), ""),
            hjust = 0.5,
            vjust = 1,
            fontface = 2
          ),
          colour = ChartColours[6],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GenSupplyReadableProcessed, 1),
          aes(x = Year,
              y = `Consumption from Public Supply`,
              show_guide = FALSE),
          colour = ChartColours[6],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GenSupplyReadableProcessed$Year),
          y = mean(GenSupplyReadableProcessed$`Consumption from Public Supply`),
          label = "Consumption from Public Supply",
          hjust = 0.5,
          vjust = 4.2,
          colour = ChartColours[6],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              Year,
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      GenSupplyReadableProcessedChart
      
      GenSupplyReadableProcessedChart <-
        StackedArea(GenSupplyReadableProcessedChart,
                    GenSupplyReadableProcessed,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      GenSupplyReadableProcessedChart <- GenSupplyReadableProcessedChart +
        xlim(
          min(GenSupplyReadableProcessed$Year)-(width*0.105),
          max(GenSupplyReadableProcessed$Year)+(width*0.105)
              )
      
      ggsave(
        file,
        plot =  GenSupplyReadableProcessedChart,
        width = 18,
        height = 17,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  output$ScotGenSupplyTimeSubtitle <- renderText({
    

    paste("Scotland,", min(GenSupplyReadable$Year),"-", max(GenSupplyReadable$Year))
  })
  
  
}
                                                                                                                                                     