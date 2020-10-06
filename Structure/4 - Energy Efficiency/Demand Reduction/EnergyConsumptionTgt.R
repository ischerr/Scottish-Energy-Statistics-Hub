require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



EnConsumptionTgtOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Final energy consumption against 2005-07 baseline", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('EnConsumptionTgtSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsumptionTgt.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("EnConsumptionTgtPlot")),
    plotlyOutput(ns("EnConsumptionTgtPlot"))%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10, h3("Data", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnConsumptionTgtTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISSubNatEnergy", "BEISSubNatElec", "BEISSubNatGas", "BEISLocalRoad"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("BEISSubNatElec"),
        SourceLookup("BEISSubNatGas"),
        SourceLookup("BEISLocalRoad")
        
      )
    )
  )
}




###### Server ######
EnConsumptionTgt <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EnConsumptionTgt.R")

  
  output$EnConsumptionTgtSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Energy consumption target", skip = 22, col_names = TRUE)[c(1,4)]
    
    Data[1,1] <- "2007"
    
    Data$Target <- NA
    EnConsumption <- Data
    
    paste("Scotland,", "2005/07","-", max(EnConsumption$Year))
  })
  
  output$EnConsumptionTgtPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Energy consumption target", skip = 22, col_names = TRUE)[c(1,4)]
    
    Data[1,1] <- "2007"
    
    Data$Target <- NA
    
    Data <- rbind(Data,c(2020, NA, -.12))
    
    names(Data) <- c("Year", "Renewables", "Tgt")
    
    Data$Year <- as.numeric(Data$Year)
    EnConsumption <- Data
    ### Variables
    ChartColours <- c("#34d1a3", "#FF8500")
    sourcecaption = "Source: BEIS"
    plottitle = "Final energy consumption against\n2005-07 baseline"
    
    EnConsumption$Year <- paste0("01/01/", EnConsumption$Year)
    
    EnConsumption$Year <- dmy(EnConsumption$Year)
    
    
    p <-  plot_ly(EnConsumption,x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "Renewables",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Progress: ",
                  percent(EnConsumption$Renewables, accuracy = 0.1),
                  "\nYear: ",
                  format(EnConsumption$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EnConsumption[which(EnConsumption$Renewables > 0 | EnConsumption$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        legendgroup = "1",
        name = "Renewable Electricity",
        text = paste0(
          "Progress: ",
          percent(EnConsumption[which(EnConsumption$Renewables > 0 | EnConsumption$Renewables < 0),][-1,]$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(EnConsumption[which(EnConsumption$Renewables > 0 | EnConsumption$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(
        data = EnConsumption,
        x = ~ Year,
        y = ~ Tgt,
        name = "Target",
        legendgroup = "2",
        text = paste0(
          "Target: ",
          percent(EnConsumption$Tgt, accuracy = 0.1),
          "\nYear: ",
          format(EnConsumption$Year, "%Y")
        ),
        hoverinfo = 'text',
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond",
                      color = ChartColours[2])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#34d1a3"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        shapes = list(list(
          type = "line",
          x0 = 0,
          x1 = 1, 
          xref = "paper",
          y0 = max(EnConsumption$Tgt, na.rm = TRUE), 
          y1 = max(EnConsumption$Tgt, na.rm = TRUE), 
          line = list(color = ChartColours[2],
                      dash = "dash")
        )
        ),
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EnConsumption$Year)-100, max(EnConsumption$Year)+100)),
        yaxis = list(
          title = "",
          tickformat = "%",
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
  
  
  output$EnConsumptionTgtTable = renderDataTable({
    
    EnConsumption <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Energy consumption target", col_names = FALSE, 
                          skip = 22)[1:4]
    EnConsumption <- tail(EnConsumption, -1)
    
    names(EnConsumption) <- c("Year","Total Energy Consumption (GWh)", "Change in Consumption from Baseline (GWh)", "% change from baseline")

    EnConsumption[2:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    EnConsumption <- as_tibble(EnConsumption)
    
    datatable(
      EnConsumption,
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
        title = "Final Energy Consumption",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Final Energy Consumption',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Final Energy Consumption')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(4, 1) %>% 
      formatRound(2:3, 0)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Demand Reduction/EnergyConsumptionTgt.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("EnConsumptionTgtTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnConsumptionTgt.png <- downloadHandler(
    filename = "EnConsumptionTgt.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Energy consumption target", skip = 22, col_names = TRUE)[c(1,4)]
      
      Data[1,1] <- "2007"
      
      Data$Target <- NA
      
      Data <- rbind(Data,c(2020, NA, -.12))
      
      names(Data) <- c("Year", "Renewables", "Tgt")
      
      Data$Year <- as.numeric(Data$Year)
      EnCons <- Data
      ### Variables
      ChartColours <- c("#34d1a3", "#FF8500")
      sourcecaption = "Source: BEIS"
      plottitle = "Final energy consumption against\n2005-07 baseline"
      
      EnConsChart <-
        TargetChart3(EnCons, plottitle, sourcecaption, ChartColours)
      
      EnConsChart <- EnConsChart +
        geom_hline(yintercept = -0.12,
                   color = ChartColours[2],
                   linetype = 5)+
        labs(subtitle = paste("Scotland, 2005-07 -", max(EnCons$Year[which(EnCons$Renewables !=0)]))) +
        annotate(
          "text",
          x = 2013,
          y = -.124,
          label = "First dropped below 12%\nfrom the baseline in 2013",
          hjust = 1,
          vjust = 1,
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        )
      
      EnConsChart
      
      ggsave(
        file,
        plot = EnConsChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
