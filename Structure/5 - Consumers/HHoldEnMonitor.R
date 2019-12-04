require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

HHoldEnMonitorOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Proportion of households with energy use monitoring devices", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('EnMonitorSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnMonitor.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("EnMonitorPlot")),
    plotlyOutput(ns("EnMonitorPlot"))%>% withSpinner(color="#68c3ea"),
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
      column(12, dataTableOutput(ns("EnMonitorTable"))%>% withSpinner(color="#68c3ea"))),
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
HHoldEnMonitor <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("Monitoring.R")

  
  output$EnMonitorSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                            sheet = "Energy monitoring devices", skip = 12, n_max = 2, col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "Proportion")
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    EnMonitor <- Data
    
    paste("Scotland,", min(EnMonitor$Year),"-", max(EnMonitor$Year))
  })
  
  output$EnMonitorPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Energy monitoring devices", skip = 12, n_max = 2, col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "Proportion")
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    EnMonitor <- Data

    ### variables
    ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: SG"
    plottitle = "Proportion of households with energy use\nmonitoring devices"
    
    EnMonitor$Year <- paste0("01/01/", EnMonitor$Year)
    
    EnMonitor$Year <- dmy(EnMonitor$Year)
    
    
    p <-  plot_ly(EnMonitor,x = ~ Year ) %>% 
      add_trace(y = ~ Proportion,
                name = "Proportion",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Proportion: ",
                  percent(EnMonitor$Proportion, accuracy = 0.1),
                  "\nYear: ",
                  format(EnMonitor$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EnMonitor[which(EnMonitor$Proportion > 0 | EnMonitor$Proportion < 0),], 1),
        x = ~ Year,
        y = ~ `Proportion`,
        name = "Proportion",
        text = paste0(
          "Proportion: ",
          percent(EnMonitor[which(EnMonitor$Proportion > 0 | EnMonitor$Proportion < 0),][-1,]$Proportion, accuracy = 0.1),
          "\nYear: ",
          format(EnMonitor[which(EnMonitor$Proportion > 0 | EnMonitor$Proportion < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>%
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EnMonitor$Year)-100, max(EnMonitor$Year)+100)),
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
  
  
  output$EnMonitorTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Energy monitoring devices", skip = 12, n_max = 2, col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "Proportion")
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    EnMonitor <- Data
    
    datatable(
      EnMonitor,
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
        title = "Households with energy use monitoring devices",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Households with energy use monitoring devices',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Households with energy use monitoring devices')
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
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/HHoldEnMonitor.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("EnMonitorTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnMonitor.png <- downloadHandler(
    filename = "EnMonitor.png",
    content = function(file) {

      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                              sheet = "Energy monitoring devices", skip = 12, n_max = 2, col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      Data <- tail(Data, -1)
      
      names(Data) <- c("Year", "Proportion")
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      EnergyMonitorDevices <- Data
      
      ### variables
      ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: SG"
      plottitle = "Proportion of households with energy use\nmonitoring devices"
      
      #EnergyMonitorDevices$ProportionPercentage <- PercentLabel(EnergyMonitorDevices$Proportion)
      
      
      EnergyMonitorDevicesChart <- EnergyMonitorDevices %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = Proportion,
              label = percent(Proportion)),
          colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .25,
            y = Proportion,
            label = ifelse(Year == min(Year), percent(Proportion, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .5,
            y = Proportion,
            label = ifelse(Year == max(Year), percent(Proportion, accuracy = 1), ""),
            hjust = 0.5,
            
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergyMonitorDevices, 1),
          aes(x = Year,
              y = Proportion,
              show_guide = FALSE),
          colour = ChartColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              Year, ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      EnergyMonitorDevicesChart
      
      EnergyMonitorDevicesChart <-
        StackedArea(EnergyMonitorDevicesChart,
                    EnergyMonitorDevices,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      EnergyMonitorDevicesChart
      
      ggsave(
        file,
        plot =  EnergyMonitorDevicesChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
}
