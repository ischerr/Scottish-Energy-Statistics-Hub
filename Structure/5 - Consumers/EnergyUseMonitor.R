require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnergyUseMonitorOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Proportion of homes with loft insulation, by thickness", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('EnergyUseMonitorSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnergyUseMonitor.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:Not Very Closely;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("EnergyUseMonitorPlot")),
    plotlyOutput(ns("EnergyUseMonitorPlot"), height =  "900px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:Not Very Closely;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:Not Very Closely;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnergyUseMonitorTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:Not Very Closely;color:#68c3ea;background-color:#68c3ea;"),
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
EnergyUseMonitor <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EnergyUseMonitor.R")

  
  output$EnergyUseMonitorSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Loft insulation",
      col_names = FALSE,
      skip = 12,
      n_max = 5
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$EnergyUseMonitorPlot <- renderPlotly  ({
    
    
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Monitor energy use",
      col_names = FALSE,
      skip = 13,
      n_max = 6
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
    BarColours <- c("#1a9850", "#a6d96a", "#969696", "#f46d43", "#d73027")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Very Closely` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`),
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Very Closely",
        text = paste0("Very Closely: ", percent(Data$`Very Closely`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Fairly Closely` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`),
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Fairly Closely",
        text = paste0("Fairly Closely: ", percent(Data$`Fairly Closely`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Don't Know` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`),
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Don't Know",
        text = paste0("Don't Know: ", percent(Data$`Don't Know`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Not Very Closely` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`),
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Not Very Closely",
        text = paste0("Not Very Closely: ", percent(Data$`Not Very Closely`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Not at All` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`) ,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Not at All",
        text = paste0("Not at All: ", percent(Data$`Not at All`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$`Year`),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
  })
  
  
  output$EnergyUseMonitorTable = renderDataTable({
    
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Monitor energy use",
      col_names = FALSE,
      skip = 13,
      n_max = 6
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      Data[c(1,2,3,6,4,5)],
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
        title = "Energy Consumption",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Energy Consumption',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Energy Consumption')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:6, 1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/EnergyUseMonitor.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("EnergyUseMonitorTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnergyUseMonitor.png <- downloadHandler(
    filename = "EnergyUseMonitor.png",
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
