require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

MarketStructureOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Market shares, combined electricity and gas", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('MarketStructureSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('MarketStructure.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("MarketStructurePlot")),
    plotlyOutput(ns("MarketStructurePlot"), height = "500px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data - Market shares, combined electricity and gas", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("MarketStructureTable"))%>% withSpinner(color="#68c3ea"))),
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
        SourceLookup("CASEnSwitch")
        
      )
    )
  )
}




###### Server ######
MarketStructure <- function(input, output, session) {
  # output$MarketStructurePlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/2 - Renewables/Electricity/MarketStructure.csv",
  #       header = TRUE,
  #       sep = ",",
  #       na.strings = "-"
  #     )
  #
  #   YearLow <- as.numeric(min(RenEn$Year))
  #   YearHigh <- as.numeric(max(RenEn$Year +1))
  #
  #   dygraph(RenEn, main = "Renewable Energy Target") %>%
  #     dyAxis("y", label = "% Progress", valueRange = c(0,30)) %>%
  #     dyAxis("x", label = "Year", drawGrid = TRUE) %>%
  #     dyOptions(colors =  c("Green","Orange", "Blue")) %>%
  #     dyLegend(width = 170 ,
  #              labelsSeparateLines = TRUE ,
  #              show = "always") %>%
  #     dyOptions(
  #       stackedGraph = TRUE,
  #       axisLineColor = "white",
  #       gridLineColor = "white",
  #       includeZero = TRUE,
  #       fillAlpha = .65
  #     ) %>%
  #     #    dyRangeSelector() %>%
  #     dyCSS("Structure/2 - Renewables/Electricity/legend.css")
  #
  # })
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("MarketStructure.R")
  ###### Renewable Energy ###### ######


  MarketStructure <- read_csv("Processed Data/Output/Consumers/MarketShare.csv")
  
  ChartColours <- c("#4292c6", "#7bccc4", "#08519c", "#ef3b2c")
  sourcecaption = "Source: BEIS"
  plottitle = "Market Shares, combined electricity and gas"
  
  ### From ESD ###
  
  output$MarketStructureSubtitle <- renderText({
    
    paste("Scotland, December 2019")
  })
  
  output$MarketStructurePlot <- renderPlotly({
    
    MarketStructure$Region <- paste0("<b>", str_wrap(MarketStructure$Region, 6), "</b>")
    
    ChartColours <- c("#5d8be1", "#FF8500")
    BarColours <-
      c(
        "#31a354",
        "#0868ac",
        "#43a2ca",
        "#7bccc4",
        "#a6bddb",
        "#d0d1e6",
        "#bdbdbd",
        "#969696"
      )
    
    
    p <- plot_ly(data = MarketStructure, y = ~ Region) %>%
      add_trace(
        data = MarketStructure,
        x = ~ `Large`,
        Region = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Large Suppliers",
        text = paste0("Large Suppliers: ", percent(MarketStructure$`Large`, 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = MarketStructure,
        x = ~ `Medium`,
        Region = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Medium Suppliers",
        text = paste0("Medium Suppliers: ", percent(MarketStructure$`Medium`, 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      
      add_trace(
        data = MarketStructure,
        x = ~ `Small`,
        Region = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Small Suppliers",
        text = paste0("Small Suppliers: ", percent(MarketStructure$`Small`,0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(
          title = "",
          showgrid = FALSE,
          autorange = "reversed"
          
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F)
    
    p
    
  })
  
  
  output$MarketStructureTable = renderDataTable({
    

    datatable(
      MarketStructure,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Proportion of customers who have switched energy supplier",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of customers who have switched energy supplier',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of customers who have switched energy supplier')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(2:5), 1)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/MarketStructure.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("MarketStructureTable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$MarketStructure.png <- downloadHandler(
    filename = "MarketStructure.png",
    content = function(file) {
      
      Data <- read_csv("Processed Data/Output/Consumers/MarketSwitching.csv")
      
      names(Data)[1] <- "Year"
      
      MarketSwitch <- Data  
      
      ChartColours <- c("#4292c6", "#7bccc4", "#08519c", "#ef3b2c")
      sourcecaption = "Source: BEIS"
      plottitle = "Proportion of customers who have switched\nenergy supplier (gas and electricity\ncombined, rolling 12 months)"
      
      ChartWidth <- (max(MarketSwitch$Year) - min(MarketSwitch$Year))
      
      MarketSwitch$Year <- as.Date(MarketSwitch$Year, format = "%d/%m/%Y")  
      
      MarketSwitchChart <- MarketSwitch %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        geom_line(
          aes(y = `GB`,
              label = paste0(`GB` * 100, "%")),
          colour = ChartColours[4],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - ChartWidth*0.055,
            y = `GB`,
            label = ifelse(Year == min(Year), percent(`GB`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + ChartWidth*0.070,
            y = `GB`,
            label = ifelse(Year == max(Year), percent(`GB`,  accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(MarketSwitch, 1),
          aes(x = Year,
              y = `GB`,
              show_guide = FALSE),
          size = 4,
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(MarketSwitch$Year),
          y = mean(MarketSwitch$`GB`),
          label = "GB",
          hjust = 0.5,
          vjust = 1.65,
          colour = ChartColours[4],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `North Scotland`,
              label = percent(`North Scotland`)),
          colour = ChartColours[2],
          size = 1.5,
          family = "Century Gothic",
          linetype = "dashed"
        ) +
        geom_text(
          aes(
            x = Year - ChartWidth*0.055,
            y = `North Scotland`,
            label = ifelse(Year == min(Year), percent(`North Scotland`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + ChartWidth*0.070,
            y = `North Scotland`,
            label = ifelse(Year == max(Year), percent(`North Scotland`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = .5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(MarketSwitch, 1),
          aes(x = Year,
              y = `North Scotland`,
              show_guide = FALSE),
          colour = ChartColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(MarketSwitch$Year),
          y = mean(MarketSwitch$`North Scotland`),
          label = "North Scotland",
          hjust = 0.5,
          vjust = 4,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `South Scotland`,
              label = paste0(`South Scotland` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic",
          linetype = "dashed"
        ) +
        geom_text(
          aes(
            x = Year - ChartWidth*0.055,
            y = `South Scotland`,
            label = ifelse(Year == min(Year), percent(`South Scotland`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + ChartWidth*0.070,
            y = `South Scotland`,
            label = ifelse(Year == max(Year), percent(`South Scotland`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = .5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(MarketSwitch, 1),
          aes(x = Year,
              y = `South Scotland`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(MarketSwitch$Year),
          y = mean(MarketSwitch$`South Scotland`),
          label = "South Scotland",
          hjust = 0.5,
          vjust = -1.2,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Whole Scotland`,
              label = paste0(`Whole Scotland` * 100, "%")),
          colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - ChartWidth*0.055,
            y = `Whole Scotland`,
            label = ifelse(Year == min(Year), percent(`Whole Scotland`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + ChartWidth*0.070,
            y = `Whole Scotland`,
            label = ifelse(Year == max(Year), percent(`Whole Scotland`,  accuracy = .1), ""),
            hjust = 0.5,
            vjust = 1.4,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(MarketSwitch, 1),
          aes(x = Year,
              y = `Whole Scotland`,
              
              show_guide = FALSE),
          size = 4,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(MarketSwitch$Year),
          y = mean(MarketSwitch$`Whole Scotland`),
          label = "Whole Scotland",
          hjust = 0.5,
          vjust = 3,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              format(Year, format = "%B %Y"),
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      MarketSwitchChart
      
      MarketSwitchChart <-
        DailyChart(MarketSwitchChart,
                   MarketSwitch,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      
      MarketSwitchChart <- MarketSwitchChart +
        coord_cartesian(xlim = c(min(MarketSwitch$Year)-(max(MarketSwitch$Year) - min(MarketSwitch$Year))*0.05, max(MarketSwitch$Year)+(max(MarketSwitch$Year) - min(MarketSwitch$Year))*0.1))
      
      MarketSwitchChart
      
      ggsave(
        file,
        plot =  MarketSwitchChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
