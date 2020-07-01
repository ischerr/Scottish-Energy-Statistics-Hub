require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

OilGasExportsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("International sales of oil and gas produced in Scotland", style = "color: #126992;  font-weight:bold"),
                    h4(textOutput(ns('OilGasExportsSubtitle')), style = "color: #;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('OilGasExports.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    #dygraphOutput(ns("OilGasExportsPlot")),
    plotlyOutput(ns("OilGasExportsPlot"))%>% withSpinner(color="#126992"),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #126992;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
               fluidRow(
    column(10, h3("Data - International sales of oil and gas produced in Scotland", style = "color: #126992;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("OilGasExportsTable"))%>% withSpinner(color="#126992"))),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    fluidRow(
      column(1,
             p("Next update:")),
      column(2,
             DateLookup(c("SGOilGasSatellite"))),
      column(1, align = "right",
             p("Sources:")),
      column(
        8,
        align = "right",
        SourceLookup("SGOilGasSatellite")
        
      )
    )
  )
}




###### Server ######
OilGasExports <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("OilGasExports.R")

  
  output$OilGasExportsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Oil and gas exports", skip = 12, col_names = TRUE)[1:4]
    
    names(Data) <- c("Year", "rUK", "rWorld", "Total")
    
    Data <- Data[complete.cases(Data),]
    
    Data$Year <- as.numeric(substr(Data$Year, 1,4))
    
    OilGasExports <- Data

    paste("Scotland,", min(OilGasExports$Year),"-", max(OilGasExports$Year))
  })
  
  output$OilGasExportsPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Oil and gas exports", skip = 12, col_names = TRUE)[1:4]
    
    names(Data) <- c("Year", "rUK", "rWorld", "Total")
    
    Data <- Data[complete.cases(Data),]
    
    Data$Year <- as.numeric(substr(Data$Year, 1,4))
    
    Data[2:4] %<>% lapply(function(x) x / 1000)
    
    OilGasExports <- Data
    ### variables
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    sourcecaption = "Source: Scottish Government"
    plottitle = "International sales from oil and gas supply chain"
    
    OilGasExports$Year <- paste0("01/01/", OilGasExports$Year)
    
    OilGasExports$Year <- dmy(OilGasExports$Year)
    
    
    p <-  plot_ly(OilGasExports,x = ~ Year ) %>% 
      add_trace(data = OilGasExports,
                x = ~ Year,
                y = ~ Total,
                name = "Total Exports",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Total: \u00A3",
                  round(OilGasExports$Total, digits = 3),
                  " billion\nYear: ",
                  format(OilGasExports$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(OilGasExports[which(OilGasExports$Total > 0 | OilGasExports$Total < 0),], 1),
        x = ~ Year,
        y = ~ Total,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Rest of the World: \u00A3",
          round(OilGasExports[which(OilGasExports$Total > 0 | OilGasExports$Total < 0),][-1,]$Total, digits = 3),
          " billion\nYear: ",
          format(OilGasExports[which(OilGasExports$Total > 0 | OilGasExports$Total < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      )  %>%  
      add_trace(data = OilGasExports,
                x = ~ Year,
                y = ~ rUK,
                name = "Rest of the U.K.",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Rest of the U.K.: \u00A3",
                  round(OilGasExports$rUK, digits = 3),
                  " billion\nYear: ",
                  format(OilGasExports$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(OilGasExports[which(OilGasExports$rUK > 0 | OilGasExports$rUK < 0),], 1),
        x = ~ Year,
        y = ~ rUK,
        legendgroup = "2",
        name = "Rest of the U.K.",
        text = paste0(
          "Rest of the U.K.: \u00A3",
          round(OilGasExports[which(OilGasExports$rUK > 0 | OilGasExports$rUK < 0),][-1,]$rUK, digits = 3),
          " billion\nYear: ",
          format(OilGasExports[which(OilGasExports$rUK > 0 | OilGasExports$rUK < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      )  %>% 
      add_trace(data = OilGasExports,
                x = ~ Year,
                y = ~ rWorld,
                name = "Rest of the World",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Rest of the World: \u00A3",
                  round(OilGasExports$rWorld, digits = 3),
                  " billion\nYear: ",
                  format(OilGasExports$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(OilGasExports[which(OilGasExports$rWorld > 0 | OilGasExports$rWorld < 0),], 1),
        x = ~ Year,
        y = ~ rWorld,
        legendgroup = "3",
        name = "Rest of the World",
        text = paste0(
          "Rest of the World: \u00A3",
          round(OilGasExports[which(OilGasExports$rWorld > 0 | OilGasExports$rWorld < 0),][-1,]$rWorld, digits = 3),
          " billion\nYear: ",
          format(OilGasExports[which(OilGasExports$rWorld > 0 | OilGasExports$rWorld < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      )  %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',

        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(OilGasExports$Year)-100, max(OilGasExports$Year)+100)),
        yaxis = list(
          title = "\u00A3 Billion",
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
  
  
  output$OilGasExportsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Oil and gas exports", skip = 12, col_names = TRUE)[1:4]
    
    names(Data) <- c("Year", "Exports to the rest of the UK (\u00A3bn)", "Exports to the rest of the World (\u00A3bn)", "Total Exports (\u00A3bn)")
    
    Data <- Data[complete.cases(Data),]
    
    Data$Year <- as.numeric(substr(Data$Year, 1,4))
    
    Data[2:4] %<>% lapply(function(x) x / 1000)
    
    OilGasExports <- Data
    
    datatable(
      OilGasExports,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
		scrollX = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "International sales of oil and gas produced in Scotland (\u00A3billion)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'International sales of oil and gas produced in Scotland (\u00A3billion)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'International sales of oil and gas produced in Scotland (\u00A3billion)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:4), 3)
  })
  

 output$Text <- renderUI({
   tagList(column(12,
                                   
                                     HTML(
                                       paste(readtext("Structure/7 - Oil Gas/OilGasExports.txt")[2])
                                     
                                   )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("OilGasExportsTable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$OilGasExports.png <- downloadHandler(
    filename = "OilGasExports.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Oil and gas exports", skip = 12, col_names = TRUE)
      
      names(Data) <- c("Year", "rUK", "rWorld", "Total")
      
      Data$Year <- as.numeric(substr(Data$Year, 1,4))
      
      Data <- Data[complete.cases(Data),]
      
      OilGasSupply <- Data
      ### variables
      ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
      sourcecaption = "Source: Scottish Government"
      plottitle = "International sales of oil and gas produced in Scotland"
      
      #OilGasSupply$OilPercentage <- PercentLabel(OilGasSupply$Oil)
      
      
      OilGasSupplyChart <- OilGasSupply %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        geom_line(
          aes(
            y = rUK,
            colour = ChartColours[2],
            label = percent(rUK, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = rWorld,
            colour = ChartColours[3],
            label = percent(rUK, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = Total,
            colour = ChartColours[1],
            label = percent(rUK, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-1,
            y = rUK,
            label = ifelse(Year == min(Year), paste0("\u00A3", format(round(rUK, digits = 0),big.mark = ",", trim = TRUE), "\nmillion"), ""),
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic",
          vjust = 0
        ) +
        geom_text(
          aes(
            x = Year+1,
            y = rUK,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(rUK, digits = 0),big.mark = "," , trim = TRUE), "\nmillion"), ""),
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(OilGasSupply, 1),
          aes(
            x = Year,
            y = rUK,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        # geom_text(
        #   aes(
        #     x = mean(Year),
        #     y = mean(rUK),
        #     label = "rUK\nr.U.K.\nRest of United Kingdom\nRemainder of the United Kingdom of Great Britain and Northern Ireland\nOutwith Scotland",
        #     hjust = 0.5,
        #     vjust = 1,
        #     colour = ChartColours[2],
        #     fontface = 2
        #   ),
        #   family = "Century Gothic"
      # ) +
      geom_text(
        aes(
          x = Year-1,
          y = rWorld,
          label = ifelse(Year == min(Year), paste0("\u00A3", format(round(rWorld, digits = 0),big.mark = "," , trim = TRUE), "\nmillion"), ""),
          colour = ChartColours[3],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
        geom_text(
          aes(
            x = Year+1,
            y = rWorld,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(rWorld, digits = 0), big.mark = ",", trim = TRUE), "\nmillion"), ""),
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(OilGasSupply, 1),
          aes(
            x = Year,
            y = rWorld,
            colour = ChartColours[3],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(rWorld),
            label = "Rest of\n World",
            hjust = 0.5,
            vjust = 1.4,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-1,
            y = Total,
            label = ifelse(Year == min(Year), paste0("\u00A3", format(round(Total, digits = 0),big.mark = ",", trim = TRUE), "\nmillion"), ""),
            colour = ChartColours[1],
            fontface = 2
          ),
          vjust = -.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+1,
            y = Total,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(Total, digits = 0),big.mark = ",", trim = TRUE), "\nmillion"), ""),
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(OilGasSupply, 1),
          aes(
            x = Year,
            y = Total,
            colour = ChartColours[1],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(Total),
            label = "Total",
            hjust = 1.5,
            vjust = -5.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) |
                             Year == min(Year), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(rUK),
            label = "Rest of\n U.K.",
            hjust = 0.5,
            vjust = 1,
            colour = ChartColours[2],
            fontface = 2
          )) +
        
        ### 0 Axis
        
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
        
        
        ### Plot Borders
        annotate(
          geom = 'segment',
          y = Inf,
          yend = Inf,
          color = ChartColours[1],
          x = -Inf,
          xend = Inf,
          size = 1.5
        ) +
        annotate(
          geom = 'segment',
          y = -Inf,
          yend = -Inf,
          color = ChartColours[1],
          x = -Inf,
          xend = Inf,
          size = 1
        )
      
      OilGasSupplyChart <-
        DailyChart(OilGasSupplyChart,
                   OilGasSupply,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      OilGasSupplyChart <- OilGasSupplyChart +
        xlim(min(OilGasSupply$Year) -1.5 , max(OilGasSupply$Year) +1.5)+
        ylim(-1000, max(OilGasSupply$Total))
      
      OilGasSupplyChart
      
      ggsave(
        file,
        plot =  OilGasSupplyChart,
        width = 21,
        height = 10,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
