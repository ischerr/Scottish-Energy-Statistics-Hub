require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

WallInsulationOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Proportion of eligible homes with wall insulation", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('WallInsulationSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('WallInsulation.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("WallInsulationPlot")),
    plotlyOutput(ns("WallInsulationPlot"))%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    tabsetPanel(
      tabPanel("Proportion with Wall Insulation",
               fluidRow(
    column(10, h3("Data - Proportion with Wall Insulation", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("WallInsulationTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Impact of Measures",
             fluidRow(
               column(10, h3("Data - Impact of Measures", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("WallInsulationImpactTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
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
WallInsulation <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("WallInsulation.R")

  
  output$WallInsulationSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Wall insulation", skip = 12,  col_names = FALSE)
    
    Data <- as_tibble(t(Data))[1:4]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "Cavity", "Solid", "Total")
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    WallInsulation <- Data
    
    
    paste("Scotland,", min(WallInsulation$Year),"-", max(WallInsulation$Year))
  })
  
  output$WallInsulationPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Wall insulation", skip = 12,  col_names = FALSE)
    
    Data <- as_tibble(t(Data))[1:4]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "Cavity", "Solid", "Total")
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    WallInsulation <- Data
    
    ### variables
    ChartColours <- c("#34d1a3", "#8da0cb", "#fc8d62", "#34d1a3")
    
    WallInsulation$Year <- paste0("01/01/", WallInsulation$Year)
    
    WallInsulation$Year <- dmy(WallInsulation$Year)
    
    
    p <-  plot_ly(WallInsulation,x = ~ Year ) %>% 
      add_trace(data = WallInsulation,
                x = ~ Year,
                y = ~ Total,
                name = "Total",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Total: ",
                  percent(WallInsulation$Total, accuracy = 0.1),
                  "\nYear: ",
                  format(WallInsulation$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(WallInsulation[which(WallInsulation$Total > 0 | WallInsulation$Total < 0),], 1),
        x = ~ Year,
        y = ~ `Total`,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Total: ",
          percent(WallInsulation[which(WallInsulation$Total > 0 | WallInsulation$Total < 0),][-1,]$Total, accuracy = 0.1),
          "\nYear: ",
          format(WallInsulation[which(WallInsulation$Total > 0 | WallInsulation$Total < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(data = WallInsulation,
                x = ~ Year,
                y = ~ Cavity,
                name = "Cavity",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Cavity: ",
                  percent(WallInsulation$Cavity, accuracy = 0.1),
                  "\nYear: ",
                  format(WallInsulation$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "dash")
      ) %>% 
      add_trace(
        data = tail(WallInsulation[which(WallInsulation$Cavity > 0 | WallInsulation$Cavity < 0),], 1),
        x = ~ Year,
        y = ~ `Cavity`,
        legendgroup = "2",
        name = "Cavity",
        text = paste0(
          "Cavity: ",
          percent(WallInsulation[which(WallInsulation$Cavity > 0 | WallInsulation$Cavity < 0),][-1,]$Cavity, accuracy = 0.1),
          "\nYear: ",
          format(WallInsulation[which(WallInsulation$Cavity > 0 | WallInsulation$Cavity < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = WallInsulation,
                x = ~ Year,
                y = ~ Solid,
                name = "Solid",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Solid: ",
                  percent(WallInsulation$Solid, accuracy = 0.1),
                  "\nYear: ",
                  format(WallInsulation$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "dash")
      ) %>% 
      add_trace(
        data = tail(WallInsulation[which(WallInsulation$Solid > 0 | WallInsulation$Solid < 0),], 1),
        x = ~ Year,
        y = ~ `Solid`,
        legendgroup = "3",
        name = "Solid",
        text = paste0(
          "Solid: ",
          percent(WallInsulation[which(WallInsulation$Solid > 0 | WallInsulation$Solid < 0),][-1,]$Solid, accuracy = 0.1),
          "\nYear: ",
          format(WallInsulation[which(WallInsulation$Solid > 0 | WallInsulation$Solid < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#34d1a3"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',

        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(WallInsulation$Year)-100, max(WallInsulation$Year)+100)),
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
  
  
  output$WallInsulationTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Wall insulation", skip = 12,  col_names = FALSE)
    
    Data <- as_tibble(t(Data))[1:4]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "Cavity Wall Dwellings with Cavity Wall Insulation", "Solid Wall Dwellings with Solid Wall Insulation", "Total Dwellings with Wall Insulation")
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    WallInsulation <- Data
    
    datatable(
      WallInsulation,
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
        title = "Proportion of eligible homes with Wall Insulation",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of eligible homes with Wall Insulation',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of eligible homes with Wall Insulation')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:4, 1)
  })
  
  output$WallInsulationImpactTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Wall insulation", skip = 20,  col_names = FALSE)
    
    Data <- as_tibble(t(Data))[c(1:3,7:8)]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "Median Saving in Consumption (kWh) - Cavity Wall Insulation", "Median percentage saving - Cavity Wall", "Median Saving in Consumption (kWh) - Solid Wall Insulation", "Median percentage saving - Solid Wall")
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    WallInsulation <- Data
    
    datatable(
      WallInsulation,
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
        title = "Impact of Measures - Wall Insulation",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Impact of Measures - Wall Insulation',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Impact of Measures - Wall Insulation')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(3,5), 1) %>% 
      formatRound(c(2,4), 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Efficiency Measures/WallInsulation.txt")[2])
                     
                   )))
  })
 
  observeEvent(input$ToggleTable, {
    toggle("WallInsulationTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("WallInsulationImpactTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$WallInsulation.png <- downloadHandler(
    filename = "WallInsulation.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Wall insulation", skip = 12,  col_names = FALSE)
      
      Data <- as_tibble(t(Data))[1:4]
      
      Data <- tail(Data, -1)
      
      names(Data) <- c("Year", "Cavity", "Solid", "Total")
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      WallInsulation <- Data
      
      ### variables
      ChartColours <- c("#34d1a3", "#8da0cb", "#fc8d62", "#34d1a3")
      
      sourcecaption = "Source: SG"
      plottitle = "Proportion of eligible homes\nwith wall insulation"
      
      #WallInsulation$CavityPercentage <- PercentLabel(WallInsulation$Cavity)
      
      
      WallInsulationChart <- WallInsulation %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Cavity,
            colour = ChartColours[2],
            label = percent(Cavity, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic",
          linetype = 2
        ) +
        geom_text(
          aes(
            x = Year,
            y = Cavity,
            label = ifelse(Year == min(Year), percent(Cavity, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = -.8,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Cavity,
            label = ifelse(Year == max(Year), percent(Cavity, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(WallInsulation, 1),
          aes(
            x = Year,
            y = Cavity,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(Cavity),
            label = "Cavity",
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = Solid,
            colour = ChartColours[3],
            label = paste0(Solid * 100, "%")
          ),
          size = 1.5,
          family = "Century Gothic",
          linetype = 2
        ) +
        geom_text(
          aes(
            x = Year,
            y = Solid,
            label = ifelse(Year == min(Year), percent(Solid, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = -.8,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Solid,
            label = ifelse(Year == max(Year), percent(Solid, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(WallInsulation, 1),
          aes(
            x = Year,
            y = Solid,
            colour = ChartColours[3],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(Solid),
            label = "Solid",
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Total,
              colour = ChartColours[4]),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Total,
            label = ifelse(Year == min(Year), percent(Total, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = -.8,
            colour = ChartColours[4],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Total,
            label = ifelse(Year == max(Year), percent(Total, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[4],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(WallInsulation, 1),
          aes(
            x = Year,
            y = Total,
            colour = ChartColours[4],
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
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[4],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) |
                             Year == min(Year), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      WallInsulationChart <-
        LinePercentChart(WallInsulationChart,
                         WallInsulation,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      ggsave(
        file,
        plot =  WallInsulationChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
