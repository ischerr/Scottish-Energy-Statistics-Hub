require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

HouseholdIntensityOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Energy Intensity",
    fluidRow(column(8,
                    h3("Average household energy intensity", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('HouseholdIntensitySubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('HouseholdIntensity.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("HouseholdIntensityPlot")),
    plotlyOutput(ns("HouseholdIntensityPlot"))%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Emissions Intensity",
             fluidRow(column(8,
                             h3("Average household emissions intensity", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('EmissionsIntensitySubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EmissionsIntensity.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("EmissionsIntensityPlot")),
             plotlyOutput(ns("EmissionsIntensityPlot"))%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10, h3("Data - Average household energy intensity", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("HouseholdIntensityTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
      column(2, p("Next update:")),
      column(2,
             DateLookup(c("BEISSubNatEnergy", "BEISUKConsump", "SGEmissions", "NRSHouseholds"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("BEISUKConsump"),
        SourceLookup("SGEmissions"),
        SourceLookup("NRSHouseholds")

        
      )
    )
  )
}




###### Server ######
HouseholdIntensity <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("HouseholdIntensity.R")

  Data <- read_excel(
    "Structure/CurrentWorking.xlsx",
    sheet = "Household energy intensity",
    col_names = FALSE,
    skip = 22,
    n_max = 10
  )
  
  Data <- as_tibble(t(Data))
  
  names(Data) <- unlist(Data[1,])
  
  Data <- Data[-1,]
  
  names(Data)[1] <- "Year"
  
  Data %<>% lapply(function(x) as.numeric(as.character(x)))
  
  Data <- as_tibble(Data)
  
  output$HouseholdIntensitySubtitle <- renderText({
    
    paste("Scotland,", min(Data[which(Data$`Energy intensity (kWh / households)` >0),]$Year, na.rm = TRUE),"-", max(Data[which(Data$`Energy intensity (kWh / households)` >0),]$Year, na.rm = TRUE))
  })
  
  output$HouseholdIntensityPlot <- renderPlotly  ({
    

    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
    
    
    p <-  plot_ly(Data,x = ~ Year ) %>% 
      add_trace(data = Data,
                x = ~ Year,
                y = ~ `Energy intensity (kWh / households)`,
                name = "Energy intensity (kWh / households)",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Energy intensity (kWh / households): ",
                  format(round(Data$`Energy intensity (kWh / households)`, digits = 0), big.mark = ","),
                  "\nYear: ",
                  paste(Data$Year)
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      )  %>% 
      add_trace(
        data = tail(Data[which(Data$`Energy intensity (kWh / households)` > 0 | Data$`Energy intensity (kWh / households)` < 0),], 1),
        x = ~ Year,
        y = ~ `Energy intensity (kWh / households)`,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Energy intensity (kWh / households): ",
          format(round(Data[which(Data$`Energy intensity (kWh / households)` > 0 | Data$`Energy intensity (kWh / households)` < 0),][-1,]$`Energy intensity (kWh / households)`, digits = 0), big.mark = ","),
          "\nYear: ",
          paste(Data[which(Data$`Energy intensity (kWh / households)` > 0 | Data$`Energy intensity (kWh / households)` < 0),][-1,]$Year)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
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
                     showgrid = FALSE),
        yaxis = list(
          title = "(kWh / households)",
          tickformat = ",n",
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
  
  
  output$HouseholdIntensityTable = renderDataTable({
    
    datatable(
      Data,
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
        title = "Average household energy intensity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average household energy intensity',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average household energy intensity')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:9), 0) %>% 
      formatPercentage(c(6,7,9,10),1) %>% 
      formatRound(c(3,8),1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Demand Reduction/HouseholdIntensity.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("HouseholdIntensityTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$HouseholdIntensity.png <- downloadHandler(
    filename = "HouseholdIntensity.png",
    content = function(file) {


      plottitle <-
        "Average household energy intensity\n(kWh / households)"
      sourcecaption <- "Source: BEIS, SG"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")

      HouseholdIntensity <- Data[c(1,5)]
      
      HouseholdIntensity <- HouseholdIntensity[complete.cases(HouseholdIntensity),]
      
      HouseholdIntensityChart <- HouseholdIntensity %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = `Energy intensity (kWh / households)`,
            colour = ChartColours[2],
            label = `Energy intensity (kWh / households)`
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Energy intensity (kWh / households)`,
            label = ifelse(Year == min(Year), paste0(format(round(`Energy intensity (kWh / households)`, digits = 0),big.mark = ",")), ""),
            hjust = 0.5,
            vjust = 2.2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Energy intensity (kWh / households)`,
            label = ifelse(Year == max(Year), paste0(format(round(`Energy intensity (kWh / households)`, digits = 0),big.mark = ",")), ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(HouseholdIntensity, 1),
          aes(
            x = Year,
            y = `Energy intensity (kWh / households)`,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
         geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) |
                             Year == min(Year), paste0(Year), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      HouseholdIntensityChart <-
        LinePercentChart(HouseholdIntensityChart,
                         HouseholdIntensity,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      HouseholdIntensityChart <- HouseholdIntensityChart +
        labs(subtitle = paste0("Scotland, ",min(HouseholdIntensity$Year),  " - ", max(HouseholdIntensity$Year)))
      
      HouseholdIntensityChart
      
      ggsave(
        file,
        plot =  HouseholdIntensityChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  
  
  
  output$EmissionsIntensitySubtitle <- renderText({
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$EmissionsIntensityPlot <- renderPlotly  ({
    
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
    
    
    p <-  plot_ly(Data,x = ~ Year ) %>% 
      add_trace(data = Data,
                x = ~ Year,
                y = ~ `Emissions intensity (tCO2 / households)`,
                name = "Emissions intensity (tCO2 / households)",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Emissions intensity (tCO2 / households): ",
                  format(round(Data$`Emissions intensity (tCO2 / households)`, digits = 1), big.mark = ","),
                  "\nYear: ",
                  paste(Data$Year)
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      )  %>% 
      add_trace(
        data = tail(Data[which(Data$`Emissions intensity (tCO2 / households)` > 0 | Data$`Emissions intensity (tCO2 / households)` < 0),], 1),
        x = ~ Year,
        y = ~ `Emissions intensity (tCO2 / households)`,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Emissions intensity (tCO2 / households): ",
          format(round(Data[which(Data$`Emissions intensity (tCO2 / households)` > 0 | Data$`Emissions intensity (tCO2 / households)` < 0),][-1,]$`Emissions intensity (tCO2 / households)`, digits = 1), big.mark = ","),
          "\nYear: ",
          paste(Data[which(Data$`Emissions intensity (tCO2 / households)` > 0 | Data$`Emissions intensity (tCO2 / households)` < 0),][-1,]$Year)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
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
                     showgrid = FALSE),
        yaxis = list(
          title = "(tCO2 / households)",
          tickformat = ",n",
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
  
  observeEvent(input$ToggleTable2, {
    toggle("EmissionsIntensityTable")
  })
  
  output$EmissionsIntensity.png <- downloadHandler(
    filename = "EmissionsIntensity.png",
    content = function(file) {
      
      
      plottitle <-
        "Average household Emissions intensity\n(tCO2 / households)"
      sourcecaption <- "Source: BEIS, SG"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
      
      EmissionsIntensity <- Data
      EmissionsIntensityChart <- EmissionsIntensity %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = `Emissions intensity (tCO2 / households)`,
            colour = ChartColours[2],
            label = `Emissions intensity (tCO2 / households)`
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Emissions intensity (tCO2 / households)`,
            label = ifelse(Year == min(Year), paste0(format(round(`Emissions intensity (tCO2 / households)`, digits = 1),big.mark = ",")), ""),
            hjust = 0.5,
            vjust = 2.2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Emissions intensity (tCO2 / households)`,
            label = ifelse(Year == max(Year), paste0(format(round(`Emissions intensity (tCO2 / households)`, digits = 1),big.mark = ",")), ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EmissionsIntensity, 1),
          aes(
            x = Year,
            y = `Emissions intensity (tCO2 / households)`,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) |
                             Year == min(Year), paste0(Year), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      EmissionsIntensityChart <-
        LinePercentChart(EmissionsIntensityChart,
                         EmissionsIntensity,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      EmissionsIntensityChart <- EmissionsIntensityChart +
        labs(subtitle = paste0("Scotland, ",min(EmissionsIntensity$Year),  " - ", max(EmissionsIntensity$Year)))
      
      EmissionsIntensityChart
      
      ggsave(
        file,
        plot =  EmissionsIntensityChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
