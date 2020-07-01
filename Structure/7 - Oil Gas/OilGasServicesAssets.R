require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

OilGasServicesAssetsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Services",
               fluidRow(column(8,
                               h3("Value of fossil fuel services", style = "color: #126992;  font-weight:bold"),
                               h4(textOutput(ns('OilGasServicesSubtitle')), style = "color: #126992;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('OilGasServices.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
               #dygraphOutput(ns("OilGasServicesAssetsPlot")),
               plotlyOutput(ns("OilGasServicesPlot"))%>% withSpinner(color="#126992"),
               tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;")),
      tabPanel("Assets",
               fluidRow(column(8,
                               h3("Value of fossil fuel assets", style = "color: #126992;  font-weight:bold"),
                               h4(textOutput(ns('OilGasAssetsSubtitle')), style = "color: #126992;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('OilGasAssets.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
               #dygraphOutput(ns("OilGasServicesAssetsPlot")),
               plotlyOutput(ns("OilGasAssetsPlot"))%>% withSpinner(color="#126992"),
               tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"))),
    fluidRow(
      column(10,h3("Commentary", style = "color: #126992;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    fluidRow(
      column(10, h3("Data - Value of fossil fuel services and assets", style = "color: #126992;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("OilGasServicesTable"))%>% withSpinner(color="#126992"))),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("ONSNatural"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
             SourceLookup("ONSNatural")
             
      )
    )
  )
}




###### Server ######
OilGasServicesAssets <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("OilGasServicesAssets.R")
  
  
  output$OilGasServicesSubtitle <- renderText({
    
    FossilFuels <- read_delim("Processed Data/Output/Services and assets/FossilFuels.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste0("Scotland, ", min(FossilFuels$Year), " - ", max(FossilFuels$Year))
  })
  
  output$OilGasServicesPlot <- renderPlotly  ({
    
    
    FossilFuels <- read_delim("Processed Data/Output/Services and assets/FossilFuels.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ### variables
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    sourcecaption = "Source: ONS, SG"
    plottitle = "Value of fossil fuel services"
    
    
    p <-  plot_ly(FossilFuels,x = ~ Year ) %>% 
      add_trace(data = FossilFuels,
                x = ~ Year,
                y = ~ Annual,
                name = "Services",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Value of fossil fuel services: \u00A3",
                  round(FossilFuels$Annual, digits = 1),
                  " billion\nYear: ",
                  paste(FossilFuels$Year)
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      )  %>% 
      add_trace(
        data = tail(FossilFuels[which(FossilFuels$Annual > 0 | FossilFuels$Annual < 0),], 1),
        x = ~ Year,
        y = ~ Annual,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Value of fossil fuel services: \u00A3",
          round(FossilFuels[which(FossilFuels$Annual > 0 | FossilFuels$Annual < 0),][-1,]$Annual, digits = 1),
          " billion\nYear: ",
          paste(FossilFuels[which(FossilFuels$Annual > 0 | FossilFuels$Annual < 0),][-1,]$Year)
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
  
  output$OilGasServicesTable = renderDataTable({
    
    FossilFuels <- read_delim("Processed Data/Output/Services and assets/FossilFuels.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(FossilFuels) <- c("Year", "Value of fossil fuel services (\u00A3 bn)", "Value of fossil fuel assets (\u00A3 bn)")
    
    datatable(
      FossilFuels,
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
        title = "Value of fossil fuel energy services and assets (\u00A3 billion)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Value of fossil fuel energy services and assets (\u00A3 billion)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Value of fossil fuel energy services and assets (\u00A3 billion)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:3), 1) 
  })
  output$OilGasServices.png <- downloadHandler(
    filename = "OilGasServicesAssets.png",
    content = function(file) {
      
      
      FossilFuels <- read_delim("Processed Data/Output/Services and assets/FossilFuels.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      ### variables
      ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: ONS, SG"
      plottitle = "Value of fossil fuel services"
      
      #FossilFuels$OilPercentage <- PercentLabel(FossilFuels$Oil)
      
      
      FossilFuelsChart <- FossilFuels %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Annual,
            colour = ChartColours[2],
            label = percent(Annual, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Annual,
            label = ifelse(Year == min(Year), paste0("\u00A3", format(round(Annual, digits = 1),nsmall = 1, trim = TRUE), " billion"), ""),
            hjust = 1.1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Annual,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(Annual, digits = 1),nsmall = 1, trim = TRUE), " billion"), ""),
            hjust = 0.5,
            vjust = -1.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(FossilFuels, 1),
          aes(
            x = Year,
            y = Annual,
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
      
      
      FossilFuelsChart <-
        LinePercentChart(FossilFuelsChart,
                         FossilFuels,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      FossilFuelsChart <- FossilFuelsChart +
        xlim(min(FossilFuels$Year)-2, max(FossilFuels$Year))+
        labs(subtitle = paste0("Scotland, ",min(FossilFuels$Year), " - ", max(FossilFuels$Year)+1))
      
      FossilFuelsChart
      
      ggsave(
        file,
        plot =  FossilFuelsChart,
        width = 26,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
      
    }
  ) 
  
  observeEvent(input$ToggleTable, {
    toggle("OilGasServicesTable")
  })
  
  
  
  
  output$OilGasAssetsSubtitle <- renderText({
    
    FossilFuels <- read_delim("Processed Data/Output/Services and assets/FossilFuels.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste0("Scotland, ", min(FossilFuels$Year), " - ", max(FossilFuels$Year))
  })
  
  output$OilGasAssetsPlot <- renderPlotly  ({
    
    
    FossilFuels <- read_delim("Processed Data/Output/Services and assets/FossilFuels.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ### variables
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    sourcecaption = "Source: ONS, SG"
    plottitle = "Value of fossil fuel Assets"
    
    
    p <-  plot_ly(FossilFuels,x = ~ Year ) %>% 
      add_trace(data = FossilFuels,
                x = ~ Year,
                y = ~ Asset,
                name = "Asset",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Value of fossil fuel assets: \u00A3",
                  round(FossilFuels$Asset, digits = 1),
                  " billion\nYear: ",
                  paste(FossilFuels$Year)
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      )  %>% 
      add_trace(
        data = tail(FossilFuels[which(FossilFuels$Asset > 0 | FossilFuels$Asset < 0),], 1),
        x = ~ Year,
        y = ~ Asset,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Value of fossil fuel assets: \u00A3",
          round(FossilFuels[which(FossilFuels$Asset > 0 | FossilFuels$Asset < 0),][-1,]$Asset, digits = 1),
          " billion\nYear: ",
          paste(FossilFuels[which(FossilFuels$Asset > 0 | FossilFuels$Asset < 0),][-1,]$Year)
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
  
  output$OilGasAssetsTable = renderDataTable({
    
    FossilFuels <- read_delim("Processed Data/Output/Services and assets/FossilFuels.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(FossilFuels) <- c("Year", "Value of fossil fuel Assets (\u00A3 bn)", "Value of fossil fuel assets (\u00A3 bn)")
    
    datatable(
      FossilFuels,
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
        title = "Oil and gas revenue (\u00A3 billion)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Oil and gas revenue (\u00A3 billion)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Oil and gas revenue (\u00A3 billion)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:3), 1) 
  })
  output$OilGasAssets.png <- downloadHandler(
    filename = "OilGasAssetsAssets.png",
    content = function(file) {
      
      
      FossilFuels <- read_delim("Processed Data/Output/Services and assets/FossilFuels.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      ### variables
      ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: ONS, SG"
      plottitle = "Value of fossil fuel Assets"
      
      #FossilFuels$OilPercentage <- PercentLabel(FossilFuels$Oil)
      
      
      FossilFuelsChart <- FossilFuels %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Asset,
            colour = ChartColours[2],
            label = percent(Asset, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Asset,
            label = ifelse(Year == min(Year), paste0("\u00A3", format(round(Asset, digits = 1),nsmall = 1, trim = TRUE), " billion"), ""),
            hjust = 1.1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Asset,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(Asset, digits = 1),nsmall = 1, trim = TRUE), " billion"), ""),
            hjust = 0.5,
            vjust = -1.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(FossilFuels, 1),
          aes(
            x = Year,
            y = Asset,
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
      
      
      FossilFuelsChart <-
        LinePercentChart(FossilFuelsChart,
                         FossilFuels,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      FossilFuelsChart <- FossilFuelsChart +
        xlim(min(FossilFuels$Year)-2, max(FossilFuels$Year))+
        labs(subtitle = paste0("Scotland, ",min(FossilFuels$Year), " - ", max(FossilFuels$Year)+1))
      
      FossilFuelsChart
      
      ggsave(
        file,
        plot =  FossilFuelsChart,
        width = 26,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
      
    }
  ) 
  
  observeEvent(input$ToggleTable, {
    toggle("OilGasAssetsTable")
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/7 - Oil Gas/OilGasServicesAssets.txt")[2])
                     
                   )))
  })
  
  
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  
}
