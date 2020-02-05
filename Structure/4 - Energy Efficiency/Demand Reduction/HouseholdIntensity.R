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
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
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
HouseholdIntensity <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("HouseholdIntensity.R")

  Data <- read_excel(
    "Structure/CurrentWorking.xlsx",
    sheet = "Household energy intensity",
    col_names = FALSE,
    skip = 15,
    n_max = 7
  )
  
  Data <- as_tibble(t(Data))
  
  names(Data) <- unlist(Data[1,])
  
  Data <- Data[-1,]
  
  names(Data)[1] <- "Year"
  
  Data %<>% lapply(function(x) as.numeric(as.character(x)))
  
  Data <- as_tibble(Data)
  
  Data <- Data[c(1,5)]
  
  Data <- Data[complete.cases(Data),]
  
  output$HouseholdIntensitySubtitle <- renderText({
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
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
          title = "kWh / households)",
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
      formatRound(2:4, 0)%>%
      formatPercentage(5:6, 1)
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
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")

      HouseholdIntensity <- Data
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
}
