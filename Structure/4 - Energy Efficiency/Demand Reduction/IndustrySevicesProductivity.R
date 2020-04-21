require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

IndustrySevicesProductivityOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Industrial energy productivity",
    fluidRow(column(8,
                    h3("Industrial energy productivity", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('IndustrialProductivitySubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('IndustrialProductivity.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("StockEPCPlot")),
    plotlyOutput(ns("IndustrialProductivityPlot"))%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Industrial Emissions",
             fluidRow(column(8,
                             h3("Industrial emissions intensity", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('IndustrialEmissionsSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('IndustrialEmissions.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("StockEPCPlot")),
             plotlyOutput(ns("IndustrialEmissionsPlot"))%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Services Productivity",
             fluidRow(column(8,
                             h3("Services energy productivity", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('ServicesProductivitySubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ServicesProductivity.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("StockEPCPlot")),
             plotlyOutput(ns("ServicesProductivityPlot"))%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Services Emissions",
             fluidRow(column(8,
                             h3("Services emissions intensity", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('ServicesEmissionsSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ServicesEmissions.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("StockEPCPlot")),
             plotlyOutput(ns("ServicesEmissionsPlot"))%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    tabsetPanel(
      tabPanel("Industrial",
    fluidRow(
    column(10, h3("Data - Industrial energy productivity", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("IndustrialTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Services",
             fluidRow(
               column(10, h3("Data - Services energy productivity", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ServicesTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
    fluidRow(
      column(1,
             p("Next update:")),
      column(2,
             DateLookup(c("BEISSubNatEnergy", "BEISSubNatFuel", "SGEmissions", "SGQNASSector"))),
      column(1, align = "right",
             p("Sources:")),
      column(
        8,
        align = "right",
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("BEISSubNatFuel"),
        SourceLookup("SGEmissions"),
        SourceLookup("SGQNASSector")
      )
    )
  )
}




###### Server ######
IndustrySevicesProductivity <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("IndustrySevicesProductivity.R")
  
  Data <- read_excel("Structure/CurrentWorking.xlsx", 
                     sheet = "Industry services productivity", skip = 20)
  
  Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
  Data <- setDT(Data, keep.rownames = TRUE)[]
  colnames(Data) <- as.character(unlist(Data[1,]))
  Data = Data[-1, ]
  
  Data <- as_tibble(sapply( Data, as.numeric ))
  
  names(Data)[2] <- "Year"
  
  Data <- Data[c(2,7,8,16,17)]

  
  output$IndustrialProductivitySubtitle <- renderText({
    
    IndustryProductivity <- Data[c(1,2)]
    
    IndustryProductivity <- IndustryProductivity[complete.cases(IndustryProductivity),]
    
    names(IndustryProductivity) <- c("Year", "GVA")
    
    paste("Scotland,", min(IndustryProductivity$Year, na.rm = TRUE),"-", max(IndustryProductivity$Year, na.rm = TRUE))
  })
  
  output$IndustrialProductivityPlot <- renderPlotly  ({
    
    IndustryProductivity <- Data[c(1,2)]
    
    IndustryProductivity <- IndustryProductivity[complete.cases(IndustryProductivity),]
    
    names(IndustryProductivity) <- c("Year", "GVA")
    
    plottitle <- "Industrial energy productivity"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#34d1a3", "#FF8500")
    
    IndustryProductivity$Year <-
      paste0("01/01/", IndustryProductivity$Year)
    
    IndustryProductivity$Year <- dmy(IndustryProductivity$Year)
    
    p <-  plot_ly(
      IndustryProductivity,
      x = ~ Year,
      y = ~ GVA,
      name = "GVA",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "GVA: ",
        round(IndustryProductivity$GVA, digits = 2),
        " GVA m per GWh\nYear: ",
        format(IndustryProductivity$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = IndustryProductivity[nrow(IndustryProductivity), ],
        x = ~ Year,
        y = ~ `GVA`,
        name = "GVA",
        text = paste0(
          "GVA: ",
          round(IndustryProductivity$GVA, digits = 2),
          " \u00A3GVA m per GWh\nYear: ",
          format(IndustryProductivity$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[1])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(IndustryProductivity$Year)-100, max(IndustryProductivity$Year)+100)),
        yaxis = list(
          title = "\u00A3GVA m per GWh",
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
  
  output$IndustrialProductivity.png <- downloadHandler(
    filename = "IndustrialProductivity.png",
    content = function(file) {
      
      
      
      
      IndustryProductivity <- Data[c(1,2)]
      
      IndustryProductivity <- IndustryProductivity[complete.cases(IndustryProductivity),]
      
      names(IndustryProductivity) <- c("Year", "GVA")
      
      length <- max(IndustryProductivity$Year) - min(IndustryProductivity$Year)
      
      plottitle <- "Industrial energy productivity"
      sourcecaption <- "Source: BEIS, SG"
      ChartColours <- c("#34d1a3", "#FF8500")
      
      IndustryProductivityChart <-
        IndustryProductivity %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = GVA,
              colour = ChartColours[1],
              label = GVA),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == min(Year), paste(round(GVA, digits = 2),"\n\u00A3GVA m per GWh"), ""),
            colour = ChartColours[1],
            fontface = 2,
            vjust = 1.25
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == max(Year), paste(round(GVA, digits = 2),"\n\u00A3GVA m per GWh"), ""),
            colour = ChartColours[1],
            fontface = 2, 
            vjust = 1.25
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(IndustryProductivity, 1),
          aes(
            x = Year,
            y = GVA,
            colour = ChartColours[1],
            label = GVA,
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
                             Year == min(Year), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      IndustryProductivityChart <-
        LinePercentChart(IndustryProductivityChart,
                         IndustryProductivity,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      IndustryProductivityChart <- IndustryProductivityChart +
        xlim(min(IndustryProductivity$Year)-length*0.2,max(IndustryProductivity$Year)+length*0.2) +
        ylim(-0.01,max(IndustryProductivity$GVA + 0.05))
      
      IndustryProductivityChart
      
      
      ggsave(
        file,
        plot = IndustryProductivityChart,
        width = 13,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )  
  
  output$IndustrialEmissionsSubtitle <- renderText({
    
    IndustrialEmissions <- Data[c(1,3)]
    
    IndustrialEmissions <- IndustrialEmissions[complete.cases(IndustrialEmissions),]
    
    names(IndustrialEmissions) <- c("Year", "GVA")
    
    paste("Scotland,", min(IndustrialEmissions$Year, na.rm = TRUE),"-", max(IndustrialEmissions$Year, na.rm = TRUE))
  })
  
  output$IndustrialEmissionsPlot <- renderPlotly  ({
    
    IndustrialEmissions <- Data[c(1,3)]
    
    IndustrialEmissions <- IndustrialEmissions[complete.cases(IndustrialEmissions),]
    
    names(IndustrialEmissions) <- c("Year", "GVA")
    
    plottitle <- "Industrial emissions intensity"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#34d1a3", "#FF8500")
    
    IndustrialEmissions$Year <-
      paste0("01/01/", IndustrialEmissions$Year)
    
    IndustrialEmissions$Year <- dmy(IndustrialEmissions$Year)
    
    p <-  plot_ly(
      IndustrialEmissions,
      x = ~ Year,
      y = ~ GVA,
      name = "GVA",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "GVA: ",
        round(IndustrialEmissions$GVA, digits = 2),
        " tCO2e/\u00A3mGVA\nYear: ",
        format(IndustrialEmissions$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = IndustrialEmissions[nrow(IndustrialEmissions), ],
        x = ~ Year,
        y = ~ `GVA`,
        name = "GVA",
        text = paste0(
          "GVA: ",
          round(IndustrialEmissions$GVA, digits = 2),
          " tCO2e/\u00A3mGVA\nYear: ",
          format(IndustrialEmissions$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[1])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(IndustrialEmissions$Year)-100, max(IndustrialEmissions$Year)+100)),
        yaxis = list(
          title = "tCO2e/\u00A3mGVA",
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
  
  output$IndustrialEmissions.png <- downloadHandler(
    filename = "IndustrialEmissions.png",
    content = function(file) {
      
      
      IndustryEmissionsIntensity <- Data[c(1,3)]
      
      IndustryEmissionsIntensity <- IndustryEmissionsIntensity[complete.cases(IndustryEmissionsIntensity),]
      
      names(IndustryEmissionsIntensity) <- c("Year", "GVA")
      
      plottitle <- "Industrial emissions intensity"
      sourcecaption <- "Source: SG"
      ChartColours <- c("#34d1a3", "#FF8500")
      
      length <- max(IndustryEmissionsIntensity$Year) - min(IndustryEmissionsIntensity$Year)
      
      IndustryEmissionsIntensityChart <-
        IndustryEmissionsIntensity %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = GVA,
              colour = ChartColours[1],
              label = GVA),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == min(Year), paste(round(GVA, digits = 2),"\ntCO2e/\u00A3mGVA"), ""),
            colour = ChartColours[1],
            fontface = 2,
            vjust = 2
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == max(Year), paste(round(GVA, digits = 2),"\ntCO2e/\u00A3mGVA"), ""),
            colour = ChartColours[1],
            fontface = 2,
            vjust = -1
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(IndustryEmissionsIntensity, 1),
          aes(
            x = Year,
            y = GVA,
            colour = ChartColours[1],
            label = GVA,
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
                             Year == min(Year), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      IndustryEmissionsIntensityChart <-
        LinePercentChart(IndustryEmissionsIntensityChart,
                         IndustryEmissionsIntensity,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      IndustryEmissionsIntensityChart <- IndustryEmissionsIntensityChart +
        xlim(min(IndustryEmissionsIntensity$Year)-(length*0.2),max(IndustryEmissionsIntensity$Year)+(length*0.2)) +
        ylim(-10,max(IndustryEmissionsIntensity$GVA + 20))
      
      IndustryEmissionsIntensityChart
      
      
      ggsave(
       file,
        plot = IndustryEmissionsIntensityChart,
        width = 13,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  output$IndustrialTable = renderDataTable({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Industry services productivity", skip = 22,  col_names = FALSE, n_max = 7)
    
    Data <- as_tibble(t(Data))[c(1:4,6:7)]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data[Data == 0] <- NA
    
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
        title = "Industrial energy productivity, Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Industrial energy productivity, Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Industrial energy productivity, Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:6, 0) %>% 
      formatRound(4,1) %>% 
      formatRound(5,2)
  })
  
  observeEvent(input$ToggleTable, {
    toggle("IndustrialTable")
  })
  
  output$ServicesProductivitySubtitle <- renderText({
    
    ServicesProductivity <- Data[c(1,4)]
    
    ServicesProductivity <- ServicesProductivity[complete.cases(ServicesProductivity),]
    
    names(ServicesProductivity) <- c("Year", "GVA")
    
    paste("Scotland,", min(ServicesProductivity$Year, na.rm = TRUE),"-", max(ServicesProductivity$Year, na.rm = TRUE))
  })
  
  output$ServicesProductivityPlot <- renderPlotly  ({
    
    ServicesProductivity <- Data[c(1,4)]
    
    ServicesProductivity <- ServicesProductivity[complete.cases(ServicesProductivity),]
    
    names(ServicesProductivity) <- c("Year", "GVA")
    
    plottitle <- "Services energy productivity"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#34d1a3", "#FF8500")
    
    ServicesProductivity$Year <-
      paste0("01/01/", ServicesProductivity$Year)
    
    ServicesProductivity$Year <- dmy(ServicesProductivity$Year)
    
    p <-  plot_ly(
      ServicesProductivity,
      x = ~ Year,
      y = ~ GVA,
      name = "GVA",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "GVA: ",
        round(ServicesProductivity$GVA, digits = 2),
        " GVA m per GWh\nYear: ",
        format(ServicesProductivity$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = ServicesProductivity[nrow(ServicesProductivity), ],
        x = ~ Year,
        y = ~ `GVA`,
        name = "GVA",
        text = paste0(
          "GVA: ",
          round(ServicesProductivity$GVA, digits = 2),
          " \u00A3GVA m per GWh\nYear: ",
          format(ServicesProductivity$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[1])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(ServicesProductivity$Year)-100, max(ServicesProductivity$Year)+100)),
        yaxis = list(
          title = "\u00A3GVA m per GWh",
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
  
  output$ServicesProductivity.png <- downloadHandler(
    filename = "ServicesProductivity.png",
    content = function(file) {
      
      
      ServicesProductivity <- Data[c(1,4)]
      
      ServicesProductivity <- ServicesProductivity[complete.cases(ServicesProductivity),]
      
      names(ServicesProductivity) <- c("Year", "GVA")
      
      length <- max(ServicesProductivity$Year) - min(ServicesProductivity$Year)
      
      plottitle <- "Services energy productivity"
      sourcecaption <- "Source: BEIS, SG"
      ChartColours <- c("#34d1a3", "#FF8500")
      
      ServicesProductivityChart <-
        ServicesProductivity %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = GVA,
              colour = ChartColours[1],
              label = GVA),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == min(Year), paste(round(GVA, digits = 2),"\n\u00A3GVA m per GWh"), ""),
            colour = ChartColours[1],
            fontface = 2,
            vjust = 1.2
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == max(Year), paste(round(GVA, digits = 2),"\n\u00A3GVA m per GWh"), ""),
            colour = ChartColours[1],
            fontface = 2,
            vjust = 1.2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ServicesProductivity, 1),
          aes(
            x = Year,
            y = GVA,
            colour = ChartColours[1],
            label = GVA,
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
                             Year == min(Year), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      ServicesProductivityChart <-
        LinePercentChart(ServicesProductivityChart,
                         ServicesProductivity,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      ServicesProductivityChart <- ServicesProductivityChart +
        xlim(min(ServicesProductivity$Year)-length*0.2,max(ServicesProductivity$Year)+length*0.2) +
        ylim(-0.01,max(ServicesProductivity$GVA + 0.05))
      
      ServicesProductivityChart
      
      
      ggsave(
        file,
        plot = ServicesProductivityChart,
        width = 13,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )  
  
  
  output$ServicesEmissionsSubtitle <- renderText({
    
    ServicesEmissions <- Data[c(1,5)]
    
    ServicesEmissions <- ServicesEmissions[complete.cases(ServicesEmissions),]
    
    names(ServicesEmissions) <- c("Year", "GVA")
    
    paste("Scotland,", min(ServicesEmissions$Year, na.rm = TRUE),"-", max(ServicesEmissions$Year, na.rm = TRUE))
  })
  
  output$ServicesEmissionsPlot <- renderPlotly  ({
    
    ServicesEmissions <- Data[c(1,5)]
    
    ServicesEmissions <- ServicesEmissions[complete.cases(ServicesEmissions),]
    
    names(ServicesEmissions) <- c("Year", "GVA")
    
    plottitle <- "Services emissions intensity"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#34d1a3", "#FF8500")
    
    ServicesEmissions$Year <-
      paste0("01/01/", ServicesEmissions$Year)
    
    ServicesEmissions$Year <- dmy(ServicesEmissions$Year)
    
    p <-  plot_ly(
      ServicesEmissions,
      x = ~ Year,
      y = ~ GVA,
      name = "GVA",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "GVA: ",
        round(ServicesEmissions$GVA, digits = 2),
        " tCO2e/\u00A3mGVA\nYear: ",
        format(ServicesEmissions$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = ServicesEmissions[nrow(ServicesEmissions), ],
        x = ~ Year,
        y = ~ `GVA`,
        name = "GVA",
        text = paste0(
          "GVA: ",
          round(ServicesEmissions$GVA, digits = 2),
          " tCO2e/\u00A3mGVA\nYear: ",
          format(ServicesEmissions$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[1])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(ServicesEmissions$Year)-100, max(ServicesEmissions$Year)+100)),
        yaxis = list(
          title = "tCO2e/\u00A3mGVA",
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
  
  output$ServicesEmissions.png <- downloadHandler(
    filename = "ServicesEmissions.png",
    content = function(file) {
      
      
      ServicesEmissionsIntensity <- Data[c(1,5)]
      
      ServicesEmissionsIntensity <- ServicesEmissionsIntensity[complete.cases(ServicesEmissionsIntensity),]
      
      names(ServicesEmissionsIntensity) <- c("Year", "GVA")
      
      plottitle <- "Services emissions intensity"
      sourcecaption <- "Source: SG"
      ChartColours <- c("#34d1a3", "#FF8500")
      
      ServicesEmissionsIntensityChart <-
        ServicesEmissionsIntensity %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = GVA,
              colour = ChartColours[1],
              label = GVA),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == min(Year), paste(round(GVA, digits = 2),"\ntCO2e/\u00A3mGVA"), ""),
            colour = ChartColours[1],
            fontface = 2,
            vjust = -0.5
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == max(Year), paste(round(GVA, digits = 2),"\ntCO2e/\u00A3mGVA"), ""),
            colour = ChartColours[1],
            fontface = 2,
            vjust = 1.5
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ServicesEmissionsIntensity, 1),
          aes(
            x = Year,
            y = GVA,
            colour = ChartColours[1],
            label = GVA,
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
                             Year == min(Year), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      ServicesEmissionsIntensityChart <-
        LinePercentChart(ServicesEmissionsIntensityChart,
                         ServicesEmissionsIntensity,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      ServicesEmissionsIntensityChart <- ServicesEmissionsIntensityChart +
        xlim(min(ServicesEmissionsIntensity$Year)-3,max(ServicesEmissionsIntensity$Year)+3) +
        ylim(-1,max(ServicesEmissionsIntensity$GVA + 6))
      
      ServicesEmissionsIntensityChart
      
      
      ggsave(
        file,
        plot = ServicesEmissionsIntensityChart,
        width = 13,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )  
  
  output$ServicesTable = renderDataTable({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Industry services productivity", skip = 31,  col_names = FALSE, n_max = 7)
    
    Data <- as_tibble(t(Data))[c(1:4,6:7)]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data[Data == 0] <- NA
    
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
        title = "Services energy productivity, Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Services energy productivity, Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Services energy productivity, Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:6, 0) %>% 
      formatRound(4,1) %>% 
      formatRound(5,2)
  })  
  
  observeEvent(input$ToggleTable2, {
    toggle("ServicesTable")
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Demand Reduction/IndustrySevicesProductivity.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
}
