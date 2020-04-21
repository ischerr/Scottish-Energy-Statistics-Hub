require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

DisplacedEmissionsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Estimated million tonnes of CO2 emissions displaced by renewables", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('DisplacedEmissionsSubtitle')), style = "color: #39ab2c;")
    ),
    column(
      4, style = 'padding:15px;',
      downloadButton(ns('DisplacedEmissions.png'), 'Download Graph', style="float:right")
    )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("DisplacedEmissionsPlot")),
    plotlyOutput(ns("DisplacedEmissionsPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("DisplacedEmissionsTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(2, p("Next update:")),
      column(2,
             DateLookup(c("BEISDUKES", "BEISElecGen"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISElecGen"),
        SourceLookup("BEISDUKES")
        
      )
    )
  )
}




###### Server ######
DisplacedEmissions <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("missions displaced renewables.R")
  
  output$DisplacedEmissionsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Emissions displaced renewables", skip = 15, col_names = FALSE)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    names(Data) <- c("Year", "Renewables")
    Data = Data[-1, ]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Displacement <- Data
    
    
    Displacement$Renewables <-
      round(Displacement$Renewables, digits = 1)
    
    paste("Scotland,", min(Displacement$Year),"-", max(Displacement$Year))
  })
  
  output$DisplacedEmissionsPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Emissions displaced renewables", skip = 15, col_names = FALSE)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    names(Data) <- c("Year", "Renewables")
    Data = Data[-1, ]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Displacement <- Data
    
    
    Displacement$Renewables <-
      round(Displacement$Renewables, digits = 1)
    
    plottitle <- "Estimated million tonnes of CO2 emissions\ndisplaced by renewables"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500")
    
    Displacement$Year <- paste0("01/01/", Displacement$Year)
    
    Displacement$Year <- dmy(Displacement$Year)
    
    
    p <-  plot_ly(Displacement,x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "DisplacedEmissions",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  round(Displacement$Renewables, digits = 1),
                  " MtCO2e\nYear: ",
                  format(Displacement$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(Displacement[which(Displacement$Renewables > 0 | Displacement$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "DisplacedEmissions",
        legendgroup = "1",
        text = paste0(
          round(Displacement[which(Displacement$Renewables > 0 | Displacement$Renewables < 0),][-1,]$Renewables, digits = 1),
          " MtCO2e\nYear: ",
          format(Displacement[which(Displacement$Renewables > 0 | Displacement$Renewables < 0),][-1,]$Year, "%Y")
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
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(Displacement$Year)-100, max(Displacement$Year)+100)),
        yaxis = list(
          title = "MtCO2e",
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
  
  
  output$DisplacedEmissionsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Emissions displaced renewables", skip = 15, col_names = FALSE)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    names(Data) <- c("Year", "Million tonnes of CO2 emissions displaced by renewables")
    Data = Data[-1, ]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Displacement <- Data
    
    RenFuel <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Fuel Readable", skip = 1)[c(1,3)]
    
    names(RenFuel) <- c("Year", "Renewables")
    
    Emissions <- read_excel("Structure/CurrentWorking.xlsx", 
                            sheet = "R - DUKES Emissions", col_names = FALSE)
    
    Emissions <- as_tibble(t(Emissions))[c(1,4)]
    
    names(Emissions) <- c("Year", "Emissions")
    
    Emissions$Year <- as.numeric(Emissions$Year)
    
    Emissions$Emissions <- as.numeric(Emissions$Emissions)
    
    Displacement <- merge(Displacement, RenFuel)
    
    Displacement <- merge(Displacement, Emissions)
    
    names(Displacement) <- c("Year", "Million tonnes of CO2 emissions displaced by renewables", "Renewable electricity generated (GWh)", " Estimated carbon dioxide emissions per GWh of electricity
supplied - All fossil fuels (tonnes of CO2 per GWh)")
    
    datatable(
      Displacement,
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
        title = "Estimated million tonnes of CO2 emissions displaced by renewables",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Estimated million tonnes of CO2 emissions displaced by renewables',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Estimated million tonnes of CO2 emissions displaced by renewables')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2, 1) %>% 
      formatRound(3:4, 0)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/DisplacedEmissions.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("DisplacedEmissionsTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$DisplacedEmissions.png <- downloadHandler(
    filename = "DisplacedEmissions.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Emissions displaced renewables", skip = 15, col_names = FALSE)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      names(Data) <- c("Year", "Renewables")
      Data = Data[-1, ]
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      Displacement <- Data
      
      
      Displacement$Renewables <-
        round(Displacement$Renewables, digits = 1)
      
      plottitle <- "Estimated million tonnes of CO2 emissions displaced by renewables"
      sourcecaption <- "Source: BEIS"
      ChartColours <- c("#39ab2c", "#FF8500")
      
      DisplacementChart <-
        Displacement %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Renewables,
              colour = ChartColours[1],
              label = Renewables),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == min(Year), Renewables, ""),
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == max(Year), Renewables, ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(Displacement, 1),
          aes(
            x = Year,
            y = Renewables,
            colour = ChartColours[1],
            label = Renewables,
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
      
      
      DisplacementChart <-
        LinePercentChart(DisplacementChart,
                         Displacement,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      DisplacementChart
      
      DisplacementChart <- DisplacementChart +
        ylim(-.5, max(Displacement$Renewables))
      ggsave(
        file,
        plot = DisplacementChart,
        width = 26,
        height = 10,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
