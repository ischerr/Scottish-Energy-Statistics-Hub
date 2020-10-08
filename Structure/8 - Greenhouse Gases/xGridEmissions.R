require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

xGridEmissionsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Average greenhouse gas emissions per kilowatt hour of electricity", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('xGridEmissionsSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('xGridEmissions.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("xGridEmissionsPlot")),
    plotlyOutput(ns("xGridEmissionsPlot"))%>% withSpinner(color="#39ab2c"),
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
      column(12, dataTableOutput(ns("xGridEmissionsTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
xGridEmissions <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("xGridEmissions.R")
  
  output$xGridEmissionsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Grid emissions", skip = 15)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    Data <- setDT(Data, keep.rownames = TRUE)[]
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    names(Data) <- c("Year", "Renewables")
    
    xGridEmissions <- Data
    
    paste("Scotland,", min(xGridEmissions$Year),"-", max(xGridEmissions$Year))
  })
  
  output$xGridEmissionsPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                                sheet = "Grid emissions", skip = 15)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    Data <- setDT(Data, keep.rownames = TRUE)[]
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    names(Data) <- c("Year", "Renewables")
    
    xGridEmissions <- Data
    
    plottitle <- "Average greenhouse gas emissions per kilowatt hour of electricity"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500")
    
    xGridEmissions$Year <- paste0("01/01/", xGridEmissions$Year)
    
    xGridEmissions$Year <- dmy(xGridEmissions$Year)
    
    
    p <-  plot_ly(xGridEmissions,x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "xGridEmissions",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  round(xGridEmissions$Renewables, digits = 1),
                  " gCO2e/kWh\nYear: ",
                  format(xGridEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(xGridEmissions[which(xGridEmissions$Renewables > 0 | xGridEmissions$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "xGridEmissions",
        legendgroup = "1",
        text = paste0(
          round(xGridEmissions[which(xGridEmissions$Renewables > 0 | xGridEmissions$Renewables < 0),][-1,]$Renewables, digits = 1),
          " gCO2e/kWh\nYear: ",
          format(xGridEmissions[which(xGridEmissions$Renewables > 0 | xGridEmissions$Renewables < 0),][-1,]$Year, "%Y")
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
                     range = c(min(xGridEmissions$Year)-100, max(xGridEmissions$Year)+100)),
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
  
  
  output$xGridEmissionsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                                sheet = "Grid emissions", skip = 15)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    Data <- setDT(Data, keep.rownames = TRUE)[]
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    names(Data) <- c("Year", "Grid emissions (gCO2e/kWh)")
    
    xGridEmissions <- Data
    
    datatable(
      xGridEmissions,
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
        title = "Grid Emissions (Average Greenhouse Gas Emissions per kilowatt hour of electricity)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Grid Emissions (Average Greenhouse Gas Emissions per kilowatt hour of electricity)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Grid Emissions (Average Greenhouse Gas Emissions per kilowatt hour of electricity)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2, 1) 
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/8 - Greenhouse Gases/xGridEmissions.txt")[2])
                     
                   )))
  })
 
  observeEvent(input$ToggleTable, {
    toggle("xGridEmissionsTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$xGridEmissions.png <- downloadHandler(
    filename = "xGridEmissions.png",
    content = function(file) {

      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                                  sheet = "Grid emissions", skip = 15)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      Data <- setDT(Data, keep.rownames = TRUE)[]
      colnames(Data) <- as.character(unlist(Data[1,]))
      Data = Data[-1, ]
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      names(Data) <- c("Year", "Renewables")
      
      xGridEmissions <- Data
      
      xGridEmissions$Renewables <-
        round(xGridEmissions$Renewables, digits = 1)
      
      plottitle <- "Average greenhouse gas emissions per kilowatt hour of electricity"
      sourcecaption <- "Source: BEIS"
      ChartColours <- c("#39ab2c", "#FF8500")
      
      xGridEmissionsChart <-
        xGridEmissions %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
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
            x = Year-1,
            y = Renewables,
            label = ifelse(Year == min(Year), paste(Renewables,"\ngCO2e/kWh"), ""),
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year+1,
            y = Renewables,
            label = ifelse(Year == max(Year), paste(sprintf("%.1f", Renewables),"\ngCO2e/kWh"), ""),
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic",
          vjust = -.5
        ) +
        geom_point(
          data = tail(xGridEmissions, 1),
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
      
      
      xGridEmissionsChart <-
        LinePercentChart(xGridEmissionsChart,
                         xGridEmissions,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      xGridEmissionsChart <- xGridEmissionsChart +
        xlim(min(xGridEmissions$Year)-1,max(xGridEmissions$Year)+1)+
        ylim(-15,max(xGridEmissions$Renewables)+20)
      
      xGridEmissionsChart

      
      ggsave(
        file,
        plot = xGridEmissionsChart,
        width = 26,
        height = 10,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
