require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenElecTargetOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Share of renewable electricity in gross final consumption", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenElecTargetSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecTarget.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenElecTargetPlot")),
    plotlyOutput(ns("RenElecTargetPlot"))%>% withSpinner(color="#39ab2c"),
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
      column(12, dataTableOutput(ns("RenElecTargetTable"))%>% withSpinner(color="#39ab2c"))),
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
RenElecTarget <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecTarget.R")

  
  output$RenElecTargetSubtitle <- renderText({
    
    RenElec <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable elec target", col_names = FALSE, 
                          skip = 15, n_max = 15)
    RenElec <- tail(RenElec[c(1,4)], -1)
    
    names(RenElec) <- c("Year", "Renewables")
    RenElec %<>% lapply(function(x) as.numeric(as.character(x)))
    RenElec <- as.data.frame(RenElec)
    
    paste("Scotland,", min(RenElec$Year),"-", max(RenElec$Year))
  })
  
  output$RenElecTargetPlot <- renderPlotly  ({
    
    RenElec <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable elec target", col_names = FALSE, 
                          skip = 15, n_max = 15)
    RenElec <- tail(RenElec[c(1,4)], -1)
    
    names(RenElec) <- c("Year", "Renewables")
    RenElec <- merge(RenElec, data.frame(Year = 2020, Renewables = NA, Tgt = 1), all = T)
    RenElec %<>% lapply(function(x) as.numeric(as.character(x)))
    RenElec <- as.data.frame(RenElec)
    ### variables
    ChartColours <- c("#39ab2c", "#FF8500")
    sourcecaption = "Source: BEIS"
    plottitle = "Share of renewable electricity in\ngross electricity consumption"
    
    RenElec$Year <- paste0("01/01/", RenElec$Year)
    
    RenElec$Year <- dmy(RenElec$Year)
    
    
    p <-  plot_ly(RenElec,x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "Renewables",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Progress: ",
                  percent(RenElec$Renewables, accuracy = 0.1),
                  "\nYear: ",
                  format(RenElec$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(RenElec[which(RenElec$Renewables > 0 | RenElec$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewable Electricity",
        text = paste0(
          "Progress: ",
          percent(RenElec[which(RenElec$Renewables > 0 | RenElec$Renewables < 0),][-1,]$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(RenElec[which(RenElec$Renewables > 0 | RenElec$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(
        data = RenElec,
        x = ~ Year,
        y = ~ Tgt,
        name = "Target",
        legendgroup = "2",
        text = paste0(
          "Target: ",
          percent(RenElec$Tgt, accuracy = 0.1),
          "\nYear: ",
          format(RenElec$Year, "%Y")
        ),
        hoverinfo = 'text',
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond",
                      color = ChartColours[2])
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
                     range = c(min(RenElec$Year)-100, max(RenElec$Year)+100)),
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
  
  
  output$RenElecTargetTable = renderDataTable({
    
    RenElec <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable elec target", col_names = FALSE, 
                          skip = 15)
    RenElec <- tail(RenElec, -1)
    
    names(RenElec) <- c("Year","Renewable Electricity (GWh)", "Gross Electricity Consumption (GWh)", "% Progress")

    RenElec %<>% lapply(function(x) as.numeric(as.character(x)))
    RenElec <- as_tibble(RenElec)
    
    datatable(
      RenElec,
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
        title = "Renewable Electricity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Electricity',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Electricity')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(4, 1) %>% 
      formatRound(2:3, 0)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenElecTarget.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("RenElecTargetTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenElecTarget.png <- downloadHandler(
    filename = "RenElecTarget.png",
    content = function(file) {

      RenElec <- read_excel("Structure/CurrentWorking.xlsx", 
                            sheet = "Renewable elec target", col_names = FALSE, 
                            skip = 15, n_max = 15)
      RenElec <- tail(RenElec[c(1,4)], -1)
      
      names(RenElec) <- c("Year", "Renewables")
      RenElec <- merge(RenElec, data.frame(Year = 2020, Renewables = NA, Tgt = 1), all = T)
      RenElec %<>% lapply(function(x) as.numeric(as.character(x)))
      RenElec <- as.data.frame(RenElec)
      ### variables
      ChartColours <- c("#39ab2c", "#FF8500")
      sourcecaption = "Source: BEIS"
      plottitle = "Share of renewable electricity in\ngross electricity consumption"
      
      RenElecChart <-
        TargetChart(RenElec, plottitle, sourcecaption, ChartColours)
      
      RenElecChart
      
      ggsave(
        file,
        plot = RenElecChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
}
