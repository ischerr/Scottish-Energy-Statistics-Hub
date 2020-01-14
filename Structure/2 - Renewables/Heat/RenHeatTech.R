require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenHeatTechOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Capacity",
    fluidRow(column(8,
                    h3("Renewable heat capacity by technology type", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenHeatTechSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenHeatTech.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenHeatTechPlot")),
    plotlyOutput(ns("RenHeatTechPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Output",
             fluidRow(column(8,
                             h3("Renewable heat output by technology type", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenHeatOutputSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenHeatOutput.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenHeatTechPlot")),
             plotlyOutput(ns("RenHeatOutputPlot"))%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Capacity",
    fluidRow(
    column(10, h3("Data - Capacity (GW)", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RenHeatTechTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Output",
      fluidRow(
        column(10, h3("Data - Output (GWh)", style = "color: #39ab2c;  font-weight:bold")),
        column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
      ),
      fluidRow(
        column(12, dataTableOutput(ns("RenHeatOutputTable"))%>% withSpinner(color="#39ab2c"))),
      tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
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
RenHeatTech <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenHeatTech.R")

  
  output$RenHeatTechSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 6)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    
    RenHeatCapTech <- Data
    
    paste("Scotland,", min(RenHeatCapTech$Year),"-", max(RenHeatCapTech$Year))
  })
  
  output$RenHeatTechPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 6)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    
    RenHeatCapTech <- Data
    
    plottitle <- "Renewable heat statistics by technology type"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500", "#FFFFFF")
    LineColours <- c("#fc4e2a","#feb24c","#fed976","#addd8e","#41ab5d")
    
    RenHeatCapTech$Year <- paste0("01/01/",  RenHeatCapTech$Year)
    
    RenHeatCapTech$Year <- dmy(RenHeatCapTech$Year)
    
    p <-  plot_ly(RenHeatCapTech, x = ~ Year ) %>% 
      add_trace(y = ~ Biomass,
                name = "Biomass",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Biomass: ",
                  round(RenHeatCapTech$Biomass, digits = 3),
                  " GW\nYear: ",
                  format(RenHeatCapTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(y = ~ CHP,
                name = "Biomass CHP",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Biomass CHP: ",
                  round(RenHeatCapTech$CHP, digits = 3),
                  " GW\nYear: ",
                  format(RenHeatCapTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[2], dash = "none")
      ) %>% 
      add_trace(y = ~ Waste,
                name = "Energy from waste",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Energy from waste: ",
                  round(RenHeatCapTech$Waste, digits = 3),
                  " GW\nYear: ",
                  format(RenHeatCapTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[3], dash = "none")
      ) %>% 
      add_trace(y = ~ Pumps,
                name = "Heat pumps",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "Heat pumps: ",
                  round(RenHeatCapTech$Pumps, digits = 3),
                  " GW\nYear: ",
                  format(RenHeatCapTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[4], dash = "none")
      ) %>% 
      add_trace(y = ~ Solar,
                name = "Solar thermal",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "5",
                text = paste0(
                  "Solar thermal: ",
                  round(RenHeatCapTech$Solar, digits = 3),
                  " GW\nYear: ",
                  format(RenHeatCapTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[5], dash = "none")
      ) %>% 
      add_trace(
        data = tail(RenHeatCapTech[which(RenHeatCapTech$`Biomass` != 0),], 1),
        x = ~ Year,
        y = ~ `Biomass`,
        name = "Biomass",
        legendgroup = "1",
        text = paste0(
          "Biomass: ",
          round(tail(RenHeatCapTech[which(RenHeatCapTech$`Biomass` != 0),], 1)$Biomass, digits = 3),
          " GW\nYear: ",
          format(tail(RenHeatCapTech[which(RenHeatCapTech$`Biomass` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>% 
      add_trace(
        data = tail(RenHeatCapTech[which(RenHeatCapTech$`CHP` != 0),], 1),
        x = ~ Year,
        y = ~ `CHP`,
        name = "Biomass CHP",
        legendgroup = "2",
        text = paste0(
          "Biomass CHP: ",
          round(tail(RenHeatCapTech[which(RenHeatCapTech$`CHP` != 0),], 1)$CHP, digits = 3),
          " GW\nYear: ",
          format(tail(RenHeatCapTech[which(RenHeatCapTech$`CHP` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[2])
      ) %>% 
      add_trace(
        data = tail(RenHeatCapTech[which(RenHeatCapTech$`Waste` != 0),], 1),
        x = ~ Year,
        y = ~ `Waste`,
        name = "Energy from waste",
        legendgroup = "3",
        text = paste0(
          "Energy from waste: ",
          round(tail(RenHeatCapTech[which(RenHeatCapTech$`Waste` != 0),], 1)$Waste, digits = 3),
          " GW\nYear: ",
          format(tail(RenHeatCapTech[which(RenHeatCapTech$`Waste` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[3])
      ) %>% 
      add_trace(
        data = tail(RenHeatCapTech[which(RenHeatCapTech$`Pumps` != 0),], 1),
        x = ~ Year,
        y = ~ `Pumps`,
        name = "Heat pumps",
        legendgroup = "4",
        text = paste0(
          "Heat pumps: ",
          round(tail(RenHeatCapTech[which(RenHeatCapTech$`Pumps` != 0),], 1)$Pumps, digits = 3),
          " GW\nYear: ",
          format(tail(RenHeatCapTech[which(RenHeatCapTech$`Pumps` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[4])
      ) %>% 
      add_trace(
        data = tail(RenHeatCapTech[which(RenHeatCapTech$`Solar` != 0),], 1),
        x = ~ Year,
        y = ~ `Solar`,
        name = "Solar thermal",
        legendgroup = "5",
        text = paste0(
          "Solar thermal: ",
          round(tail(RenHeatCapTech[which(RenHeatCapTech$`Solar` != 0),], 1)$Solar, digits = 3),
          " GW\nYear: ",
          format(tail(RenHeatCapTech[which(RenHeatCapTech$`Solar` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[5])
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
                     range = c(min(RenHeatCapTech$Year)-100, max(RenHeatCapTech$Year)+100)),
        yaxis = list(
          title = "GW",
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
  
  output$RenHeatOutputSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 6)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- arrange(Data, -row_number())
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    RenHeatOutputTech <- Data
    
    paste("Scotland,", min(RenHeatOutputTech$Year),"-", max(RenHeatOutputTech$Year))
  })
  
  output$RenHeatOutputPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 6)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- arrange(Data, -row_number())
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    RenHeatOutputTech <- Data
    
    plottitle <- "Renewable heat output by technology type"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500", "#FFFFFF")
    LineColours <- c("#fc4e2a","#feb24c","#fed976","#addd8e","#41ab5d")
    
    RenHeatOutputTech$Year <- paste0("01/01/",  RenHeatOutputTech$Year)
    
    RenHeatOutputTech$Year <- dmy(RenHeatOutputTech$Year)
    
    p <-  plot_ly(RenHeatOutputTech, x = ~ Year ) %>% 
      add_trace(y = ~ Biomass,
                name = "Biomass",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Biomass: ",
                  format(round(RenHeatOutputTech$Biomass, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(y = ~ CHP,
                name = "Biomass CHP",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Biomass CHP: ",
                  format(round(RenHeatOutputTech$CHP, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[2], dash = "none")
      ) %>% 
      add_trace(y = ~ Waste,
                name = "Energy from waste",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Energy from waste: ",
                  format(round(RenHeatOutputTech$Waste, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[3], dash = "none")
      ) %>% 
      add_trace(y = ~ Pumps,
                name = "Heat pumps",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "Heat pumps: ",
                  format(round(RenHeatOutputTech$Pumps, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[4], dash = "none")
      ) %>% 
      add_trace(y = ~ Solar,
                name = "Solar thermal",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "5",
                text = paste0(
                  "Solar thermal: ",
                  format(round(RenHeatOutputTech$Solar, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[5], dash = "none")
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`Biomass` != 0),], 1),
        x = ~ Year,
        y = ~ `Biomass`,
        name = "Biomass",
        legendgroup = "1",
        text = paste0(
          "Biomass: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Biomass` != 0),], 1)$Biomass, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Biomass` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`CHP` != 0),], 1),
        x = ~ Year,
        y = ~ `CHP`,
        name = "Biomass CHP",
        legendgroup = "2",
        text = paste0(
          "Biomass CHP: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`CHP` != 0),], 1)$CHP, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`CHP` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[2])
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`Waste` != 0),], 1),
        x = ~ Year,
        y = ~ `Waste`,
        name = "Energy from waste",
        legendgroup = "3",
        text = paste0(
          "Energy from waste: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Waste` != 0),], 1)$Waste, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Waste` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[3])
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`Pumps` != 0),], 1),
        x = ~ Year,
        y = ~ `Pumps`,
        name = "Heat pumps",
        legendgroup = "4",
        text = paste0(
          "Heat pumps: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Pumps` != 0),], 1)$Pumps, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Pumps` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[4])
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`Solar` != 0),], 1),
        x = ~ Year,
        y = ~ `Solar`,
        name = "Solar thermal",
        legendgroup = "5",
        text = paste0(
          "Solar thermal: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Solar` != 0),], 1)$Solar, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Solar` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[5])
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
                     range = c(min(RenHeatOutputTech$Year)-100, max(RenHeatOutputTech$Year)+100)),
        yaxis = list(
          title = "GWh",
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
  
  
  output$RenHeatTechTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 7)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "Biomass CHP", "Energy from waste", "Heat pumps", "Solar thermal", "Total")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    RenHeatTech <- Data
    
    datatable(
      RenHeatTech,
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
        title = "Renewable heat capacity by technology type (GW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Renewable heat capacity by technology type (GW)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Renewable heat capacity by technology type (GW)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:7), 3)
  })
  
  output$RenHeatOutputTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 7)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "Biomass CHP", "Energy from waste", "Heat pumps", "Solar thermal", "Total")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- arrange(Data, -row_number())
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    RenHeatOutputTech <- Data
    
    datatable(
      RenHeatOutputTech,
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
        title = "Renewable heat output by technology type (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Renewable heat output by technology type (GWh)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Renewable heat output by technology type (GWh)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:7), 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Heat/RenHeatTech.txt")[2])
                     
                   )))
  })
  
 observeEvent(input$ToggleTable1, {
    toggle("RenHeatTechTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("RenHeatOutputTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenHeatTech.png <- downloadHandler(
    filename = "RenHeatTech.png",
    content = function(file) {

      Data <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Renewable heat by tech type", col_names = FALSE, 
          skip = 13, n_max = 6)
      
      Data <- as.data.frame(t(Data))
      names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
      Data <- Data[complete.cases(Data),]
      
      RenHeatCapTech <- Data
      
      plottitle <- "Renewable heat capacity by technology type"
      sourcecaption <- "Source: BEIS"
      ChartColours <- c("#39ab2c", "#FF8500", "#FFFFFF")
      LineColours <- c("#fc4e2a","#feb24c","#fed976","#addd8e","#41ab5d")
      
      RenHeatCapTechChart <-
        RenHeatCapTech %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Biomass,
              label = Biomass),
          colour = LineColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = Biomass,
            label = ifelse(Year == min(Year), paste(round(Biomass, digits = 3), "GW"), ""),
            hjust = 1,
            
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = Biomass,
            label = ifelse(Year == max(Year), paste0("Biomass\n",round(Biomass, digits = 3), " GW"), ""),
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(RenHeatCapTech, 1),
          aes(
            x = Year,
            y = Biomass,
            label = Biomass,
            show_guide = FALSE
          ), 
          colour = LineColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = CHP,
              
              label = CHP),
          size = 1.5,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = CHP,
            label = ifelse(Year == min(Year), paste(round(CHP, digits = 3),"GW"), ""),
            hjust = 1,
            
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = CHP,
            label = ifelse(Year == max(Year), paste0("Biomass CHP\n",round(CHP, digits = 3), " GW"), ""),
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(RenHeatCapTech, 1),
          aes(
            x = Year,
            y = CHP,
            label = CHP,
            show_guide = FALSE
          ),
          colour = LineColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Waste,
              label = Waste),
          size = 1.5,
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = Waste,
            label = ifelse(Year == min(Year), paste(sprintf("%.3f", round(Waste, digits = 3)),"GW"), ""),
            hjust = 1,
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = Waste,
            label = ifelse(Year == max(Year), paste0("Waste\n",round(Waste, digits = 3), " GW"), ""),
            fontface = 2,
            vjust = 0.25
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(RenHeatCapTech, 1),
          aes(
            x = Year,
            y = Waste,
            label = Waste,
            show_guide = FALSE
          ),
          colour = LineColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Pumps,
              label = Pumps),
          size = 1.5,
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = Pumps,
            label = ifelse(Year == min(Year), paste(round(Pumps, digits = 3), "GW"), ""),
            hjust = 1,
            vjust= -1.5,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = Pumps,
            label = ifelse(Year == max(Year), paste0("Heat pumps\n", round(Pumps, digits = 3)," GW"), ""),
            vjust = -0.2,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(RenHeatCapTech, 1),
          aes(
            x = Year,
            y = Pumps,
            label = Pumps,
            show_guide = FALSE
          ),
          colour = LineColours[4],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Solar,
              label = Solar),
          size = 1.5,
          colour = LineColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = Solar,
            label = ifelse(Year == min(Year), paste(round(Solar, digits = 3),"GW"), ""),
            hjust = 1,
            vjust = -.8,
            fontface = 2
          ),
          colour = LineColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = Solar,
            label = ifelse(Year == max(Year), paste0("Solar thermal\n",round(Solar, digits = 3)," GW"), ""),
            fontface = 2,
            vjust = 0.3
          ),
          colour = LineColours[5],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(RenHeatCapTech, 1),
          aes(
            x = Year,
            y = Solar,
            label = Solar,
            show_guide = FALSE
          ),
          colour = LineColours[5],
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
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      RenHeatCapTechChart
      
      RenHeatCapTechChart <-
        LinePercentChart(RenHeatCapTechChart,
                         RenHeatCapTech,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      RenHeatCapTechChart

      
      RenHeatCapTechChart <- RenHeatCapTechChart +
        ylim(-.008, max(RenHeatCapTech$Biomass))+
        xlim(min(RenHeatCapTech$Year-.8), max(RenHeatCapTech$Year+1.1))
      ggsave(
       file,
       plot = RenHeatCapTechChart,
        width = 16,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )



output$RenHeatOutput.png <- downloadHandler(
  filename = "RenHeatOutput.png",
  content = function(file) {
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 6)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- arrange(Data, -row_number())
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    RenHeatOutputTech <- Data
    
    plottitle <- "Renewable heat output by technology type"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500", "#FFFFFF")
    LineColours <- c("#fc4e2a","#feb24c","#fed976","#addd8e","#41ab5d")
    
    RenHeatOutputTechChart <-
      RenHeatOutputTech %>%  ggplot(aes(x = Year), family = "Century Gothic") +
      
      ### Line of Values
      geom_line(
        aes(y = Biomass,
            label = Biomass),
        colour = LineColours[1],
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Biomass,
          label = ifelse(Year == min(Year), paste(format(round(Biomass, digits = 0), big.mark = ","), "GWh"), ""),
          hjust = 1,
          vjust= 0,
          fontface = 2
        ),
        colour = LineColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = Biomass,
          label = ifelse(Year == max(Year), paste0("Biomass\n",format(round(Biomass, digits = 0), big.mark = ","), " GWh"), ""),
          fontface = 2
        ),
        colour = LineColours[1],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = Biomass,
          label = Biomass,
          show_guide = FALSE
        ), 
        colour = LineColours[1],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = CHP,
            
            label = CHP),
        size = 1.5,
        colour = LineColours[2],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = CHP,
          label = ifelse(Year == min(Year), paste(round(CHP, digits = 0),"GWh"), ""),
          hjust = 1,
          vjust = 1,
          fontface = 2
        ),
        colour = LineColours[2],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = CHP,
          label = ifelse(Year == max(Year), paste0("Biomass CHP\n",round(CHP, digits = 0), " GWh"), ""),
          fontface = 2,
        ),
        colour = LineColours[2],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = CHP,
          label = CHP,
          show_guide = FALSE
        ),
        colour = LineColours[2],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Waste,
            label = Waste),
        size = 1.5,
        colour = LineColours[3],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Waste,
          label = ifelse(Year == min(Year), paste(round(Waste, digits = 0),"GWh"), ""),
          hjust = 1,
          vjust = -.9,
          fontface = 2
        ),
        colour = LineColours[3],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = Waste,
          label = ifelse(Year == max(Year), paste0("Waste\n",round(Waste, digits = 0), " GWh"), ""),
          fontface = 2,
          vjust =-.3
        ),
        colour = LineColours[3],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = Waste,
          label = Waste,
          show_guide = FALSE
        ),
        colour = LineColours[3],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Pumps,
            label = Pumps),
        size = 1.5,
        colour = LineColours[4],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Pumps,
          label = ifelse(Year == min(Year), paste(round(Pumps, digits = 0), "GWh"), ""),
          hjust = 1,
          vjust= 0,
          fontface = 2
        ),
        colour = LineColours[4],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = Pumps,
          label = ifelse(Year == max(Year), paste0("Heat pumps\n", round(Pumps, digits = 0)," GWh"), ""),
          vjust = .5,
          fontface = 2
        ),
        colour = LineColours[4],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = Pumps,
          label = Pumps,
          show_guide = FALSE
        ),
        colour = LineColours[4],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Solar,
            label = Solar),
        size = 1.5,
        colour = LineColours[5],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Solar,
          label = ifelse(Year == min(Year), paste(round(Solar, digits = 0),"GWh"), ""),
          hjust = 1,
          vjust = 0,
          fontface = 2
        ),
        colour = LineColours[5],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = Solar,
          label = ifelse(Year == max(Year), paste0("Solar thermal\n",format(round(Solar, digits = 0), big.mark = ",")," GWh"), ""),
          fontface = 2,
          vjust = 0
        ),
        colour = LineColours[5],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = Solar,
          label = Solar,
          show_guide = FALSE
        ),
        colour = LineColours[5],
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
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )
    
    
    RenHeatOutputTechChart
    
    RenHeatOutputTechChart <-
      LinePercentChart(RenHeatOutputTechChart,
                       RenHeatOutputTech,
                       plottitle,
                       sourcecaption,
                       ChartColours)
    
    
    RenHeatOutputTechChart

    
    RenHeatOutputTechChart <- RenHeatOutputTechChart +
      ylim(-.008, max(RenHeatOutputTech$Biomass))+
      xlim(min(RenHeatOutputTech$Year-0.9), max(RenHeatOutputTech$Year+1.15))
    ggsave(
      file,
      plot = RenHeatOutputTechChart,
      width = 16,
      height = 16,
      units = "cm",
      dpi = 300
    )
    
  }
)
}
    
    