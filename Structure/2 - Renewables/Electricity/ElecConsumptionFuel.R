require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



ElecConsumptionFuelOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Proportion of electricity consumption by fuel", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('ElecConsumptionFuelSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecConsumptionFuel.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("ElecConsumptionFuelPlot")),
    plotlyOutput(ns("ElecConsumptionFuelPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
    tabPanel("Scotland",
    fluidRow(
    column(10, h3("Data - Scotland", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ElecConsumptionFuelTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("England & Wales",
               fluidRow(
                 column(10, h3("Data - England & Wales", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("ElecConsumptionFuelEWTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("NGCarbonAPI"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("NGCarbonAPI"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("NGCarbonAPI")
        
      )
    )
  )
}




###### Server ######
ElecConsumptionFuel <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ElecConsumptionFuel.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$ElecConsumptionFuelSubtitle <- renderText({
    
    Time <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec consump by fuel",
        col_names = FALSE,
        skip = 11,
        n_max = 1
      )
    
    Time <- Time[1, ncol(Time)]
    
    Time <- ymd(paste0(substr(Time,1,4), "/", as.numeric(substr(Time,7,7))*3, "/01"))
    
    paste0(format(Time[length(Time)] - months(11),"%B %Y"), " - ", format(Time[length(Time)], "%B %Y"))
  })
  
  output$ElecConsumptionFuelPlot <- renderPlotly  ({
    

    
    DataScot <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec consump by fuel",
        col_names = FALSE,
        skip = 12,
        n_max = 10
      )
    
    DataScot <- as_tibble(t(DataScot))
    
    names(DataScot) <- unlist(DataScot[1,])
    
    DataScot <- tail(DataScot,1)
    DataScot$Sector <- "Scotland"
    
    DataEW <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec consump by fuel",
        col_names = FALSE,
        skip = 24,
        n_max = 10
      )
    
    DataEW <- as_tibble(t(DataEW))
    
    names(DataEW) <- unlist(DataEW[1,])
    
    DataEW <- tail(DataEW,1)
    DataEW$Sector <- "England and Wales"
    
    ElecConsumptionFuel <- rbind(DataEW, DataScot)[c(10,1:9)]
    
    ElecConsumptionFuel[2:10] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    
    ElecConsumptionFuel$Sector <- c(1,2)
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#006d2c",
        "#238b45",
        "#41ab5d",
        "#74c476",
        "#6baed6",
        "#969696",
        "#fdae6b",
        "#f16913",
        "#d94801",
        "#696969"
      )
    
    p <-
      plot_ly(data = ElecConsumptionFuel, y = ~ Sector) %>%
      add_trace(
        data = ElecConsumptionFuel,
        x = ~ Wind,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Wind",
        text = paste0("Wind: ", percent(ElecConsumptionFuel$Wind, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = ElecConsumptionFuel,
        x = ~ Hydro,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Hydro",
        text = paste0("Hydro: ", percent(ElecConsumptionFuel$Hydro, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = ElecConsumptionFuel,
        x = ~ Biomass,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Bioenergy and Waste",
        text = paste0("Bioenergy and Waste: ", percent(ElecConsumptionFuel$Biomass, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = ElecConsumptionFuel,
        x = ~ `Solar`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Solar",
        text = paste0(
          "Solar: ",
          percent(ElecConsumptionFuel$`Solar`, accuracy = 0.1)
        ),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = ElecConsumptionFuel,
        x = ~ `Nuclear`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Nuclear",
        text = paste0("Nuclear: ", percent(ElecConsumptionFuel$Nuclear, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = ElecConsumptionFuel,
        x = ~ `Imports`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Imports",
        text = paste0(
          "Imports: ",
          percent(ElecConsumptionFuel$`Imports`, accuracy = 0.1)
        ),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = ElecConsumptionFuel,
        x = ~ Other,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Other",
        text = paste0("Other: ", percent(ElecConsumptionFuel$Other, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[7]),
        legendgroup = 7
      ) %>%
      add_trace(
        data = ElecConsumptionFuel,
        x = ~ Coal,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Coal",
        text = paste0("Coal: ", percent(ElecConsumptionFuel$`Coal`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[8]),
        legendgroup = 8
      ) %>%
      add_trace(
        data = ElecConsumptionFuel,
        x = ~ Gas,
        type = 'bar',
        width = 0.3,
        
        orientation = 'h',
        name = "Gas",
        text = paste0("Gas: ", percent(ElecConsumptionFuel$`Gas`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[9]),
        legendgroup = 9
      ) %>%
      add_trace(
        x = ~ c(
          0,
          (
            ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar`
          )[1]
        ),
        y = ~ 1.2,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE ,
        hoverinfo = 'skip',
        legendgroup = 10,
        name = "Renewables",
        line = list(
          width = 2,
          color = BarColours[2],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = (
          ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar`
        )[1] / 2,
        y = 1.37,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Renewables:\n ", percent((
          ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar`
        )[1]
        , 0.1) , "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[2])
        
      ) %>%
      add_trace(
        x = ~ c(
          0,
          (
            ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar`
          )[2]
        ),
        y = ~ 2.2,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE ,
        hoverinfo = 'skip',
        legendgroup = 10,
        name = "Renewables",
        line = list(
          width = 2,
          color = BarColours[2],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = (
          ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar`
        )[2] / 2,
        y = 2.37,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Renewables:\n ", percent((
          ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar`
        )[2]
        , 0.1) , "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[2])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          0,
          (
            ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar` + ElecConsumptionFuel$Nuclear
          )[1]
        ),
        y = ~ 0.8,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE ,
        hoverinfo = 'skip',
        legendgroup = 10,
        name = "Renewables",
        line = list(
          width = 2,
          color = BarColours[5],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = (
          ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar` + ElecConsumptionFuel$Nuclear
        )[1] / 2,
        y = .63,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Low Carbon:\n ", percent((
          ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar` + ElecConsumptionFuel$Nuclear
        )[1]
        , 0.1) , "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[5])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          0,
          (
            ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar` + ElecConsumptionFuel$Nuclear
          )[2]
        ),
        y = ~ 1.8,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE ,
        hoverinfo = 'skip',
        legendgroup = 10,
        name = "Renewables",
        line = list(
          width = 2,
          color = BarColours[5],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = (
          ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar` + ElecConsumptionFuel$Nuclear
        )[2] / 2,
        y = 1.63,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Low Carbon:\n ", percent((
          ElecConsumptionFuel$Wind + ElecConsumptionFuel$Hydro + ElecConsumptionFuel$Biomass + ElecConsumptionFuel$`Solar` + ElecConsumptionFuel$Nuclear
        )[2]
        , 0.1) , "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[5])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          1,
          1 - (
            ElecConsumptionFuel$Coal + ElecConsumptionFuel$Gas
          )[1]
        ),
        y = ~ 0.8,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE ,
        hoverinfo = 'skip',
        legendgroup = 10,
        name = "Renewables",
        line = list(
          width = 2,
          color = BarColours[8],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = 1 - (
          ElecConsumptionFuel$Coal + ElecConsumptionFuel$Gas
        )[1] / 2,
        y = .63,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Fossil Fuels:\n ", percent((
          ElecConsumptionFuel$Coal + ElecConsumptionFuel$Gas
        )[1]
        , 0.1) , "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[8])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          1,
          1 - (
            ElecConsumptionFuel$Coal + ElecConsumptionFuel$Gas
          )[2]
        ),
        y = ~ 1.8,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE ,
        hoverinfo = 'skip',
        legendgroup = 10,
        name = "Renewables",
        line = list(
          width = 2,
          color = BarColours[8],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = 1 - (
          ElecConsumptionFuel$Coal + ElecConsumptionFuel$Gas
        )[2] / 2,
        y = 1.63,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Fossil Fuels:\n ", percent((
          ElecConsumptionFuel$Coal + ElecConsumptionFuel$Gas
        )[2]
        , 0.1) , "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[8])
        
      ) %>%
      
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(
          title = "",
          showgrid = FALSE,
          ticktext = list("<b>England\nand Wales</b>", "<b>Scotland</b>"),
          tickvals = list(1, 2),
          tickmode = "array"
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F) %>% 
      onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")
    
    p
    
  })
  
  output$ElecConsumptionFuelTable = renderDataTable({
    
    DataScot <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec consump by fuel",
        col_names = FALSE,
        skip = 11,
        n_max = 10
      )

    DataScot <- as.data.frame(t(DataScot))
    
    DataScot <- setDT(DataScot, keep.rownames = FALSE)
    
    names(DataScot) <- as.character(unlist(DataScot[1, ]))

    DataScot[,2:10] %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    names(DataScot)[1] <- "Twelve Months Ending"
    
    DataScot <- as_tibble(DataScot)
    DataScot <- DataScot[complete.cases(DataScot),]
    
    DataScot$`Twelve Months Ending` <- ymd(paste0(substr(DataScot$`Twelve Months Ending`,1,4), "/", as.numeric(substr(DataScot$`Twelve Months Ending`,7,7))*3, "/01"))

    DataScot$`Twelve Months Ending` <- paste0(format(DataScot$`Twelve Months Ending` - months(11),"%B %Y"), " - ", format(DataScot$`Twelve Months Ending`, "%B %Y"))
    
    
    
    DataScot <- DataScot[seq(dim(DataScot)[1],1),]
    
    DataScot$Renewables <- DataScot$Wind + DataScot$Hydro + DataScot$Biomass + DataScot$Solar
    
    DataScot$`Low Carbon` <- DataScot$Renewables + DataScot$Nuclear
    
    DataScot$`Fossil Fuels` <- DataScot$Gas + DataScot$Coal
    
    
    names(DataScot)[4] <- "Bioenergy and Waste"
    datatable(
      DataScot[c(1,2,3,4,5,11,6,12,9,10,13,7,8)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Electricity Consumption - Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity Consumption - Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity Consumption - Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:ncol(DataScot), 1) %>% 
      formatStyle(c(6,8,11), fontStyle = "italic")
  })
  
  output$ElecConsumptionFuelEWTable = renderDataTable({
    
    DataEW <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec consump by fuel",
        col_names = FALSE,
        skip = 23,
        n_max = 10
      )
    
    DataEW <- as.data.frame(t(DataEW))
    
    DataEW <- setDT(DataEW, keep.rownames = FALSE)
    
    names(DataEW) <- as.character(unlist(DataEW[1, ]))
    
    DataEW[,2:10] %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    names(DataEW)[1] <- "Twelve Months Ending"
    
    DataEW <- as_tibble(DataEW)
    DataEW <- DataEW[complete.cases(DataEW),]
    
    DataEW$`Twelve Months Ending` <- ymd(paste0(substr(DataEW$`Twelve Months Ending`,1,4), "/", as.numeric(substr(DataEW$`Twelve Months Ending`,7,7))*3, "/01"))
    
    DataEW$`Twelve Months Ending` <- paste0(format(DataEW$`Twelve Months Ending` - months(11),"%B %Y"), " - ", format(DataEW$`Twelve Months Ending`, "%B %Y"))
    
    
    
    DataEW <- DataEW[seq(dim(DataEW)[1],1),]
    
    DataEW$Renewables <- DataEW$Wind + DataEW$Hydro + DataEW$Biomass + DataEW$Solar
    
    DataEW$`Low Carbon` <- DataEW$Renewables + DataEW$Nuclear
    
    DataEW$`Fossil Fuels` <- DataEW$Gas + DataEW$Coal
    
    names(DataEW)[4] <- "Bioenergy and Waste"
    
    datatable(
      DataEW[c(1,2,3,4,5,11,6,12,9,10,13,7,8)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Electricity Consumption - England and Wales",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity Consumption - England and Wales',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity Consumption - England and Wales')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:ncol(DataEW), 1) 
  
  })
  
  output$ElecConsumptionFuel.png <- downloadHandler(
    filename = "ElecConsumptionFuel.png",
    content = function(file) {
      
      
      Time <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Elec consump by fuel",
          col_names = FALSE,
          skip = 11,
          n_max = 1
        )
      
      Time <- Time[1, ncol(Time)]
      
      Time <- ymd(paste0(substr(Time,1,4), "/", as.numeric(substr(Time,7,7))*3, "/01"))
      
      Time <- paste0(format(Time[length(Time)] - months(11),"%B %Y"), " - ", format(Time[length(Time)], "%B %Y"))
      
      ### Load Packages and Functions
      
      DataScot <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Elec consump by fuel",
          col_names = FALSE,
          skip = 12,
          n_max = 10
        )
      
      DataScot <- as_tibble(t(DataScot))
      
      names(DataScot) <- unlist(DataScot[1,])
      
      DataScot <- tail(DataScot,1)
      DataScot$Sector <- "Scotland"
      
      DataEW <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Elec consump by fuel",
          col_names = FALSE,
          skip = 24,
          n_max = 10
        )
      
      DataEW <- as_tibble(t(DataEW))
      
      names(DataEW) <- unlist(DataEW[1,])
      
      DataEW <- tail(DataEW,1)
      DataEW$Sector <- "England and Wales"
      
      ElecConsumptionFuel <- rbind(DataEW, DataScot)[c(10,1:9)]
      
      ElecConsumptionFuel[2:10] %<>% lapply(function(x) as.numeric(as.character(x)))
      
      ElecConsumptionFuel$Sector <-
        factor(ElecConsumptionFuel$Sector,
               levels = unique(ElecConsumptionFuel$Sector),
               ordered = TRUE)
      
      ElecConsumptionFuel <- melt(ElecConsumptionFuel, id.vars = "Sector")
      
      
      ElecConsumptionFuel$variable <-
        factor(ElecConsumptionFuel$variable,
               levels = rev(unique(ElecConsumptionFuel$variable)),
               ordered = TRUE)
      
      ElecConsumptionFuel <- ElecConsumptionFuel %>%
        group_by(Sector) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Proportion of electricity consumption by fuel"
      sourcecaption <- "Source: National Grid"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <- c("#006d2c", "#238b45", "#41ab5d", "#74c476", "#6baed6","#969696","#525252","#f16913", "#d94801")
      
      
      ElecConsumptionFuelChart <- ElecConsumptionFuel %>%
        ggplot(aes(x = Sector, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Wind" = BarColours[1],
            "Hydro" = BarColours[2],
            "Biomass"= BarColours[3],
            "Solar" = BarColours[4],
            "Nuclear" = BarColours[5],
            "Imports" = BarColours[6],
            "Other" = BarColours[7],
            "Coal" = BarColours[8],
            "Gas" = BarColours[9]
          )
        )+
        geom_bar(stat = "identity", width = .4) +
        geom_text(
          aes(
            y = pos,
            label = ifelse(value > 0.03, percent(value, 0.1)," "),
            
            fontface = 2
          ),
          colour = "white",
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Sector,
            y = -0.05,
            label = str_wrap(Sector, width = 9),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )  + 
        annotate(
          "segment",
          x = 1.25,
          xend = 1.25,
          y = 0,
          yend = sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$Sector == "England and Wales")]),
          colour =  BarColours[3]
        ) + 
        annotate(
          "text",
          x = 1.36,
          y =  (sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$Sector == "England and Wales")]))/2,
          label = paste("Renewables:",
                        percent(sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$Sector == "England and Wales")]), 0.1)
          ),
          colour =  BarColours[3],
          family = "Century Gothic",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = 0.75,
          xend = 0.75,
          y = 0,
          yend = sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports"  & ElecConsumptionFuel$Sector == "England and Wales")]),
          colour =  BarColours[5]
        ) + 
        annotate(
          "text",
          x = 0.65,
          y =  (sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports"  & ElecConsumptionFuel$Sector == "England and Wales")]))/2,
          label = paste("Low Carbon:",
                        percent(sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports"& ElecConsumptionFuel$Sector == "England and Wales")]), 0.1)
          ),
          colour =  BarColours[5],
          family = "Century Gothic",
          fontface = 2
        ) +
        
        
        
        
        
        annotate(
          "segment",
          x = 0.75,
          xend = 0.75,
          y = 1,
          yend = 1 -  sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Wind" & ElecConsumptionFuel$variable != "Hydro" & ElecConsumptionFuel$variable != "Biomass" & ElecConsumptionFuel$variable != "Solar" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Other"  & ElecConsumptionFuel$Sector == "England and Wales")]),
          colour =  BarColours[8]
        ) +
        annotate(
          "text",
          x = 0.65,
          y = 1 - (sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Wind" & ElecConsumptionFuel$variable != "Hydro" & ElecConsumptionFuel$variable != "Biomass" & ElecConsumptionFuel$variable != "Solar" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Other"  & ElecConsumptionFuel$Sector == "England and Wales")]))/2,
          label = paste("Fossil Fuels:",
                        percent(sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Wind" & ElecConsumptionFuel$variable != "Hydro" & ElecConsumptionFuel$variable != "Biomass" & ElecConsumptionFuel$variable != "Solar" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Other" &  ElecConsumptionFuel$Sector == "England and Wales")]), 0.1)
          ),
          colour =  BarColours[8],
          family = "Century Gothic",
          fontface = 2
        ) +
        
        
        
        annotate(
          "segment",
          x = 2.25,
          xend = 2.25,
          y = 0,
          yend = sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$Sector == "Scotland")]),
          colour =  BarColours[3]
        ) + 
        annotate(
          "text",
          x = 2.36,
          y =  (sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$Sector == "Scotland")]))/2,
          label = paste("Renewables:",
                        percent(sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$Sector == "Scotland")]), 0.1)
          ),
          colour =  BarColours[3],
          family = "Century Gothic",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = 1.75,
          xend = 1.75,
          y = 0,
          yend = sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports"  & ElecConsumptionFuel$Sector == "Scotland")]),
          colour =  BarColours[5]
        ) + 
        annotate(
          "text",
          x = 1.65,
          y =  (sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports"  & ElecConsumptionFuel$Sector == "Scotland")]))/2,
          label = paste("Low Carbon:",
                        percent(sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Gas" & ElecConsumptionFuel$variable != "Coal" & ElecConsumptionFuel$variable != "Other" & ElecConsumptionFuel$variable != "Imports"& ElecConsumptionFuel$Sector == "Scotland")]), 0.1)
          ),
          colour =  BarColours[5],
          family = "Century Gothic",
          fontface = 2
        ) +
        
        annotate(
          "segment",
          x = 1.75,
          xend = 1.75,
          y = 1,
          yend = 1 -  sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Wind" & ElecConsumptionFuel$variable != "Hydro" & ElecConsumptionFuel$variable != "Biomass" & ElecConsumptionFuel$variable != "Solar" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Other"  & ElecConsumptionFuel$Sector == "Scotland")]),
          colour =  BarColours[8]
        ) + 
        annotate(
          "text",
          x = 1.65,
          y =  1 - (sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Wind" & ElecConsumptionFuel$variable != "Hydro" & ElecConsumptionFuel$variable != "Biomass" & ElecConsumptionFuel$variable != "Solar" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Other"  &  ElecConsumptionFuel$Sector == "Scotland")]))/2,
          label = paste("Fossil Fuels:",
                        percent(sum(ElecConsumptionFuel$value[which(ElecConsumptionFuel$variable != "Wind" & ElecConsumptionFuel$variable != "Hydro" & ElecConsumptionFuel$variable != "Biomass" & ElecConsumptionFuel$variable != "Solar" & ElecConsumptionFuel$variable != "Nuclear" & ElecConsumptionFuel$variable != "Imports" & ElecConsumptionFuel$variable != "Other"  &  ElecConsumptionFuel$Sector == "Scotland")]), 0.1)
          ),
          colour =  BarColours[8],
          family = "Century Gothic",
          fontface = 2
        ) +
        
        
        geom_text(
          aes(x = 2.9,
              y = .5/9,
              label = "Wind",
              fontface = 2
          ),
          colour =  BarColours[1],
          family = "Century Gothic"
        )+
        geom_text(
          aes(x = 2.9,
              y = 1.5/9,
              label = "Hydro",
              fontface = 2
          ),
          colour =  BarColours[2],
          family = "Century Gothic"
        )+
        geom_text(
          aes(x = 2.9,
              y = 2.5/9,
              label = "Bioenergy\nand Waste",
              fontface = 2
          ),
          colour =  BarColours[3],
          family = "Century Gothic"
        )+
        geom_text(
          aes(x = 2.9,
              y = 3.5/9,
              label = "Solar",
              fontface = 2
          ),
          colour =  BarColours[4],
          family = "Century Gothic"
        )+
        geom_text(
          aes(x = 2.9,
              y = 4.5/9,
              label = "Nuclear",
              fontface = 2
          ),
          colour =  BarColours[5],
          family = "Century Gothic"
        )+
        geom_text(
          aes(x = 2.9,
              y = 5.5/9,
              label = "Imports",
              fontface = 2
          ),
          colour =  BarColours[6],
          family = "Century Gothic"
        )+
        geom_text(
          aes(x = 2.9,
              y = 6.5/9,
              label = "Other",
              fontface = 2
          ),
          colour =  BarColours[7],
          family = "Century Gothic"
        )+
        geom_text(
          aes(x = 2.9,
              y = 7.5/9,
              label = "Coal",
              fontface = 2
          ),
          colour =  BarColours[8],
          family = "Century Gothic"
        )+
        geom_text(
          aes(x = 2.9,
              y = 8.5/9,
              label = "Gas",
              fontface = 2
          ),
          colour =  BarColours[9],
          family = "Century Gothic"
        )+
        geom_text(
          aes(x=3.2,
              y = 0,
              label = " ")
        )
      
      
      
      ElecConsumptionFuelChart
      
      
      ElecConsumptionFuelChart <-
        StackedBars(ElecConsumptionFuelChart,
                    ElecConsumptionFuel,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ElecConsumptionFuelChart <-
        ElecConsumptionFuelChart +
        labs(subtitle = Time) +
        ylim(-.06,1.01) +
        coord_flip()
      
      ElecConsumptionFuelChart
      
      ggsave(
        file,
        plot = ElecConsumptionFuelChart,
        width = 27,
        height = 10,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  

  output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/ElecConsumptionFuel.txt")[2])
                    
                  )))
 })
 
  observeEvent(input$ToggleTable, {
    toggle("ElecConsumptionFuelTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ElecConsumptionFuelEWTable")
  })

  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  

  
}
