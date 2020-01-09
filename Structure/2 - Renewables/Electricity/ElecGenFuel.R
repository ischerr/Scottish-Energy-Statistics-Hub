require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ElecGenFuelOutput <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    tabPanel("Charts",
    fluidRow(column(8,
                    h3("Proportion of electricity generation by fuel", style = "color: #39ab2c;  font-weight:bold"),
                    selectInput(ns("YearSelect"), "Year:", c(2018, 2017,2016,2015,2014,2013,2012,2011,2010,2009), selected = 2018, multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL)
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecGenFuel.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("ElecGenFuelPlot")),
    plotlyOutput(ns("ElecGenFuelPlot"), height = "675px")%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Animation",
             imageOutput(ns("ElecGenAnimation"), height = "675px")%>% withSpinner(color="#39ab2c"))),
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
      column(12, dataTableOutput(ns("ElecGenFuelTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("England & Wales",
               fluidRow(
                 column(10, h3("Data - England & Wales", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("ElecGenFuelEWTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
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
ElecGenFuel <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ElecGenFuel.R")
  


  output$ElecGenFuelSubtitle <- renderText({
    
    EURenElec <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = TRUE, 
                          skip = 18, n_max = 30)
    
    EURenElec <- EURenElec[,c(1:ncol(EURenElec))]
    
    
    
    names(EURenElec)[1] <- c("Countries")
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenElec[2:ncol(EURenElec)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EURenElec)), na.rm = TRUE))
  })
  
  output$ElecGenFuelPlot <- renderPlotly  ({
    
    DataScot <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen by fuel",
        col_names = FALSE,
        skip = 15,
        n_max = 16
      )
    
    DataScot <- as.data.frame(t(DataScot))
    
    DataScot <- setDT(DataScot, keep.rownames = FALSE)
    
    names(DataScot) <- as.character(unlist(DataScot[1,]))
    
    DataScot <- tail(DataScot, -1)
    
    DataScot <- head(DataScot, -1)
    
    DataScot %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    DataScot <- as_tibble(DataScot)
    
    for(i in 2:16){
    DataScot[i] <- DataScot[i] / DataScot[16]
    }
    DataScot$Renewables <- NULL
    
    DataScot$Total <- NULL
    
    DataScot$`Other Renewables` <-
      (
        DataScot$`Wave / tidal`
      )
    
    DataScot$Biofuels <-  DataScot$`Landfill Gas` + DataScot$`Sewage Gas` + DataScot$`Other biofuels and co-firing`
    
    DataScot$`Wave / tidal` <- NULL
    
    DataScot$`Landfill Gas` <- NULL
    
    DataScot$`Sewage Gas` <- NULL
    
    DataScot$`Other biofuels and co-firing` <- NULL
    
    names(DataScot)[1] <- "Year"
    
    names(DataScot)[4] <- "Solar"
    
    names(DataScot)[5] <- "Pumped Hydro"
    
    DataScot$Sector <- "Scotland"
    
    DataScot <- as_tibble(DataScot)
    
    DataScot <- DataScot[which(DataScot$Year == as.numeric(input$YearSelect)),]
    
    DataScot <-
      DataScot[c(
        "Sector",
        "Wind",
        "Hydro",
        "Biofuels",
        "Solar",
        "Other Renewables",
        "Nuclear",
        "Pumped Hydro",
        "Other",
        "Coal",
        "Oil",
        "Gas"
      )]
    
    DataEW <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen by fuel",
        col_names = FALSE,
        skip = 35,
        n_max = 16
      )
    
    DataEW <- as.data.frame(t(DataEW))
    
    DataEW <- setDT(DataEW, keep.rownames = FALSE)
    
    names(DataEW) <- as.character(unlist(DataEW[1,]))
    
    DataEW <- tail(DataEW, -1)
    
    DataEW <- head(DataEW, -1)
    
    DataEW %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    DataEW <- as_tibble(DataEW)
    
    for(i in 2:16){
      DataEW[i] <- DataEW[i] / DataEW[16]
    }
    DataEW$Renewables <- NULL
    
    DataEW$Total <- NULL
    
    DataEW$`Other Renewables` <-
      (
        DataEW$`Wave / tidal`
      )
    
    DataEW$Biofuels <-  DataEW$`Landfill Gas` + DataEW$`Sewage Gas` + DataEW$`Other biofuels and co-firing`
    
    DataEW$`Wave / tidal` <- NULL
    
    DataEW$`Landfill Gas` <- NULL
    
    DataEW$`Sewage Gas` <- NULL
    
    DataEW$`Other biofuels and co-firing` <- NULL
    
    names(DataEW)[1] <- "Year"
    
    names(DataEW)[4] <- "Solar"
    
    names(DataEW)[5] <- "Pumped Hydro"
    
    DataEW$Sector <- "Scotland"
    
    DataEW <- as_tibble(DataEW)
    
    DataEW <- DataEW[which(DataEW$Year == as.numeric(input$YearSelect)),]
    
    DataEW <-
      DataEW[c(
        "Sector",
        "Wind",
        "Hydro",
        "Biofuels",
        "Solar",
        "Other Renewables",
        "Nuclear",
        "Pumped Hydro",
        "Other",
        "Coal",
        "Oil",
        "Gas"
      )]
    
    ElecGenFuel <- rbind(DataEW, DataScot)
    
    ElecGenFuel$Sector <-
      c(1, 2)
    
    ElecGenFuel$Sector <- as.numeric(ElecGenFuel$Sector)
    
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#005a32",
        "#238b45",
        "#41ab5d",
        "#74c476",
        "#a1d99b",
        "#6baed6",
        "#969696",
        "#fdae6b",
        "#f16913",
        "#d94801",
        "#696969"
      )
    
    p <-
      plot_ly(data = ElecGenFuel, y = ~ Sector) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ Wind,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Wind",
        text = paste0("Wind: ", percent(ElecGenFuel$Wind, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ Hydro,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Hydro",
        text = paste0("Hydro: ", percent(ElecGenFuel$Hydro, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ Biofuels,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Biofuels",
        text = paste0("Biofuels: ", percent(ElecGenFuel$Biofuels, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ Solar,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Solar",
        text = paste0(
          "Solar: ",
          percent(ElecGenFuel$`Solar`, accuracy = 0.1)
        ),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ `Other Renewables`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Other Renewables",
        text = paste0(
          "Other Renewables: ",
          percent(ElecGenFuel$`Other Renewables`, accuracy = 0.1)
        ),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ `Nuclear`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Nuclear",
        text = paste0("Nuclear: ", percent(ElecGenFuel$Nuclear, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ `Pumped Hydro`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Pumped Hydro",
        text = paste0(
          "Pumped Hydro: ",
          percent(ElecGenFuel$`Pumped Hydro`, accuracy = 0.1)
        ),
        hoverinfo = 'text',
        marker = list(color = BarColours[7]),
        legendgroup = 7
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ `Other`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Other",
        text = paste0(
          "Other: ",
          percent(ElecGenFuel$`Other`, accuracy = 0.1)
        ),
        hoverinfo = 'text',
        marker = list(color = BarColours[11]),
        legendgroup = 8
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ Coal,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Coal",
        text = paste0("Coal: ", percent(ElecGenFuel$Coal, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[8]),
        legendgroup = 9
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ Oil,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Oil",
        text = paste0("Oil: ", percent(ElecGenFuel$`Oil`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[9]),
        legendgroup = 10
      ) %>%
      add_trace(
        data = ElecGenFuel,
        x = ~ Gas,
        type = 'bar',
        width = 0.3,
        
        orientation = 'h',
        name = "Gas",
        text = paste0("Gas: ", percent(ElecGenFuel$`Gas`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[10]),
        legendgroup = 11
      ) %>%
      add_trace(
        x = ~ c(
          0,
          (
            ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar +  ElecGenFuel$`Other Renewables`
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
          ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables`
        )[1] / 2,
        y = 1.37,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Renewables:\n ", percent(1 - (ElecGenFuel$Gas[1] +
                                                         ElecGenFuel$Oil[1] +
                                                         ElecGenFuel$Coal[1] +
                                                         ElecGenFuel$Other[1] +
                                                         ElecGenFuel$`Pumped Hydro`[1]+
                                                         ElecGenFuel$Nuclear[1]), 0.1),
        "</b>"),
        textposition = ifelse( (1-(ElecGenFuel$Gas[1] +
                                   ElecGenFuel$Oil[1] +
                                   ElecGenFuel$Coal[1] +
                                   ElecGenFuel$Other[1] +
                                   ElecGenFuel$`Pumped Hydro`[1]+
                                   ElecGenFuel$Nuclear[1])) > 0.1, "middle center", "middle right"),
        textfont = list(color = BarColours[2])
        
      ) %>%
      add_trace(
        x = ~ c(
          0,
          (
            ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables`
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
          ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables`
        )[2] / 2,
        y = 2.37,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Renewables:\n ", percent(1 - (ElecGenFuel$Gas[2] +
                                                              ElecGenFuel$Oil[2] +
                                                              ElecGenFuel$Coal[2] +
                                                              ElecGenFuel$Other[2] +
                                                              ElecGenFuel$`Pumped Hydro`[2]+
                                                              ElecGenFuel$Nuclear[2]), 0.1), "</b>"),
        textposition = ifelse((1 - (ElecGenFuel$Gas[2] +
                                 ElecGenFuel$Oil[2] +
                                 ElecGenFuel$Coal[2] +
                                 ElecGenFuel$Other[2] +
                                 ElecGenFuel$`Pumped Hydro`[2]+
                                 ElecGenFuel$Nuclear[2])) > 0.1, 'middle center', 'middle right'),
        textfont = list(color = BarColours[2])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          0,
          (
            ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear
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
          color = BarColours[4],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = (
          ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear
        )[1] / 2,
        y = .63,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Low Carbon:\n ", percent(1 - (ElecGenFuel$Gas[1] +
                                                              ElecGenFuel$Oil[1] +
                                                              ElecGenFuel$Coal[1] +
                                                              ElecGenFuel$Other[1] +
                                                              ElecGenFuel$`Pumped Hydro`[1]
                                                              ), 0.1), "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[4])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          0,
          (
            ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear
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
          ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear
        )[2] / 2,
        y = 1.63,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Low Carbon:\n ", percent(1 - (ElecGenFuel$Gas[2] +
                                                              ElecGenFuel$Oil[2] +
                                                              ElecGenFuel$Coal[2] +
                                                              ElecGenFuel$Other[2] +
                                                              ElecGenFuel$`Pumped Hydro`[2]
                                                              ), 0.1), "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[4])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[1],
          (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[1] - (
            ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas
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
          color = BarColours[9],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[1] - (
          ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas
        )[1] / 2,
        y = .63,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Fossil:\n ", percent((
          ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas
        )[1]
        , 0.1) , "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[9])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[2],
          (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$Solar + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[2] - (
            ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas
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
          color = BarColours[9],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = 1 - (
          ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas
        )[2] / 2,
        y = 1.63,
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Fossil:\n ", percent((
          ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas
        )[2]
        , 0.1) , "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[9])
        
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
  
  output$ElecGenFuelTable = renderDataTable({
    
    DataScot <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen by fuel",
        col_names = FALSE,
        skip = 15,
        n_max = 15
      )

    DataScot <- as.data.frame(t(DataScot))
    
    DataScot <- setDT(DataScot, keep.rownames = FALSE)
    
    names(DataScot) <- as.character(unlist(DataScot[1, ]))
    
    DataScot <- tail(DataScot,-1)
    
    DataScot <- head(DataScot,-1)
    
    DataScot %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    names(DataScot)[1] <- "Year"
    
    DataScot <- as_tibble(DataScot)
    
    datatable(
      DataScot,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0 , 'desc')),
        title = "Electricity Generation - Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity Generation - Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity Generation - Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(DataScot), 0) %>% 
      formatStyle(c(9, 15), fontStyle = "italic")
  })
  
  output$ElecGenFuelEWTable = renderDataTable({
    
    DataEW <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen by fuel",
        col_names = FALSE,
        skip = 33,
        n_max = 15
      )
    
    DataEW <- as.data.frame(t(DataEW))
    
    DataEW <- setDT(DataEW, keep.rownames = FALSE)
    
    names(DataEW) <- as.character(unlist(DataEW[1, ]))
    
    DataEW <- tail(DataEW,-1)
    
    DataEW <- head(DataEW,-1)
    
    DataEW %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    names(DataEW)[1] <- "Year"
    
    DataEW <- as_tibble(DataEW)
    
    YearList <- as.list(DataEW$Year[which(DataEW$Year >= 2009)])
    
    datatable(
      DataEW,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0 , 'desc')),
        title = "Electricity Generation - England and Wales",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity Generation - England and Wales',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity Generation - England and Wales')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(DataEW), 0) %>% 
      formatStyle(c(9, 15), fontStyle = "italic")
  })
  
  DataScot <-
    read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Elec gen by fuel",
      col_names = FALSE,
      skip = 15,
      n_max = 16
    )
  
  DataScot <- as.data.frame(t(DataScot))
  
  DataScot <- setDT(DataScot, keep.rownames = FALSE)
  
  names(DataScot) <- as.character(unlist(DataScot[1,]))
  
  DataScot <- tail(DataScot, -1)
  
  DataScot <- head(DataScot, -1)
  
  DataScot %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  DataScot <- as_tibble(DataScot)
  
  for(i in 2:16){
    DataScot[i] <- DataScot[i] / DataScot[16]
  }
  DataScot$Renewables <- NULL
  
  DataScot$Total <- NULL
  
  DataScot$`Other Renewables` <-
    (
      DataScot$`Wave / tidal`
    )
  
  DataScot$Biofuels <- DataScot$`Landfill Gas` + DataScot$`Sewage Gas` + DataScot$`Other biofuels and co-firing`
  
  DataScot$`Wave / tidal` <- NULL
  
  DataScot$`Landfill Gas` <- NULL
  
  DataScot$`Sewage Gas` <- NULL
  
  DataScot$`Other biofuels and co-firing` <- NULL
  
  names(DataScot)[1] <- "Year"
  
  names(DataScot)[4] <- "Solar"
  
  names(DataScot)[5] <- "Pumped Hydro"
  
  DataScot$Sector <- "1.8"
  
  DataScot <- as_tibble(DataScot)
  
  DataScot <-
    DataScot[c(
      "Sector",
      "Year",
      "Wind",
      "Hydro",
      "Biofuels",
      "Solar",
      "Other Renewables",
      "Nuclear",
      "Pumped Hydro",
      "Other",
      "Coal",
      "Oil",
      "Gas"
    )]
  
  DataEW <-
    read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Elec gen by fuel",
      col_names = FALSE,
      skip = 35,
      n_max = 16
    )
  
  DataEW <- as.data.frame(t(DataEW))
  
  DataEW <- setDT(DataEW, keep.rownames = FALSE)
  
  names(DataEW) <- as.character(unlist(DataEW[1,]))
  
  DataEW <- tail(DataEW, -1)
  
  DataEW <- head(DataEW, -1)
  
  DataEW %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  DataEW <- as_tibble(DataEW)
  
  for(i in 2:16){
    DataEW[i] <- DataEW[i] / DataEW[16]
  }
  
  DataEW$Renewables <- NULL
  
  DataEW$Total <- NULL
  
  DataEW$`Other Renewables` <-
    (
      DataEW$`Wave / tidal`
    )
  
  DataEW$Biofuels <- DataEW$`Landfill Gas` + DataEW$`Sewage Gas` + DataEW$`Other biofuels and co-firing`
  
  DataEW$`Wave / tidal` <- NULL
  
  DataEW$`Landfill Gas` <- NULL
  
  DataEW$`Sewage Gas` <- NULL
  
  DataEW$`Other biofuels and co-firing` <- NULL
  
  names(DataEW)[1] <- "Year"
  
  names(DataEW)[4] <- "Solar"
  
  names(DataEW)[5] <- "Pumped Hydro"
  
  DataEW$Sector <- "1"
  
  DataEW <- as_tibble(DataEW)
  
  DataEW <-
    DataEW[c(
      "Sector",
      "Year",
      "Wind",
      "Hydro",
      "Biofuels",
      "Solar",
      "Other Renewables",
      "Nuclear",
      "Pumped Hydro",
      "Other",
      "Coal",
      "Oil",
      "Gas"
    )]
  
  ElecGenFuel <- rbind(DataEW, DataScot)
  
  #ElecGenFuel$Sector <- ifelse(ElecGenFuel == "1", 1, 2)
  
  ElecGenFuel <- ElecGenFuel[complete.cases(ElecGenFuel),]
  
  ElecGenFuel$Sector <- as.numeric(ElecGenFuel$Sector)
  
  ElecGenFuel <- melt(ElecGenFuel, id.vars = c("Sector", "Year"))
  
  ElecGenFuel$variable <-
    factor(ElecGenFuel$variable,
           levels = rev(unique(ElecGenFuel$variable)),
           ordered = TRUE)
  
  ElecGenFuel <- ElecGenFuel %>%
    group_by(Year, Sector)  %>%
    mutate(pos = cumsum(value) - value / 2) %>%
    mutate(top = sum(value)) %>% 
    mutate(RenLineX = Sector + 0.23) %>% 
    mutate(RenLineY = sum(value[which(variable %in% c("Wind", "Hydro", "Biofuels", "Solar", "Other Renewables"))])) %>% 
    mutate(RenText = 1 - sum(value[which(variable %in% c("Coal", "Oil", "Gas", "Other", "Pumped Hydro", "Nuclear"))])) %>% 
    mutate(LCLineX = Sector - 0.23) %>% 
    mutate(LCLineY = sum(value[which(variable %in% c("Wind", "Hydro", "Biofuels",  "Solar", "Other Renewables", "Nuclear"))])) %>% 
    mutate(LCText = 1 - sum(value[which(variable %in% c("Coal", "Oil", "Gas", "Other", "Pumped Hydro"))])) %>% 
    mutate(FossilLineX = Sector - 0.23) %>% 
    mutate(FossilLineY = sum(value[which(variable %in% c("Coal", "Oil", "Gas"))])) %>% 
    mutate(FossilText = sum(value[which(variable %in% c("Coal", "Oil", "Gas"))])) %>% 
    mutate(Infinite = Inf)
  
  CountryList <- c("Scotland", "England and Wales")
  plottitle <-
    "Proportion of electricity generation by fuel"
  sourcecaption <- "Source: BEIS"
  
  ChartColours <- c("#39ab2c", "#FF8500")
  BarColours <-
    c(
      "#005a32",
      "#238b45",
      "#41ab5d",
      "#74c476",
      "#a1d99b",
      "#6baed6",
      "#969696",
      "#fdae6b",
      "#f16913",
      "#d94801",
      "#696969"
    )
  
  output$ElecGenFuel.png <- downloadHandler(
    filename = "ElecGenFuel.png",
    content = function(file) {
      
      ### Load Packages and Functions
      

      
      #ElecGenFuelChart <- ElecGenFuelChart[which(ElecGenFuel$Year > 2016)]
      
      Year = as.numeric(input$YearSelect)
      
      ElecGenFuel <- ElecGenFuel[which(ElecGenFuel$Year == Year),]
      
      
      
      ElecGenFuelChart <- ElecGenFuel %>%
        ggplot(aes(x = Sector, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Wind" = BarColours[1],
            "Hydro" = BarColours[2],
            "Biofuels" = BarColours[3],
            "Solar" = BarColours[4],
            "Other Renewables" = BarColours[5],
            "Nuclear" = BarColours[6],
            "Pumped Hydro" = BarColours[7],
            "Coal" = BarColours[8],
            "Oil" = BarColours[9],
            "Gas" = BarColours[10],
            "Other" = BarColours[11]
          )
        ) +
        geom_bar(stat = "identity", width = .4) +
        geom_text(aes(
          y = pos,
          label = ifelse(value > 0.03, percent(value, accuracy = .1), " "),
          fontface = 2
          
          
        ),
        colour = "white",
        size = 7,
        family = "Century Gothic") +
        geom_text(
          aes(
            x = Sector,
            y = -0.08,
            label = str_wrap(ifelse(Sector == 1, "England and Wales", "Scotland"), width = 9),
            fontface = 2
          ),
          colour = ChartColours[1],
          size = 8,
          family = "Century Gothic"
        )  +
        # annotate(
        #   "text",
        #   x = 2.375,
        #   y =  (sum(ElecGenFuel$value[which(
        #     ElecGenFuel$variable != "Gas" &
        #       ElecGenFuel$variable != "Oil" &
        #       ElecGenFuel$variable != "Coal" &
        #       ElecGenFuel$variable != "Pumped Hydro" &
        #       ElecGenFuel$variable != "Nuclear" &
        #       ElecGenFuel$Sector == "1"
        #   )])) / 2,
      #   label = paste("Renewables:",
      #                 percent(1 - sum(ElecGenFuel$value[which(
      #                   ElecGenFuel$variable != "Wind" &
      #                     ElecGenFuel$variable != "Hydro" &
      #                     ElecGenFuel$variable != "Biofuels" &
      #                     ElecGenFuel$variable != "Other Renewables" &
      #                     ElecGenFuel$Sector == "1"
      #                 )]))),
      #   colour =  BarColours[3],
      #   family = "Century Gothic",
      #   fontface = 2
      # ) +
      geom_segment(mapping = aes(
        x = RenLineX,
        xend = RenLineX,
        y = RenLineY,
        yend = RenLineY - RenLineY),
        colour =  BarColours[2],
        size = 1.5
      ) +
        geom_text(aes(
          x =  RenLineX + 0.1,
          y = RenLineY * 0.5,
          label = paste("Renewables:", percent(RenText, accuracy = .1))
        ),
        size = 8,
        colour = BarColours[2],
        family = "Century Gothic",
        fontface = 2
        ) +
        
        geom_segment(mapping = aes(
          x = LCLineX,
          xend = LCLineX,
          y = LCLineY,
          yend = LCLineY - LCLineY),
          colour =  BarColours[6],
          size = 1.5
        ) +
        
        geom_text(aes(
          x =  LCLineX - 0.121,
          y = LCLineY * 0.5,
          label = paste("Low Carbon:", percent(LCText, accuracy = .1))),
          size = 8,
          colour = BarColours[6],
          family = "Century Gothic",
          fontface = 2
        ) +
        
        geom_segment(mapping = aes(
          x = FossilLineX,
          xend = FossilLineX,
          y = top,
          yend = top - FossilLineY),
          colour =  BarColours[9],
          size = 1.5
        ) +
        
        geom_text(aes(
          x =  FossilLineX - 0.121,
          y = top - (FossilLineY * 0.5),
          label = paste("Fossil Fuels:", percent(FossilText, accuracy = .1))),
          size = 8,
          colour = BarColours[9],
          family = "Century Gothic",
          fontface = 2
        ) +
        geom_text(aes(
          x = 2.375,
          y = ((0 / 10) *1.15) - 0.121,
          label = "Wind"
        ),
        colour =  BarColours[1],
        size = 7,
        family = "Century Gothic") +
        geom_text(
          aes(
            x = 2.375,
            y = ((1 / 10) *1.15) - 0.121,
            label = "Hydro"
          ),
          colour =  BarColours[2],
          size = 7,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.375,
            y = ((2 / 10) *1.15) - 0.121,
            label = "Bioenergy\n& wastes"
          ),
          colour =  BarColours[3],
          size = 7,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.375,
            y = ((3 / 10) *1.15) - 0.121,
            label = "Solar\nPV"
          ),
          colour =  BarColours[3],
          size = 7,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.375,
            y = ((4 / 10) *1.15) - 0.121,
            label = "Other\nRenewables"
          ),
          colour =  BarColours[5],
          size = 7,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.375,
            y = ((5 / 10) *1.15) - 0.121,
            label = "Nuclear"
          ),
          colour =  BarColours[6],
          size = 7,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.375,
            y = ((6 / 10) *1.15) - 0.121,
            label = "Pumped\nHydro"
          ),
          colour =  BarColours[7],
          size = 7,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.375,
            y = ((7 / 10) *1.15) - 0.121,
            label = "Other"
          ),
          colour =  BarColours[11],
          size = 7,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.375,
            y = ((8 / 10) *1.15) - 0.121,
            label = "Coal"
          ),
          colour =  BarColours[8],
          size = 7,
          family = "Century Gothic"
        ) +
        geom_text(aes(
          x = 2.375,
          y = ((9 / 10) *1.15) - 0.121,
          label = "Oil"
        ),
        colour =  BarColours[9],
        size = 7,
        family = "Century Gothic") +
        geom_text(aes(
          x = 2.375,
          y = ((10 / 10) *1.15) - 0.121,
          label = "Gas"
        ),
        colour =  BarColours[10],
        size = 7,
        family = "Century Gothic") +
        geom_text(aes(x = 2.8,
                      y = 0,
                      label = " ")) +
        geom_text(aes(
          x = 2.75,
          y = -0.15,
          label = "Proportion of electricity generation by fuel",
          fontface = 2,
          hjust = 0
        ),
        colour =  ChartColours[1],
        size = 10,
        family = "Century Gothic") +
        geom_text(aes(
          x = 2.615,
          y = -0.15,
          label = paste(Year),
          fontface = 1,
          hjust = 0
        ),
        colour =  ChartColours[1],
        size = 8,
        family = "Century Gothic") +
        geom_text(aes(
          x = 0.4,
          y = 1.05,
          label = paste("Source: BEIS"),
          hjust = 1
        ),
        colour =  ChartColours[1],
        size = 8,
        family = "Century Gothic") +
        geom_segment(
          aes(x = 0.5,
              xend = 0.5,
              y = -0.15,
              yend = 1.05),
          colour = ChartColours[1],
          size = 2
        )+
        geom_segment(
          aes(x = 2.52,
              xend = 2.52,
              y = -0.15,
              yend = 1.05),
          colour = ChartColours[1],
          size = 2
        )+
        coord_flip()+
        ### Axis Settingss
        scale_y_continuous(labels = scales::percent) +
        
        scale_color_manual(values = ChartColours) +
        
        ### Theme Options ###
        
        ggplot2::theme(
          text = element_text(family = "Century Gothic")
          ,
          panel.background = element_rect(fill = "transparent") # bg of the panel
          ,
          plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
          ,
          legend.background = element_rect(fill = "transparent") # get rid of legend bg
          ,
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
          ,
          legend.title = ggplot2::element_blank()
          #, axis.text.x = element_text(colour = "black", face="bold")
          ,
          axis.text.x = ggplot2::element_blank()
          ,
          axis.text.y =  ggplot2::element_blank()
          ,
          axis.title = ggplot2::element_blank()
          ,
          legend.text = element_text(colour = "black", family = "Century Gothic")
          ,
          axis.ticks = ggplot2::element_blank()
          ,
          panel.grid.major = ggplot2::element_blank()
          ,
          legend.position = "none"
          ,
          title = element_text(colour = ChartColours[1], size = 14)
          ,
          plot.title = ggplot2::element_text(face = "bold")
        ) 
      
      ElecGenFuelChart
      
      ggsave(
        file,
        plot = ElecGenFuelChart,
        width = 40,
        height = 24,
        units = "cm",
        dpi = 600
      )
    }
  )
  

  output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/ElecGenFuel.txt")[2])
                    
                  )))
 })
 
  observeEvent(input$ToggleTable, {
    toggle("ElecGenFuelTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ElecGenFuelEWTable")
  })

  observeEvent(input$ToggleText, {
    toggle("Text")
  })

output$ElecGenAnimation <- renderImage({

  outfile <- tempfile(fileext='.gif')
    file.copy("Structure/2 - Renewables/Electricity/ElecGenAnimation.gif", outfile)
  
    list(src = outfile,
         alt = "This is alternate text")
}, deleteFile = TRUE)

  
}
