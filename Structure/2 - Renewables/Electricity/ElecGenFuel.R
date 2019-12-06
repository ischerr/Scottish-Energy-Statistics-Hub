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
                    selectInput(ns("YearSelect"), "Year:", c(2017,2016,2015,2014,2013,2012,2011,2010,2009), selected = 2017, multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL)
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecGenFuel.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("ElecGenFuelPlot")),
    plotlyOutput(ns("ElecGenFuelPlot"))%>% withSpinner(color="#39ab2c"),
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
        DataScot$`Wave / tidal` + DataScot$`Landfill Gas` + DataScot$`Sewage Gas` + DataScot$`Solar PV`
      )
    
    DataScot$`Wave / tidal` <- NULL
    
    DataScot$`Landfill Gas` <- NULL
    
    DataScot$`Sewage Gas` <- NULL
    
    DataScot$`Solar PV` <- NULL
    
    names(DataScot)[1] <- "Year"
    
    names(DataScot)[4] <- "Biofuels"
    
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
        DataEW$`Wave / tidal` + DataEW$`Landfill Gas` + DataEW$`Sewage Gas` + DataEW$`Solar PV`
      )
    
    DataEW$`Wave / tidal` <- NULL
    
    DataEW$`Landfill Gas` <- NULL
    
    DataEW$`Sewage Gas` <- NULL
    
    DataEW$`Solar PV` <- NULL
    
    names(DataEW)[1] <- "Year"
    
    names(DataEW)[4] <- "Biofuels"
    
    names(DataEW)[5] <- "Pumped Hydro"
    
    DataEW$Sector <- "EWland"
    
    DataEW <- as_tibble(DataEW)
    
    DataEW <- DataEW[which(DataEW$Year == as.numeric(input$YearSelect)),]
    
    DataEW <-
      DataEW[c(
        "Sector",
        "Wind",
        "Hydro",
        "Biofuels",
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
        marker = list(color = BarColours[4]),
        legendgroup = 4
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
        marker = list(color = BarColours[5]),
        legendgroup = 5
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
        marker = list(color = BarColours[6]),
        legendgroup = 6
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
        marker = list(color = BarColours[10]),
        legendgroup = 7
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
        marker = list(color = BarColours[7]),
        legendgroup = 8
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
        marker = list(color = BarColours[8]),
        legendgroup = 9
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
        marker = list(color = BarColours[9]),
        legendgroup = 10
      ) %>%
      add_trace(
        x = ~ c(
          0,
          (
            ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables`
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
          ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables`
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
                                                         ElecGenFuel$Nuclear[1])),
        "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[2])
        
      ) %>%
      add_trace(
        x = ~ c(
          0,
          (
            ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables`
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
          ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables`
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
                                                              ElecGenFuel$Nuclear[2])), "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[2])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          0,
          (
            ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear
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
          ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear
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
                                                              )), "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[5])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          0,
          (
            ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear
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
          ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear
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
                                                              )), "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[5])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[1],
          (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[1] - (
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
          color = BarColours[8],
          dash = "none"
        )
      ) %>%
      add_trace(
        x = (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[1] - (
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
        ) , "</b>"),
        textposistion = 'center',
        textfont = list(color = BarColours[8])
        
      ) %>%
      
      add_trace(
        x = ~ c(
          (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[2],
          (ElecGenFuel$Wind + ElecGenFuel$Hydro + ElecGenFuel$Biofuels + ElecGenFuel$`Other Renewables` + ElecGenFuel$Nuclear + ElecGenFuel$`Pumped Hydro` + ElecGenFuel$Other + ElecGenFuel$Coal + ElecGenFuel$Oil + ElecGenFuel$Gas )[2] - (
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
          color = BarColours[8],
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
        ) , "</b>"),
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
          ticktext = list("England and Wales", "Scotland"),
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
  
  output$ElecGenFuel.png <- downloadHandler(
    filename = "ElecGenFuel.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      DataScot <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Elec gen by fuel",
          col_names = FALSE,
          skip = 15,
          n_max = 16
        )
      
      DataScot <- DataScot[c(1, ncol(DataScot))]
      
      DataScot <- as.data.frame(t(DataScot))
      
      DataScot <- setDT(DataScot, keep.rownames = FALSE)
      
      DataScot$V1 <- NULL
      
      names(DataScot) <- as.character(unlist(DataScot[1, ]))
      
      DataScot <- tail(DataScot,-1)
      
      DataScot %<>% lapply(function(x)
        as.numeric(as.character(x)))
      
      DataScot$Renewables <- NULL
      
      DataScot$Total <- NULL
      
      DataScot$`Coal Renewables` <-
        (
          DataScot$`Wave / tidal` + DataScot$`Landfill Gas` + DataScot$`Sewage Gas` + DataScot$`Solar PV`
        )
      
      DataScot$`Wave / tidal` <- NULL
      
      DataScot$`Landfill Gas` <- NULL
      
      DataScot$`Sewage Gas` <- NULL
      
      DataScot$`Solar PV` <- NULL
      
      names(DataScot)[3] <- "Biofuels"
      
      names(DataScot)[4] <- "Pumped Hydro"
      
      DataScot$Sector <- "Scotland"
      
      DataScot <- as_tibble(DataScot)
      
      DataScot <-
        DataScot[c(
          "Sector",
          "Wind",
          "Hydro",
          "Biofuels",
          "Coal Renewables",
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
      
      DataEW <- DataEW[c(1, ncol(DataEW))]
      
      DataEW <- as.data.frame(t(DataEW))
      
      DataEW <- setDT(DataEW, keep.rownames = FALSE)
      
      DataEW$V1 <- NULL
      
      names(DataEW) <- as.character(unlist(DataEW[1, ]))
      
      DataEW <- tail(DataEW,-1)
      
      DataEW %<>% lapply(function(x)
        as.numeric(as.character(x)))
      
      DataEW$Renewables <- NULL
      
      DataEW$Total <- NULL
      
      DataEW$`Coal Renewables` <-
        (DataEW$`Wave / tidal` + DataEW$`Landfill Gas` + DataEW$`Sewage Gas` + DataEW$`Solar PV`)
      
      DataEW$`Wave / tidal` <- NULL
      
      DataEW$`Landfill Gas` <- NULL
      
      DataEW$`Sewage Gas` <- NULL
      
      DataEW$`Solar PV` <- NULL
      
      names(DataEW)[3] <- "Biofuels"
      
      names(DataEW)[4] <- "Pumped Hydro"
      
      DataEW$Sector <- "England and Wales"
      
      DataEW <- as_tibble(DataEW)
      
      DataEW <-
        DataEW[c(
          "Sector",
          "Wind",
          "Hydro",
          "Biofuels",
          "Coal Renewables",
          "Nuclear",
          "Pumped Hydro",
          "Other",
          "Coal",
          "Oil",
          "Gas"
        )]
      
      ElecGenFuel <- rbind(DataEW, DataScot)
      
      ElecGenFuel$Sector <-
        factor(ElecGenFuel$Sector,
               levels = unique(ElecGenFuel$Sector),
               ordered = TRUE)
      
      ElecGenFuel <- melt(ElecGenFuel, id.vars = "Sector")
      
      
      ElecGenFuel$variable <-
        factor(ElecGenFuel$variable,
               levels = rev(unique(ElecGenFuel$variable)),
               ordered = TRUE)
      
      ElecGenFuel <- ElecGenFuel %>%
        group_by(Sector) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Proportion of electricity generation by fuel"
      sourcecaption <- "Source: BEIS"
      
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
      
      
      ElecGenFuelChart <- ElecGenFuel %>%
        ggplot(aes(x = Sector, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Wind" = BarColours[1],
            "Hydro" = BarColours[2],
            "Biofuels" = BarColours[3],
            "Coal Renewables" = BarColours[4],
            "Nuclear" = BarColours[5],
            "Pumped Hydro" = BarColours[6],
            "Coal" = BarColours[7],
            "Oil" = BarColours[8],
            "Gas" = BarColours[9],
            "Other" = BarColours[10]
          )
        ) +
        geom_bar(stat = "identity", width = .4) +
        geom_text(aes(
          y = pos,
          label = ifelse(value > 0.03, percent(value), " "),
          
          fontface = 2
        ),
        colour = "white",
        family = "Century Gothic") +
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
          x = 2.25,
          xend = 2.25,
          y = 0,
          yend = sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Gas" &
              ElecGenFuel$variable != "Oil" &
              ElecGenFuel$variable != "Coal" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Nuclear" &
              ElecGenFuel$variable != "Other"  &
              ElecGenFuel$Sector == "Scotland"
          )]),
          colour =  BarColours[3]
        ) +
        annotate(
          "text",
          x = 2.35,
          y =  (sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Gas" &
              ElecGenFuel$variable != "Oil" &
              ElecGenFuel$variable != "Coal" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Nuclear" &
              ElecGenFuel$Sector == "Scotland"
          )])) / 2,
          label = paste("Renewables:",
                        percent(1 - sum(ElecGenFuel$value[which(
                          ElecGenFuel$variable != "Wind" &
                            ElecGenFuel$variable != "Hydro" &
                            ElecGenFuel$variable != "Biofuels" &
                            ElecGenFuel$variable != "Coal Renewables" &
                            ElecGenFuel$Sector == "Scotland"
                        )]))),
          colour =  BarColours[3],
          family = "Century Gothic",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = 1.75,
          xend = 1.75,
          y = (sum(ElecGenFuel$value[which(
            ElecGenFuel$Sector == "Scotland"
          )])),
          yend = (sum(ElecGenFuel$value[which(
            ElecGenFuel$Sector == "Scotland"
          )])) - sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Wind" &
              ElecGenFuel$variable != "Hydro" &
              ElecGenFuel$variable != "Biofuels" &
              ElecGenFuel$variable != "Coal Renewables" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Nuclear" &
              ElecGenFuel$variable != "Other" &
              ElecGenFuel$Sector == "Scotland"
          )]),
          colour =  BarColours[8]
        ) +
        annotate(
          "text",
          x = 1.655,
          y =  (sum(ElecGenFuel$value[which(
            ElecGenFuel$Sector == "Scotland"
          )])) - (sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Wind" &
              ElecGenFuel$variable != "Hydro" &
              ElecGenFuel$variable != "Biofuels" &
              ElecGenFuel$variable != "Coal Renewables" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Nuclear" &
              ElecGenFuel$variable != "Other" &
              ElecGenFuel$Sector == "Scotland"
          )])) / 2,
          label = paste("Fossil:",
                        percent(sum(ElecGenFuel$value[which(
                          ElecGenFuel$variable != "Wind" &
                            ElecGenFuel$variable != "Hydro" &
                            ElecGenFuel$variable != "Biofuels" &
                            ElecGenFuel$variable != "Coal Renewables" &
                            ElecGenFuel$variable != "Pumped Hydro" &
                            ElecGenFuel$variable != "Nuclear" &
                            ElecGenFuel$variable != "Other" &
                            ElecGenFuel$Sector == "Scotland"
                        )]))),
          colour =  BarColours[8],
          family = "Century Gothic",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = 1.75,
          xend = 1.75,
          y = 0,
          yend = sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Gas" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Oil" &
              ElecGenFuel$variable != "Coal"  &
              ElecGenFuel$variable != "Other"  &
              ElecGenFuel$Sector == "Scotland"
          )]),
          colour =  BarColours[5]
        ) +
        annotate(
          "text",
          x = 1.655,
          y =  (sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Gas" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Oil" &
              ElecGenFuel$variable != "Coal"  &
              ElecGenFuel$variable != "Other"  &
              ElecGenFuel$Sector == "Scotland"
          )])) / 2,
          label = paste("Low Carbon:",
                        percent(1 - sum(ElecGenFuel$value[which(
                          ElecGenFuel$variable != "Wind" &
                            ElecGenFuel$variable != "Hydro" &
                            ElecGenFuel$variable != "Biofuels" &
                            ElecGenFuel$variable != "Coal Renewables" &
                            ElecGenFuel$variable != "Nuclear" &
                            ElecGenFuel$Sector == "Scotland"
                        )]))),
          colour =  BarColours[5],
          family = "Century Gothic",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = 1.25,
          xend = 1.25,
          y = 0,
          yend = sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Gas" &
              ElecGenFuel$variable != "Oil"  &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Coal" &
              ElecGenFuel$variable != "Nuclear" &
              ElecGenFuel$variable != "Other"  &
              ElecGenFuel$Sector == "England and Wales"
          )]),
          colour =  BarColours[3]
        ) +
        annotate(
          "text",
          x = 1.35,
          y =  (sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Gas" &
              ElecGenFuel$variable != "Oil"  &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Coal" &
              ElecGenFuel$variable != "Nuclear" &
              ElecGenFuel$variable != "Other"  &
              ElecGenFuel$Sector == "England and Wales"
          )])) / 2,
          label = paste("Renewables:",
                        percent(1 - sum(ElecGenFuel$value[which(
                          ElecGenFuel$variable != "Wind" &
                            ElecGenFuel$variable != "Hydro" &
                            ElecGenFuel$variable != "Biofuels" &
                            ElecGenFuel$variable != "Coal Renewables" &
                            ElecGenFuel$Sector == "England and Wales"
                        )]))),
          colour =  BarColours[3],
          family = "Century Gothic",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = .75,
          xend = .75,
          y = 1,
          yend = 1 - sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Wind" &
              ElecGenFuel$variable != "Hydro" &
              ElecGenFuel$variable != "Biofuels" &
              ElecGenFuel$variable != "Coal Renewables" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Nuclear" &
              ElecGenFuel$variable != "Other" &
              ElecGenFuel$Sector == "England and Wales"
          )]),
          colour =  BarColours[8]
        ) +
        annotate(
          "text",
          x = .65,
          y = 1 -  (sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Wind" &
              ElecGenFuel$variable != "Hydro" &
              ElecGenFuel$variable != "Biofuels" &
              ElecGenFuel$variable != "Coal Renewables" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Nuclear" &
              ElecGenFuel$variable != "Other" &
              ElecGenFuel$Sector == "England and Wales"
          )])) / 2,
          label = paste("Fossil:",
                        percent(sum(ElecGenFuel$value[which(
                          ElecGenFuel$variable != "Wind" &
                            ElecGenFuel$variable != "Hydro" &
                            ElecGenFuel$variable != "Biofuels" &
                            ElecGenFuel$variable != "Coal Renewables" &
                            ElecGenFuel$variable != "Pumped Hydro" &
                            ElecGenFuel$variable != "Nuclear" &
                            ElecGenFuel$variable != "Other" &
                            ElecGenFuel$Sector == "England and Wales"
                        )]))),
          colour =  BarColours[8],
          family = "Century Gothic",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = .75,
          xend = .75,
          y = 0,
          yend = sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Gas" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Oil" &
              ElecGenFuel$variable != "Coal"  &
              ElecGenFuel$variable != "Other"  &
              ElecGenFuel$Sector == "England and Wales"
          )]),
          colour =  BarColours[5]
        ) +
        annotate(
          "text",
          x = .65,
          y =  (sum(ElecGenFuel$value[which(
            ElecGenFuel$variable != "Gas" &
              ElecGenFuel$variable != "Pumped Hydro" &
              ElecGenFuel$variable != "Oil" &
              ElecGenFuel$variable != "Coal"  &
              ElecGenFuel$variable != "Other"  &
              ElecGenFuel$Sector == "England and Wales"
          )])) / 2,
          label = paste("Low Carbon:",
                        percent(1 - sum(ElecGenFuel$value[which(
                          ElecGenFuel$variable != "Wind" &
                            ElecGenFuel$variable != "Hydro" &
                            ElecGenFuel$variable != "Biofuels" &
                            ElecGenFuel$variable != "Coal Renewables" &
                            ElecGenFuel$variable != "Nuclear" &
                            ElecGenFuel$Sector == "England and Wales"
                        )]))),
          colour =  BarColours[5],
          family = "Century Gothic",
          fontface = 2
        ) +
        geom_text(aes(
          x = 2.6,
          y = 0 / 9,
          label = "Wind",
          fontface = 2
        ),
        colour =  BarColours[1],
        family = "Century Gothic") +
        geom_text(
          aes(
            x = 2.6,
            y = 1 / 9,
            label = "Hydro",
            fontface = 2
          ),
          colour =  BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.6,
            y = 2 / 9,
            label = "Biofuels",
            fontface = 2
          ),
          colour =  BarColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.6,
            y = 3 / 9,
            label = "Other\nRenewables",
            fontface = 2
          ),
          colour =  BarColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.6,
            y = 4 / 9,
            label = "Nuclear",
            fontface = 2
          ),
          colour =  BarColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.6,
            y = 5 / 9,
            label = "Pumped\nHydro",
            fontface = 2
          ),
          colour =  BarColours[6],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.6,
            y = 6 / 9,
            label = "Other",
            fontface = 2
          ),
          colour =  BarColours[10],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.6,
            y = 7 / 9,
            label = "Coal",
            fontface = 2
          ),
          colour =  BarColours[7],
          family = "Century Gothic"
        ) +
        geom_text(aes(
          x = 2.6,
          y = 8 / 9,
          label = "Oil",
          fontface = 2
        ),
        colour =  BarColours[8],
        family = "Century Gothic") +
        geom_text(aes(
          x = 2.6,
          y = 9 / 9,
          label = "Gas",
          fontface = 2
        ),
        colour =  BarColours[9],
        family = "Century Gothic") +
        geom_text(aes(x = 2.8,
                      y = 0,
                      label = " "))
      
      
      
      ElecGenFuelChart
      
      
      ElecGenFuelChart <-
        StackedBars(ElecGenFuelChart,
                    ElecGenFuel,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ElecGenFuelChart <-
        ElecGenFuelChart +
        labs(subtitle = "2017") +
        ylim(-.06, 1.01) +
        coord_flip()
      
      ElecGenFuelChart
      
      ggsave(
        file,
        plot = ElecGenFuelChart,
        width = 27,
        height = 12,
        units = "cm",
        dpi = 300
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
