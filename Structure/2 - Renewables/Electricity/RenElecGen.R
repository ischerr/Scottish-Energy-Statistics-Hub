require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenElecGenOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Electricity generated quarterly",
               fluidRow(column(8,
                               h3("Quarterly electricity generated from renewable sources", style = "color: #39ab2c;  font-weight:bold"),
                               h4(textOutput(ns('RenElecQuarterGenerationSubtitle')), style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('RenElecQuarterGeneration.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               plotlyOutput(ns("RenElecQuarterGenerationPlot"), height = "900px")%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Electricity generated annual",
             fluidRow(column(8,
                             h3("Electricity generated from renewable sources", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecFuelGenSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecFuelGen.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecFuelPlot")),
             plotlyOutput(ns("RenElecFuelGenPlot"), height = "900px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Scottish Proportion",
             fluidRow(column(8,
                             h3("Renewable generation as proportion of UK", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('ScotRenGenSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ScotRenGen.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("ScotRenGenPlot")),
             plotlyOutput(ns("ScotRenGenPlot"))%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")
    ),
    tabPanel("Wind",
             fluidRow(column(8,
                             h3("Wind generation in European countries", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('EUWindSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EUWind.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("EUWindPlot")),
             plotlyOutput(ns("EUWindPlot"), height = "900px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Hydro",
             fluidRow(column(8,
                             h3("Hydro generation in European countries", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('EUHydroSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EUHydro.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("EUHydroPlot")),
             plotlyOutput(ns("EUHydroPlot"), height = "900px")%>% withSpinner(color="#39ab2c"),
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
      tabPanel("Quarterly electricity generation",
               fluidRow(
                 column(10, h3("Data - Quarterly electricity generation (GWh)", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable5"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("RenElecQuarterTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Annual electricity generation",
             fluidRow(
               column(10, h3("Data - Annual electricity generation (GWh)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecFuelGenTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Scottish proportion",
             fluidRow(
               column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ScotRenGenTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Wind",
             fluidRow(
               column(10, h3("Data - Wind generation (GWh)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EUWindTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Hydro",
             fluidRow(
               column(10, h3("Data - Hydro generation (GWh)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EUHydroTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISRenElec", "EURORenEn", "BEISSubNatEnergy"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISRenElec"),
        SourceLookup("EURORenEn"),
        SourceLookup("BEISSubNatEnergy")
        
      )
    )
  )
}




###### Server ######
RenElecGen <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecFuel.R")
  ###### Renewable Energy ###### ######
  
  
  output$RenElecFuelGenSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecGenFuel <- as_tibble(Data)
    
    paste(min(RenElecGenFuel$Year),"-", max(RenElecGenFuel$Year))
  })

  output$RenElecFuelGenTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecGenFuel <- as_tibble(Data)
    
    RenElecGenFuel <- RenElecGenFuel[c(1, (ncol(RenElecGenFuel) - 1):2)]
    
    RenElecGenFuel <- arrange(RenElecGenFuel,-row_number())
    
    RenElecGenFuel$Total <- RenElecGenFuel$`Other bioenergy` + RenElecGenFuel$`Sewage gas` + RenElecGenFuel$`Wave and tidal` + RenElecGenFuel$`Landfill gas` + RenElecGenFuel$`Solar PV` + RenElecGenFuel$Hydro + RenElecGenFuel$`Offshore Wind` + RenElecGenFuel$`Onshore Wind`
    
    datatable(
      RenElecGenFuel[c(1,9:2,10)],
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
        title = "Renewable Electricity - Electricity Generated (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Electricity - Electricity Generated (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Electricity - Electricity Generated (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecGenFuel), 0)
  })
  
 output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecGen.txt")[2])
                    
                  )))
 })

  
  observeEvent(input$ToggleTable, {
    toggle("RenElecFuelGenTable")
  })

  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$RenElecFuelGenPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecGenFuel <- as_tibble(Data)
    
    RenElecGenFuel <- RenElecGenFuel[c(1, (ncol(RenElecGenFuel) - 1):2)]
    
    RenElecGenFuel <- arrange(RenElecGenFuel,-row_number())
    
    RenElecGenFuel[is.na(RenElecGenFuel)] <- 0
    
    RenElecGenFuel$Total <- RenElecGenFuel$`Other bioenergy` + RenElecGenFuel$`Sewage gas` + RenElecGenFuel$`Wave and tidal` + RenElecGenFuel$`Landfill gas` + RenElecGenFuel$`Solar PV` + RenElecGenFuel$Hydro + RenElecGenFuel$`Offshore Wind` + RenElecGenFuel$`Onshore Wind`
    
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#004529",
        "#006837",
        "#238443",
        "#41ab5d",
        "#78c679",
        "#7bccc4",
        "#4eb3d3",
        "#2b8cbe"
      )
    
    RenElecGenFuel$Year <- paste0("<b>", RenElecGenFuel$Year, "</b>")
    
    
    
    p <- plot_ly(
      data = RenElecGenFuel,
      y = ~Year,
      x = ~`Onshore Wind`,
      legendgroup = 1,
      text = paste0(
        "Onshore Wind: ",
        format(round(RenElecGenFuel$`Onshore Wind`, digits = 0),big.mark = ","),
        " GWh\nYear: ",
        RenElecGenFuel$Year
      ),
      name = "Onshore Wind",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>%
      add_trace(
        data = RenElecGenFuel,
        y = ~Year,
        x = ~Total + 300,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste("<b>",format(round(RenElecGenFuel$Total, digits = 0), big.mark = ","),"GWh</b>"),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      ) %>% 
      add_trace(
        data = RenElecGenFuel,
        y = ~Year,
        x = ~`Offshore Wind`,
        legendgroup = 2,
        text = paste0(
          "Offshore Wind: ",
          format(round(RenElecGenFuel$`Offshore Wind`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          RenElecGenFuel$Year
        ),
        name = "Offshore Wind",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      )  %>% 
      add_trace(
        data = RenElecGenFuel,
        y = ~Year,
        x = ~`Hydro`,
        legendgroup = 3,
        text = paste0(
          "Hydro: ",
          format(round(RenElecGenFuel$`Hydro`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          RenElecGenFuel$Year
        ),
        name = "Hydro",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[3])
      )  %>% 
      add_trace(
        data = RenElecGenFuel,
        y = ~Year,
        x = ~`Solar PV`,
        legendgroup = 4,
        text = paste0(
          "Solar PV: ",
          format(round(RenElecGenFuel$`Solar PV`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          RenElecGenFuel$Year
        ),
        name = "Solar PV",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[4])
      )  %>% 
      add_trace(
        data = RenElecGenFuel,
        y = ~Year,
        x = ~`Landfill gas`,
        legendgroup = 5,
        text = paste0(
          "Landfill gas: ",
          format(round(RenElecGenFuel$`Landfill gas`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          RenElecGenFuel$Year
        ),
        name = "Landfill gas",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[5])
      )  %>% 
      add_trace(
        data = RenElecGenFuel,
        y = ~Year,
        x = ~`Wave and tidal`,
        legendgroup = 6,
        text = paste0(
          "Wave and tidal: ",
          format(round(RenElecGenFuel$`Wave and tidal`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          RenElecGenFuel$Year
        ),
        name = "Wave and tidal",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[6])
      )  %>% 
      add_trace(
        data = RenElecGenFuel,
        y = ~Year,
        x = ~`Sewage gas`,
        legendgroup = 7,
        text = paste0(
          "Sewage gas: ",
          format(round(RenElecGenFuel$`Sewage gas`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          RenElecGenFuel$Year
        ),
        name = "Sewage gas",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[7])
      )  %>% 
      add_trace(
        data = RenElecGenFuel,
        y = ~Year,
        x = ~`Other bioenergy`,
        legendgroup = 8,
        text = paste0(
          "Other bioenergy: ",
          format(round(RenElecGenFuel$`Other bioenergy`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          RenElecGenFuel$Year
        ),
        name = "Other bioenergy",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[8])
      )  %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     autorange = "reversed",
                     dtick = 1),
        xaxis = list(
          title = "",
          tickformat = "",
          range = c(0, 30500),
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F) %>% 
      onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")
    
    p
    
    
    
    
  })
  
  output$RenElecFuelGen.png <- downloadHandler(
    filename = "RenElecFuelGen.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                                   sheet = "Renewable elec by fuel",
                                   col_names = TRUE,
                                   skip = 12
      )
      
      Data<- Data[complete.cases(Data),]
      
      Data<-Data[dim(Data)[1]:1,]
      
      Data <- distinct(Data, Year, .keep_all = TRUE)
      
      Data <- head(Data, -1)
      
      Data<-Data[dim(Data)[1]:1,]
      
      Data %<>% lapply(function(x)
        as.numeric(as.character(x)))
      
      RenElecGenFuel <- as_tibble(Data)
      
      RenElecGenFuel <- RenElecGenFuel[c(1, (ncol(RenElecGenFuel) - 1):2)]
      
      RenElecGenFuel <- arrange(RenElecGenFuel,-row_number())
      
      RenElecGenFuel[is.na(RenElecGenFuel)] <- 0
      
      RenElecGenFuel <- melt(RenElecGenFuel, id.vars = "Year")
      
      
      RenElecGenFuel$variable <-
        factor(RenElecGenFuel$variable,
               levels = unique(RenElecGenFuel$variable),
               ordered = TRUE)
      
      RenElecGenFuel <- RenElecGenFuel %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Electricity generated from renewable\nsources"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#004529",
          "#006837",
          "#238443",
          "#41ab5d",
          "#78c679",
          "#7bccc4",
          "#4eb3d3",
          "#2b8cbe"
        )
      
      
      
      RenElecGenFuelChart <- RenElecGenFuel %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Onshore Wind" = BarColours[1],
            "Offshore Wind" = BarColours[2],
            "Hydro" = BarColours[3],
            "Solar PV" = BarColours[4],
            "Landfill gas" = BarColours[5],
            "Wave and tidal" = BarColours[6],
            "Sewage gas" = BarColours[7],
            "Other bioenergy" = BarColours[8],
            "Total" = "White"
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = RenElecGenFuel$Year,
          y = -20,
          label = ifelse(RenElecGenFuel$Year == "z", "", str_wrap(RenElecGenFuel$Year, width = 20)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
          size = 3,
          hjust = 1.05
        ) +
        geom_text(
          aes(
           x = 1998.5,
            y = (max(RenElecGenFuel$top)*1.1) * (.5 / 8),
            label = "Onshore\nWind"
          ),
          fontface = 2,
          colour =  BarColours[1],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1998.5,
            y = (max(RenElecGenFuel$top)*1.1) * (1.5 / 8),
            label = "Offshore\nWind"
          ),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1998.5,
            y = (max(RenElecGenFuel$top)*1.1) * (2.5 / 8),
            label = "Hydro"
          ),
          fontface = 2,
          colour =  BarColours[3],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1998.5,
            y = (max(RenElecGenFuel$top)*1.1) * (3.5 / 8),
            label = "Solar PV"
          ),
          fontface = 2,
          colour =  BarColours[4],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1998.5,
            y = (max(RenElecGenFuel$top)*1.1) * (4.5 / 8),
            label = "Landfill\ngas"
          ),
          fontface = 2,
          colour =  BarColours[5],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1998.5,
            y = (max(RenElecGenFuel$top)*1.1) * (5.5 / 8),
            label = "Wave\nand Tidal"
          ),
          fontface = 2,
          colour =  BarColours[6],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1998.5,
            y = (max(RenElecGenFuel$top)*1.1) * (6.5 / 8),
            label = "Sewage\nGas"
          ),
          fontface = 2,
          colour =  BarColours[7],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1998.5,
            y = (max(RenElecGenFuel$top)*1.1) * (7.5 / 8),
            label = "Other\nBioenergy"
          ),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 2010.5,
              y = (max(RenElecGenFuel$top)*1.1) * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = (max(RenElecGenFuel$top)*1.1) * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = -350,
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = RenElecGenFuel$Year ,
            y = RenElecGenFuel$top,
            label = paste(format(
              round(RenElecGenFuel$top, digits = 0), big.mark = ","
            ), "GWh")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = -0.1,
          size = 3
        ) 
      RenElecGenFuelChart
      
      
      RenElecGenFuelChart <-
        StackedBars(RenElecGenFuelChart,
                    RenElecGenFuel,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      RenElecGenFuelChart <-
        RenElecGenFuelChart +
        labs(subtitle = paste("Scotland,", min(RenElecGenFuel$Year), "-", max(RenElecGenFuel$Year))) +
        coord_flip() +
        xlim(max(RenElecGenFuel$Year+.5),min(RenElecGenFuel$Year-1.5))+
        ylim(-(max(RenElecGenFuel$top)*0.03), (max(RenElecGenFuel$top)*1.15))
      
      RenElecGenFuelChart
      
      ggsave(
        file,
        plot = RenElecGenFuelChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$ScotRenGenSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish renewables generation", col_names = TRUE, 
                       skip = 13)
    
    Data <- Data[c(1,4,7,10)]
    
    names(Data) <- c("Year", "Renewables", "Wind", "Hydro")
    
    ScotRenGen <- Data
    ### variables
    
    paste("Scotland,", min(ScotRenGen$Year),"-", max(ScotRenGen$Year))
  })
  
  output$ScotRenGenPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish renewables generation", col_names = TRUE, 
                       skip = 13)
    
    Data <- Data[c(1,4,7,10)]
    
    names(Data) <- c("Year", "Renewables", "Wind", "Hydro")
    
    ScotRenGen <- Data
    
    
    ChartColours <- c("#39ab2c",  "#fdb462", "#34d1a3", "#66c2a5","#8da0cb")
    sourcecaption = "Source: BEIS"
    plottitle = "Renewable generation as proportion of UK"
    
    ScotRenGen$Year <- paste0("01/01/", ScotRenGen$Year)
    
    ScotRenGen$Year <- dmy(ScotRenGen$Year)
    
    
    p <-  plot_ly(data = ScotRenGen,
                  x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "Renewables",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "All Renewables: ",
                  percent(ScotRenGen$Renewables, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotRenGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotRenGen[which(ScotRenGen$Renewables > 0 | ScotRenGen$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewable Electricity",
        text = paste0(
          "All Renewables: ",
          percent(ScotRenGen[which(ScotRenGen$Renewables > 0 | ScotRenGen$Renewables < 0),][-1,]$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(ScotRenGen[which(ScotRenGen$Renewables > 0 | ScotRenGen$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(data = ScotRenGen,
                x = ~ Year,
                y = ~ Wind,
                name = "Wind",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Wind: ",
                  percent(ScotRenGen$Wind, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotRenGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotRenGen[which(ScotRenGen$Wind > 0 | ScotRenGen$Wind < 0),], 1),
        x = ~ Year,
        y = ~ `Wind`,
        name = "Wind",
        legendgroup = "2",
        text = paste0(
          "Wind: ",
          percent(ScotRenGen[which(ScotRenGen$Wind > 0 | ScotRenGen$Wind < 0),][-1,]$Wind, accuracy = 0.1),
          "\nYear: ",
          format(ScotRenGen[which(ScotRenGen$Wind > 0 | ScotRenGen$Wind < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_trace(data = ScotRenGen,
                x = ~ Year,
                y = ~ Hydro,
                name = "Hydro",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Hydro: ",
                  percent(ScotRenGen$Hydro, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotRenGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[5], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotRenGen[which(ScotRenGen$Hydro > 0 | ScotRenGen$Hydro < 0),], 1),
        x = ~ Year,
        y = ~ `Hydro`,
        name = "Hydro",
        legendgroup = "3",
        text = paste0(
          "Hydro: ",
          percent(ScotRenGen[which(ScotRenGen$Hydro > 0 | ScotRenGen$Hydro < 0),][-1,]$Hydro, accuracy = 0.1),
          "\nYear: ",
          format(ScotRenGen[which(ScotRenGen$Hydro > 0 | ScotRenGen$Hydro < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[5])
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
                     range = c(min(ScotRenGen$Year)-100, max(ScotRenGen$Year)+100)),
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
  
  
  output$ScotRenGenTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish renewables generation", col_names = TRUE, 
                       skip = 13)
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    ScotRenGen <- as_tibble(Data)
    
    names(ScotRenGen) <- c("Year", "Renewables - Scotland (GWh)", "Renewables - UK (GWh)", "Renewables - Scottish proportion of UK Total", 
                           "Wind - Scotland (GWh)", "Wind - UK (GWh)", "Wind - Scottish proportion of UK Total",
                           "Hydro - Scotland (GWh)", "Hydro - UK (GWh)", "Hydro - Scottish proportion of UK Total")
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Year'),
          th(colspan = 3, 'Total Renewables'),
          th(colspan = 3, 'Wind'),
          th(colspan = 3, 'Hydro')
        ),
        tr(
          lapply(rep(c('Scotland', 'UK', "% Scotland of UK Total"), 3), th)
        )
      )
    ))
    
    print(sketch)
    datatable(
      ScotRenGen,
      extensions = 'Buttons',
      # container = sketch,
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "Scottish Renewable Generation",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish Renewable Generation',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish Renewable Generation')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(c(4,7,10), 1) %>% 
      formatRound(c(2:3, 5:6,8:9), 0)
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ScotRenGenTable")
  })
  
  
  output$ScotRenGen.png <- downloadHandler(
    filename = "ScotRenGen.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Scottish renewables generation", col_names = TRUE, 
                         skip = 13)
      
      Data <- Data[c(1,4,7,10)]
      
      names(Data) <- c("Year", "Renewables", "Wind", "Hydro")
      
      ScotRenGen <- Data
      ### variables
      ChartColours <- c("#39ab2c",  "#fdb462", "#34d1a3", "#66c2a5","#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = "Renewable generation as proportion of UK"
      
      #ScotRenGen$`Renewables`Percentage <- PercentLabel(ScotRenGen$`Renewables`)
      
      
      ScotRenGenChart <- ScotRenGen %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = `Renewables`,
              label = percent(`Renewables`, 0.1)),
          colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1.4,
            y = `Renewables`,
            label = ifelse(Year == min(Year), percent(`Renewables`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.65,
            y = `Renewables`,
            label = ifelse(Year == max(Year), percent(`Renewables`, accuracy = .1), ""),
            hjust = 0.5,
            
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotRenGen, 1),
          aes(x = Year,
              y = `Renewables`,
              show_guide = FALSE),
          colour = ChartColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotRenGen$Year),
          y = mean(ScotRenGen$`Renewables`),
          label = "All renewables",
          hjust = 0.5,
          vjust = 2.5,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Wind`,
              label = paste0(`Wind` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1.4,
            y = `Wind`,
            label = ifelse(Year == min(Year), percent(`Wind`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = -0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.65,
            y = `Wind`,
            label = ifelse(Year == max(Year), percent(`Wind`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotRenGen, 1),
          aes(x = Year,
              y = `Wind`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotRenGen$Year),
          y = mean(ScotRenGen$`Wind`),
          label = "Wind",
          hjust = 0.5,
          vjust = -3,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Hydro`,
              label = percent(`Hydro`, 0.1)),
          colour = ChartColours[5],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1.4,
            y = `Hydro`,
            label = ifelse(Year == min(Year), percent(`Hydro`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.65,
            y = `Hydro`,
            label = ifelse(Year == max(Year), percent(`Hydro`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = 0,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotRenGen, 1),
          aes(x = Year,
              y = `Hydro`,
              show_guide = FALSE),
          colour = ChartColours[5],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotRenGen$Year),
          y = mean(ScotRenGen$`Hydro`),
          label = "Hydro",
          hjust = 0.5,
          vjust = -1,
          colour = ChartColours[5],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              Year,
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      ScotRenGenChart
      
      ScotRenGenChart <-
        StackedArea(ScotRenGenChart,
                    ScotRenGen,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ScotRenGenChart <- ScotRenGenChart
      
      ScotRenGenChart
      
      ggsave(
        file,
        plot =  ScotRenGenChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EUWindSubtitle <- renderText({
    
    EURenElec <- read_excel("Structure/CurrentWorking.xlsx",
                            sheet = "Wind and hydro gen EU", col_names = TRUE, 
                            skip = 15, n_max = 30)
    
    EURenElec <- EURenElec[,c(1:ncol(EURenElec))]
    
    
    
    names(EURenElec)[1] <- c("Countries")
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenElec[2:ncol(EURenElec)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EURenElec)), na.rm = TRUE))
  })
  
  output$EUWindPlot <- renderPlotly  ({
    
    ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
    
    EUWind <- read_delim("Processed Data/Output/EU Wind Hydro/EUWind.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)
    
    EUWind <- EUWind[,c(1,ncol(EUWind))]
    
    names(EUWind) <- c("Countries", "Renewables")
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUWind$Renewables <- as.numeric(EUWind$Renewables)
    
    EUWind <- merge(EUWind, EUFlagLookup)
    
    EUWind$Group <- ifelse(EUWind$Renewables > 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (27)"), ChartColours[1],
                           ifelse(EUWind$Renewables <= 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (27)"), "D",
                                  ifelse(EUWind$Renewables > 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), ChartColours[2],
                                         ifelse(EUWind$Renewables <= 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), "E",      
                                                ifelse(EUWind$Renewables <= 0 , "D",  
                                                       ChartColours[2])))))
    
    EUWind <- EUWind[order(-EUWind$Renewables),]
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "U.K.", "Rest of the UK"))
    
    EUWind <- EUWind[-1:-2,]
    
    EUWind$Countries <- factor(EUWind$Countries, levels = unique(EUWind$Countries)[order(EUWind$Renewables, decreasing = FALSE)])
    
    p <- plot_ly(
      data = EUWind,
      y = ~Countries,
      x = ~Renewables,
      text = paste0(
        "Wind Generation: ",
        format(round(EUWind$Renewables, digits = 0), big.mark = ","),
        " GWh\nCountry: ",
        EUWind$Countries
      ),
      name = "EU Renewable Energy",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  as.list(EUWind$Group))
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE),
        xaxis = list(
          title = "GWh",
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
  
  output$EUWindTable = renderDataTable({
    
    ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
    
    EUWind <- read_delim("Processed Data/Output/EU Wind Hydro/EUWind.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)
    
    EUWind <- EUWind[,c(1, 16:ncol(EUWind))]
    
    
    
    names(EUWind)[1] <- c("Countries")
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EUWind[2:ncol(EUWind)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EUWind,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EUWind)-1, 'desc')),
        title = "European Wind Generation (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'European Wind Generation (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'European Wind Generation (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(EUWind), 0) %>% 
      formatStyle(1,
                  target = 'row',
                  backgroundColor = styleEqual(c('Scotland'), c('#bdbdbd')))
  })
  
  output$EUWind.png <- downloadHandler(
    filename = "EUWind.png",
    content = function(file) {
      
      EUWind <- read_delim("Processed Data/Output/EU Wind Hydro/EUWind.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)
      
      EUWind <- EUWind[-1:-3,]
      
      EUWind <- EUWind[,c(1,ncol(EUWind))]
      
      names(EUWind) <- c("Countries", "Renewables")
      
      EUWind <- EUWind[which(EUWind$Renewables > 100),]
      
      EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
      
      EUWind <- merge(EUWind, EUFlagLookup)
      
      EUWind$Group <- ifelse(EUWind$Renewables > 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                             ifelse(EUWind$Renewables <= 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                    ifelse(EUWind$Renewables > 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), "C",
                                           ifelse(EUWind$Renewables <= 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), "E",      
                                                  ifelse(EUWind$Renewables <= 0 , "D",  
                                                         "A")))))
      
      EUWind <- EUWind[order(-EUWind$Renewables),]
      
      EUWind$Renewables <- EUWind$Renewables /100000
      
      ### variables
      ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Wind generation in European countries"
      
      
      EUWind <- EUWind[order(EUWind$Renewables),]
      EUWind$Countries <-
        factor(EUWind$Countries, levels = EUWind$Countries)
      
      EUWindChart <-
        EUWind %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
        geom_flag(aes(
          y = -.025,
          size = 10,
          country = Flag
        )) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.64, 1.10) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual("Group",
                          values = c("A" = ChartColours[2], "B" = ChartColours[1], "C" = ChartColours[2], "D" = ChartColours[2], "E" = ChartColours[2])) +
        geom_text(
          label = ifelse(
            EUWind$Group == "B" |
              EUWind$Group == "C" |
              EUWind$Group == "E" ,
            paste(format(round(EUWind$Renewables*100000, digits = 0), big.mark = ","), "GWh") ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUWind$Renewables > .3, 1.1, 0),
          vjust = .5,
          color = ifelse(EUWind$Renewables > .3, "white", ChartColours[2])
        ) +
        geom_text(
          y = -0.055,
          label = EUWind$Countries,
          fontface = 2,
          family = "Century Gothic",
          hjust = 1,
          vjust = .5,
          color = "Black"
        ) +
        theme(
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
          ,
          axis.text.x = element_blank()
          ,
          axis.text.y = element_blank()
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
        ) + ### Label Plot
        labs(y = "Percentage", caption = sourcecaption) +
        labs(title = plottitle,
             face = "bold",
             subtitle = 2018) +
        ### 0 Axis
        
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
        
        
        ### Plot Borders
        annotate(
          geom = 'segment',
          x = Inf,
          xend = Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1.5
        ) +
        annotate(
          geom = 'segment',
          x = -Inf,
          xend = -Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1
        ) +
        annotation_custom(
          ScotFlag,
          xmin = match("SCOTLAND", EUWind$Countries) - .38,
          xmax = match("SCOTLAND", EUWind$Countries) + .38,
          ymax = .675
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EUWind$Countries) - .38,
          xmax = match("Latvia", EUWind$Countries) + .38,
          ymax = .675
        )
      
      
      EUWindChart
      
      
      ggsave(
        file,
        plot =  EUWindChart,
        width = 15,
        height = 22,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EUHydroSubtitle <- renderText({
    
    EUHydro <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = TRUE, 
                          skip = 48, n_max = 30)
    
    EUHydro <- EUHydro[,c(1:ncol(EUHydro))]
    
    
    
    names(EUHydro)[1] <- c("Countries")
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EUHydro[2:ncol(EUHydro)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EUHydro)), na.rm = TRUE))
  })
  
  output$EUHydroPlot <- renderPlotly  ({
    
    ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
    
    EUHydro <- read_delim("Processed Data/Output/EU Wind Hydro/EUHydro.txt", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)
    
    EUHydro <- EUHydro[,c(1,ncol(EUHydro))]
    
    names(EUHydro) <- c("Countries", "Renewables")
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUHydro$Renewables <- as.numeric(EUHydro$Renewables)
    
    EUHydro <- merge(EUHydro, EUFlagLookup)
    
    EUHydro$Group <- ifelse(EUHydro$Renewables > 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (27)"), ChartColours[1],
                            ifelse(EUHydro$Renewables <= 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (27)"), "D",
                                   ifelse(EUHydro$Renewables > 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), ChartColours[2],
                                          ifelse(EUHydro$Renewables <= 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), "E",      
                                                 ifelse(EUHydro$Renewables <= 0 , "D",  
                                                        ChartColours[2])))))
    
    EUHydro <- EUHydro[order(-EUHydro$Renewables),]
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "U.K.", "Rest of the UK"))
    
    EUHydro <- EUHydro[-1:-2,]
    
    EUHydro$Countries <- factor(EUHydro$Countries, levels = unique(EUHydro$Countries)[order(EUHydro$Renewables, decreasing = FALSE)])
    
    p <- plot_ly(
      data = EUHydro,
      y = ~Countries,
      x = ~Renewables,
      text = paste0(
        "Hydro Generation: ",
        format(round(EUHydro$Renewables, digits = 0), big.mark = ","),
        " GWh\nCountry: ",
        EUHydro$Countries
      ),
      name = "EU Renewable Energy",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  as.list(EUHydro$Group))
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE),
        xaxis = list(
          title = "GWh",
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
  
  output$EUHydroTable = renderDataTable({
    
    ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
    
    EUHydro <- read_delim("Processed Data/Output/EU Wind Hydro/EUHydro.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)
    
    EUHydro <- EUHydro[,c(1, 16:ncol(EUHydro))]
    
    
    
    names(EUHydro)[1] <- c("Countries")
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EUHydro[2:ncol(EUHydro)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EUHydro,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EUHydro)-1, 'desc')),
        title = "European Hydro Generation (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'European Hydro Generation (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'European Hydro Generation (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(EUHydro), 0) %>% 
      formatStyle(1,
                  target = 'row',
                  backgroundColor = styleEqual(c('Scotland'), c('#bdbdbd')))
  })

  
  output$EUHydro.png <- downloadHandler(
    filename = "EUHydro.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      EUHydro <- read_delim("Processed Data/Output/EU Wind Hydro/EUHydro.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)
      
      EUHydro <- EUHydro[-1:-3,]
      
      EUHydro <- EUHydro[,c(1,ncol(EUHydro))]
      
      names(EUHydro) <- c("Countries", "Renewables")
      
      EUHydro <- EUHydro[which(EUHydro$Renewables > 100),]
      
      EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
      
      EUHydro <- merge(EUHydro, EUFlagLookup)
      
      EUHydro$Group <- ifelse(EUHydro$Renewables > 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                              ifelse(EUHydro$Renewables <= 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                     ifelse(EUHydro$Renewables > 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), "C",
                                            ifelse(EUHydro$Renewables <= 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), "E",      
                                                   ifelse(EUHydro$Renewables <= 0 , "D",  
                                                          "A")))))
      
      EUHydro <- EUHydro[order(-EUHydro$Renewables),]
      
      EUHydro$Renewables <- EUHydro$Renewables /100000
      
      ### variables
      ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Hydro generation in European countries"
      
      
      EUHydro <- EUHydro[order(EUHydro$Renewables),]
      EUHydro$Countries <-
        factor(EUHydro$Countries, levels = EUHydro$Countries)
      
      EUHydroChart <-
        EUHydro %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
        geom_flag(aes(
          y = -.05,
          size = 10,
          country = Flag
        )) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.84, 1.40) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual("Group",
                          values = c("A" = ChartColours[2], "B" = ChartColours[1], "C" = ChartColours[2], "D" = ChartColours[2], "E" = ChartColours[2])) +
        geom_text(
          label = ifelse(
            EUHydro$Group == "B" |
              EUHydro$Group == "C" |
              EUHydro$Group == "E" ,
            paste(format(round(EUHydro$Renewables*100000, digits = 0), big.mark = ","), "GWh") ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUHydro$Renewables > .3, 1.1, 0),
          vjust = .5,
          color = ifelse(EUHydro$Renewables > .3, "white", ChartColours[2])
        ) +
        geom_text(
          y = -0.095,
          label = EUHydro$Countries,
          fontface = 2,
          family = "Century Gothic",
          hjust = 1,
          vjust = .5,
          color = "Black"
        ) +
        theme(
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
          ,
          axis.text.x = element_blank()
          ,
          axis.text.y = element_blank()
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
        ) + ### Label Plot
        labs(y = "Percentage", caption = sourcecaption) +
        labs(title = plottitle,
             face = "bold",
             subtitle = 2018) +
        ### 0 Axis
        
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
        
        
        ### Plot Borders
        annotate(
          geom = 'segment',
          x = Inf,
          xend = Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1.5
        ) +
        annotate(
          geom = 'segment',
          x = -Inf,
          xend = -Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1
        ) +
        annotation_custom(
          ScotFlag,
          xmin = match("SCOTLAND", EUHydro$Countries) - .4,
          xmax = match("SCOTLAND", EUHydro$Countries) + .4,
          ymax = .847
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EUHydro$Countries) - .4,
          xmax = match("Latvia", EUHydro$Countries) + .4,
          ymax = .847
        )
      
      
      EUHydroChart
      
      
      ggsave(
        file,
        plot =  EUHydroChart,
        width = 15,
        height = 22,
        units = "cm",
        dpi = 300
      )
    })
  
  
  
  observeEvent(input$ToggleTable3, {
    toggle("EUWindTable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("EUHydroTable")
  })
  
  Data <- read_excel("Structure/CurrentWorking.xlsx",
                     sheet = "R - QTRScotGen",
                     col_names = FALSE
  )
  
  Data <- as_tibble(t(Data))
  
  names(Data) <- unlist(Data[1,])
  
  names(Data)[1] <- "Year"
  
  Data <- Data[-1,]
  
  Data[2:10] %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  Data <- as_tibble(Data)
  
  Data$Quarter <- paste0("Q",substr(Data$Year,8,8))
  
  Data$Year <- as.numeric(substr(Data$Year,1,4))
  
  names(Data) <- c("Year", "Onshore wind", "Offshore wind", "Shoreline wave / tidal", "Solar PV", "Hydro", "Landfill gas", "Sewage sludge digestion", "Other biomass (inc. co-firing)", "Total" , "Quarter")
  
  Data <- melt(Data, id.vars = c("Year", "Quarter"))
  
  Subset <- Data[which(Data$variable == "Total"),]
  
  Subset <- dcast(Subset, Year ~ Quarter)
  
  Subset <- Subset[which(Subset$Year >= min(Subset[complete.cases(Subset),]$Year)),]
  
  Subset[is.na(Subset)] <- 0
  
  Subset$Total <- Subset$Q1 + Subset$Q2 + Subset$Q3 + Subset$Q4
  
  Subset <- Subset[order(-Subset$Year),]
  
  row.names(Subset) <- NULL
  
  output$RenElecQuarterGenerationSubtitle <- renderText({
    
    paste("Scotland,", min(Subset$Year), " - " ,max(Subset$Year))
  })
  
  output$RenElecQuarterGenerationPlot = renderPlotly({
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#004529",
        "#006837",
        "#238443",
        "#41ab5d",
        "#78c679",
        "#7bccc4",
        "#4eb3d3",
        "#2b8cbe"
      )
    
    Subset$Year <- paste0("<b>", Subset$Year, "</b>")
    
    p <- plot_ly(
      data = Subset,
      y = ~Year,
      x = ~`Q1`,
      legendgroup = 1,
      text = paste0(
        "Q1: ",
        format(round(Subset$`Q1`, digits = 0),big.mark = ","),
        " GWh\nYear: ",
        Subset$Year
      ),
      name = "Q1",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>% 
      add_trace(
        data = Subset,
        y = ~Year,
        x = ~`Q2`,
        legendgroup = 2,
        text = paste0(
          "Q2: ",
          format(round(Subset$`Q2`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          Subset$Year
        ),
        name = "Q2",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      )  %>% 
      add_trace(
        data = Subset,
        y = ~Year,
        x = ~`Q3`,
        legendgroup = 3,
        text = paste0(
          "Q3: ",
          format(round(Subset$`Q3`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          Subset$Year
        ),
        name = "Q3",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[3])
      )  %>% 
      add_trace(
        data = Subset,
        y = ~Year,
        x = ~`Q4`,
        legendgroup = 4,
        text = paste0(
          "Q4: ",
          format(round(Subset$`Q4`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          Subset$Year
        ),
        name = "Q4",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[4])
      )  %>% 
      add_trace(
        data = Subset,
        y = ~Year,
        x = ~Total + 100,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Subset$Q4 <= 0,
                      paste("<b>",format(round(Subset$Total, digits = 0), big.mark = ","),"GWh\n Year to date</b>"),
          paste("<b>",format(round(Subset$Total, digits = 0), big.mark = ","),"GWh</b>")
          ),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     autorange = 'reversed',
                     dtick = 1),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          range = c(0, max(Subset$Total)*1.15),
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
  })
  
  output$RenElecQuarterGeneration.png <- downloadHandler(
    filename = "RenElecQuarterGeneration.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      Subset$Total <- NULL
      
      Subset <- melt(Subset, id.vars = "Year")
      
      Subset$variable <-
        factor(Subset$variable,
               levels = rev(unique(Subset$variable)),
               ordered = TRUE)
      
      Subset <- Subset %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Quarterly electricity generated from\nrenewable sources"
      sourceGention <- "Source: BEIS"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#004529",
          "#006837",
          "#238443",
          "#41ab5d",
          "#78c679",
          "#7bccc4",
          "#4eb3d3",
          "#2b8cbe"
        )
      
      
      SubsetChart <- Subset %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Q1" = BarColours[1],
            "Q2" = BarColours[2],
            "Q3" = BarColours[3],
            "Q4" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = Subset$Year,
          y = -.01,
          label = ifelse(Subset$Year == "z", "", str_wrap(Subset$Year, width = 20)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
          hjust = 1.05
        ) +
        geom_text(
          aes(
            x = 2010,
            y = max(Subset$top) * (0.1/ 3),
            label = "Q1"
          ),
          fontface = 2,
          colour =  BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(
            x = 2010,
            y = max(Subset$top) * (1 / 3),
            label = "Q2"
          ),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(
            x = 2010,
            y = max(Subset$top) * (2 / 3),
            label = "Q3"
          ),
          fontface = 2,
          colour =  BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(
            x = 2010,
            y = max(Subset$top) * (3 / 3),
            label = "Q4"
          ),
          fontface = 2,
          colour =  BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(
            x = Subset$Year ,
            y = Subset$top,
            label = paste(format(
              round(Subset$top, digits = 0), big.mark = ","
            ), "GWh")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = -0.1
          
        ) 
      SubsetChart
      
      
      SubsetChart <-
        StackedBars(SubsetChart,
                    SubsetFuel,
                    plottitle,
                    sourceGention,
                    ChartColours)
      
      SubsetChart <-
        SubsetChart +
        labs(subtitle = paste("Scotland,", min(Subset$Year), "-", max(Subset$Year))) +
        coord_flip() +
        xlim(max(Subset$Year+.5),min(Subset$Year-1)) +
        ylim(-max(Subset$top*0.05),max(Subset$top)*1.25)
      
      SubsetChart
      
      ggsave(
        file,
        plot = SubsetChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenElecQuarterTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "R - QTRScotGen",
                       col_names = FALSE
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year & Quarter"
    
    Data <- Data[-1,]
    
    Data[2:10] %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    names(Data) <- c("Quarter", "Onshore wind", "Offshore wind", "Shoreline wave / tidal", "Solar PV", "Hydro", "Landfill gas", "Sewage sludge digestion", "Other biomass (inc. co-firing)", "Total")
    
    Data$Quarter <- paste0(substr(Data$Quarter,1,4), " Q", substr(Data$Quarter,8,8))
    
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
        title = "Installed Generation by Quarter (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Installed Generation by Quarter (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Installed Generation by Quarter (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(Data), 0)
  })
  
  observeEvent(input$ToggleTable5, {
    toggle("RenElecQuarterTable")
  })
}
