require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenElecFuelOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Installed capacity",
    fluidRow(column(8,
                    h3("Installed capacity of sites generating electricity from renewable sources", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenElecFuelSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecFuel.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenElecFuelPlot")),
    plotlyOutput(ns("RenElecFuelPlot"), height = "900px")%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Electricity generated",
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
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Installed Capacity",
    fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RenElecFuelCapTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Electricity Generated",
             fluidRow(
               column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecFuelGenTable"))%>% withSpinner(color="#39ab2c"))),
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
RenElecFuel <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecFuel.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RenElecFuelSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecCapFuel <- as_tibble(Data)
    
    paste(min(RenElecCapFuel$Year),"-", max(RenElecCapFuel$Year))
  })
  
  output$RenElecFuelGenSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -2)
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecGenFuel <- as_tibble(Data)
    
    paste(min(RenElecGenFuel$Year),"-", max(RenElecGenFuel$Year))
  })
  
  output$RenElecFuelPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecCapFuel <- as_tibble(Data)
    
    RenElecCapFuel <- RenElecCapFuel[c(1, (ncol(RenElecCapFuel) - 1):2)]
    
    RenElecCapFuel <- arrange(RenElecCapFuel,-row_number())
    
    RenElecCapFuel$Total <- RenElecCapFuel$`Other bioenergy` + RenElecCapFuel$`Sewage gas` + RenElecCapFuel$`Wave and tidal` + RenElecCapFuel$`Landfill gas` + RenElecCapFuel$`Solar PV` + RenElecCapFuel$Hydro + RenElecCapFuel$`Offshore Wind` + RenElecCapFuel$`Onshore Wind`
    
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
    
    RenElecCapFuel$Year <- paste0("<b>", RenElecCapFuel$Year, "</b>")
    
    p <- plot_ly(
      data = RenElecCapFuel,
      y = ~Year,
      x = ~`Onshore Wind`,
      legendgroup = 1,
      text = paste0(
        "Onshore Wind: ",
        format(round(RenElecCapFuel$`Onshore Wind`, digits = 0),big.mark = ","),
        " MWe\nYear: ",
        RenElecCapFuel$Year
      ),
      name = "Onshore Wind",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
      ) %>% 
        add_trace(
          data = RenElecCapFuel,
          y = ~Year,
          x = ~`Offshore Wind`,
          legendgroup = 2,
          text = paste0(
            "Offshore Wind: ",
            format(round(RenElecCapFuel$`Offshore Wind`, digits = 0),big.mark = ","),
            " MWe\nYear: ",
            RenElecCapFuel$Year
          ),
          name = "Offshore Wind",
          type = "bar",
          hoverinfo = "text",
          orientation = 'h',
          marker = list(color =  BarColours[2])
    )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Hydro`,
        legendgroup = 3,
        text = paste0(
          "Hydro: ",
          format(round(RenElecCapFuel$`Hydro`, digits = 0),big.mark = ","),
          " MWe\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Hydro",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[3])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Solar PV`,
        legendgroup = 4,
        text = paste0(
          "Solar PV: ",
          format(round(RenElecCapFuel$`Solar PV`, digits = 0),big.mark = ","),
          " MWe\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Solar PV",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[4])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Landfill gas`,
        legendgroup = 5,
        text = paste0(
          "Landfill gas: ",
          format(round(RenElecCapFuel$`Landfill gas`, digits = 0),big.mark = ","),
          " MWe\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Landfill gas",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[5])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Wave and tidal`,
        legendgroup = 6,
        text = paste0(
          "Wave and tidal: ",
          format(round(RenElecCapFuel$`Wave and tidal`, digits = 0),big.mark = ","),
          " MWe\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Wave and tidal",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[6])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Sewage gas`,
        legendgroup = 7,
        text = paste0(
          "Sewage gas: ",
          format(round(RenElecCapFuel$`Sewage gas`, digits = 0),big.mark = ","),
          " MWe\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Sewage gas",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[7])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Other bioenergy`,
        legendgroup = 8,
        text = paste0(
          "Other bioenergy: ",
          format(round(RenElecCapFuel$`Other bioenergy`, digits = 0),big.mark = ","),
          " MWe\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Other bioenergy",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[8])
      )  %>%
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~Total + 100,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste("<b>",format(round(RenElecCapFuel$Total, digits = 0), big.mark = ","),"MWe</b>"),
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
                     autorange = "reversed",
                     dtick = 1),
        xaxis = list(
          title = "",
          tickformat = "",
          range = c(0,13000),
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
  
  
  output$RenElecFuelCapTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecCapFuel <- as_tibble(Data)
    
    RenElecCapFuel <- RenElecCapFuel[c(1, (ncol(RenElecCapFuel) - 1):2)]
    
    RenElecCapFuel <- arrange(RenElecCapFuel,-row_number())
    
    RenElecCapFuel$Total <- RenElecCapFuel$`Other bioenergy` + RenElecCapFuel$`Sewage gas` + RenElecCapFuel$`Wave and tidal` + RenElecCapFuel$`Landfill gas` + RenElecCapFuel$`Solar PV` + RenElecCapFuel$Hydro + RenElecCapFuel$`Offshore Wind` + RenElecCapFuel$`Onshore Wind`
    
    
    datatable(
      RenElecCapFuel[c(1,9:2,10)],
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
        title = "Renewable Electricity - Installed Capacity (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Electricity - Installed Capacity (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Electricity - Installed Capacity (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecCapFuel), 0)
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
    
    Data <- head(Data, -2)
    
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
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecFuel.txt")[2])
                    
                  )))
 })
  observeEvent(input$ToggleTable, {
    toggle("RenElecFuelCapTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("RenElecFuelGenTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenElecFuel.png <- downloadHandler(
    filename = "RenElecFuel.png",
    content = function(file) {

      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      Data <- read_excel("Structure/CurrentWorking.xlsx",
                           sheet = "Renewable elec by fuel",
                           col_names = TRUE,
                           skip = 12
      )
      
      Data<- Data[complete.cases(Data),]
      
      Data <- distinct(Data, Year, .keep_all = TRUE)
      
      Data <- head(Data, -1)
      
      Data %<>% lapply(function(x)
        as.numeric(as.character(x)))
      
      RenElecCapFuel <- as_tibble(Data)
      
      RenElecCapFuel <- RenElecCapFuel[c(1, (ncol(RenElecCapFuel) - 1):2)]
      
      RenElecCapFuel <- arrange(RenElecCapFuel,-row_number())
      
      
      RenElecCapFuel <- melt(RenElecCapFuel, id.vars = "Year")
      
      
      RenElecCapFuel$variable <-
        factor(RenElecCapFuel$variable,
               levels = unique(RenElecCapFuel$variable),
               ordered = TRUE)
      
      RenElecCapFuel <- RenElecCapFuel %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Installed capacity of sites generating\nelectricity from renewable sources"
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
      
      
      RenElecCapFuelChart <- RenElecCapFuel %>%
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
          x = RenElecCapFuel$Year,
          y = -.01,
          label = ifelse(RenElecCapFuel$Year == "z", "", str_wrap(RenElecCapFuel$Year, width = 20)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
          size = 3,
          hjust = 1.05
        ) +
        geom_text(
          aes(
            x = 1999,
            y = 12350 * (.5 / 8),
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
            x = 1999,
            y = 12350 * (1.5 / 8),
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
            x = 1999,
            y = 12350 * (2.5 / 8),
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
            x = 1999,
            y = 12350 * (3.5 / 8),
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
            x = 1999,
            y = 12350 * (4.5 / 8),
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
            x = 1999,
            y = 12350 * (5.5 / 8),
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
            x = 1999,
            y = 12350 * (6.5 / 8),
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
            x = 1999,
            y = 12350 * (7.5 / 8),
            label = "Other\nBioenergy"
          ),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 1998,
              y = 12350 * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = 12350 * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = -100,
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = RenElecCapFuel$Year ,
            y = RenElecCapFuel$top,
            label = paste(format(
              round(RenElecCapFuel$top, digits = 0), big.mark = ","
            ), "MWe")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = -0.1,
          size = 3
        ) 
      RenElecCapFuelChart
      
      
      RenElecCapFuelChart <-
        StackedBars(RenElecCapFuelChart,
                    RenElecCapFuel,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      RenElecCapFuelChart <-
        RenElecCapFuelChart +
        labs(subtitle = paste("Scotland,", min(RenElecCapFuel$Year), "-", max(RenElecCapFuel$Year))) +
        coord_flip() +
        xlim(max(RenElecCapFuel$Year+.5),min(RenElecCapFuel$Year-1))
      
      RenElecCapFuelChart
      
      ggsave(
        file,
        plot = RenElecCapFuelChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
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
        " MWe\nYear: ",
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
          " MWe\nYear: ",
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
          " MWe\nYear: ",
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
          " MWe\nYear: ",
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
          " MWe\nYear: ",
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
          " MWe\nYear: ",
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
          " MWe\nYear: ",
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
          " MWe\nYear: ",
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
        "Electricity generated from renewable sources"
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
           x = 1999,
            y = 31000 * (.5 / 8),
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
           x = 1999,
            y = 31000 * (1.5 / 8),
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
           x = 1999,
            y = 31000 * (2.5 / 8),
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
           x = 1999,
            y = 31000 * (3.5 / 8),
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
           x = 1999,
            y = 31000 * (4.5 / 8),
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
           x = 1999,
            y = 31000 * (5.5 / 8),
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
           x = 1999,
            y = 31000 * (6.5 / 8),
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
           x = 1999,
            y = 31000 * (7.5 / 8),
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
              y = 31000 * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = 31000 * (8 / 8),
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
        xlim(max(RenElecGenFuel$Year+.5),min(RenElecGenFuel$Year-1))
      
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
}
