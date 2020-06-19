require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######percent

source("Structure/Global.R")

ElecGenOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
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
               imageOutput(ns("ElecGenAnimation"), height = "675px")%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Low Carbon vs Fossil Fuels",
    fluidRow(column(8,
                    h3("Electricity generation - low carbon versus fossil fuels", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('ElecGenLCFFSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecGenLCFF.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    plotlyOutput(ns("ElecGenLCFFPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Nuclear vs Renewables",
             fluidRow(column(8,
                             h3("Electricity generation - nuclear versus renewables", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('ElecGenRNSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecGenRN.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("ElecGenLCFFPlot")),
             plotlyOutput(ns("ElecGenRNPlot"))%>% withSpinner(color="#39ab2c"),
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
      tabPanel("Scotland",
               fluidRow(
                 column(10, h3("Data - Scotland (GWh)", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("ElecGenFuelTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Scotland Proportion",
               fluidRow(
                 column(10, h3("Data - Scottish electricity generation proportions", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("ElecGenFuelScotPropTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("England & Wales",
               fluidRow(
                 column(10, h3("Data - England & Wales (GWh)", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("ElecGenFuelEWTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISRenElec", "BEISElecGen"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISRenElec"),
        SourceLookup("BEISElecGen")
        
      )
    )
  )
}




###### Server ######
ElecGen <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ElecGen.R")

  
  output$ElecGenLCFFSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen low carbon and fossil",
        col_names = FALSE,
        skip = 28,
        n_max = 15
      )
    
    Data <- as_tibble(t(Data))
    
    Data <- Data[c(1,4,9)]
    
    Data <- Data[complete.cases(Data),]
    
    names(Data) <- c("Year", "Low Carbon", "Fossil Fuels")
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    ElecFuelLowCarbon <- as_tibble(Data)
    
    paste("Scotland,", min(ElecFuelLowCarbon$Year),"-", max(ElecFuelLowCarbon$Year))
  })
  
  output$ElecGenLCFFPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen low carbon and fossil",
        col_names = FALSE,
        skip = 28,
        n_max = 15
      )
    
    Data <- as_tibble(t(Data))
    
    Data <- Data[c(1,4,9)]
    
    Data <- Data[complete.cases(Data),]
    
    names(Data) <- c("Year", "Low Carbon", "Fossil Fuels")
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    ElecFuelLowCarbon <- as_tibble(Data)
    
    ElecFuelLowCarbon$Year <- paste0("01/01/", ElecFuelLowCarbon$Year)
    
    ElecFuelLowCarbon$Year <- dmy(ElecFuelLowCarbon$Year)
    
    ### variables
    ChartColours <- c("#39ab2c", "#1a9850", "#f46d43", "#39ab2c")
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity generation -\nlow carbon versus fossil fuels"
    
    
    p <-  plot_ly(ElecFuelLowCarbon, x = ~ Year ) %>% 
      add_trace(y = ~ `Low Carbon`,
                name = "Low Carbon",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Low Carbon Proportion: ",
                  percent(ElecFuelLowCarbon$`Low Carbon`, accuracy = 0.1),
                  "\nYear: ",
                  format(ElecFuelLowCarbon$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecFuelLowCarbon[which(ElecFuelLowCarbon$`Low Carbon` > 0 | ElecFuelLowCarbon$`Low Carbon` < 0),], 1),
        x = ~ Year,
        y = ~ `Low Carbon`,
        name = "Low Carbon",
        text = paste0(
          "Low Carbon Proportion: ",
          percent(ElecFuelLowCarbon[which(ElecFuelLowCarbon$`Low Carbon` > 0 | ElecFuelLowCarbon$`Low Carbon` < 0),][-1,]$`Low Carbon`, accuracy = 0.1),
          "\nYear: ",
          format(ElecFuelLowCarbon[which(ElecFuelLowCarbon$`Low Carbon` > 0 | ElecFuelLowCarbon$`Low Carbon` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(
        x = ElecFuelLowCarbon$Year,
        y = ~ ElecFuelLowCarbon$`Fossil Fuels`,
                name = "Fossil Fuels",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Fossil Fuels Proportion: ",
                  percent(ElecFuelLowCarbon$`Fossil Fuels`, accuracy = 0.1),
                  "\nYear: ",
                  format(ElecFuelLowCarbon$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecFuelLowCarbon[which(ElecFuelLowCarbon$`Fossil Fuels` > 0 | ElecFuelLowCarbon$`Fossil Fuels` < 0),], 1),
        x = ~ Year,
        y = ~ `Fossil Fuels`,
        name = "Fossil Fuels",
        text = paste0(
          "Fossil Fuels Proportion: ",
          percent(ElecFuelLowCarbon[which(ElecFuelLowCarbon$`Fossil Fuels` > 0 | ElecFuelLowCarbon$`Fossil Fuels` < 0),][-1,]$`Fossil Fuels`, accuracy = 0.1),
          "\nYear: ",
          format(ElecFuelLowCarbon[which(ElecFuelLowCarbon$`Fossil Fuels` > 0 | ElecFuelLowCarbon$`Fossil Fuels` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
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
                     range = c(min(ElecFuelLowCarbon$Year)-100, max(ElecFuelLowCarbon$Year)+100)),
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
  
  output$ElecGenRNSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen low carbon and fossil",
        col_names = FALSE,
        skip = 28,
        n_max = 15
      )
    
    Data <- as_tibble(t(Data))
    
    Data <- Data[c(1,2,3)]
    
    Data <- Data[complete.cases(Data),]
    
    names(Data) <- c("Year", "Renewables", "Nuclear")
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    ElecFuelRenNuclear <- as_tibble(Data)
    
    paste("Scotland,", min(ElecFuelRenNuclear$Year),"-", max(ElecFuelRenNuclear$Year))
  })
  
  output$ElecGenRNPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen low carbon and fossil",
        col_names = FALSE,
        skip = 28,
        n_max = 15
      )
    
    Data <- as_tibble(t(Data))
    
    Data <- Data[c(1,2,3)]
    
    Data <- Data[complete.cases(Data),]
    
    names(Data) <- c("Year", "Renewables", "Nuclear")
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    ElecFuelRenNuclear <- as_tibble(Data)
    
    ElecFuelRenNuclear$Year <- paste0("01/01/", ElecFuelRenNuclear$Year)
    
    ElecFuelRenNuclear$Year <- dmy(ElecFuelRenNuclear$Year)
    
    ### variables
    ChartColours <- c("#39ab2c", "#1a9850", "#4393c3", "#39ab2c")
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity generation -\nnuclear versus renewables"
    
    
    p <-  plot_ly(ElecFuelRenNuclear, x = ~ Year ) %>% 
      add_trace(y = ~ `Renewables`,
                name = "Renewables",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Renewables Proportion: ",
                  percent(ElecFuelRenNuclear$`Renewables`, accuracy = 0.1),
                  "\nYear: ",
                  format(ElecFuelRenNuclear$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecFuelRenNuclear[which(ElecFuelRenNuclear$`Renewables` > 0 | ElecFuelRenNuclear$`Renewables` < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewables",
        text = paste0(
          "Renewables Proportion: ",
          percent(ElecFuelRenNuclear[which(ElecFuelRenNuclear$`Renewables` > 0 | ElecFuelRenNuclear$`Renewables` < 0),][-1,]$`Renewables`, accuracy = 0.1),
          "\nYear: ",
          format(ElecFuelRenNuclear[which(ElecFuelRenNuclear$`Renewables` > 0 | ElecFuelRenNuclear$`Renewables` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(
        x = ElecFuelRenNuclear$Year,
        y = ~ ElecFuelRenNuclear$`Nuclear`,
        name = "Nuclear",
        type = 'scatter',
        mode = 'lines',
        text = paste0(
          "Nuclear Proportion: ",
          percent(ElecFuelRenNuclear$`Nuclear`, accuracy = 0.1),
          "\nYear: ",
          format(ElecFuelRenNuclear$Year, "%Y")
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecFuelRenNuclear[which(ElecFuelRenNuclear$`Nuclear` > 0 | ElecFuelRenNuclear$`Nuclear` < 0),], 1),
        x = ~ Year,
        y = ~ `Nuclear`,
        name = "Nuclear",
        text = paste0(
          "Nuclear Proportion: ",
          percent(ElecFuelRenNuclear[which(ElecFuelRenNuclear$`Nuclear` > 0 | ElecFuelRenNuclear$`Nuclear` < 0),][-1,]$`Nuclear`, accuracy = 0.1),
          "\nYear: ",
          format(ElecFuelRenNuclear[which(ElecFuelRenNuclear$`Nuclear` > 0 | ElecFuelRenNuclear$`Nuclear` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
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
                     range = c(min(ElecFuelRenNuclear$Year)-100, max(ElecFuelRenNuclear$Year)+100)),
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
  
  
  output$ElecGenLCFFTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen low carbon and fossil",
        col_names = FALSE,
        skip = 28,
        n_max = 10
      )
    
    Data[1,1] <- "Year"
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    ElecFuelLowCarbon <- as_tibble(Data)
    
    ### variables
    ChartColours <- c("#39ab2c", "#1a9850", "#f46d43", "#39ab2c")
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity generation -\nlow carbon versus fossil fuels (GWh)"
    
    datatable(
      ElecFuelLowCarbon,
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
        title = "Electricity generation - Fuel proportion (%)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Electricity generation - Fuels proportion (%)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Electricity generation - Fuels proportion (%)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(2:10), 1) %>% 
      formatStyle(c(4,5,9), fontStyle = "italic")
  })
  
  output$ElecGenFuel2Table = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen low carbon and fossil",
        col_names = FALSE,
        skip = 16,
        n_max = 10
      )
    
    Data[1,1] <- "Year"
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    ElecFuelLowCarbon <- as_tibble(Data)
    
    ### variables
    ChartColours <- c("#39ab2c", "#1a9850", "#f46d43", "#39ab2c")
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity generation -\nlow carbon versus fossil fuels (GWh)"
    
    datatable(
      ElecFuelLowCarbon,
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
        title = "Electricity generation - Fuels (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Electricity generation - Fuels (GWh)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Electricity generation - Fuels (GWh)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:11), 0) %>% 
      formatStyle(c(4,5,9,11), fontStyle = "italic")
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/ElecGen.txt")[2])
                     
                   )))
  })
  
 observeEvent(input$ToggleTable3, {
    toggle("ElecGenFuel2Table")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("ElecGenLCFFTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ElecGenLCFF.png <- downloadHandler(
    filename = "ElecGenLCFF.png",
    content = function(file) {

      Data <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Elec gen low carbon and fossil",
          col_names = FALSE,
          skip = 28,
          n_max = 15
        )
      
      Data <- as_tibble(t(Data))
      
      Data <- Data[c(1,4,9)]
      
      Data <- Data[complete.cases(Data),]
      
      names(Data) <- c("Year", "Low Carbon", "Fossil Fuels")
      
      Data %<>% lapply(function(x)
        as.numeric(as.character(x)))
      
      ElecFuelLowCarbon <- as_tibble(Data)
      
      ### variables
      ChartColours <- c("#39ab2c", "#1a9850", "#f46d43", "#39ab2c")
      sourcecaption = "Source: BEIS"
      plottitle = "Electricity generation -\nlow carbon versus fossil fuels"
      
      #ElecFuelLowCarbon$`Low Carbon`Percentage <- PercentLabel(ElecFuelLowCarbon$`Low Carbon`)
      
      
      ElecFuelLowCarbonChart <- ElecFuelLowCarbon %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = `Low Carbon`,
            colour = ChartColours[2],
            label = percent(`Low Carbon`, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Low Carbon`,
            label = ifelse(Year == min(Year), percent(`Low Carbon`, 0.1), ""),
            hjust = 0.5,
            vjust = -.8,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Low Carbon`,
            label = ifelse(Year == max(Year), percent(`Low Carbon`, 0.1), ""),
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElecFuelLowCarbon, 1),
          aes(
            x = Year,
            y = `Low Carbon`,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(`Low Carbon`),
            label = "Low Carbon",
            hjust = 0.5,
            vjust = -1.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `Fossil Fuels`,
            colour = ChartColours[3],
            label = paste0(`Fossil Fuels` * 100, "%")
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Fossil Fuels`,
            label = ifelse(Year == 2010, percent(`Fossil Fuels`, 0.1), ""),
            hjust = 0.5,
            vjust = 3.5,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Low Carbon`,
            label = ifelse(Year == 2010, percent(`Low Carbon`, 0.1), ""),
            hjust = 0.5,
            vjust = -2.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Fossil Fuels`,
            label = ifelse(Year == min(Year), percent(`Fossil Fuels`, 0.1), ""),
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Fossil Fuels`,
            label = ifelse(Year == max(Year), percent(`Fossil Fuels`, 0.1), ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElecFuelLowCarbon, 1),
          aes(
            x = Year,
            y = `Fossil Fuels`,
            colour = ChartColours[3],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(`Fossil Fuels`),
            label = "Fossil Fuels",
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) |
                             Year == min(Year) | Year == 2010, Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      ElecFuelLowCarbonChart <-
        LinePercentChart(ElecFuelLowCarbonChart,
                         ElecFuelLowCarbon,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      ElecFuelLowCarbonChart
      
 
      
      ggsave(
        file,
        plot =  ElecFuelLowCarbonChart,
        width = 16,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )



output$ElecGenRN.png <- downloadHandler(
  filename = "ElecGenRN.png",
  content = function(file) {
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen low carbon and fossil",
        col_names = FALSE,
        skip = 28,
        n_max = 15
      )
    
    Data <- as_tibble(t(Data))
    
    Data <- Data[c(1,2,3)]
    
    Data <- Data[complete.cases(Data),]
    
    names(Data) <- c("Year", "Renewables", "Nuclear")
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    ElecFuelRenNuclear <- as_tibble(Data)
    
    
    ### variables
    ChartColours <- c("#39ab2c", "#1a9850", "#4393c3", "#39ab2c")
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity generation -\nnuclear versus renewables"
    
    #ElecFuelRenNuclear$`Low Carbon`Percentage <- PercentLabel(ElecFuelRenNuclear$`Low Carbon`)
    
    
    ElecFuelRenNuclearChart <- ElecFuelRenNuclear %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      
      geom_line(
        aes(
          y = Renewables,
          colour = ChartColours[2],
          label = percent(Renewables, 0.1)
        ),
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = Renewables,
          label = ifelse(Year == min(Year), percent(Renewables, 0.1), ""),
          hjust = 0.5,
          vjust = 1.3,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      
      geom_text(
        aes(
          x = Year,
          y = Renewables,
          label = ifelse(Year == max(Year), percent(Renewables, 0.1), ""),
          hjust = 1.3,
          vjust = .5,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(ElecFuelRenNuclear, 1),
        aes(
          x = Year,
          y = Renewables,
          colour = ChartColours[2],
          show_guide = FALSE
        ),
        size = 4,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = mean(Year),
          y = mean(Renewables),
          label = "Renewables",
          hjust = 0,
          vjust = 3,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_line(
        aes(
          y = Nuclear,
          colour = ChartColours[3],
          label = paste0(Nuclear * 100, "%")
        ),
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = Nuclear,
          label = ifelse(Year == min(Year), percent(Nuclear, 0.1), ""),
          hjust = 0.5,
          vjust = -1,
          colour = ChartColours[3],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = Nuclear,
          label = ifelse(Year == max(Year), percent(Nuclear, 0.1), ""),
          hjust = 0.5,
          vjust = 2,
          colour = ChartColours[3],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(ElecFuelRenNuclear, 1),
        aes(
          x = Year,
          y = Nuclear,
          colour = ChartColours[3],
          show_guide = FALSE
        ),
        size = 4,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = mean(Year),
          y = mean(Nuclear),
          label = "Nuclear",
          hjust = 0.5,
          vjust = -2,
          colour = ChartColours[3],
          fontface = 2
        ),
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
    
    
    ElecFuelRenNuclearChart <-
      LinePercentChart(
        ElecFuelRenNuclearChart,
        ElecFuelRenNuclear,
        plottitle,
        sourcecaption,
        ChartColours
      )
    
    ElecFuelRenNuclearChart
    
    ggsave(
      file,
      plot =  ElecFuelRenNuclearChart,
      width = 14,
      height = 16,
      units = "cm",
      dpi = 300
    )
  }
)

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
      n_max = 16
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
  
  YearList <- as.list(DataScot$Year[which(DataScot$Year >= 2009)])
  
  DataScot$`Low Carbon` <- DataScot$Renewables + DataScot$Nuclear
  
  DataScot$`Fossil Fuels` <- DataScot$Gas + DataScot$Coal + DataScot$Oil
  
  DataScot <- DataScot[c(1,2,3,4,5,6,7,8,9,11,17,12,13,14,18,10,15,16)]
  
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
      title = "Electricity Generation - Scotland (GWh)",
      dom = 'ltBp',
      buttons = list(
        list(extend = 'copy'),
        list(
          extend = 'excel',
          title = 'Electricity Generation - Scotland (GWh)',
          header = TRUE
        ),
        list(extend = 'csv',
             title = 'Electricity Generation - Scotland (GWh)')
      ),
      
      # customize the length menu
      lengthMenu = list( c(10, 20, -1) # declare values
                         , c(10, 20, "All") # declare titles
      ), # end of lengthMenu customization
      pageLength = 10
    )
  ) %>%
    formatRound(2:ncol(DataScot), 0) %>% 
    formatStyle(c(9, 11, 17, 16), fontStyle = "italic") %>% 
    formatStyle(18, fontWeight = "bold")
})

output$ElecGenFuelScotPropTable = renderDataTable({
  
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
  
  names(DataScot) <- as.character(unlist(DataScot[1, ]))
  
  DataScot <- tail(DataScot,-1)
  
  DataScot <- head(DataScot,-1)
  
  DataScot %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  names(DataScot)[1] <- "Year"
  
  DataScot <- as_tibble(DataScot)
  
  YearList <- as.list(DataScot$Year[which(DataScot$Year >= 2009)])
  
  DataScot$`Low Carbon` <- DataScot$Renewables + DataScot$Nuclear
  
  DataScot$`Fossil Fuels` <- DataScot$Gas + DataScot$Coal + DataScot$Oil
  
  DataScot <- DataScot[c(1,9,11,17,18,16)]
  
  DataScot$RenewablesProp <- DataScot$Renewables / DataScot$Total
  DataScot$NuclearProp <- DataScot$Nuclear / DataScot$Total
  DataScot$LowCarbonProp <- DataScot$`Low Carbon` / DataScot$Total
  DataScot$FossilProp <- DataScot$`Fossil Fuels` / DataScot$Total
  
  DataScot <- DataScot[c(1,2,7,3,8,4,9,5,10)]
  
  names(DataScot) <- c("Year", "Renewables (GWh)", "Renewables Proportion (%)", "Nuclear (GWh)", "Nuclear Proportion (%)", "Low Carbon (GWh)", "Low Carbon Proportion (%)", "Fossil Fuels (GWh)", "Fossil Fuels Proportion (%)")
  
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
      title = "Scottish electricity generation proportions",
      dom = 'ltBp',
      buttons = list(
        list(extend = 'copy'),
        list(
          extend = 'excel',
          title = 'Scottish electricity generation proportions',
          header = TRUE
        ),
        list(extend = 'csv',
             title = 'Scottish electricity generation proportions')
      ),
      
      # customize the length menu
      lengthMenu = list( c(10, 20, -1) # declare values
                         , c(10, 20, "All") # declare titles
      ), # end of lengthMenu customization
      pageLength = 10
    )
  ) %>%
    formatRound(2:ncol(DataScot), 0) %>% 
    formatPercentage(c(3,5,7,9), 1)
})

output$ElecGenFuelEWTable = renderDataTable({
  
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
  
  names(DataEW) <- as.character(unlist(DataEW[1, ]))
  
  DataEW <- tail(DataEW,-1)
  
  DataEW <- head(DataEW,-1)
  
  DataEW %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  names(DataEW)[1] <- "Year"
  
  DataEW <- as_tibble(DataEW)
  
  YearList <- as.list(DataEW$Year[which(DataEW$Year >= 2009)])
  
  DataEW$`Low Carbon` <- DataEW$Renewables + DataEW$Nuclear
  
  DataEW$`Fossil Fuels` <- DataEW$Gas + DataEW$Coal + DataEW$Oil
  
  DataEW <- DataEW[c(1,2,3,4,5,6,7,8,9,11,17,12,13,14,18,10,15,16)]
  
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
      title = "Electricity Generation - England and Wales (GWh)",
      dom = 'ltBp',
      buttons = list(
        list(extend = 'copy'),
        list(
          extend = 'excel',
          title = 'Electricity Generation - England and Wales (GWh)',
          header = TRUE
        ),
        list(extend = 'csv',
             title = 'Electricity Generation - England and Wales (GWh)')
      ),
      
      # customize the length menu
      lengthMenu = list( c(10, 20, -1) # declare values
                         , c(10, 20, "All") # declare titles
      ), # end of lengthMenu customization
      pageLength = 10
    )
  ) %>%
    formatRound(2:ncol(DataEW), 0) %>% 
    formatStyle(c(9, 11, 17, 16), fontStyle = "italic") %>% 
    formatStyle(18, fontWeight = "bold")
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



observeEvent(input$ToggleTable, {
  toggle("ElecGenFuelTable")
})

observeEvent(input$ToggleTable2, {
  toggle("ElecGenFuelEWTable")
})

output$ElecGenAnimation <- renderImage({
  
  outfile <- tempfile(fileext='.gif')
  file.copy("Structure/2 - Renewables/Electricity/ElecGenAnimation.gif", outfile)
  
  list(src = outfile,
       alt = "This is alternate text")
}, deleteFile = TRUE)
}
    
    