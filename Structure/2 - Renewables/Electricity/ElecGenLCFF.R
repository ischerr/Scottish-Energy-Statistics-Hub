require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######percent

source("Structure/Global.R")

ElecGenLCFFOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
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
    #dygraphOutput(ns("ElecGenLCFFPlot")),
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
      tabPanel("Fuels",
    fluidRow(
    column(10, h3("Data - Fuels (GWh)", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ElecGenFuelTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Fuel Proportion",
      fluidRow(
        column(10, h3("Data - Fuel Proportion", style = "color: #39ab2c;  font-weight:bold")),
        column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
      ),
      fluidRow(
        column(12, dataTableOutput(ns("ElecGenLCFFTable"))%>% withSpinner(color="#39ab2c"))),
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
ElecGenLCFF <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ElecGenLCFF.R")

  
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
        n_max = 14
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
    plottitle = "Electricity generation -\nlow carbon versus fossil fuels"
    
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
        title = "Electricity generation - Fuel proportion",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Electricity generation - Fuels proportion",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Electricity generation - Fuels proportion")
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
  
  output$ElecGenFuelTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec gen low carbon and fossil",
        col_names = FALSE,
        skip = 16,
        n_max = 11
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
    plottitle = "Electricity generation -\nlow carbon versus fossil fuels"
    
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
                     paste(readtext("Structure/2 - Renewables/Electricity/ElecGenLCFF.txt")[2])
                     
                   )))
  })
  
 observeEvent(input$ToggleTable1, {
    toggle("ElecGenFuelTable")
  })
  
  observeEvent(input$ToggleTable2, {
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
            vjust = -1,
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
            vjust = 2,
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
        width = 14,
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
          vjust = -.8,
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
          hjust = .7,
          vjust = -0.7,
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
          vjust = -.3,
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
          hjust = 0.7,
          vjust = -1,
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
}
    
    