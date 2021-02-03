require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



LoftInsulationOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Insulation by thickness",
    fluidRow(column(8,
                    h3("Proportion of homes with loft insulation, by thickness", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('LoftInsulationSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('LoftInsulation.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("LoftInsulationPlot")),
    plotlyOutput(ns("LoftInsulationPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Government supported schemes",
             fluidRow(column(8,
                             h3("Cumulative recorded loft insulations under government schemes", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('LoftInsulationSchemesSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('LoftInsulationSchemes.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("LoftInsulationPlot")),
             plotlyOutput(ns("LoftInsulationSchemesPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    tabsetPanel(
      tabPanel("Insulation Thickness",
    fluidRow(
    column(10, h3("Data - Proportion of homes with loft insulation, by thickness", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("LoftInsulationTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Impact",
             fluidRow(
               column(10, h3("Data - Impact of measures", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("LoftInsulationImpactTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Schemes",
             fluidRow(
               column(10, h3("Data - Cumulative recorded loft insulations under government schemes", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("LoftInsulationSchemesTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("SGSHCS", "BEISNEED", "BEISHHoldEE"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("SGSHCS"),
        SourceLookup("BEISNEED"),
        SourceLookup("BEISHHoldEE")
        
      )
    )
  )
}




###### Server ######
LoftInsulation <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("LoftInsulation.R")

  
  output$LoftInsulationSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Loft insulation",
      col_names = FALSE,
      skip = 19,
      n_max = 6
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[complete.cases(Data),]
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$LoftInsulationPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Loft insulation",
      col_names = FALSE,
      skip = 19,
      n_max = 6
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    Data <- Data[complete.cases(Data),]
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#1a9850", "#a6d96a", "#ffeda0", "#f46d43", "#d73027")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `300mm or more`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "300mm or more",
        text = paste0("300mm or more: ", percent(Data$`300mm or more`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `200mm-299mm`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "200mm-299mm",
        text = paste0("200mm-299mm: ", percent(Data$`200mm-299mm`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `100mm-199mm`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "100mm-199mm",
        text = paste0("100mm-199mm: ", percent(Data$`100mm-199mm`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `1mm-99mm`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "1mm-99mm",
        text = paste0("1mm-99mm: ", percent(Data$`1mm-99mm`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        x = ~ `None`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "None",
        text = paste0("None: ", percent(Data$`None`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = Data,
        x = ~ 1.05 ,
        showlegend = TRUE,
        name = '200m or better',
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = ChartColours[1]),
        text =  paste0("<b>", percent(Data$`300mm or more`+Data$`200mm-299mm`, accuracy = 0.1), "</b>"),
        legendgroup = 8
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$`Year`),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
  })
  
  
  output$LoftInsulationTable = renderDataTable({
    
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Loft insulation",
      col_names = FALSE,
      skip = 19,
      n_max = 5
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$`200mm or better` <- Data$`300mm or more` + Data$`200mm-299mm`
    
    datatable(
      Data[complete.cases(Data),],
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
        title = "Proportion of homes with loft insulation, by thickness",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of homes with loft insulation, by thickness',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of homes with loft insulation, by thickness')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:6, 0) %>% 
      formatStyle(6, fontWeight = "bold")
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Efficiency Measures/LoftInsulation.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("LoftInsulationTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$LoftInsulation.png <- downloadHandler(
    filename = "LoftInsulation.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Loft insulation", skip = 19, n_max = 6, col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      Data <- tail(Data[c(1,6:2)], -1)
      
      names(Data) <- c("Year", "None", "1mm-99mm", "100mm-199mm", "200mm-299mm", "300mm or more")
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      Data$Total <- Data$`200mm-299mm` + Data$`300mm or more`
      
      Data <- Data[c(1,7,2:6)]
      
      InsulationThickness <- Data[complete.cases(Data),]
      
      InsulationThickness <-
        InsulationThickness[order(InsulationThickness$Year),]
      
      InsulationThickness <- melt(InsulationThickness, id.vars = "Year")
      
      
      InsulationThickness$variable <-
        factor(InsulationThickness$variable,
               levels = unique(InsulationThickness$variable))
      
      InsulationThickness <- InsulationThickness %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Proportion of homes with loft insulation, by thickness"
      sourcecaption <- "Source: SG"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#1a9850", "#a6d96a", "#ffeda0", "#f46d43", "#d73027")
      
      InsulationThicknessChart <- InsulationThickness %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "300mm or more" = BarColours[1],
            "200mm-299mm" = BarColours[2],
            "100mm-199mm" = BarColours[3],
            "1mm-99mm" = BarColours[4],
            "None" = BarColours[5],
            "Total" = "white"
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = 0 - .04,
            label = ifelse(variable == "None", Year, ""),
            color = ChartColours[2]
          ),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = 2009,
              y = 0.025,
              label = "300mm\nor more"),
          fontface = 2,
          colour = BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2009,
              y = 0.2,
              label = "200mm\n- 299mm"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2009,
              y = 0.57,
              label = "100mm\n- 199mm"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2009,
              y = 0.89697,
              label = "1mm\n- 99mm"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2009,
              y = 0.99227,
              label = "None"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        annotate(
          "text",
          x = InsulationThickness$Year,
          y = 1.11,
          label = ifelse(
            InsulationThickness$Year == "z",
            "",
            percent(InsulationThickness$value[which(InsulationThickness$variable == "Total")], 0.1)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1]
        ) +
        geom_text(
          aes(x = 2009,
              y = 1.11,
              label = "200m\nor better"),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5
        )+
        geom_text(
          y = InsulationThickness$top - InsulationThickness$pos,
          label =
            ifelse(
              InsulationThickness$Year == min(InsulationThickness$Year) |
                InsulationThickness$Year ==  max(InsulationThickness$Year),
              ifelse(InsulationThickness$value >= .05, percent(InsulationThickness$value,1), " "),
              ""
            ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = "White"
        )
      
      
      InsulationThicknessChart
      
      
      InsulationThicknessChart <-
        BaselineChart(
          InsulationThicknessChart,
          InsulationThickness,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      InsulationThicknessChart <-
        InsulationThicknessChart +
        xlim(max(InsulationThickness$Year) + .5,
             min(InsulationThickness$Year) - 1.1) +
        coord_flip() +
        ylim(-.04, 1.13)
      
      InsulationThicknessChart
      
      ggsave(
        file,
        plot = InsulationThicknessChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    })
    
    output$LoftInsulationImpactTable = renderDataTable({
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Loft insulation", skip = 33,  col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      Data <- tail(Data, -1)
      
      names(Data) <- c("Year", "Median Saving in Consumption (kWh)", "Median percentage saving in energy Consumption")
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      LoftInsulation <- Data[complete.cases(Data),]
      
      datatable(
        LoftInsulation,
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
          title = "Impact of Measures - Wall Insulation",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Impact of Measures - Wall Insulation',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Impact of Measures - Wall Insulation')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = 10
        )
      ) %>%
        formatPercentage(c(3), 1) %>% 
        formatRound(c(2), 0)
    })
    
    
    observeEvent(input$ToggleTable2, {
      toggle("LoftInsulationImpactTable")
    })
    
    LoftInsulationData <- read_excel("Structure/CurrentWorking.xlsx", 
                                     sheet = "Loft insulation", n_max = 3, skip = 26, col_names = FALSE)
  
    LoftInsulationData <- as_tibble(t(LoftInsulationData))
    
    LoftInsulationData <- LoftInsulationData[-1,]
    
    LoftInsulationData[1] <- NULL
    
    names(LoftInsulationData) <- c("Year", "CERT + ECO")
    
    LoftInsulationData <- LoftInsulationData[complete.cases(LoftInsulationData),]
    
    output$LoftInsulationSchemesTable = renderDataTable({
      
      datatable(
        LoftInsulationData,
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
          title = "Cumulative recorded Loft insulations under government schemes, CERT + ECO",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Cumulative recorded Loft insulations under government schemes, CERT + ECO',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Cumulative recorded Loft insulations under government schemes, CERT + ECO')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = 10
        )
      ) %>%
        formatRound(c(2), 0)
    })
    
    output$LoftInsulationSchemesPlot <- renderPlotly  ({
      
      LoftInsulationPlotlyData <- LoftInsulationData
      
      names(LoftInsulationPlotlyData) <- c("Year", "Amount")
      
      LoftInsulationPlotlyData <- as_tibble(sapply( LoftInsulationPlotlyData, as.numeric ))
      
      ### variables
      ChartColours <- c("#34d1a3", "#8da0cb", "#fc8d62", "#34d1a3")
      
      LoftInsulationPlotlyData$Year <- paste0("01/01/", LoftInsulationPlotlyData$Year)
      
      LoftInsulationPlotlyData$Year <- dmy(LoftInsulationPlotlyData$Year)
      
      p <-  plot_ly(LoftInsulationPlotlyData,x = ~ Year ) %>% 
        add_trace(data = LoftInsulationPlotlyData,
                  x = ~ Year,
                  y = ~ Amount,
                  name = "Total Loft insulation - CERT + ECO",
                  type = 'scatter',
                  mode = 'lines',
                  legendgroup = "1",
                  text = paste0(
                    "Total Loft insulation - CERT + ECO: ",
                    format(LoftInsulationPlotlyData$Amount, big.mark = ","),
                    "\nYear: ",
                    format(LoftInsulationPlotlyData$Year, "%Y")
                  ),
                  hoverinfo = 'text',
                  line = list(width = 6, color = ChartColours[1], dash = "none")
        ) %>% 
        add_trace(
          data = tail(LoftInsulationPlotlyData[which(LoftInsulationPlotlyData$Amount > 0 | LoftInsulationPlotlyData$Amount < 0),], 1),
          x = ~ Year,
          y = ~ `Amount`,
          legendgroup = "1",
          name = "Total Loft insulation - CERT + ECO",
          text = paste0(
            "Total Loft insulation - CERT + ECO: ",
            format(LoftInsulationPlotlyData[which(LoftInsulationPlotlyData$Amount > 0 | LoftInsulationPlotlyData$Amount < 0),][-1,]$Amount, big.mark = ","), 
            "\nYear: ",
            format(LoftInsulationPlotlyData[which(LoftInsulationPlotlyData$Amount > 0 | LoftInsulationPlotlyData$Amount < 0),][-1,]$Year, "%Y")
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
          legend = list(font = list(color = "#34d1a3"),
                        orientation = 'h'),
          hoverlabel = list(font = list(color = "white"),
                            hovername = 'text'),
          hovername = 'text',
          
          xaxis = list(title = "",
                       showgrid = FALSE,
                       range = c(min(LoftInsulationPlotlyData$Year)-100, max(LoftInsulationPlotlyData$Year)+100)),
          yaxis = list(
            title = "",
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
    
    output$LoftInsulationSchemes.png <- downloadHandler(
      filename = "LoftInsulationSchemes.png",
      content = function(file) {
        
        
        LoftInsulationggplotData <- LoftInsulationData
        
        names(LoftInsulationggplotData) <- c("Year", "Amount")
        
        LoftInsulationggplotData <- as_tibble(sapply(LoftInsulationggplotData, as.numeric ))
        
        ### variables
        ChartColours <- c("#34d1a3", "#8da0cb", "#fc8d62", "#34d1a3")
        
        sourcecaption = "Source: BEIS"
        plottitle = "Cumulative recorded Loft insulations\nunder government schemes"
        
        #LoftInsulationggplotData$CavityPercentage <- PercentLabel(LoftInsulationggplotData$Cavity)
        
        
        LoftInsulationggplotDataChart <- LoftInsulationggplotData %>%
          ggplot(aes(x = Year), family = "Century Gothic") +
          geom_line(
            aes(y = Amount,
                colour = ChartColours[4]),
            size = 1.5,
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              y = Amount,
              label = ifelse(Year == min(Year), format(Amount, big.mark = ","), ""),
              hjust = 0.5,
              vjust = 1.5,
              colour = ChartColours[4],
              fontface = 2
            ),
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              y = Amount,
              label = ifelse(Year == max(Year), format(Amount, big.mark = ","), ""),
              hjust = 0.5,
              vjust = 2,
              colour = ChartColours[4],
              fontface = 2
            ),
            family = "Century Gothic"
          ) +
          geom_point(
            data = tail(LoftInsulationggplotData, 1),
            aes(
              x = Year,
              y = Amount,
              colour = ChartColours[4],
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
        
        
        LoftInsulationggplotDataChart <-
          LinePercentChart(LoftInsulationggplotDataChart,
                           LoftInsulationggplotData,
                           plottitle,
                           sourcecaption,
                           ChartColours)
        
        LoftInsulationggplotDataChart <- LoftInsulationggplotDataChart +
          xlim(min(LoftInsulationggplotData$Year)-.5,max(LoftInsulationggplotData$Year)+.5)
        
        ggsave(
          file,
          plot =  LoftInsulationggplotDataChart,
          width = 14,
          height = 14,
          units = "cm",
          dpi = 300
        )
        
      }
    )
    
    output$LoftInsulationSchemesSubtitle <- renderText({
      
      paste("Scotland,", min(LoftInsulationData$Year),"-", max(LoftInsulationData$Year))
      
    })
    
    observeEvent(input$ToggleTable3, {
      toggle("LoftInsulationSchemesTable")
    })
}
