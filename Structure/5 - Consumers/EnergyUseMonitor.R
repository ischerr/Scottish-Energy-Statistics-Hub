require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnergyUseMonitorOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Extend energy use is monitored", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('EnergyUseMonitorSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnergyUseMonitor.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:Not Very Closely;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("EnergyUseMonitorPlot")),
    plotlyOutput(ns("EnergyUseMonitorPlot"), height =  "900px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:Not Very Closely;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:Not Very Closely;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnergyUseMonitorTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:Not Very Closely;color:#68c3ea;background-color:#68c3ea;"),
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
EnergyUseMonitor <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EnergyUseMonitor.R")

  
  output$EnergyUseMonitorSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Monitor energy use",
      col_names = FALSE,
      skip = 13,
      n_max = 6
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$EnergyUseMonitorPlot <- renderPlotly  ({
    
    
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Monitor energy use",
      col_names = FALSE,
      skip = 13,
      n_max = 6
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
    BarColours <- c("#1a9850", "#a6d96a", "#969696", "#f46d43", "#d73027")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Very Closely` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`),
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Very Closely",
        text = paste0("Very Closely: ", percent(Data$`Very Closely`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Fairly Closely` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`),
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Fairly Closely",
        text = paste0("Fairly Closely: ", percent(Data$`Fairly Closely`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Don't Know` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`),
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Don't Know",
        text = paste0("Don't Know: ", percent(Data$`Don't Know`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Not Very Closely` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`),
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Not Very Closely",
        text = paste0("Not Very Closely: ", percent(Data$`Not Very Closely`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Not at All` / (`Very Closely` + `Fairly Closely` + `Don't Know` + `Not Very Closely` + `Not at All`) ,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Not at All",
        text = paste0("Not at All: ", percent(Data$`Not at All`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
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
  
  
  output$EnergyUseMonitorTable = renderDataTable({
    
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Monitor energy use",
      col_names = FALSE,
      skip = 13,
      n_max = 6
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      Data[c(1,2,3,6,4,5)],
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
        title = "Proportion of People who monitor their Energy Use, by closeness",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of People who monitor their Energy Use, by closeness',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of People who monitor their Energy Use, by closeness')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:6, 1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/EnergyUseMonitor.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("EnergyUseMonitorTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnergyUseMonitor.png <- downloadHandler(
    filename = "EnergyUseMonitor.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Monitor energy use", skip = 13, n_max = 6, col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      Data[1,1] <- "Year"
      
      names(Data) <- as.character(unlist(Data[1,]))
      
      Data <- Data[-1,]
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      EnergyUseMonitor <- Data[c(1,5,4,6,3,2)]
      
      EnergyUseMonitor <-
        EnergyUseMonitor[order(EnergyUseMonitor$Year),]
      
      EnergyUseMonitor <- melt(EnergyUseMonitor, id.vars = "Year")
      
      
      EnergyUseMonitor$variable <-
        factor(EnergyUseMonitor$variable,
               levels = unique(EnergyUseMonitor$variable))
      
      EnergyUseMonitor <- EnergyUseMonitor %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Extent energy use is monitored"
      sourcecaption <- "Source: SG"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#1a9850", "#a6d96a", "#969696", "#f46d43", "#d73027")
      
      
      EnergyUseMonitorChart <- EnergyUseMonitor %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Very Closely" = BarColours[1],
            "Fairly Closely" = BarColours[2],
            "Don't Know" = BarColours[3],
            "Not Very Closely" = BarColours[4],
            "Not at All" = BarColours[5]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = 0 - .04,
            label = ifelse(variable == "Very Closely", Year, ""),
            color = ChartColours[2]
          ),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = 2007,
              y = 0.5/5,
              label = "Very Closely"),
          fontface = 2,
          colour = BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2007,
              y = 1.5/5,
              label = "Fairly Closely"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2007,
              y = 2.5/5,
              label = "Don't Know"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2007,
              y = 3.5/5,
              label = "Not Very Closely"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2007,
              y = 4.5/5,
              label = "Not at all"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          y = EnergyUseMonitor$top - EnergyUseMonitor$pos,
          label =
            ifelse(
              EnergyUseMonitor$Year == min(EnergyUseMonitor$Year) |
                EnergyUseMonitor$Year ==  max(EnergyUseMonitor$Year),
              percent(EnergyUseMonitor$value, 0.1),
              ""
            ),
          hjust = ifelse(EnergyUseMonitor$value > .05, .5,-0.1),
          family = "Century Gothic",
          fontface = 2,
          color = ifelse(EnergyUseMonitor$value > .05, "white", BarColours[4])
        )
      
      
      EnergyUseMonitorChart
      
      
      EnergyUseMonitorChart <-
        BaselineChart(
          EnergyUseMonitorChart,
          EnergyUseMonitor,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      EnergyUseMonitorChart <-
        EnergyUseMonitorChart +
        xlim(max(EnergyUseMonitor$Year) + .5,
             min(EnergyUseMonitor$Year) - 1.1) +
        coord_flip()
      
      EnergyUseMonitorChart
      
      ggsave(
        file,
        plot = EnergyUseMonitorChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
    }
  )
}
