require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

DualFuelBreakdownOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Breakdown of a dual fuel bill", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('DualFuelBreakdownSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('DualFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("DualFuelBreakdownPlot")),
    plotlyOutput(ns("DualFuelBreakdownPlot"))%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("DualFuelBreakdownTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
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
        SourceLookup("OFGEMConsumers")
        
      )
    )
  )
}




###### Server ######
DualFuelBreakdown <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("DualFuelBreakdown.R")

  
  output$DualFuelBreakdownSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Dual fuel bill breakdown",
      col_names = FALSE,
      skip = 13,
      n_max = 9
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:9] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data <- Data[complete.cases(Data),]
    
    paste("United Kingdom,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$DualFuelBreakdownPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Dual fuel bill breakdown",
      col_names = FALSE,
      skip = 13,
      n_max = 9
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:9] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data <- Data[complete.cases(Data),]
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Wholesale costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(Data$`Wholesale costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(Data$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Env & Soc costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Env & Soc costs",
        text = paste0("Env & Soc costs: ", percent(Data$`Env & Soc costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Other direct costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Other direct costs",
        text = paste0("Other direct costs: ", percent(Data$`Other direct costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Operating`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating",
        text = paste0("Operating: ", percent(Data$`Operating`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = Data,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(Data$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = Data,
        x = ~ `VAT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "VAT",
        text = paste0("VAT: ", percent(Data$`VAT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[7]),
        legendgroup = 7
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
  
  
  output$DualFuelBreakdownTable = renderDataTable({
    
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Dual fuel bill breakdown",
      col_names = FALSE,
      skip = 13,
      n_max = 9
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- Data[-1,]
    
    Data[1:9] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
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
        order = list(list(0, 'desc')),
        title = "Breakdown of a dual fuel bill",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Breakdown of a dual fuel bill',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Breakdown of a dual fuel bill')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:9, 1) %>% 
      formatStyle(4:6, fontStyle = "italic")
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/DualFuelBreakdown.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("DualFuelBreakdownTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$DualFuelBreakdown.png <- downloadHandler(
    filename = "DualFuelBreakdown.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Dual fuel bill breakdown", skip = 13, n_max = 9, col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      Data[1,1] <- "Year"
      
      names(Data) <- as.character(unlist(Data[1,]))
      Data <- Data[-1,]
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      Data <- Data[complete.cases(Data),]
      DualFuelBreakdown <- Data[c(1, 9:4,2)]
      
      DualFuelBreakdown <-
        DualFuelBreakdown[order(DualFuelBreakdown$Year),]
      
      DualFuelBreakdown <- melt(DualFuelBreakdown, id.vars = "Year")
      
      
      DualFuelBreakdown$variable <-
        factor(DualFuelBreakdown$variable,
               levels = unique(DualFuelBreakdown$variable))
      
      DualFuelBreakdown <- DualFuelBreakdown %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a dual fuel bill"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      DualFuelBreakdownChart <- DualFuelBreakdown %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Wholesale costs" = BarColours[1],
            "Network costs" = BarColours[2],
            "Env & Soc costs" = BarColours[3],
            "Other direct costs" = BarColours[4],
            "Operating" = BarColours[5],
            "EBIT" = BarColours[6],
            "VAT" = BarColours[7]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = 0 - .04,
            label = ifelse(variable == "VAT", Year, ""),
            color = ChartColours[2]
          ),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = 2012.25,
              y = (0.5/7)*1,
              label = "Wholesale\nCosts"),
          fontface = 2,
          colour = BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2012.25,
              y = (1.5/7)*1,
              label = "Network\nCosts"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2012.25,
              y = (2.5/7)*1,
              label = "Env & Soc\ncosts"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2012.25,
              y = (3.5/7)*1,
              label = "Other direct\ncosts"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2012.25,
              y = (4.5/7)*1,
              label = "Operating\ncosts"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2012.25,
              y = (5.5/7)*1,
              label = "EBIT"),
          fontface = 2,
          colour = BarColours[6],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2012.25,
              y = (6.5/7)*1,
              label = "VAT"),
          fontface = 2,
          colour = BarColours[7],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          y = DualFuelBreakdown$top - DualFuelBreakdown$pos,
          label =
            ifelse(
              DualFuelBreakdown$Year == min(DualFuelBreakdown$Year) |
                DualFuelBreakdown$Year ==  max(DualFuelBreakdown$Year),
              ifelse(DualFuelBreakdown$value > .03, percent(DualFuelBreakdown$value, 0.1), "") ,
              ""
            ),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      DualFuelBreakdownChart
      
      
      DualFuelBreakdownChart <-
        BaselineChart(
          DualFuelBreakdownChart,
          DualFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      DualFuelBreakdownChart <-
        DualFuelBreakdownChart +
        xlim(max(DualFuelBreakdown$Year) + .5,
             min(DualFuelBreakdown$Year) - .9) +
        coord_flip()+
        labs(subtitle = paste("UK,", min(DualFuelBreakdown$Year),"-", max(DualFuelBreakdown$Year)))
      
      DualFuelBreakdownChart
      
      ggsave(
        file,
        plot = DualFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
