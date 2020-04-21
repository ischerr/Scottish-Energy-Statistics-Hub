require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

OilGasConsumptionOutput <- function(id) {
  ns <- NS(id)
  tagList(
     fluidRow(column(8,
                             h3("Oil and gas consumption", style = "color: #126992;  font-weight:bold"),
                             h4(textOutput(ns('OilGasConsumptionSubtitle')), style = "color: #126992;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('OilGasConsumption.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
             #dygraphOutput(ns("OilGasConsumptionPlot")),
             plotlyOutput(ns("OilGasConsumptionPlot"), height = "600px")%>% withSpinner(color="#126992"),
             tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #126992;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    tabsetPanel(
      tabPanel("Scotland",
  fluidRow(
    column(10, h3("Data - Scottish Oil and gas consumption", style = "color: #126992;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("OilGasConsumptionTable"))%>% withSpinner(color="#126992"))),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;")),
  tabPanel("UK",
           fluidRow(
             column(10, h3("Data - UK Oil and gas consumption", style = "color: #126992;  font-weight:bold")),
             column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
           ),
           fluidRow(
             column(12, dataTableOutput(ns("OilGasConsumptionUKTable"))%>% withSpinner(color="#126992"))),
           tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"))),
    fluidRow(
      column(2, p("Next update:")),
      column(2,
             DateLookup(c("BEISSubNatEnergy"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISSubNatEnergy")
        
      )
    )
  )
}




###### Server ######
OilGasConsumption <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("OilGasConsumption.R")

  
  output$OilGasConsumptionSubtitle <- renderText({
    
    paste("Scotland, 2017")

      })
  
  output$OilGasConsumptionPlot <- renderPlotly  ({
    
    
    ChartColours <- c("#126992", "#FF8500")
    BarColours <- c("#034e7b", "#0570b0", "#969696", "#f46d43", "#d73027")
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Oil and gas consumption", col_names = TRUE, 
        skip = 12,
        n_max = 8)
    
    Data <- Data[c(1,3,5,7),]
    
    names(Data)[1] <- c("Year")
    
    Data$YearFormat <- paste0("<b>",Data$Year, "</b>")
    
    Data$Total <- Data$`Petroleum products` + Data$Gas + Data$`Other fuel`
    
    p <-  plot_ly(Data, y = ~ YearFormat ) %>%  
      add_trace(x = ~ `Petroleum products` / Total, 
                orientation = 'h',
                name = "Petroleum products",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  "Petroleum products: ", format(round(Data$`Petroleum products`,0), big.mark = ",") ," GWh\n",
                  "Proportion: ", percent(Data$`Petroleum products`/ Data$Total, 0.1), "\n" ,
                  "Year: ", Data$Year),
                hoverinfo = 'text',
                marker = list(color = BarColours[1])
      ) %>% 
      add_trace(x = ~ `Gas` / Data$Total, 
                orientation = 'h',
                name = "Gas",
                type = 'bar',
                legendgroup = "2",
                text = paste0(
                  "Gas: ", format(round(Data$`Gas`,0), big.mark = ",") ," GWh\n",
                  "Proportion: ", percent(Data$`Gas`/ Data$Total, 0.1), "\n",
                  "Year: ", Data$Year),
                hoverinfo = 'text',
                marker = list(color = BarColours[2])
      ) %>% 
      add_trace(x = ~ `Other fuel` / Data$Total, 
                orientation = 'h',
                name = "Other fuel",
                type = 'bar',
                legendgroup = "3",
                text = paste0(
                  "Other fuel: ", format(round(Data$`Other fuel`,0), big.mark = ",") ," GWh\n",
                  "Proportion: ", percent(Data$`Other fuel`/ Data$Total, 0.1), "\n",
                  "Year: ", Data$Year),
                hoverinfo = 'text',
                marker = list(color = BarColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(
          title = "",
          tickformat = "",
          autorange = "reversed",
          ticktext = as.list(Data$`Year`),
          tickmode = "array",
          tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
          showgrid = FALSE,
          zeroline = FALSE,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
  })
  
  output$OilGasConsumptionTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Oil and gas consumption", col_names = TRUE, 
        skip = 12,
        n_max = 8)
    
    Data <- Data[c(1,3,5,7),]
    
    names(Data)[1] <- c("Year")
    
    Data$`Petroleum proportion` <- Data$`Petroleum products` / (Data$`Petroleum products`+ Data$Gas + Data$`Other fuel`)
    
    Data$`Gas proportion` <- Data$`Gas` / (Data$`Petroleum products`+ Data$Gas + Data$`Other fuel`)
    
    Data$`Other proportion` <- Data$`Other fuel` / (Data$`Petroleum products`+ Data$Gas + Data$`Other fuel`)
    
    names(Data)[c(2:4)] <- c("Petroleum products (GWh)", "Gas (GWh)", "Other fuel (GWh)" )
    
    datatable(
      Data[c(1,2,5,3,6,4,7)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Oil and gas consumption - Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Oil and gas consumption - Scotland",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Oil and gas consumption - Scotland")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:7, 0) %>% 
      formatPercentage(c(3,5,7), 1)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/7 - Oil Gas/OilGasConsumption.txt")[2])
                     
                   )))
  })
  
  
 observeEvent(input$ToggleTable1, {
    toggle("OilGasConsumptionTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$OilGasConsumption.png <- downloadHandler(
    filename = "OilGasConsumption.png",
  content = function(file) {
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Oil and gas consumption", skip = 12, col_names = TRUE)[c(2,4,6,8),]
    
    names(Data)[1] <- "Type"
    
    Data[2:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    OilGasConsump <- as_tibble(Data)
    
    OilGasConsump <- arrange(OilGasConsump,-row_number())
    
    OilGasConsump$Type <-
      factor(OilGasConsump$Type,
             levels = unique(OilGasConsump$Type),
             ordered = TRUE)
    
    OilGasConsump <- melt(OilGasConsump, id.vars = "Type")
    
    
    OilGasConsump$variable <-
      factor(OilGasConsump$variable,
             levels = rev(unique(OilGasConsump$variable)),
             ordered = TRUE)
    
    OilGasConsump <- OilGasConsump %>%
      group_by(Type) %>%
      mutate(pos = cumsum(value) - value / 2) %>%
      mutate(top = sum(value))
    
    plottitle <-
      "Oil and gas consumption"
    sourcecaption <- "Source: BEIS"
    
    ChartColours <- c("#126992", "#FF8500")
    BarColours <-
      c(
        "#034e7b",
        "#0570b0",
        "#3690c0",
        "#74a9cf",
        "#a6bddb",
        "#d0d1e6",
        "#bdbdbd",
        "#969696"
      )
    
    
    OilGasConsumpChart <- OilGasConsump %>%
      ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
      scale_fill_manual(
        "variable",
        values = c(
          "Petroleum products" = BarColours[1],
          "Gas" = BarColours[2],
          "Other fuel" = BarColours[3]
        )
      ) +
      geom_bar(stat = "identity", width = .8) +
      geom_text(
        aes(
          y = pos,
          label = percent(value, 0.1),
          
          fontface = 2
        ),
        colour = "white",
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Type,
          y = -0.01,
          label = Type,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 1
      ) +
      geom_text(
        aes(
          x = 4.77,
          y = .45/2,
          label = "Petroleum\nproducts",
          fontface = 2
        ),
        colour = BarColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = 4.7,
          y = (.33/2)+.45,
          label = "Gas",
          fontface = 2
        ),
        colour = BarColours[2],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = 4.7,
          y = (.22/2)+.33+.45,
          label = "Other\nfuels",
          fontface = 2
        ),
        colour = BarColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = 5.05,
          y = .5,
          label = "",
          fontface = 2
        ),
        colour = "black",
        family = "Century Gothic"
      )
    
    
    
    OilGasConsumpChart
    
    
    OilGasConsumpChart <-
      StackedBars(OilGasConsumpChart,
                  OilGasConsump,
                  plottitle,
                  sourcecaption,
                  ChartColours)
    
    OilGasConsumpChart <-
      OilGasConsumpChart +
      labs(subtitle = "Scotland, 2017") +
      ylim(-.16,1) +
      coord_flip()
    
    OilGasConsumpChart
    
    ggsave(
      file,
      plot = OilGasConsumpChart,
      width = 17.5,
      height = 12,
      units = "cm",
      dpi = 300
    )
    
  }
) 
  
  output$OilGasConsumptionUKTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Oil and gas consumption", col_names = TRUE, 
        skip = 23,
        n_max = 8)
    
    Data <- Data[c(1,3,5,7),]
    
    names(Data)[1] <- c("Year")
    
    Data$`Petroleum proportion` <- Data$`Petroleum products` / (Data$`Petroleum products`+ Data$Gas + Data$`Other fuel`)
    
    Data$`Gas proportion` <- Data$`Gas` / (Data$`Petroleum products`+ Data$Gas + Data$`Other fuel`)
    
    Data$`Other proportion` <- Data$`Other fuel` / (Data$`Petroleum products`+ Data$Gas + Data$`Other fuel`)
    
    names(Data)[c(2:4)] <- c("Petroleum products (GWh)", "Gas (GWh)", "Other fuel (GWh)" )
    
    datatable(
      Data[c(1,2,5,3,6,4,7)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Oil and gas consumption - UK",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Oil and gas consumption - UK",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Oil and gas consumption - UK")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:7, 0) %>% 
      formatPercentage(c(3,5,7), 1)
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("OilGasConsumptionUKTable")
  })
  
}
    
    