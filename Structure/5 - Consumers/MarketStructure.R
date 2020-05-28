require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

MarketStructureOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Market Shares",
    fluidRow(column(8,
                    h3("Market shares, combined electricity and gas", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('MarketStructureSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('MarketStructure.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("MarketStructurePlot")),
    plotlyOutput(ns("MarketStructurePlot"), height = "500px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Market Suppliers",
             fluidRow(column(8,
                             h3("Market Supplier", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('MarketSupplierSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('MarketSupplier.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("MarketStructurePlot")),
             plotlyOutput(ns("MarketSupplierPlot"), height = "800px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data - Market shares, combined electricity and gas", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("MarketStructureTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("Xoserve", "Ofgem"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("Ofgem")
        
      )
    )
  )
}




###### Server ######
MarketStructure <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("MarketStructure.R")
  ###### Renewable Energy ###### ######


  MarketStructure <- read_csv("Processed Data/Output/Consumers/MarketShare.csv")
  
  ChartColours <- c("#4292c6", "#7bccc4", "#08519c", "#ef3b2c")
  sourcecaption = "Source: Xoserve, Ofgem"
  plottitle = "Market Shares, combined electricity and gas"
  
  ### From ESD ###
  
  output$MarketStructureSubtitle <- renderText({
    
    paste("Scotland, December 2019")
  })
  
  output$MarketStructurePlot <- renderPlotly({
    
    MarketStructure$Region <- paste0("<b>", str_wrap(MarketStructure$Region, 6), "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <-
      c(
        "#31a354",
        "#0868ac",
        "#43a2ca",
        "#7bccc4",
        "#a6bddb",
        "#d0d1e6",
        "#bdbdbd",
        "#969696"
      )
    
    
    p <- plot_ly(data = MarketStructure, y = ~ Region) %>%
      add_trace(
        data = MarketStructure,
        x = ~ `Large`,
        Region = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Large Suppliers",
        text = paste0("Large Suppliers: ", percent(MarketStructure$`Large`, 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = MarketStructure,
        x = ~ `Medium`,
        Region = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Medium Suppliers",
        text = paste0("Medium Suppliers: ", percent(MarketStructure$`Medium`, 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      
      add_trace(
        data = MarketStructure,
        x = ~ `Small`,
        Region = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Small Suppliers",
        text = paste0("Small Suppliers: ", percent(MarketStructure$`Small`,0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
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
          autorange = "reversed"
          
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
      config(displayModeBar = F)
    
    p
    
  })
  
  
  output$MarketStructureTable = renderDataTable({
    

    datatable(
      MarketStructure,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Proportion of customers who have switched energy supplier",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of customers who have switched energy supplier',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of customers who have switched energy supplier')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(2:5), 1)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/MarketStructure.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("MarketStructureTable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$MarketStructure.png <- downloadHandler(
    filename = "MarketStructure.png",
    content = function(file) {
      
      MarketStructureStatic <- MarketStructure
      
      plottitle = "Market Shares, combined electricity and gas"
      
      MarketStructureStatic <- arrange(MarketStructureStatic,row_number())
      
      MarketStructureStatic$Region <-
        factor(MarketStructureStatic$Region,
               levels = unique(MarketStructureStatic$Region),
               ordered = TRUE)
      
      MarketStructureStatic <- melt(MarketStructureStatic, id.vars = "Region")
      
      
      MarketStructureStatic$variable <-
        factor(MarketStructureStatic$variable,
               levels = rev(unique(MarketStructureStatic$variable)),
               ordered = TRUE)
      
      MarketStructureStatic <- MarketStructureStatic %>%
        group_by(Region) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      sourcecaption <- "Source: Xoserve, Ofgem"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <-
        c(
          "#31a354",
          "#0868ac",
          "#43a2ca",
          "#7bccc4",
          "#a6bddb",
          "#d0d1e6",
          "#bdbdbd",
          "#969696"
        )
      
      
      MarketStructureStaticChart <- MarketStructureStatic %>%
        ggplot(aes(x = Region, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Large" = BarColours[2],
            "Medium" = BarColours[3],
            "Small" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            x = Region,
            y = -0.08,
            label = str_wrap(Region, 10),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
        ) +
        geom_text(
          aes(
            x = 2.5,
            y = ((0/3) *1),
            label = "Large Suppliers",
            fontface = 2
          ),
          colour = BarColours[2],
          hjust = 0,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.5,
            y = ((1.5/3) *1),
            label = "Medium Suppliers",
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.5,
            y = ((3/3) *1),
            label = "Small Suppliers",
            fontface = 2
          ),
          colour = BarColours[4],
          hjust = 1,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Region,
            y = pos,
            label = ifelse(value > 0, percent(value, 0.1),""),
            fontface = 2
          ),
          colour = "white",
          family = "Century Gothic"
        )
      
      
      MarketStructureStaticChart
      
      
      MarketStructureStaticChart <-
        StackedBars(MarketStructureStaticChart,
                    MarketStructureStatic,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      MarketStructureStaticChart <-
        MarketStructureStaticChart +
        labs(subtitle = "Scotland, December 2019") +
        ylim(-0.1,1) +
        scale_x_discrete(limits = rev(levels(MarketStructureStatic$Region)))+
        coord_flip()
      
      MarketStructureStaticChart
      
      ggsave(
        file,
        plot = MarketStructureStaticChart,
        width = 19,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  output$MarketSupplierSubtitle <- renderText({
    
    Data <- read_delim("Processed Data/Output/Domestic Suppliers/DomesticSuppliers.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste("Scotland, ", format(min(Data$Date), format = "%B %Y")," - " ,format(max(Data$Date), format = "%B %Y"))
  })
  
  output$MarketSupplierPlot <- renderPlotly({
    Data <- read_delim("Processed Data/Output/Domestic Suppliers/DomesticSuppliers.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ChartColours <- c("#081d58", "#225ea8", "#41b6c4", "#41ab5d")
    
    Data$Date <- ymd(Data$Date)
    
    p <- plot_ly(data = Data, x = ~ Date) %>%
      add_trace(
        data = Data,
        y = ~ Dual,
        type = 'bar',
        name = "Dual fuel suppliers",
        text = paste0("Dual fuel suppliers: ", Data$Dual, "\n", format(Data$Date, format = "%B %Y")),
        hoverinfo = 'text',
        hoveron = 'points+fills',
        marker = list(color = ChartColours[1]),
        legendgroup = 1
      ) %>% 
      add_trace(
        data = Data,
        y = ~ Electricity,
        type = 'bar',
        name = "Electricity only suppliers",
        text = paste0("Electricity only suppliers: ", Data$Electricity, "\n", format(Data$Date, format = "%B %Y")),
        hoverinfo = 'text',
        hoveron = 'points+fills',
        marker = list(color = ChartColours[2]),
        legendgroup = 2
      ) %>% 
      add_trace(
        data = Data,
        y = ~ Gas,
        type = 'bar',
        name = "Gas only suppliers",
        text = paste0("Gas only suppliers: ", Data$Gas, "\n", format(Data$Date, format = "%B %Y")),
        hoverinfo = 'text',
        hoveron = 'points+fills',
        marker = list(color = ChartColours[3]),
        legendgroup = 3
      ) %>% 
      add_trace(data = Data,
                x = ~ Date,
                y = ~ Total,
                name = "Total Suppliers",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "Total Suppliers: ",
                  Data$Total,
                  "\n",
                  Data$Date
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[4], dash = "dash")
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.1,
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "",
                     showgrid = FALSE),
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
  
  output$MarketSupplier.png <- downloadHandler(
    filename = "MarketSupplier.png",
    content = function(file) {
      
      MarketSupplierStatic <- MarketSupplier
      
      plottitle = "Market Shares, combined electricity and gas"
      
      MarketSupplierStatic <- arrange(MarketSupplierStatic,row_number())
      
      MarketSupplierStatic$Region <-
        factor(MarketSupplierStatic$Region,
               levels = unique(MarketSupplierStatic$Region),
               ordered = TRUE)
      
      MarketSupplierStatic <- melt(MarketSupplierStatic, id.vars = "Region")
      
      
      MarketSupplierStatic$variable <-
        factor(MarketSupplierStatic$variable,
               levels = rev(unique(MarketSupplierStatic$variable)),
               ordered = TRUE)
      
      MarketSupplierStatic <- MarketSupplierStatic %>%
        group_by(Region) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      sourcecaption <- "Source: Xoserve, Ofgem"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <-
        c(
          "#31a354",
          "#0868ac",
          "#43a2ca",
          "#7bccc4",
          "#a6bddb",
          "#d0d1e6",
          "#bdbdbd",
          "#969696"
        )
      
      
      MarketSupplierStaticChart <- MarketSupplierStatic %>%
        ggplot(aes(x = Region, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Large" = BarColours[2],
            "Medium" = BarColours[3],
            "Small" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            x = Region,
            y = -0.08,
            label = str_wrap(Region, 10),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
        ) +
        geom_text(
          aes(
            x = 2.5,
            y = ((0/3) *1),
            label = "Large Suppliers",
            fontface = 2
          ),
          colour = BarColours[2],
          hjust = 0,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.5,
            y = ((1.5/3) *1),
            label = "Medium Suppliers",
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 2.5,
            y = ((3/3) *1),
            label = "Small Suppliers",
            fontface = 2
          ),
          colour = BarColours[4],
          hjust = 1,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Region,
            y = pos,
            label = ifelse(value > 0, percent(value, 0.1),""),
            fontface = 2
          ),
          colour = "white",
          family = "Century Gothic"
        )
      
      
      MarketSupplierStaticChart
      
      
      MarketSupplierStaticChart <-
        StackedBars(MarketSupplierStaticChart,
                    MarketSupplierStatic,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      MarketSupplierStaticChart <-
        MarketSupplierStaticChart +
        labs(subtitle = "Scotland, December 2019") +
        ylim(-0.1,1) +
        scale_x_discrete(limits = rev(levels(MarketSupplierStatic$Region)))+
        coord_flip()
      
      MarketSupplierStaticChart
      
      ggsave(
        file,
        plot = MarketSupplierStaticChart,
        width = 19,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
}
