require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



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
                             h3("Number of active domestic suppliers by fuel type", style = "color: #68c3ea;  font-weight:bold"),
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
    tabsetPanel(
      tabPanel("Market Shares",
    fluidRow(
    column(10, h3("Data - Market shares, combined electricity and gas", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("MarketStructureTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Market Suppliers",
             fluidRow(
               column(10, h3("Data - Number of active domestic suppliers by fuel type", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("MarketSupplierTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("XOServe", "OFGEMSuppliers"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("XOServe", "OFGEMSuppliers"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
             SourceLookup("XOServe"),
        SourceLookup("OFGEMSuppliers")
        
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
        title = "Market shares, combined electricity and gas",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Market shares, combined electricity and gas',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Market shares, combined electricity and gas')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(2:4), 1)
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
  
  observeEvent(input$ToggleTable2, {
    toggle("MarketSupplierTable")
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
    
    Data <- read_delim("Processed Data/Output/Domestic Sales/DomesticSuppliers.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste("Scotland, ", format(min(Data$Date), format = "%B %Y")," - " ,format(max(Data$Date), format = "%B %Y"))
  })
  
  output$MarketSupplierPlot <- renderPlotly({
    Data <- read_delim("Processed Data/Output/Domestic Sales/DomesticSuppliers.txt", 
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
  
  output$MarketSupplierTable = renderDataTable({
    
    MarketSupplier <- read_delim("Processed Data/Output/Domestic Sales/DomesticSuppliers.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    MarketSupplier$Date <- as.character(as.yearmon(MarketSupplier$Date))
    
    MarketSupplier <- MarketSupplier[seq(dim(MarketSupplier)[1],1),]
    
    datatable(
      MarketSupplier[c(1,4,3,2,5)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Number of active domestic suppliers by fuel type (GB)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Number of active domestic suppliers by fuel type (GB)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Number of active domestic suppliers by fuel type (GB)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) 
  })
  
  output$MarketSupplier.png <- downloadHandler(
    filename = "MarketSupplier.png",
    content = function(file) {
      
      Data <- read_delim("Processed Data/Output/Domestic Sales/DomesticSuppliers.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)
      
      names(Data)[1] <- "Year"
      
      DataMin <- head(Data, 1)
      
      DataMax <- tail(Data, 1)
      
      ChartColours <- c("#68c3ea", "#081d58", "#225ea8", "#41b6c4", "#41ab5d")
      
      Data$Year <- ymd(Data$Year)
      
      DataMelt <- Data[1:4]
      
      DataMelt <- melt(DataMelt, id.vars = "Year")
      
      plottitle = "Number of active domestic suppliers by fuel type"
      sourcecaption = "Source: Ofgem"
      
      width = max(Data$Year)- min(Data$Year)
      
      DataChart <- DataMelt %>%
        ggplot(aes(
          x = Year,
          y = value,
          group = variable,
          fill = variable
        )) +
        scale_fill_manual(
          "variable",
          values = c(
            "Dual" = ChartColours[2],
            "Electricity" = ChartColours[3],
            "Gas" = ChartColours[4]
          )
        ) +
        geom_bar(stat = "identity") +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              format(Year, format = "%b\n%Y"),
              ""
            ),
            vjust = 1.2,
            colour = "white",
            fontface = 2,
            family = "Century Gothic"
          )
        ) +
        annotate("line",
                 x = Data$Year,
                 y = Data$Total,
                 size = 1.5,
                 colour = ChartColours[5],
                 family = "Century Gothic"
        ) +
        annotate("text",
                 x = Data$Year,
                 y = Data$Total,
                 size = 3,
                 label = ifelse(Data$Year %in% c(min(Data$Year)), paste0("Total\nsuppliers:\n", Data$Total), " "),
                 vjust = -.2,
                 colour = ChartColours[5],
                 family = "Century Gothic",
                 fontface = 2
        ) +
        annotate("text",
                 x = max(Data$Year)+(width*0.04),
                 y = Data$Total,
                 size = 3,
                 label = ifelse(Data$Year %in% c(max(Data$Year)), paste0("Total\nsuppliers:\n", Data$Total), " "),
                 vjust = 0,
                 colour = ChartColours[5],
                 family = "Century Gothic",
                 fontface = 2
        ) +
        annotate(
          "segment",
          x = min(Data$Year)-100,
          xend = max(Data$Year)+10,
          y = 0,
          yend = 0,
          colour = "grey",
          alpha = 0.8,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.05),
            y = 25,
            label = "25",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        )+
        annotate(
          "segment",
          x = min(Data$Year)-100,
          xend = max(Data$Year)+10,
          y = 25,
          yend = 25,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.05),
            y = 50,
            label = "50",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        )+
        annotate(
          "segment",
          x = min(Data$Year)-100,
          xend = max(Data$Year)+10,
          y = 75,
          yend = 75,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.05),
            y = 75,
            label = "75",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        )+
        annotate(
          "segment",
          x = min(Data$Year)-100,
          xend = max(Data$Year)+10,
          y = 50,
          yend = 50,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = max(Year)+(width*0.015),
            y = DataMax$Dual/2,
            label = "Dual fuel",
            fontface = 2
          ),
          hjust = 0,
          colour = ChartColours[2],
          family = "Century Gothic",
          size = 3
        ) +
        geom_text(
          aes(
            x = max(Year)+(width*0.015),
            y = DataMax$Dual + (DataMax$Electricity/2),
            label = "Electricity",
            fontface = 2
          ),
          hjust = 0,
          colour = ChartColours[3],
          family = "Century Gothic",
          size = 3
        ) +
        geom_text(
          aes(
            x = max(Year)+(width*0.015),
            y = DataMax$Dual + DataMax$Electricity + (DataMax$Gas/2),
            label = "Gas",
            fontface = 2
          ),
          hjust = 0,
          colour = ChartColours[4],
          family = "Century Gothic",
          size = 3
        )
      
      
      DataChart
      
      
      DataChart <-
        DailyChart(DataChart,
                   Data,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      DataChart <- DataChart +
        coord_cartesian(xlim = c(min(DataMelt$Year)-(width*0.01), max(DataMelt$Year)+(width*0.1))) +
        ylim(-4,76) +
        labs(
          title = plottitle,
          face = 2,
          subtitle = paste(
            "Scotland,",
            format(min(Data$Year), format = "%b %Y"),
            "-",
            format(max(Data$Year), format = "%b %Y")
          )
        ) 
      
      DataChart <- DataChart 
      
      DataChart
      
      
      ggsave(
        file,
        plot =  DataChart,
        width = 20,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
}
