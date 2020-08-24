require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ComplaintsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Complaints volume",
    fluidRow(column(8,
                    h3("Volume of complaints in Scotland and elsewhere", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('ComplaintsPropSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ComplaintsProp.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("ComplaintsPropPlot")),
    plotlyOutput(ns("ComplaintsPropPlot"), height = "500px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Market Suppliers",
             fluidRow(column(8,
                             h3("Number of active domestic suppliers by fuel type", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('ComplaintsAreaSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ComplaintsArea.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("ComplaintsPropPlot")),
             plotlyOutput(ns("ComplaintsAreaPlot"), height = "800px")%>% withSpinner(color="#68c3ea"),
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
      column(12, dataTableOutput(ns("ComplaintsPropTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Market Suppliers",
             fluidRow(
               column(10, h3("Data - Number of active domestic suppliers by fuel type", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ComplaintsAreaTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("OFGEMSuppliers"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
             SourceLookup("XOServe"),
        SourceLookup("OFGEMSuppliers")
        
      )
    )
  )
}




###### Server ######
Complaints <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ComplaintsProp.R")
  ###### Renewable Energy ###### ######


  ComplaintsProp <- read_csv("Processed Data/Output/Consumers/MarketShare.csv")
  
  ChartColours <- c("#4292c6", "#7bccc4", "#08519c", "#ef3b2c")
  sourcecaption = "Source: Xoserve, Ofgem"
  plottitle = "Market Shares, combined electricity and gas"
  
  ### From ESD ###
  
  output$ComplaintsPropSubtitle <- renderText({
    
    paste("Scotland, 2019")
  })
  
  output$ComplaintsPropPlot <- renderPlotly({
    
    ComplaintsProp <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsScotProp.csv", 
                  "\t", escape_double = FALSE, trim_ws = TRUE)[1:2]
    
    names(ComplaintsProp) <- c("variable", "value")
    
    
    ChartColours <- c("#2b8cbe", "#fc9272", "#34d1a3", "#02818a")

    ComplaintsProp$variable <- paste0("<b>", ComplaintsProp$variable, "</b>")
    
    ComplaintsProp <- ComplaintsProp[seq(dim(ComplaintsProp)[1],1),]
    
    p <- plot_ly(
      data = ComplaintsProp,
      labels = ~variable,
      type = 'pie',
      sort = FALSE,
      values = ~value,
      text = paste0(
        ComplaintsProp$variable,
        ": ", format(round(ComplaintsProp$value, 0), big.mark = ","), " Complaints" 
      ),
      textposition = 'outside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      marker = list(colors = ChartColours,
                    line = list(color = '#FFFFFF', width = 1))
    )  %>% 
      layout(
        barmode = 'stack',
        sort = 'false',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE),
        xaxis = list(
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
    
    #orca(p, "StaticCharts/ComplaintsPropSectorPie.svg")
    
  })
  
  
  output$ComplaintsPropTable = renderDataTable({
    
    ComplaintsProp <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsScotProp.csv", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ComplaintsProp <- ComplaintsProp[seq(dim(ComplaintsProp)[1],1),]

    datatable(
      ComplaintsProp,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Volume of complaints in Scotland and elsewhere",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Volume of complaints in Scotland and elsewhere',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Volume of complaints in Scotland and elsewhere')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(3), 1)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/Complaints.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("ComplaintsPropTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ComplaintsAreaTable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ComplaintsProp.png <- downloadHandler(
    filename = "ComplaintsProp.png",
    content = function(file) {
      
      ComplaintsPropStatic <- ComplaintsProp
      
      plottitle = "Market Shares, combined electricity and gas"
      
      ComplaintsPropStatic <- arrange(ComplaintsPropStatic,row_number())
      
      ComplaintsPropStatic$Region <-
        factor(ComplaintsPropStatic$Region,
               levels = unique(ComplaintsPropStatic$Region),
               ordered = TRUE)
      
      ComplaintsPropStatic <- melt(ComplaintsPropStatic, id.vars = "Region")
      
      
      ComplaintsPropStatic$variable <-
        factor(ComplaintsPropStatic$variable,
               levels = rev(unique(ComplaintsPropStatic$variable)),
               ordered = TRUE)
      
      ComplaintsPropStatic <- ComplaintsPropStatic %>%
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
      
      
      ComplaintsPropStaticChart <- ComplaintsPropStatic %>%
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
      
      
      ComplaintsPropStaticChart
      
      
      ComplaintsPropStaticChart <-
        StackedBars(ComplaintsPropStaticChart,
                    ComplaintsPropStatic,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ComplaintsPropStaticChart <-
        ComplaintsPropStaticChart +
        labs(subtitle = "Scotland, December 2019") +
        ylim(-0.1,1) +
        scale_x_discrete(limits = rev(levels(ComplaintsPropStatic$Region)))+
        coord_flip()
      
      ComplaintsPropStaticChart
      
      ggsave(
        file,
        plot = ComplaintsPropStaticChart,
        width = 19,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  output$ComplaintsAreaSubtitle <- renderText({
    
    paste("Scotland, 2019")
  })
  
  output$ComplaintsAreaPlot <- renderPlotly({
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
  
  output$ComplaintsAreaTable = renderDataTable({
    
    ComplaintsArea <- read_delim("Processed Data/Output/Domestic Sales/DomesticSuppliers.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ComplaintsArea$Date <- as.character(as.yearmon(ComplaintsArea$Date))
    
    ComplaintsArea <- ComplaintsArea[seq(dim(ComplaintsArea)[1],1),]
    
    datatable(
      ComplaintsArea[c(1,4,3,2,5)],
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
  
  output$ComplaintsArea.png <- downloadHandler(
    filename = "ComplaintsArea.png",
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
