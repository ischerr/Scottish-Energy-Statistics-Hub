require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######


GasBillPricesOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
    tabPanel("Average Bill Time Series",
             fluidRow(column(8,
                             h3("Average annual domestic standard gas bills in Scotland (based on 2010 prices)", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('AverageGasBillsSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('AverageGasBills.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("GasBillPricesPlot")),
             plotlyOutput(ns("AverageGasBillsPlot"))%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Regional breakdown",
    fluidRow(column(8,
                    h3("Average annual domestic standard gas bills in Scotland (Current Prices)", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('GasBillPricesSubtitle')), style = "color: #68c3ea;")
    ),
    column(
      4, style = 'padding:15px;',
      downloadButton(ns('GasBillPrices.png'), 'Download Graph', style="float:right")
    )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("GasBillPricesPlot")),
    plotlyOutput(ns("GasBillPricesPlot"))%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))
    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      
    tabPanel("Data - Time Series",
      fluidRow(
        column(10, h3("Data - gas Bills Time Series (2010 \u00A3)", style = "color: #68c3ea;  font-weight:bold")),
        column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
      ),
      fluidRow(
        column(12, dataTableOutput(ns("AverageGasBillsTable"))%>% withSpinner(color="#68c3ea"))),
      tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Data - Regional breakdown",
             fluidRow(
               column(10, h3("Data - Average Annual gas Bills (Current Prices)", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasBillPricesTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))
    ),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("BEISAnnualGas", "BEISRegionalGasBill"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("BEISAnnualGas", "BEISRegionalGasBill"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("BEISAnnualGas"),
        SourceLookup("BEISRegionalGasBill")
        
        
      )
    )
  )
}




###### Server ######
GasBillPrices <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("GasBillPrices.R")

  
  output$GasBillPricesSubtitle <- renderText({
    
    GasRegionAverageBill <- read_csv("Processed Data/Output/Energy Bills/GasRegionAverageBill.csv")
  
    paste("Scotland,", max(GasRegionAverageBill$Year))
      
  })
  
  output$GasBillPricesPlot <- renderPlotly  ({
    
    Data <- read_csv("Processed Data/Output/Energy Bills/GasRegionAverageBill.csv")
    
    #Keep Max Year
    Data <- Data[which(Data$Year == max(Data$Year)),]
    
    #Cast Data
    Data <- dcast(Data, variable ~ Region, value.var = "value")
    
    names(Data)[1] <- "Tech"
    
    Data$TechLabel <- Data$Tech
    
    Data$Tech <- paste0("<b>",str_wrap(Data$Tech, 9),"</b>")
    
   
    
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <-
      c(    "#0868ac","#43a2ca","#7bccc4"
      )
    
    p <-  plot_ly(Data, 
                  y = ~Tech, 
                  x = ~ `North Scotland`, 
                  type = 'bar', 
                  name = 'North Scotland',
                  hoverinfo = "text",
                  text = paste0("North Scotland: \u00A3",round(Data$`North Scotland`, digits = 0)),
                  orientation = 'h',
                  marker = list(color = BarColours[1])
                  )%>%
      add_trace(x = ~ `South Scotland`, 
                type = 'bar', 
                name = 'South Scotland',
                hoverinfo = "text",
                text = paste0("South Scotland: \u00A3",round(Data$`South Scotland`, digits = 0)),
                orientation = 'h',
                marker = list(color = BarColours[2])
                ) %>% 
      add_trace(x = ~ `United Kingdom`, 
                type = 'bar', 
                name = 'United Kingdom',
                hoverinfo = "text",
                text = paste0("United Kingdom: \u00A3",round(Data$`United Kingdom`, digits = 0)),
                orientation = 'h',
                marker = list(color = BarColours[3])
      ) %>% 
      layout(
        barmode = 'group',
        bargap = 0.25,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "",
                     tickprefix = "\u00A3",
                     showgrid = TRUE,
                     range = c(-0.01, 800),
                     x = 0.5
                     
                     ),
        yaxis = list(
          title = "",
          tickformat = "",
          autorange = "reversed",
          ticktext = as.list(Data$`Tech`),
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

  output$GasBillPricesTable = renderDataTable({
    
    Data <- read_csv("Processed Data/Output/Energy Bills/GasRegionAverageBill.csv")
    
    #Keep Max Year
    Data <- Data[which(Data$Year == max(Data$Year)),]
    
    #Cast Data
    Data <- dcast(Data, variable ~ Region, value.var = "value")
    
    names(Data)[1] <- "Payment Method"
    
    GasBillPrices <- select(Data,
                            "Payment Method",
                            "North Scotland",
                            "South Scotland",
                            "United Kingdom"
                            )
    
    datatable(
      GasBillPrices,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Average annual domestic standard gas bills in Scotland (\u00A3)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Average annual domestic standard gas bills in Scotland (\u00A3)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Average annual domestic standard gas bills in Scotland (\u00A3)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatCurrency(c(2:4), currency = "\u00A3", digits = 0)
  })
  
  output$AverageGasBillsSubtitle <- renderText({
    
    Data <- read_csv("Processed Data/Output/Energy Bills/GasAverageBill.csv")
    
    Data <- as_tibble(Data)
    
    paste("Scotland,", min(Data$Year),"-", max(Data$Year))
  })
  
  output$AverageGasBillsPlot <- renderPlotly  ({
    
    
    ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
    
    LineColours <- c("#8da0cb", "#fc8d62", "#66c2a5")
    
    Data <- read_csv("Processed Data/Output/Energy Bills/GasAverageBill.csv")
    
    Data <- as_tibble(Data)
    
    Data$Year <-
      paste0("01/01/", Data$Year)
    
    Data$Year <- dmy(Data$Year)
    
    p <-  plot_ly(Data, x = ~ Year ) %>%  
      add_trace(y = ~ `Prepayment`, 
                name = "Prepayment",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Prepayment: \u00A3",
                  format(round(Data$`Prepayment`, digits = 0), big.mark = ","),
                  "\nYear: ",
                  format(Data$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(y = ~ `Standard Credit`,
                name = "Standard Credit",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Standard Credit: \u00A3",
                  format(round(Data$`Standard Credit`, digits = 0), big.mark = ","),
                  "\nYear: ",
                  format(Data$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[2], dash = "none")
      ) %>% 
      add_trace(y = ~ `Direct Debit`, 
                name = "Direct Debit",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Direct Debit: \u00A3",
                  format(round(Data$`Direct Debit`, digits = 0), big.mark = ","),
                  "\nYear: ",
                  format(Data$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(Data[which(Data$`Prepayment` != 0),], 1),
        x = ~ Year,
        y = ~ `Prepayment`,
        name = "Prepayment",
        legendgroup = "1",
        text = paste0(
          "Prepayment: \u00A3",
          format(round(tail(Data[which(Data$`Prepayment` != 0),], 1)$`Prepayment`, digits = 0), big.mark = ","),
          "\nYear: ",
          format(tail(Data[which(Data$`Prepayment` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>%
      add_trace(
        data = tail(Data[which(Data$`Standard Credit` != 0),], 1),
        x = ~ Year,
        y = ~ `Standard Credit`,
        name = "Standard Credit",
        legendgroup = "2",
        text = paste0(
          "Standard Credit: \u00A3",
          format(round(tail(Data[which(Data$`Standard Credit` != 0),], 1)$`Standard Credit`, digits = 0), big.mark = ","),
          "\nYear: ",
          format(tail(Data[which(Data$`Standard Credit` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[2])
      ) %>% 
      add_trace(
        data = tail(Data[which(Data$`Direct Debit` != 0),], 1),
        x = ~ Year,
        y = ~ `Direct Debit`,
        name = "Direct Debit",
        legendgroup = "3",
        text = paste0(
          "Direct Debit: \u00A3",
          format(round(tail(Data[which(Data$`Direct Debit` != 0),], 1)$`Direct Debit`, digits = 0), big.mark = ","),
          "\nYear: ",
          format(tail(Data[which(Data$`Direct Debit` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE),
        yaxis = list(
          title = "",
          tickformat = "",
          tickprefix = "\u00A3",
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
  
  output$AverageGasBillsTable = renderDataTable({
    
    Data <- read_csv("Processed Data/Output/Energy Bills/GasAverageBill.csv")
    
    Data <- as_tibble(Data)
    
    AverageGasBillsTech <- Data
    
    datatable(
      AverageGasBillsTech,
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
        title = "Average annual domestic standard gas bills in Scotland based on 2010 prices (\u00A3)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Average annual domestic standard gas bills in Scotland based on 2010 prices (\u00A3)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Average annual domestic standard gas bills in Scotland based on 2010 prices (\u00A3)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatCurrency(c(2:4), currency = "\u00A3", digits = 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/GasBillPrices.txt")[2])
                     
                   )))
  })
  
  
 observeEvent(input$ToggleTable1, {
    toggle("AverageGasBillsTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("GasBillPricesTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$GasBillPrices.png <- downloadHandler(
    filename = "GasBillPrices.png",
    content = function(file) {

      Data <- read_csv("Processed Data/Output/Energy Bills/GasRegionAverageBill.csv")
      
      #Keep Max Year
      Data <- Data[which(Data$Year == max(Data$Year)),]
      
      ChartYear <- max(Data$Year)
      
      #Cast Data
      Data <- dcast(Data, variable ~ Region, value.var = "value")
      
      Data <- select(Data,
                     variable,
                     "United Kingdom",
                     "South Scotland",
                     "North Scotland")
      
      names(Data)[1:2] <- c("Country", "United Kingdom")
      
      AvgGasBills <- Data
      
      AvgGasBills <- arrange(AvgGasBills, -row_number())
      
      AvgGasBills$Country <-
        factor(AvgGasBills$Country,
               levels = unique(AvgGasBills$Country),
               ordered = TRUE)
      
      AvgGasBills <- melt(AvgGasBills, id.vars = "Country")
      
      
      AvgGasBills$variable <-
        factor(
          AvgGasBills$variable,
          levels = unique(AvgGasBills$variable),
          ordered = TRUE
        )
      
      AvgGasBills <- AvgGasBills %>%
        group_by(Country) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Average annual domestic standard Gas bills\n(current prices)"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <-
        c(    "#0868ac","#43a2ca","#7bccc4"
        )
      
      
      AvgGasBillsChart <- AvgGasBills %>%
        ggplot(aes(x = Country, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "North Scotland" = BarColours[1],
            "South Scotland" = BarColours[2],
            "United Kingdom" = BarColours[3]
          )
        ) +
        geom_bar(position = "dodge",
                 stat = "identity",
                 width = .8) +
        geom_text(position = position_dodge(width = .8),
                  aes(
                    y = value + 20,
                    fill = variable,
                    label = paste0("\u00A3",format(round(value, digits = 0), big.mark = ",")),
                    hjust = 0
                  ),
                  fontface = 2,
                  colour =  ChartColours[1],
                  family = "Century Gothic",
                  size = 3) +
        geom_text(position = position_dodge(width = .8),
                  aes(
                    y = 20,
                    fill = variable,
                    label = ifelse(Country == max(Country), as.character(variable), ""),
                    hjust = 0
                  ),
                  fontface = 2,
                  colour =  "white",
                  family = "Century Gothic",
                  size = 4) +
        annotate(
          "text",
          x = AvgGasBills$Country,
          y = -65,
          label = ifelse(AvgGasBills$Country == "z", "", str_wrap(AvgGasBills$Country, width = 9)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
        )
      
      AvgGasBillsChart
      
      
      AvgGasBillsChart <-
        StackedBars(AvgGasBillsChart,
                    AvgGasBills,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      AvgGasBillsChart <- AvgGasBillsChart +
        labs(subtitle = paste("Scotland,", ChartYear)) +
        ylim(-90, 740)+
        coord_flip()
      
      AvgGasBillsChart
      
      ggsave(
        file,
        plot = AvgGasBillsChart,
        width = 17.5,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
  )



output$AverageGasBills.png <- downloadHandler(
  filename = "AverageGasBills.png",
  content = function(file) {
    
    Data <- read_csv("Processed Data/Output/Energy Bills/GasAverageBill.csv")
    
    names(Data) <- c("Year", "Prepayment", "Standard Credit", "Direct Debit")
    
    Data <- Data[-1,]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data <- as_tibble(sapply( Data, round ))
    
    GasBills <- Data
    
    ### variables
    ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: BEIS"
    plottitle = "Average annual domestic standard gas\nbills in Scotland (based on 2010 prices)"
    
    #GasBills$PrepaymentPercentage <- PercentLabel(GasBills$Prepayment)
    
    
    GasBillsChart <- GasBills %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      
      geom_line(
        aes(
          y = Prepayment,
          colour = ChartColours[2],
          label = percent(Prepayment)
        ),
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year - .5,
          y = Prepayment,
          label = ifelse(Year == min(Year), paste0("\u00A3", Prepayment), ""),
          hjust = 0.5,
          vjust = -.8,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year + .5,
          y = Prepayment,
          label = ifelse(Year == max(Year), paste0("\u00A3", Prepayment), ""),
          hjust = 0.5,
          vjust= 0.5,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(GasBills, 1),
        aes(
          x = Year,
          y = Prepayment,
          colour = ChartColours[2],
          show_guide = FALSE
        ),
        size = 4,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = mean(Year),
          y = mean(Prepayment),
          label = "Prepayment",
          hjust = 0.5,
          vjust = 8,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_line(
        aes(
          y = `Standard Credit`,
          colour = ChartColours[3],
          label = paste0(`Standard Credit` * 100, "%")
        ),
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year - .5,
          y = `Standard Credit`,
          label = ifelse(Year == min(Year), paste0("\u00A3", `Standard Credit`), ""),
          hjust = 0.5,
          vjust = 1,
          colour = ChartColours[3],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year + .5,
          y = `Standard Credit`,
          label = ifelse(Year == max(Year), paste0("\u00A3", `Standard Credit`), ""),
          hjust = 0.5,
          vjust = 0,
          colour = ChartColours[3],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(GasBills, 1),
        aes(
          x = Year,
          y = `Standard Credit`,
          colour = ChartColours[3],
          show_guide = FALSE
        ),
        size = 4,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = mean(Year),
          y = mean(`Standard Credit`),
          label = "Standard Credit",
          hjust = 0.5,
          vjust = 8.5,
          colour = ChartColours[3],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_line(
        aes(
          y = `Direct Debit`,
          colour = ChartColours[4],
          label = paste0(`Direct Debit` * 100, "%")
        ),
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year - .5,
          y = `Direct Debit`,
          label = ifelse(Year == min(Year), paste0("\u00A3", `Direct Debit`), ""),
          hjust = 0.5,
          vjust = 2,
          colour = ChartColours[4],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year + .5,
          y = `Direct Debit`,
          label = ifelse(Year == max(Year), paste0("\u00A3", `Direct Debit`), ""),
          hjust = 0.5,
          vjust = 0.5,
          colour = ChartColours[4],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(GasBills, 1),
        aes(
          x = Year,
          y = `Direct Debit`,
          colour = ChartColours[4],
          show_guide = FALSE
        ),
        size = 4,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = mean(Year),
          y = mean(`Direct Debit`),
          label = "Direct Debit",
          hjust = 0.5,
          vjust = 8.3,
          colour = ChartColours[4],
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
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )
    
    
    GasBillsChart <-
      LinePercentChart(GasBillsChart,
                       GasBills,
                       plottitle,
                       sourcecaption,
                       ChartColours)
    
    GasBillsChart
    
    ggsave(
      file,
      plot =  GasBillsChart,
      width = 18,
      height = 18,
      units = "cm",
      dpi = 300
    )
    
  }
)
}
    
    