require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

GasConsumpOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Total gas consumption by sector", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('GasConsumpSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasConsump.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("GasConsumpPlot")),
    plotlyOutput(ns("GasConsumpPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10, h3("Data", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("GasConsumpTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
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
GasConsump <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("GasConsump.R")

  
  output$GasConsumpSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump",
      col_names = FALSE,
      skip = 12
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$GasConsumpPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump",
      col_names = FALSE,
      skip = 12
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2,1] <- "Baseline\n2005/2007"
    
    Data[3,1] <- ""
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    Data <- Data[-1,]
    
    Data$RowNumber <- as.numeric(rownames(Data))
    
    Data[is.na(Data)] <- 0
    
    DataTail <- tail(Data,1)
    
    DataLatest <- Data[nrow(Data)-1,]
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Domestic`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Domestic",
        text = paste0("Domestic: ", format(round(Data$`Domestic`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Non-domestic`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Non-domestic",
        text = paste0("Non-domestic: ", format(round(Data$`Non-domestic`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        y = ~ Year,
        x = ~ (Data$`Domestic` + Data$`Non-domestic`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Domestic` >0, paste("<b>",format(round((Data$`Domestic` + Data$`Non-domestic`), digits = 0), big.mark = ","),"MW</b>")," "),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      )  %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Domestic`)/2,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[1]),
        text = paste0("<b>", percent(DataTail$`Domestic`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x =  mean(DataLatest$`Domestic`) + (mean(DataLatest$`Non-domestic`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[3]),
        text =  paste0("<b>", percent(DataTail$`Non-domestic`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Total`)+ 3700,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = ChartColours[1]),
        text =  paste0("<b>", percent(DataTail$Total, accuracy = 0.1), "</b>")
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
                     ticktext = as.list(Data$Year),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          range = c(0,72000)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  
  output$GasConsumpTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump",
      col_names = FALSE,
      skip = 12
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2,1] <- " Baseline\n2005/2007"
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
  
    Data <- Data[-1,]
    
    Data <- head(Data, -1)
    
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
        title = "Energy Consumption",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Energy Consumption',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Energy Consumption')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:6, 0)%>% 
      formatStyle(c(4:5), fontStyle = 'italic') %>% 
      formatStyle(c(6), fontWeight = 'bold')
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/GasConsump.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("GasConsumpTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$GasConsump.png <- downloadHandler(
    filename = "GasConsump.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Gas consump", skip = 13, col_names = FALSE)[c(1,3,2,6)]
      
      Data[1,1] <- 2003
      
      names(Data) <- c("Year", "Non-domestic", "Domestic", "Total")
      
      Data <- Data[complete.cases(Data),]
      
      Data[nrow(Data),1] <- max(as.numeric(Data$Year),na.rm = TRUE)+1
      
      Data$Year <- as.numeric(Data$Year)
      
      GasConsumption <- Data
      
      GasConsumption <- GasConsumption[order(-GasConsumption$Year),]
      
      GasConsumption <- melt(GasConsumption, id.vars = "Year")
      
      GasConsumptionMax <-
        subset(GasConsumption, Year == max(GasConsumption$Year))
      
      GasConsumption <-
        subset(GasConsumption,
               Year < max(GasConsumption$Year) & variable != "Total")
      
      GasConsumption$variable <-
        factor(GasConsumption$variable, levels = unique(GasConsumption$variable))
      
      GasConsumption <- GasConsumption %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Total gas consumption by sector"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
      
      
      GasConsumptionChart <- GasConsumption %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Domestic" = BarColours[1],
                                     "Non-domestic" = BarColours[3])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = GasConsumption$top,
          label = ifelse(
            GasConsumption$variable == "Domestic",
            paste0(format(
              round(GasConsumption$top, digits = 0), big.mark = ","
            ), " GWh"),
            ""
          ),
          hjust = -0.1,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = -6000,
          label =   ifelse(
            GasConsumption$variable == "Domestic",
            ifelse(
              GasConsumption$Year == 2003,
              "2005/2007\n(baseline)",
              GasConsumption$Year
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = GasConsumption$top - GasConsumption$pos,
          label =   ifelse(
            GasConsumption$pos > 0,
            ifelse(
              GasConsumption$Year == 2003 |
                GasConsumption$Year ==  max(GasConsumption$Year),
              paste0(format(
                round(GasConsumption$value, digits = 0), big.mark = ","
              ), " GWh"),
              ""
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = "white"
        ) +
        annotate(
          "text",
          x = 2004,
          y = 17357,
          label = "Domestic",
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2004,
          y = 47247,
          label = "Non-domestic",
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(GasConsumption$Year) + 1.2,
          y = as.numeric(subset(
            GasConsumption,
            Year == max(GasConsumption$Year) &
              variable == "Domestic"
          )[1, 5]) - as.numeric(subset(
            GasConsumption,
            Year == max(GasConsumption$Year) & variable == "Domestic"
          )[1, 4]),
          label = percent((
            subset(GasConsumptionMax, variable == "Domestic")[1, 3]
          )),
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(GasConsumption$Year) + 1.2,
          y = as.numeric(
            subset(
              GasConsumption,
              Year == max(GasConsumption$Year) &
                variable == "Non-domestic"
            )[1, 5]
          ) - as.numeric(
            subset(
              GasConsumption,
              Year == max(GasConsumption$Year) &
                variable == "Non-domestic"
            )[1, 4]
          ),
          label = percent((
            subset(GasConsumptionMax, variable == "Non-domestic")[1, 3]
          )),
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(GasConsumption$Year) + 1.2,
          y = as.numeric(subset(
            GasConsumption,
            Year == max(GasConsumption$Year) & variable == "Domestic"
          )[1, 5]),
          label = percent((
            subset(GasConsumptionMax, variable == "Total")[1, 3]
          )),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic",
          hjust = -.75
        ) + annotate(
          "text",
          x = max(GasConsumption$Year) + 1.2,
          y = -6000,
          label = "% Change\nfrom baseline",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      
      GasConsumptionChart
      
      
      GasConsumptionChart <-
        BaselineChart(GasConsumptionChart,
                      GasConsumption,
                      plottitle,
                      sourcecaption,
                      ChartColours)
      
      GasConsumptionChart <-
        GasConsumptionChart +
        coord_flip() +
        labs(subtitle = paste("Scotland, 2005 -", max(GasConsumption$Year))) +
        ylim(-10000, max(GasConsumption$top) + 10500) +
        xlim(max(GasConsumption$Year) + 1.2, 2002.5)
      
      GasConsumptionChart
      
      ggsave(
        file,
        plot = GasConsumptionChart,
        width = 17,
        height = 15.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
