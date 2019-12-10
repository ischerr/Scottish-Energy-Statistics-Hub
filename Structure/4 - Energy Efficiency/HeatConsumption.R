require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

HeatConsumptionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Nom-electrical heat demand by sector", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('HeatConsumptionSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('HeatConsumption.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("HeatConsumptionPlot")),
    plotlyOutput(ns("HeatConsumptionPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
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
      column(12, dataTableOutput(ns("HeatConsumptionTable"))%>% withSpinner(color="#34d1a3"))),
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
HeatConsumption <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("HeatConsumption.R")

  
  output$HeatConsumptionSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat consump",
      col_names = FALSE,
      skip = 12,
      n_max = 3
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$HeatConsumptionPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat consump",
      col_names = FALSE,
      skip = 12,
      n_max = 4
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2,1] <- "Baseline\n2005/2007"
    
    Data[3,1] <- ""
    
    Data <- head(Data, -1)
    
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
        x = ~ `Non-Domestic`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Non-Domestic",
        text = paste0("Non-Domestic: ", format(round(Data$`Non-Domestic`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        y = ~ Year,
        x = ~ (Data$`Domestic` + Data$`Non-Domestic`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Domestic` >0, paste("<b>",format(round((Data$`Domestic` + Data$`Non-Domestic`), digits = 0), big.mark = ","),"MW</b>")," "),
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
        x =  mean(DataLatest$`Domestic`) + (mean(DataLatest$`Non-Domestic`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[3]),
        text =  paste0("<b>", percent(DataTail$`Non-Domestic`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Total`)+ 15000,
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
          range = c(0,125000)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  
  output$HeatConsumptionTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat consump",
      col_names = FALSE,
      skip = 12,
      n_max = 3
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2,1] <- " Baseline\n2005/2007"
    
    Data <- head(Data, -2)
    
    Data <- Data[-1,]
    
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
        title = "Non-electrical heat demand by sector",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Non-electrical heat demand by sector',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Non-electrical heat demand by sector')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:4, 0)%>% 
      formatStyle(c(4), fontWeight = 'bold')
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/HeatConsumption.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("HeatConsumptionTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$HeatConsumption.png <- downloadHandler(
    filename = "HeatConsumption.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Heat consump", skip = 12, col_names = FALSE)
      
      Data <- head(Data, 4)
      
      Data <- as_tibble(t(Data))
      
      Data <- Data[complete.cases(Data),]
      
      Data <- head(Data, -1)
      
      Data [1,1] <- 2003
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      names(Data) <- c("Year", "Domestic", "Industrial/Commercial", "Total")
      
      Data[nrow(Data),1] <- max(as.numeric(Data$Year),na.rm = TRUE)+1
      
      HeatDemand <- Data[c(1,3,2,4)]
      
      HeatDemand <- HeatDemand[order(-HeatDemand$Year),]
      
      HeatDemand <- melt(HeatDemand, id.vars = "Year")
      
      HeatDemandMax <- subset(HeatDemand, Year == max(HeatDemand$Year))
      
      HeatDemand <-
        subset(HeatDemand, Year < max(HeatDemand$Year) &
                 variable != "Total")
      
      HeatDemand$variable <-
        factor(HeatDemand$variable, levels = unique(HeatDemand$variable))
      
      HeatDemand <- HeatDemand %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Non-electrical heat demand by sector"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
      
      
      HeatDemandChart <- HeatDemand %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Domestic" = BarColours[1],
                                     "Industrial/Commercial" = BarColours[3])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = HeatDemand$top,
          label = ifelse(HeatDemand$variable == "Domestic", paste0(format(
            round(HeatDemand$top, digits = 0), big.mark = ","
          ), " GWh"), ""),
          hjust = -0.1,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = -10000,
          label =   ifelse(
            HeatDemand$variable == "Domestic",
            ifelse(
              HeatDemand$Year == 2003,
              "2005/2007\n(baseline)",
              HeatDemand$Year
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = HeatDemand$top - HeatDemand$pos,
          label =   ifelse(
            HeatDemand$pos > 0,
            ifelse(
              HeatDemand$Year == 2003 |
                HeatDemand$Year ==  max(HeatDemand$Year),
              paste0(format(
                round(HeatDemand$value, digits = 0), big.mark = ","
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
          y = 20257,
          label = "Domestic",
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2004,
          y = 69699,
          label = "Industrial/Commercial",
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(HeatDemand$Year) + 1.2,
          y = as.numeric(subset(
            HeatDemand, Year == max(HeatDemand$Year) &
              variable == "Domestic"
          )[1, 5]) - as.numeric(subset(
            HeatDemand, Year == max(HeatDemand$Year) &
              variable == "Domestic"
          )[1, 4]),
          label = percent((
            subset(HeatDemandMax, variable == "Domestic")[1, 3]
          )),
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(HeatDemand$Year) + 1.2,
          y = as.numeric(
            subset(
              HeatDemand,
              Year == max(HeatDemand$Year) &
                variable == "Industrial/Commercial"
            )[1, 5]
          ) - as.numeric(
            subset(
              HeatDemand,
              Year == max(HeatDemand$Year) &
                variable == "Industrial/Commercial"
            )[1, 4]
          ),
          label = percent((
            subset(HeatDemandMax, variable == "Industrial/Commercial")[1, 3]
          )),
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(HeatDemand$Year) + 1.2,
          y = as.numeric(subset(
            HeatDemand, Year == max(HeatDemand$Year) &
              variable == "Domestic"
          )[1, 5]),
          label = percent((
            subset(HeatDemandMax, variable == "Total")[1, 3]
          )),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic",
          hjust = -.75
        ) + annotate(
          "text",
          x = max(HeatDemand$Year) + 1.2,
          y = -10000,
          label = "% Change\nfrom baseline",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      
      HeatDemandChart
      
      
      HeatDemandChart <-
        BaselineChart(HeatDemandChart,
                      HeatDemand,
                      plottitle,
                      sourcecaption,
                      ChartColours)
      
      HeatDemandChart <-
        HeatDemandChart +
        coord_flip() +
        labs(subtitle = paste("Scotland, 2005 -", max(HeatDemand$Year))) +
        ylim(-16000, max(HeatDemand$top) + 18500) +
        xlim(max(HeatDemand$Year) + 1.2, 2002.5)
      
      HeatDemandChart
      
      ggsave(
        file,
        plot = HeatDemandChart,
        width = 17,
        height = 15.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
