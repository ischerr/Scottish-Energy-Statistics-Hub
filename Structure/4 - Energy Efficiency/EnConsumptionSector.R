require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnConsumptionSectorOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Total final energy consumption by consuming sector", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('EnConsumptionSectorSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsumptionSector.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("EnConsumptionSectorPlot")),
    plotlyOutput(ns("EnConsumptionSectorPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
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
      column(12, dataTableOutput(ns("EnConsumptionSectorTable"))%>% withSpinner(color="#34d1a3"))),
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
EnConsumptionSector <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EnConsumptionSector.R")

  
  output$EnConsumptionSectorSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump sector",
      col_names = FALSE,
      skip = 12,
      n_max = 7
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:7] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$EnConsumptionSectorPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump sector",
      col_names = FALSE,
      skip = 12,
      n_max = 7
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:7] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2,1] <- "Baseline\n2005/2007"
    
    Data[3,1] <- " "
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    Data <- Data[-1,c(1,2,5,6,7)]
    
    Data$RowNumber <- as.numeric(rownames(Data))
    
    Data[is.na(Data)] <- 0
    
    DataTail <- tail(Data,1)
    
    DataLatest <- Data[nrow(Data)-1,]
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#00441b", "#238b45", "#66c2a4", "#99d8c9", "ffffff")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Industry & Commercial`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Industry & Commercial",
        text = paste0("Industry & Commercial: ", format(round(Data$`Industry & Commercial`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Domestic`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Domestic",
        text = paste0("Domestic: ", format(round(Data$`Domestic`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 3
      ) %>%
      
      add_trace(
        data = Data,
        x = ~ `Transport`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Transport",
        text = paste0("Transport: ", format(round(Data$`Transport`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        y = ~ Year,
        x = ~ (Data$`Industry & Commercial` + Data$`Domestic` + Data$`Transport`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Industry & Commercial` >0, paste("<b>",format(round((Data$`Industry & Commercial` + Data$`Domestic` + Data$`Transport`), digits = 0), big.mark = ","),"MW</b>")," "),
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
        x = mean(DataLatest$`Industry & Commercial`)/2,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[1]),
        text = paste0("<b>", percent(DataTail$`Industry & Commercial`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x =  mean(DataLatest$`Industry & Commercial`) + (mean(DataLatest$`Domestic`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[2]),
        text =  paste0("<b>", percent(DataTail$`Domestic`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Industry & Commercial`) + mean(DataLatest$`Domestic`) + (mean(DataLatest$`Transport`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[3]),
        text =  paste0("<b>", percent(DataTail$Transport, accuracy = 0.1), "</b>")
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
          range = c(0,220000)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  
  output$EnConsumptionSectorTable = renderDataTable({
    
    EnConsumption <- read_excel("Structure/CurrentWorking.xlsx", 
                                sheet = "Energy consump sector", skip = 12, col_names = FALSE)
    EnConsumption <- head(EnConsumption, -1)
    
    EnConsumption <- as_tibble(t(EnConsumption))
    
    names(EnConsumption) <- c("Year","Industry & Commercial", "Industry (estimate)", "Commercial (estimate)", "Domestic", "Transport", "Total")

    EnConsumption <- head(EnConsumption, -1)
    
    EnConsumption <- tail(EnConsumption, -1)
    
    EnConsumption[1:7] %<>% lapply(function(x) as.numeric(as.character(x)))
    EnConsumption <- as_tibble(EnConsumption)
    
    EnConsumption[1,1] <- " Baseline 2005 - 2007"
    
    datatable(
      EnConsumption,
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
      formatRound(2:7, 0)%>% 
      formatStyle(c(3,4), fontStyle = "italic") %>% 
      formatStyle(c(7), fontWeight = 'bold')
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/EnConsumpSector.txt")[2])
                     
                   )))
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnConsumptionSector.png <- downloadHandler(
    filename = "EnConsumptionSector.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Energy consump sector", skip = 12, col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      Data[9] <- NULL
      
      names(Data) <- as.character(unlist(Data[1,]))
      
      names(Data)[1] <- "Year"
      Data <- tail(Data, -1)
      
      Data[1,1] <- 2003
      
      Data$Year <- as.numeric(Data$Year)
      Data[nrow(Data),1] <- max(Data$Year + 1, na.rm =  TRUE)
      Data <- Data[which(Data$Year> 0),]
      #Data <- Data[complete.cases(Data),]
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      FinalConsumptionSectors <- Data[c(1,6,5,2,7)]
      
      FinalConsumptionSectors <-
        FinalConsumptionSectors[order(-FinalConsumptionSectors$Year),]
      
      FinalConsumptionSectors <-
        melt(FinalConsumptionSectors, id.vars = "Year")
      
      FinalConsumptionMax <-
        subset(FinalConsumptionSectors,
               Year == max(FinalConsumptionSectors$Year))
      
      FinalConsumptionSectors <-
        subset(
          FinalConsumptionSectors,
          Year < max(FinalConsumptionSectors$Year) & variable != "Total"
        )
      
      FinalConsumptionSectors$variable <-
        factor(FinalConsumptionSectors$variable,
               levels = unique(FinalConsumptionSectors$variable))
      
      FinalConsumptionSectors <- FinalConsumptionSectors %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Total final energy consumption by consuming sector"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45", "#66c2a4", "#99d8c9", "ffffff")
      
      
      FinalConsumptionSectorsChart <- FinalConsumptionSectors %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Industry & Commercial" = BarColours[1],
            "Domestic" = BarColours[2],
            "Transport" = BarColours[3],
            "Bioenergy & Wastes" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = FinalConsumptionSectors$top,
          label = ifelse(
            FinalConsumptionSectors$value > 60000,
            paste0(format(
              round(FinalConsumptionSectors$top, digits = 0), big.mark = ","
            ), " GWh"),
            ""
          ),
          hjust = 0,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = -17500,
          label =   ifelse(
            FinalConsumptionSectors$value > 60000,
            ifelse(
              FinalConsumptionSectors$Year == 2003,
              "2005/2007\n(baseline)",
              FinalConsumptionSectors$Year
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = FinalConsumptionSectors$top - FinalConsumptionSectors$pos,
          label =   ifelse(
            FinalConsumptionSectors$value > 7000,
            ifelse(
              FinalConsumptionSectors$Year == 2003 |
                FinalConsumptionSectors$Year ==  max(FinalConsumptionSectors$Year),
              paste0(format(
                round(FinalConsumptionSectors$value, digits = 0), big.mark = ","
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
          x = 2002,
          y = 33275,
          label = "Industry\n& Commercial",
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = 100560,
          label = "Domestic",
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = 141998,
          label = "Transport",
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = 180998,
          label = "Total",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionSectors$Year) + 1.2,
          y = as.numeric(
            subset(
              FinalConsumptionSectors,
              Year == max(FinalConsumptionSectors$Year) &
                variable == "Industry & Commercial"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionSectors,
              Year == max(FinalConsumptionSectors$Year) &
                variable == "Industry & Commercial"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Industry & Commercial")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionSectors$Year) + 1.2,
          y = as.numeric(
            subset(
              FinalConsumptionSectors,
              Year == max(FinalConsumptionSectors$Year) &
                variable == "Domestic"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionSectors,
              Year == max(FinalConsumptionSectors$Year) &
                variable == "Domestic"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Domestic")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionSectors$Year) + 1.2,
          y = as.numeric(
            subset(
              FinalConsumptionSectors,
              Year == max(FinalConsumptionSectors$Year) &
                variable == "Transport"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionSectors,
              Year == max(FinalConsumptionSectors$Year) &
                variable == "Transport"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Transport")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionSectors$Year) + 1.2,
          y = as.numeric(
            subset(
              FinalConsumptionSectors,
              Year == max(FinalConsumptionSectors$Year) &
                variable == "Domestic"
            )[1, 5]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Total")[1, 3]
          ), accuracy  = .1),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic",
          hjust = -.75
        ) + annotate(
          "text",
          x = max(FinalConsumptionSectors$Year) + 1.2,
          y = -17500,
          label = "% Change\nfrom baseline",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      
      FinalConsumptionSectorsChart
      
      
      FinalConsumptionSectorsChart <-
        BaselineChart(
          FinalConsumptionSectorsChart,
          FinalConsumptionSectors,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      FinalConsumptionSectorsChart <-
        FinalConsumptionSectorsChart +
        coord_flip() +
        labs(subtitle = paste("Scotland, 2005 -", max(FinalConsumptionSectors$Year))) +
        ylim(-26000, max(FinalConsumptionSectors$top) + 25000) +
        xlim(max(FinalConsumptionSectors$Year) + 1.2, 2002)
      
      FinalConsumptionSectorsChart
      
      ggsave(
        file,
        plot = FinalConsumptionSectorsChart,
        width = 17,
        height = 15.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
