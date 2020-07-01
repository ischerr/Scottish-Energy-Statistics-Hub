require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnergyConsumptionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Sector",
    fluidRow(column(8,
                    h3("Total final energy consumption by consuming sector", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('EnConsumptionSectorSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsumptionSector.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    plotlyOutput(ns("EnConsumptionSectorPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Fuel",
             fluidRow(column(8,
                             h3("Final energy consumption by fuel type", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('EnConsumptionFuelSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsumptionFuel.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             plotlyOutput(ns("EnConsumptionFuelPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Local Authority",
             fluidRow(column(8,
                             h3("Total final energy consumption by local authority", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('EnConsumptionLASubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsumptionLA.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             leafletOutput(ns("EnConsumptionMap"), height = "800px")%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")
             )),

    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    tabsetPanel(
      tabPanel("Sector",
    fluidRow(
    column(10, h3("Data - Total final energy consumption by consuming sector (GWh)", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnConsumptionSectorTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Fuel",
             fluidRow(
               column(10, h3("Data - Final energy consumption by fuel type (GWh)", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EnConsumptionFuelTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")
             ),
    tabPanel("Local Authority",
             fluidRow(
             column(10, h3("Data - Local Authority", style = "color: #34d1a3;  font-weight:bold")),
             column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnConsumptionLATable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"
             ))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISSubNatEnergy", "BEISDUKESBalance"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("BEISDUKESBalance")
        
      )
    )
  )
}




###### Server ######
EnergyConsumption <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EnConsumptionSector.R")

  
  output$EnConsumptionSectorSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump sector",
      col_names = FALSE,
      skip = 16,
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
      skip = 16,
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
        text = paste0("Industry & Commercial: ", format(round(Data$`Industry & Commercial`, digits = 0), big.mark = ","), " GWh"),
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
        text = paste0("Domestic: ", format(round(Data$`Domestic`, digits = 0), big.mark = ","), " GWh"),
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
        text = paste0("Transport: ", format(round(Data$`Transport`, digits = 0), big.mark = ","), " GWh"),
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
        text = ifelse(Data$`Industry & Commercial` >0, paste("<b>",format(round((Data$`Industry & Commercial` + Data$`Domestic` + Data$`Transport`), digits = 0), big.mark = ","),"GWh</b>")," "),
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
        legend = list(font = list(color = "#34d1a3"),
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
          tickformat = ",d",
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
                                sheet = "Energy consump sector", skip = 16, col_names = FALSE)
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
        title = "Total final energy consumption by consuming sector (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Total final energy consumption by consuming sector (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Total final energy consumption by consuming sector (GWh)')
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
                     paste(readtext("Structure/4 - Energy Efficiency/Demand Reduction/EnConsumption.txt")[2])
                     
                   )))
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnConsumptionSector.png <- downloadHandler(
    filename = "EnConsumptionSector.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Energy consump sector", skip = 16, col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      Data[9] <- NULL
      
      names(Data) <- as.character(unlist(Data[1,]))
      
      names(Data)[1] <- "Year"
      Data <- tail(Data, -1)
      
      Data[1,1] <- 2003
      
      Data$Year <- as.numeric(Data$Year)
      Data[nrow(Data),1] <- max(Data$Year + 1, na.rm =  TRUE)
      Data <- Data[which(Data$Year> 0),]
   
      
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
        ylim(-26500, max(FinalConsumptionSectors$top) + 25000) +
        xlim(max(FinalConsumptionSectors$Year) + 1.2, 2002)
      
      FinalConsumptionSectorsChart
      
      ggsave(
        file,
        plot = FinalConsumptionSectorsChart,
        width = 20,
        height = 17.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  observeEvent(input$ToggleTable, {
    toggle("EnConsumptionSectorTable")
  })
  
  output$EnConsumptionFuelSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump fuel type",
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
  
  output$EnConsumptionFuelPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump fuel type",
      col_names = FALSE,
      skip = 12,
      n_max = 8
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:8] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2,1] <- "Baseline\n2005/2007"
    
    Data[3,1] <- " "
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    Data <- Data[-1,]
    
    Data$RowNumber <- as.numeric(rownames(Data))
    
    Data[is.na(Data)] <- 0
    
    DataTail <- tail(Data,1)
    DataLatest <- Data[nrow(Data)-1,]
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Petroleum products`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Petroleum products",
        text = paste0("Petroleum products: ", format(round(Data$`Petroleum products`, digits = 0), big.mark = ","), " GWh"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Gas`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Gas",
        text = paste0("Gas: ", format(round(Data$`Gas`, digits = 0), big.mark = ","), " GWh"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 3
      ) %>%
      
      add_trace(
        data = Data,
        x = ~ `Electricity`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Electricity",
        text = paste0("Electricity: ", format(round(Data$`Electricity`, digits = 0), big.mark = ","), " GWh"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Bioenergy & wastes`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Bioenergy & wastes",
        text = paste0("Bioenergy & wastes: ", format(round(Data$`Bioenergy & wastes`, digits = 0), big.mark = ","), " GWh"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Coal`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Coal",
        text = paste0("Coal: ", format(round(Data$`Coal`, digits = 0), big.mark = ","), " GWh"),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Manufactured fuels`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Manufactured fuels",
        text = paste0("Manufactured fuels: ", format(round(Data$`Manufactured fuels`, digits = 0), big.mark = ","), " GWh"),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 7
      ) %>%
      add_trace(
        data = Data,
        y = ~ Year,
        x = ~ (Data$`Petroleum products` + Data$Gas + Data$Electricity + Data$`Bioenergy & wastes` + Data$`Coal` + Data$`Manufactured fuels`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Petroleum products` >0, paste("<b>",format(round((Data$`Petroleum products` + Data$Gas + Data$Electricity + Data$`Bioenergy & wastes` + Data$`Coal` + Data$`Manufactured fuels`), digits = 0), big.mark = ","),"GWh</b>")," "),
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
        x = mean(DataLatest$`Petroleum products`)/2,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[1]),
        text = paste0("<b>", percent(DataTail$`Petroleum products`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x =  mean(DataLatest$`Petroleum products`) + (mean(DataLatest$`Gas`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[2]),
        text =  paste0("<b>", percent(DataTail$`Gas`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Petroleum products`) + mean(DataLatest$`Gas`) + (mean(DataLatest$`Electricity`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[3]),
        text =  paste0("<b>", percent(DataTail$Electricity, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Total`)+ 25000,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = ChartColours[1]),
        text =  paste0("<b>", percent(DataTail$Total, accuracy = 0.1), "</b>")
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#34d1a3"),
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
          tickformat = ",d",
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
  
  
  output$EnConsumptionFuelTable = renderDataTable({
    
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump fuel type",
      col_names = FALSE,
      skip = 12,
      n_max = 8
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:8] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2,1] <- " Baseline\n2005/2007"
    
    Data[3,1] <- " "
    
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
        title = "Final energy consumption by fuel type (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Final energy consumption by fuel type (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Final energy consumption by fuel type (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:8, 0)%>% 
      formatStyle(c(8), fontWeight = 'bold')
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EnConsumptionFuelTable")
  })
  
  output$EnConsumptionLASubtitle <- renderText({
    
    
    
    paste("Scotland, 2017")
  })
  
  output$EnConsumptionLAPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    writePNG(readPNG("Structure/4 - Energy Efficiency/Demand Reduction/LAConsumptionMap.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  EnConsumptionLA <- read_csv("Processed Data/Output/Consumption/CorrectedFinalConsumptionbyLA.csv")
  
  Year <- 2017
  
  output$EnConsumptionLATable = renderDataTable({
    
    unique(EnConsumptionLA$Year)
    
    EnConsumptionLATable <- EnConsumptionLA[which(EnConsumptionLA$Year == Year),]
    
    EnConsumptionLATable <- EnConsumptionLATable[c(3,1,26,27,28,25)]
    
    names(EnConsumptionLATable) <- c("Local Authority", "Geography Code", "Industry & Commercial", "Domestic", "Transport", "Total consumption")
    
    EnConsumptionLATable[order(substr(EnConsumptionLATable$`Geography Code`,1,3), EnConsumptionLATable$`Local Authority`),]
    
    datatable(
      EnConsumptionLATable,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Total final energy consumption by consuming sector (GWh), by local authority in Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Total final energy consumption by consuming sector (GWh), by local authority in Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Total final energy consumption by consuming sector (GWh), by local authority in Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(3:6, 0)
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("EnConsumptionLATable")
  })
  
  output$EnConsumptionLA.png <- downloadHandler(
    filename = "EnConsumptionLA.png",
    content = function(file) {
      writePNG(readPNG("Structure/4 - Energy Efficiency/Demand Reduction/LAConsumptionMapChart.png"), file) 
    }
  )
  
  output$EnConsumptionFuel.png <- downloadHandler(
    filename = "EnConsumptionFuel.png",
    content = function(file) {
      
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Energy consump fuel type", skip = 12, col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      names(Data) <- as.character(unlist(Data[1,]))
      
      names(Data)[1] <- "Year"
      Data <- tail(Data, -1)
      
      Data[1,1] <- 2003
      
      Data <- Data[complete.cases(Data),]
      
      Data[nrow(Data),1] <- max(as.numeric(Data$Year),na.rm = TRUE)+1
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      FinalConsumptionFuel <- Data[c(1,7:2,8)]
      
      FinalConsumptionFuel <-
        FinalConsumptionFuel[order(-FinalConsumptionFuel$Year),]
      
      FinalConsumptionFuel <-
        melt(FinalConsumptionFuel, id.vars = "Year")
      
      FinalConsumptionMax <-
        subset(FinalConsumptionFuel,
               Year == max(FinalConsumptionFuel$Year))
      
      FinalConsumptionFuel <-
        subset(
          FinalConsumptionFuel,
          Year < max(FinalConsumptionFuel$Year) & variable != "Total"
        )
      
      FinalConsumptionFuel$variable <-
        factor(FinalConsumptionFuel$variable,
               levels = unique(FinalConsumptionFuel$variable))
      
      FinalConsumptionFuel <- FinalConsumptionFuel %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Final energy consumption by fuel type"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
      
      
      FinalConsumptionFuelChart <- FinalConsumptionFuel %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Petroleum products" = BarColours[1],
            "Gas" = BarColours[2],
            "Electricity" = BarColours[3],
            "Bioenergy & wastes" = BarColours[4],
            "Coal" = BarColours[5],
            "Manufactured fuels" = BarColours[6]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = FinalConsumptionFuel$top,
          label = ifelse(
            FinalConsumptionFuel$value < 7000,
            paste0(format(
              round(FinalConsumptionFuel$top, digits = 0), big.mark = ","
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
            FinalConsumptionFuel$value < 7000,
            ifelse(
              FinalConsumptionFuel$Year == 2003,
              "2005/2007\n(baseline)",
              FinalConsumptionFuel$Year
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = FinalConsumptionFuel$top - FinalConsumptionFuel$pos,
          label =   ifelse(
            FinalConsumptionFuel$value > 7000,
            ifelse(
              FinalConsumptionFuel$Year == 2003 |
                FinalConsumptionFuel$Year ==  max(FinalConsumptionFuel$Year),
              paste0(format(
                round(FinalConsumptionFuel$value, digits = 0), big.mark = ","
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
          y = (.5/6)*max(FinalConsumptionFuel$top),
          label = "Petroleum\nProducts",
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (1.5/6)*max(FinalConsumptionFuel$top),
          label = "Gas",
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (2.5/6)*max(FinalConsumptionFuel$top),
          label = "Electricity",
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (3.5/6)*max(FinalConsumptionFuel$top),
          label = "Bioenergy\n& wastes",
          fontface = 2,
          color = BarColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (4.5/6)*max(FinalConsumptionFuel$top),
          label = "Coal",
          fontface = 2,
          color = BarColours[5],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (5.5/6)*max(FinalConsumptionFuel$top),
          label = "Manufactured\nFuels",
          fontface = 2,
          color = BarColours[6],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (6.5/6)*max(FinalConsumptionFuel$top),
          label = "Total",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        ) +
        
        annotate(
          "text",
          x = max(FinalConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Petroleum products"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Petroleum products"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Petroleum products")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Gas"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Gas"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Gas")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Electricity"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Electricity"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Electricity")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Bioenergy & Wastes"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Bioenergy & Wastes"
            )[1, 4]
          ),
          label = paste0("+",percent((
            subset(FinalConsumptionMax, variable == "Bioenergy & Wastes")[1, 3]
          ), accuracy = .1)),
          fontface = 2,
          color = BarColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Gas"
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
          x = max(FinalConsumptionFuel$Year) + 1.2,
          y = -17500,
          label = "% Change\nfrom baseline",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      
      FinalConsumptionFuelChart
      
      
      FinalConsumptionFuelChart <-
        BaselineChart(
          FinalConsumptionFuelChart,
          FinalConsumptionFuel,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      FinalConsumptionFuelChart <-
        FinalConsumptionFuelChart +
        coord_flip() +
        labs(subtitle = paste("Scotland, 2005 -", max(FinalConsumptionFuel$Year))) +
        ylim(-max(FinalConsumptionFuel$top * 0.16), max(FinalConsumptionFuel$top * 1.16)) +
        xlim(max(FinalConsumptionFuel$Year) + 1.2, 2002)
      
      FinalConsumptionFuelChart
      
      ggsave(
        file,
        plot = FinalConsumptionFuelChart,
        width = 20,
        height = 17.5,
        units = "cm",
        dpi = 300
      )
      
      
    }

  )
  
  
  
  
  
  output$EnConsumptionMap <- renderLeaflet({
    
    ### Load Packages
    library(readr)
    library("maptools")
    library(tmaptools)
    library(tmap)
    library("sf")
    library("leaflet")
    library("rgeos")
    library(readxl)
    library(ggplot2)
    library(spatialEco)
    
    print("MapsandGasGrid")
    
    # This is unlikely to change from 2012
    yearstart <- 2012
    
    ### Set the final year for the loop as the current year ###
    yearend <- format(Sys.Date(), "%Y")
    
    
    ### Add Simplified shape back to the Shapefile
    LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")
    
    LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
    ############ RENEWABLE ELECTRICITY ################################################
    
    EnConsumptionLA <- read_csv("Processed Data/Output/Consumption/CorrectedFinalConsumptionbyLA.csv")
    
    Year <- 2017
    
      unique(EnConsumptionLA$Year)
      
      EnConsumptionLAMap <- EnConsumptionLA[which(EnConsumptionLA$Year == Year),]
      
      EnConsumptionLAMap <- EnConsumptionLAMap[c(3,1,26,27,28,25)]
      
      names(EnConsumptionLAMap) <- c("LocalAuthority", "CODE", "Industry & Commercial", "Domestic", "Transport", "Total")
      
      EnConsumptionLAMap[order(substr(EnConsumptionLAMap$`CODE`,1,3), EnConsumptionLAMap$`LocalAuthority`),]
      
    
    EnConsumptionLAMap <- EnConsumptionLAMap[c(1,2,ncol(EnConsumptionLAMap))]
    
    EnConsumptionLAMap <- EnConsumptionLAMap[which(substr(EnConsumptionLAMap$CODE, 1,3)== "S12"),]
    
    EnConsumptionLAMap[is.na(EnConsumptionLAMap)] <- 0
    
    EnConsumptionLAMap$Content <- paste0("<b>",EnConsumptionLAMap$LocalAuthority, "</b><br/>Total final energy consumption:<br/><em>", ifelse(EnConsumptionLAMap$Total > 0,paste0(format(round(EnConsumptionLAMap$Total,0 ),  big.mark = ",")," GWh</em>"),"N/A" ))
    
    EnConsumptionLAMap$Hover <- paste0(EnConsumptionLAMap$LocalAuthority, " - ", ifelse(EnConsumptionLAMap$Total > 0,paste0(format(round(EnConsumptionLAMap$Total,0 ),  big.mark = ",")," GWh"),"N/A" ))
  
    
    
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    EnConsumptionLAMap <- EnConsumptionLAMap[order(EnConsumptionLAMap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      append_data(LA, EnConsumptionLAMap, key.shp = "CODE", key.data = "CODE")
    
    pal <- colorNumeric(
      palette = "Greens",
      domain = LAMap$Total)
    
    leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas" ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(Total),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~Total,
                         title = "Total final energy consumption",
                         labFormat = labelFormat(suffix = " GWh"),
                         opacity = 1
      ) %>% 
      htmlwidgets::prependContent(html_fix) 
    
  }) 
}
