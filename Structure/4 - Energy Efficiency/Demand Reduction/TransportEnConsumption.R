require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



TransportEnConsumptionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Road and rail energy consumption", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('TransportEnConsumptionSubtitle')), style = "color: #34d1a3;"),
                    selectInput(ns("UnitSelect"), "Unit:", TransportMultipliers$Unit, selected = TransportMultipliers$Unit[1], multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL)
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('TransportEnConsumption.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("TransportEnConsumptionPlot")),
    plotlyOutput(ns("TransportEnConsumptionPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10, h3("Data - Road and rail energy consumption", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))),
    fluidRow(column(12,selectInput(ns("UnitSelect2"), "Unit:", TransportMultipliers$Unit, selected = TransportMultipliers$Unit[1], multiple = FALSE,
                                   selectize = TRUE, width = NULL, size = NULL))),
    fluidRow(
      column(12, dataTableOutput(ns("TransportEnConsumptionTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("SGGrowth"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("SGGrowth"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("BEISLocalRoad")
        
      )
    )
  )
}




###### Server ######
TransportEnConsumption <- function(input, output, session) {
  
  
  observe({
    TransportDropdown$Unit <- input$UnitSelect
  })
  
  observe({
    TransportDropdown$Unit <- input$UnitSelect2
  })
  

  observe(
    {
      updateSelectInput(session, 'UnitSelect', selected = TransportDropdown$Unit)
      updateSelectInput(session, 'UnitSelect2', selected = TransportDropdown$Unit)
    }
  )
  
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("TransportEnConsumption.R")

  
  output$TransportEnConsumptionSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Transport energy consump",
      col_names = TRUE,
      skip = 14
    )
    
    Data <- Data[complete.cases(Data),]
    
    Data <- head(Data,-1)
    
    names(Data)[c(1,12,13)] <- c("Year", "Rail", "Total Transport Consumption")
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$TransportEnConsumptionPlot <- renderPlotly  ({
    
    unit <- as.character(TransportDropdown$Unit)
    
    multiplier <- TransportMultipliers[which(TransportMultipliers$Unit == unit),]$Multiplier
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Transport energy consump",
      col_names = FALSE,
      skip = 14
    )[c(1,6,10,12,13)]
    
    names(Data) <- c("Year", "Road - Personal", "Road - Freight", "Rail", "Total")
    
    Data[1:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2:5] %<>% lapply(function(x) as.numeric(as.character(x))*multiplier)
    
    Data$Year <- as.character(Data$Year)
    
    Data[2,1] <- "Baseline\n2005/2007"
    
    Data[3,1] <- " "
    
    Data<- head(Data, -3)
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    Data <- Data[-1,]
    
    Data$RowNumber <- as.numeric(rownames(Data))
    
    Data[is.na(Data)] <- 0
    
    Data[nrow(Data),2] <- (Data[nrow(Data)-1,2]/Data[1,2])-1
    Data[nrow(Data),3] <- (Data[nrow(Data)-1,3]/Data[1,3])-1
    Data[nrow(Data),4] <- (Data[nrow(Data)-1,4]/Data[1,4])-1
    Data[nrow(Data),5] <- (Data[nrow(Data)-1,5]/Data[1,5])-1
    
    DataTail <- tail(Data,1)
    DataLatest <- Data[nrow(Data)-1,]
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
    
    
    Data <- Data[c(-3, -4, -5),]
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Road - Personal`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Road - Personal",
        text = paste0("Road - Personal: ", format(round(Data$`Road - Personal`, digits = 0), big.mark = ","), unit),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Road - Freight`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Road - Freight",
        text = paste0("Road - Freight: ", format(round(Data$`Road - Freight`, digits = 0), big.mark = ","), unit),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 3
      ) %>%
      
      add_trace(
        data = Data,
        x = ~ `Rail`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Rail",
        text = paste0("Rail: ", format(round(Data$`Rail`, digits = 0), big.mark = ","), "", unit),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        y = ~ Year,
        x = ~ (Data$`Road - Personal` + Data$`Road - Freight` + Data$`Rail`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Road - Personal` >0, paste("<b>",format(round((Data$`Road - Personal` + Data$`Road - Freight` + Data$`Rail`), digits = 0), big.mark = ","), unit, "</b>")," "),
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
        x = mean(DataLatest$`Road - Personal`)/2,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[1]),
        text = paste0("<b>", percent(DataTail$`Road - Personal`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x =  mean(DataLatest$`Road - Personal`) + (mean(DataLatest$`Road - Freight`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[2]),
        text =  paste0("<b>", percent(DataTail$`Road - Freight`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Road - Personal`) + mean(DataLatest$`Road - Freight`) + (mean(DataLatest$`Rail`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[3]),
        text =  paste0("<b>", percent(DataTail$`Rail`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Total`)*1.05,
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
          range = c(0,(50000*multiplier))
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  
  output$TransportEnConsumptionTable = renderDataTable({
    
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Transport energy consump",
      col_names = TRUE,
      skip = 14
    )
    
    unit <- as.character(TransportDropdown$Unit)
    
    multiplier <- TransportMultipliers[which(TransportMultipliers$Unit == unit),]$Multiplier
    
    Data <- Data[complete.cases(Data),]
    
    Data <- head(Data,-1)
    
    Data[1:13] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2:13] %<>% lapply(function(x) as.numeric(as.character(x))*multiplier)
    
    names(Data)[c(1,12,13)] <- c("Year", "Rail", "Total Transport Consumption")
    
    Data <- Data[seq(dim(Data)[1],1),]
    
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
        title = paste0("Road and rail energy consumption (", unit,")"),
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = paste0("Road and rail energy consumption (", unit,")"),
            header = TRUE
          ),
          list(extend = 'csv',
               title = paste0("Road and rail energy consumption (", unit,")"))
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:13, 0)%>% 
      formatStyle(c(11,13), fontWeight = 'bold')%>% 
      formatStyle(c(6,10), fontStyle = 'italic')
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Demand Reduction/TransportEnConsumption.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("TransportEnConsumptionTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$TransportEnConsumption.png <- downloadHandler(
    filename = "TransportEnConsumption.png",
    content = function(file) {


      Data <- read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Transport energy consump",
        col_names = FALSE,
        skip = 14
      )[c(1,6,10,12,13)]
      
      unit <- as.character(TransportDropdown$Unit)
      
      multiplier <- TransportMultipliers[which(TransportMultipliers$Unit == unit),]$Multiplier
      
      names(Data) <- c("Year", "Road - Personal", "Road - Freight", "Rail", "Total")
  
      names(Data)[1] <- "Year"
      Data <- tail(Data, -1)
      
      Data[1,1] <- "2003"
      
      Data <- Data[complete.cases(Data),]
      
      Data[nrow(Data),1] <- as.character(max(as.numeric(Data$Year),na.rm = TRUE)+1)
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      FinalConsumptionFuel <- Data[c(1,4:2,5)]
      
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
        "Road and rail energy consumption"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
      
      
      FinalConsumptionFuelChart <- FinalConsumptionFuel %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Road - Personal" = BarColours[1],
            "Road - Freight" = BarColours[2],
            "Rail" = BarColours[3]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = FinalConsumptionFuel$top *1.05,
          label = ifelse(
            FinalConsumptionFuel$value < 7000,
            paste0(format(
              round(FinalConsumptionFuel$top*multiplier, digits = 0), big.mark = ","
            ), " ", unit),
            ""
          ),
          hjust = 0,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = -(max(FinalConsumptionFuel$top)*0.1),
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
                round(FinalConsumptionFuel$value*multiplier, digits = 0), big.mark = ","
              ), " ", unit),
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
          y = (.5/3)*max(FinalConsumptionFuel$top),
          label = "Road - Personal",
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (1.5/3)*max(FinalConsumptionFuel$top),
          label = "Road - Freight",
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (2.5/3)*max(FinalConsumptionFuel$top),
          label = "Rail",
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (3.3/3)*max(FinalConsumptionFuel$top),
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
                variable == "Road - Personal"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Road - Personal"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Road - Personal")[1, 3]
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
                variable == "Road - Freight"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Road - Freight"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Road - Freight")[1, 3]
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
                variable == "Rail"
            )[1, 5]
          ) - as.numeric(
            subset(
              FinalConsumptionFuel,
              Year == max(FinalConsumptionFuel$Year) &
                variable == "Rail"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Rail")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionFuel$Year) + 1.2,
          y = mean(FinalConsumptionFuel$top)*1.15,
          label = percent((mean(FinalConsumptionFuel[which(FinalConsumptionFuel$Year == max(FinalConsumptionFuel$Year)),]$top) / 
                             mean(FinalConsumptionFuel[which(FinalConsumptionFuel$Year == min(FinalConsumptionFuel$Year)),]$top))-1, 0.1),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(FinalConsumptionFuel$Year) + 1.2,
          y = -(max(FinalConsumptionFuel$top)*0.1),
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
        ylim(-(max(FinalConsumptionFuel$top)*0.14), max(FinalConsumptionFuel$top)*1.2) +
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
}
