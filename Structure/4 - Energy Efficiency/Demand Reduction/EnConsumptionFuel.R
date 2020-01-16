require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnConsumptionFuelOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Final energy consumption by fuel type", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('EnConsumptionFuelSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsumptionFuel.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("EnConsumptionFuelPlot")),
    plotlyOutput(ns("EnConsumptionFuelPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10, h3("Data - Final energy consumption by fuel type (GWh)", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnConsumptionFuelTable"))%>% withSpinner(color="#34d1a3"))),
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
EnConsumptionFuel <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EnConsumptionFuel.R")

  
  output$EnConsumptionFuelSubtitle <- renderText({
    
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
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Demand Reduction/EnConsumptionFuel.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("EnConsumptionFuelTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
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
          y = (.5/6)*170535,
          label = "Petroleum\nProducts",
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (1.5/6)*165580,
          label = "Gas",
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (2.5/6)*165580,
          label = "Electricity",
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (3.5/6)*165580,
          label = "Bioenergy\n& wastes",
          fontface = 2,
          color = BarColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (4.5/6)*165580,
          label = "Coal",
          fontface = 2,
          color = BarColours[5],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (5.5/6)*165580,
          label = "Manufactured\nFuels",
          fontface = 2,
          color = BarColours[6],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (6.5/6)*165580,
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
        ylim(-25000, max(FinalConsumptionFuel$top) + 20000) +
        xlim(max(FinalConsumptionFuel$Year) + 1.2, 2002)
      
      FinalConsumptionFuelChart
      
      ggsave(
        file,
        plot = FinalConsumptionFuelChart,
        width = 17,
        height = 15.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
