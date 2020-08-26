require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



HeatConsumptionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Sector",
    fluidRow(column(8,
                    h3("Non-electrical heat demand by sector", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('HeatConsumptionSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('HeatConsumption.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("HeatConsumptionPlot")),
    plotlyOutput(ns("HeatConsumptionPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Fuel",
             fluidRow(column(8,
                             h3("Non-electrical heat demand by fuel", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('HeatConsumptionFuelSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('HeatConsumptionFuel.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("HeatConsumptionPlot")),
             plotlyOutput(ns("HeatConsumptionFuelPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
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
    column(10, h3("Data - Non-electrical heat demand by sector (GWh)", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("HeatConsumptionTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Fuel",
             fluidRow(
               column(10, h3("Data - Non-electrical heat demand (GWh), by fuel", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("HeatConsumptionFuelTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))
    ),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISSubNatEnergy", "BEISUKConsump"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("BEISUKConsump")
        
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
      skip = 16,
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
      skip = 16,
      n_max = 4
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- as.character(Data$Year)
    
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
        text = paste0("Domestic: ", format(round(Data$`Domestic`, digits = 0), big.mark = ","), " GWh"),
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
        text = paste0("Non-Domestic: ", format(round(Data$`Non-Domestic`, digits = 0), big.mark = ","), " GWh"),
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
        text = ifelse(Data$`Domestic` >0, paste("<b>",format(round((Data$`Domestic` + Data$`Non-Domestic`), digits = 0), big.mark = ","),"GWh</b>")," "),
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
      skip = 16,
      n_max = 3
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- as.character(Data$Year)
    
    Data[2,1] <- " Baseline\n2005/2007"
    
    Data <- head(Data, -2)
    
    Data <- Data[-1,]
    
    Data$Total <- Data$Domestic+Data$`Non-Domestic`
    
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
        title = "Non-electrical heat demand by sector (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Non-electrical heat demand by sector (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Non-electrical heat demand by sector (GWh)')
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
                     paste(readtext("Structure/4 - Energy Efficiency/Demand Reduction/HeatConsumption.txt")[2])
                     
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
                         sheet = "Heat consump", skip = 16, col_names = FALSE)
      
      Data <- head(Data, 4)
      
      Data <- as_tibble(t(Data))
      
      Data <- Data[complete.cases(Data),]
      
      Data <- head(Data, -1)
      
      Data [1,1] <- "2003"
      
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
          ),.1),
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
          ),.1),
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
          ), .1),
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
        width = 20,
        height = 17.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  HeatConsumptionFuel <- read_csv("Processed Data/Output/Consumption/HeatConsumptionbyLA.csv")
  
  HeatConsumptionFuel <- HeatConsumptionFuel[which(HeatConsumptionFuel$`LA Code` == "S92000003"),]
  
  HeatConsumptionFuel$Coal <- HeatConsumptionFuel$`Coal - Industrial & Commercial` + HeatConsumptionFuel$`Coal - Domestic`
  
  HeatConsumptionFuel$`Manufactured fuels` <- HeatConsumptionFuel$`Manufactured fuels - Industrial` + HeatConsumptionFuel$`Manufactured fuels - Domestic`
  
  HeatConsumptionFuel$`Petroleum products` <- HeatConsumptionFuel$`Petroleum products - Industrial & Commercial` + HeatConsumptionFuel$`Petroleum products - Domestic` + HeatConsumptionFuel$`Petroleum products - Public Sector` + HeatConsumptionFuel$`Petroleum products - Agriculture`
  
  HeatConsumptionFuel$Gas <- HeatConsumptionFuel$`Sales (GWh) - Non-domestic consumption` + HeatConsumptionFuel$`Sales (GWh) - Domestic consumption`
  
  HeatConsumptionFuel$`Bioenergy & wastes` <- HeatConsumptionFuel$`Bioenergy & wastes - Total`
  
  HeatConsumptionFuel <- HeatConsumptionFuel[c(2, 19,20,21,17,18, 16)]
  
  HeatConsumptionFuel <- rbind(HeatConsumptionFuel, read_csv("Structure/4 - Energy Efficiency/Demand Reduction/HeatFuelExtra.csv"))
  
  HeatConsumptionFuel <- HeatConsumptionFuel[order(-HeatConsumptionFuel$Year),]
  
  HeatConsumptionFuelBaseline <- as.data.frame.list(colMeans(HeatConsumptionFuel[which(HeatConsumptionFuel$Year %in% c(2005,2006,2007)),] ))
  
  HeatConsumptionFuelBaseline$Year <- as.character(HeatConsumptionFuelBaseline$Year)
  
  HeatConsumptionFuelBaseline[1,1] <- "Baseline 2005/2007"
  
  names(HeatConsumptionFuelBaseline) <- names(HeatConsumptionFuel)
  
  HeatConsumptionFuel <- rbind(HeatConsumptionFuel, HeatConsumptionFuelBaseline)
  
  output$HeatConsumptionFuelTable = renderDataTable({
    
    
    datatable(
      HeatConsumptionFuel,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Non-electrical heat demand (GWh), by fuel",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Non-electrical heat demand (GWh), by fuel',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Non-electrical heat demand (GWh), by fuel')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:7, 0) %>% 
      formatStyle(c(7), fontWeight = 'bold')
    
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("HeatConsumptionFuelTable")
  })
  
  output$HeatConsumptionFuelSubtitle <- renderText({
    
    paste("Scotland,", min(as.numeric(HeatConsumptionFuel$Year), na.rm = TRUE),"-", max(as.numeric(HeatConsumptionFuel$Year), na.rm = TRUE))

      })
  
  output$HeatConsumptionFuelPlot <- renderPlotly  ({
    Data <- HeatConsumptionFuel
    
    Data<- Data[seq(dim(Data)[1],1),]
    
    DataCalc <- -(1-(Data[nrow(Data),2:7]/Data[1,2:7]))
    
    DataCalc$Year <- "% Change\nfrom baseline"
    
    Data <- rbind(Data, DataCalc)
    
    Data[nrow(Data)+1,] <- NA
    
    Data <- Data[c(1,nrow(Data), 2:(nrow(Data)-1)),]
    
    Data$Year <- as.character(Data$Year)
    
    Data[1,1] <- "Baseline\n2005/2007"
    
    Data[2,1] <- " "
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
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
        x = ~ (Data$`Petroleum products` + Data$Gas  + Data$`Bioenergy & wastes` + Data$`Coal` + Data$`Manufactured fuels`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Petroleum products` >0, paste("<b>",format(round((Data$`Petroleum products` + Data$Gas  + Data$`Bioenergy & wastes` + Data$`Coal` + Data$`Manufactured fuels`), digits = 0), big.mark = ","),"GWh</b>")," "),
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
          range = c(0,125000)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  output$HeatConsumptionFuel.png <- downloadHandler(
    filename = "HeatConsumptionFuel.png",
    content = function(file) {
      
      Data <- HeatConsumptionFuel
      
      Data<- Data[seq(dim(Data)[1],1),]
      
      DataCalc <- -(1-(Data[nrow(Data),2:7]/Data[1,2:7]))
      
      DataCalc$Year <- "% Change\nfrom baseline"
      
      Data <- rbind(Data, DataCalc)
      
      Data[1,1] <- "2003"
      
      Data <- Data[complete.cases(Data),]
      
      Data[nrow(Data),1] <- as.character(max(as.numeric(Data$Year),na.rm = TRUE)+1)
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      HeatConsumptionFuel <- Data[c(1,6:2,7)]
      
      HeatConsumptionFuel <-
        HeatConsumptionFuel[order(-HeatConsumptionFuel$Year),]
      
      HeatConsumptionFuel <-
        melt(HeatConsumptionFuel, id.vars = "Year")
      
      HeatConsumptionMax <-
        subset(HeatConsumptionFuel,
               Year == max(HeatConsumptionFuel$Year))
      
      HeatConsumptionFuel <-
        subset(
          HeatConsumptionFuel,
          Year < max(HeatConsumptionFuel$Year) & variable != "Total"
        )
      
      HeatConsumptionFuel$variable <-
        factor(HeatConsumptionFuel$variable,
               levels = unique(HeatConsumptionFuel$variable))
      
      HeatConsumptionFuel <- HeatConsumptionFuel %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Non-electrical heat demand by fuel"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
      
      
      HeatConsumptionFuelChart <- HeatConsumptionFuel %>%
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
          y = HeatConsumptionFuel$top,
          label = ifelse(
            HeatConsumptionFuel$value < 7000,
            paste0(format(
              round(HeatConsumptionFuel$top, digits = 0), big.mark = ","
            ), " GWh"),
            ""
          ),
          hjust = 0,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = -11500,
          label =   ifelse(
            HeatConsumptionFuel$value < 7000,
            ifelse(
              HeatConsumptionFuel$Year == 2003,
              "2005/2007\n(baseline)",
              HeatConsumptionFuel$Year
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = HeatConsumptionFuel$top - HeatConsumptionFuel$pos,
          label =   ifelse(
            HeatConsumptionFuel$value > 7000,
            ifelse(
              HeatConsumptionFuel$Year == 2003 |
                HeatConsumptionFuel$Year ==  max(HeatConsumptionFuel$Year),
              paste0(format(
                round(HeatConsumptionFuel$value, digits = 0), big.mark = ","
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
          y = (.5/5)*max(HeatConsumptionFuel$top),
          label = "Petroleum\nProducts",
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (1.5/5)*max(HeatConsumptionFuel$top),
          label = "Gas",
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (2.5/5)*max(HeatConsumptionFuel$top),
          label = "Bioenergy\n& wastes",
          fontface = 2,
          color = BarColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (3.5/5)*max(HeatConsumptionFuel$top),
          label = "Coal",
          fontface = 2,
          color = BarColours[5],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (4.5/5)*max(HeatConsumptionFuel$top),
          label = "Manufactured\nFuels",
          fontface = 2,
          color = BarColours[6],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2002,
          y = (5.5/5)*max(HeatConsumptionFuel$top),
          label = "Total",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        ) +
        
        annotate(
          "text",
          x = max(HeatConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              HeatConsumptionFuel,
              Year == max(HeatConsumptionFuel$Year) &
                variable == "Petroleum products"
            )[1, 5]
          ) - as.numeric(
            subset(
              HeatConsumptionFuel,
              Year == max(HeatConsumptionFuel$Year) &
                variable == "Petroleum products"
            )[1, 4]
          ),
          label = percent((
            subset(HeatConsumptionMax, variable == "Petroleum products")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(HeatConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              HeatConsumptionFuel,
              Year == max(HeatConsumptionFuel$Year) &
                variable == "Gas"
            )[1, 5]
          ) - as.numeric(
            subset(
              HeatConsumptionFuel,
              Year == max(HeatConsumptionFuel$Year) &
                variable == "Gas"
            )[1, 4]
          ),
          label = percent((
            subset(HeatConsumptionMax, variable == "Gas")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(HeatConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              HeatConsumptionFuel,
              Year == max(HeatConsumptionFuel$Year) &
                variable == "Electricity"
            )[1, 5]
          ) - as.numeric(
            subset(
              HeatConsumptionFuel,
              Year == max(HeatConsumptionFuel$Year) &
                variable == "Electricity"
            )[1, 4]
          ),
          label = percent((
            subset(HeatConsumptionMax, variable == "Electricity")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(HeatConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              HeatConsumptionFuel,
              Year == max(HeatConsumptionFuel$Year) &
                variable == "Bioenergy & Wastes"
            )[1, 5]
          ) - as.numeric(
            subset(
              HeatConsumptionFuel,
              Year == max(HeatConsumptionFuel$Year) &
                variable == "Bioenergy & Wastes"
            )[1, 4]
          ),
          label = paste0("+",percent((
            subset(HeatConsumptionMax, variable == "Bioenergy & Wastes")[1, 3]
          ), accuracy = .1)),
          fontface = 2,
          color = BarColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(HeatConsumptionFuel$Year) + 1.2,
          y = as.numeric(
            subset(
              HeatConsumptionFuel,
              Year == max(HeatConsumptionFuel$Year) &
                variable == "Gas"
            )[1, 5]
          ),
          label = percent((
            subset(HeatConsumptionMax, variable == "Total")[1, 3]
          ), accuracy  = .1),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic",
          hjust = -.75
        ) + annotate(
          "text",
          x = max(HeatConsumptionFuel$Year) + 1.2,
          y = -17500,
          label = "% Change\nfrom baseline",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      
      HeatConsumptionFuelChart
      
      
      HeatConsumptionFuelChart <-
        BaselineChart(
          HeatConsumptionFuelChart,
          HeatConsumptionFuel,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      HeatConsumptionFuelChart <-
        HeatConsumptionFuelChart +
        coord_flip() +
        labs(subtitle = paste("Scotland, 2005 -", max(HeatConsumptionFuel$Year))) +
        ylim(-max(HeatConsumptionFuel$top * 0.16), max(HeatConsumptionFuel$top * 1.16)) +
        xlim(max(HeatConsumptionFuel$Year) + 1.2, 2002)
      
      HeatConsumptionFuelChart
      
      ggsave(
        file,
        plot = HeatConsumptionFuelChart,
        width = 20,
        height = 17.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
}
