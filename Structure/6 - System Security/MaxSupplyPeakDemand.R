require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

MaxSupplyPeakDemandOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Demand",
    fluidRow(column(8,
                    h3("Installed supply capacity and peak electricity demand", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('MaxSupplyPeakDemandSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('MaxSupplyPeakDemand.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("MaxSupplyPeakDemandPlot")),
    plotlyOutput(ns("MaxSupplyPeakDemandPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Supply",
             fluidRow(column(8,
                             h3("Maximum supply capacity by source", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('MaxSupplyCapacitySubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('MaxSupplyCapacity.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("MaxSupplyPeakDemandPlot")),
             plotlyOutput(ns("MaxSupplyCapacityPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))
    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    tabsetPanel(
      tabPanel("Peak Demand",
    fluidRow(
    column(10, h3("Data - Peak Demand", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("MaxSupplyPeakDemandTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Demand",
      fluidRow(
        column(10, h3("Data - Demand", style = "color: #5d8be1;  font-weight:bold")),
        column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
      ),
      fluidRow(
        column(12, dataTableOutput(ns("MaxSupplyCapacityTable"))%>% withSpinner(color="#5d8be1"))),
      tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))
    ),
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
        SourceLookup("ESTDomRHIInstallations")
        
      )
    )
  )
}




###### Server ######
MaxSupplyPeakDemand <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("MaxSupplyPeakDemand.R")

  
  output$MaxSupplyPeakDemandSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec capacity and peak demand", 
        skip = 14)[2:6]
    
    Data <- Data[which(nchar(Data$Year) == 7),]
    
    Data[2:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$`Peak electricity demand date` <- as.Date(Data$`Peak electricity demand date`,  origin = "1899-12-30")
    
    paste("Scotland,", min(Data$Year), "-", max(Data$Year))
    
  })
  
  output$MaxSupplyPeakDemandPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec capacity and peak demand", 
        skip = 14)[2:6]
    
    Data <- Data[which(nchar(Data$Year) == 7),]
    
    Data[2:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$`Peak electricity demand date` <- as.Date(Data$`Peak electricity demand date`,  origin = "1899-12-30")
    
    Data$Year <- paste0("<b>", Data$Year, "</b>")
    
    ChartColours <- c("#5d8be1", "#FF8500")
    BarColours <-
      c(
        "#225ea8",
        "#ef6548"
      )
    
    p <-  plot_ly(Data, 
                  y = ~Year, 
                  x = ~ `Peak electricity demand for the year`, 
                  type = 'bar', 
                  name = 'Peak electricity demand for the year',
                  hoverinfo = "text",
                  text = paste0("Peak electricity demand for the year: ",format(round(Data$`Peak electricity demand for the year`, digits = 0), big.mark = ","), " MW\nDate: ", format(Data$`Peak electricity demand date`, "%d/%m/%y")),
                  orientation = 'h',
                  marker = list(color = BarColours[2])
                  )%>%
      add_trace(x = ~ `Maximum supply capacity`, 
                type = 'bar', 
                name = 'Maximum supply capacity',
                hoverinfo = "text",
                text = paste0("Maximum supply capacity: ",format(round(Data$`Maximum supply capacity`, digits = 0), big.mark = ",")," MW\nPeak demand as percentage of maximum supply capacity: ", percent(Data$`Peak demand as percentage of maximum supply capacity`, 0.1)),
                orientation = 'h',
                marker = list(color = BarColours[1])
      ) %>% 
     
      layout(
        barmode = 'group',
        bargap = 0.25,
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "",
                     ticksuffix = " MW",
                     showgrid = TRUE,
                     x = 0.5
                     
                     ),
        yaxis = list(
          title = "",
          tickformat = "",
          autorange = "reversed",
          ticktext = as.list(Data$`Year`),
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

  output$MaxSupplyPeakDemandTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec capacity and peak demand", 
        skip = 14)[2:6]
    
    Data <- Data[which(nchar(Data$Year) == 7),]
    
    Data[2:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$`Peak electricity demand date` <- as.Date(Data$`Peak electricity demand date`,  origin = "1899-12-30")
    
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
        title = "Average annual domestic standard electricity bills in Scotland (\u00A3)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Average annual domestic standard electricity bills in Scotland (\u00A3)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Average annual domestic standard electricity bills in Scotland (\u00A3)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:3, 0) %>% 
      formatPercentage(5, 1) %>% 
      formatDate(4)
  })
  
  output$MaxSupplyCapacitySubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec capacity and peak demand", col_names = TRUE, 
        skip = 37)[c(1:11)]
    
    names(Data)[1:2] <- c("Year", "Period")
    
    Data <- Data[which(Data$Year >= 2010),]
    
    Data[3:11] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    paste("Scotland,", min(Data$Period),"-", max(Data$Period))
  })
  
  output$MaxSupplyCapacityPlot <- renderPlotly  ({
    
    
    ChartColours <- c("#5d8be1", "#FF8500")
    BarColours <-
      c(
        "#034e7b",
        "#0570b0",
        "#3690c0",
        "#74a9cf",
        "#a6bddb",
        "#d0d1e6",
        "#bdbdbd",
        "#969696"
      )
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec capacity and peak demand", col_names = TRUE, 
        skip = 37)[c(1:11)]
    
    names(Data)[1:2] <- c("Year", "Period")
    
    Data <- Data[which(Data$Year >= 2010),]
    
    Data[3:11] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data$YearFormat <- paste0("<b>",Data$Period, "</b>")
    
    p <-  plot_ly(Data, y = ~ YearFormat ) %>%  
      add_trace(x = ~ `CCGT`, 
                orientation = 'h',
                name = "CCGT",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  "CCGT: ", format(Data$CCGT, big.mark = ",")," MW\n",
                  "Year: ", Data$Year, "\n",
                  "Total Scottish generation: ", format( round((Data$CCGT + Data$Coal + Data$Nuclear + Data$`Large Hydro` + Data$`Pumped Storage` + Data$Diesel), digits = 0), big.mark = ","), " MW"
                ),
                hoverinfo = 'text',
                marker = list(color = BarColours[1])
      ) %>% 
      add_trace(x = ~ `Coal`, 
                orientation = 'h',
                name = "Coal",
                type = 'bar',
                legendgroup = "2",
                text = paste0(
                  "Coal: ", format(Data$Coal, big.mark = ",")," MW\n",
                  "Year: ", Data$Year, "\n",
                  "Total Scottish generation: ", format( round((Data$CCGT + Data$Coal + Data$Nuclear + Data$`Large Hydro` + Data$`Pumped Storage` + Data$Diesel), digits = 0), big.mark = ","), " MW"
                ),
                hoverinfo = 'text',
                marker = list(color = BarColours[2])
      ) %>% 
      add_trace(x = ~ `Nuclear`, 
                orientation = 'h',
                name = "Nuclear",
                type = 'bar',
                legendgroup = "3",
                text = paste0(
                  "Nuclear: ", format(Data$Nuclear, big.mark = ",")," MW\n",
                  "Year: ", Data$Year, "\n",
                  "Total Scottish generation: ", format( round((Data$CCGT + Data$Coal + Data$Nuclear + Data$`Large Hydro` + Data$`Pumped Storage` + Data$Diesel), digits = 0), big.mark = ","), " MW"
                ),
                hoverinfo = 'text',
                marker = list(color = BarColours[3])
      ) %>% 
      add_trace(x = ~ `Large Hydro`, 
                orientation = 'h',
                name = "Large Hydro",
                type = 'bar',
                legendgroup = "4",
                text = paste0(
                  "Large Hydro: ", format(Data$`Large Hydro`, big.mark = ",")," MW\n",
                  "Year: ", Data$Year, "\n",
                  "Total Scottish generation: ", format( round((Data$CCGT + Data$Coal + Data$Nuclear + Data$`Large Hydro` + Data$`Pumped Storage` + Data$Diesel), digits = 0), big.mark = ","), " MW"
                ),
                hoverinfo = 'text',
                marker = list(color = BarColours[4])
      ) %>% 
      add_trace(x = ~ `Pumped Storage`, 
                orientation = 'h',
                name = "Pumped Storage",
                type = 'bar',
                legendgroup = "5",
                text = paste0(
                  "Pumped Storage: ", format(Data$`Pumped Storage`, big.mark = ",")," MW\n",
                  "Year: ", Data$Year, "\n",
                  "Total Scottish generation: ", format( round((Data$CCGT + Data$Coal + Data$Nuclear + Data$`Large Hydro` + Data$`Pumped Storage` + Data$Diesel), digits = 0), big.mark = ","), " MW"
                ),
                hoverinfo = 'text',
                marker = list(color = BarColours[5])
      ) %>% 
      add_trace(x = ~ `Diesel`, 
                orientation = 'h',
                name = "Diesel",
                type = 'bar',
                legendgroup = "6",
                text = paste0(
                  "Diesel: ", format(Data$`Diesel`, big.mark = ",")," MW\n",
                  "Year: ", Data$Year, "\n",
                  "Total Scottish generation: ", format( round((Data$CCGT + Data$Coal + Data$Nuclear + Data$`Large Hydro` + Data$`Pumped Storage` + Data$Diesel), digits = 0), big.mark = ","), " MW"
                ),
                hoverinfo = 'text',
                marker = list(color = BarColours[6])
      ) %>% 
      add_trace(x = ~ `Moyle Interconnector`, 
                orientation = 'h',
                name = "Moyle Interconnector",
                type = 'bar',
                legendgroup = "7",
                text = paste0(
                  "Moyle Interconnector: ", format(Data$`Moyle Interconnector`, big.mark = ",")," MW\n",
                  "Year: ", Data$Year
                ),
                hoverinfo = 'text',
                marker = list(color = BarColours[7])
      ) %>% 
      add_trace(x = ~ `Secure import capability of GB Transmission Network into Scotland`, 
                orientation = 'h',
                name = "Secure import capability",
                type = 'bar',
                legendgroup = "8",
                text = paste0(
                  "Secure import: ", format(Data$`Secure import capability of GB Transmission Network into Scotland`, big.mark = ",")," MW\n",
                  "Year: ", Data$Year
                ),
                hoverinfo = 'text',
                marker = list(color = BarColours[8])
      ) %>% 
      add_trace(
        data = Data,
        y = ~ YearFormat,
        x = ~ (Data$`Total maximum supply capacity`)*1.01,
        name = "Total maximum supply capacity",
        legendgroup = 9,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Total maximum supply capacity` >0, paste0("<b>",format(round((Data$`Total maximum supply capacity`), digits = 0), big.mark = ",")," MW</b>")," "),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      )  %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "",
                     ticksuffix = " MW",
                     showgrid = TRUE,
                     x = 0.5,
                     range = c(0, max(Data$`Total maximum supply capacity`) * 1.135)
                     
        ),
        yaxis = list(
          title = "",
          tickformat = "",
          autorange = "reversed",
          ticktext = as.list(Data$`Year`),
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
  
  output$MaxSupplyCapacityTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec capacity and peak demand", col_names = TRUE, 
        skip = 37)[c(1:11)]
    
    names(Data)[1:2] <- c("Year", "Period")
    
    Data <- Data[which(Data$Year >= 2010),]
    
    Data[3:11] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    MaxSupplyCapacityTech <- Data[c(2:11)]
    
    datatable(
      MaxSupplyCapacityTech,
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
        title = "Average annual domestic standard electricity bills in Scotland (2010 \u00A3)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Average annual domestic standard electricity bills in Scotland (2010 \u00A3)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Average annual domestic standard electricity bills in Scotland (2010 \u00A3)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:10, 0) %>% 
      formatStyle(10, fontWeight = "bold")
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/6 - System Security/MaxSupplyPeakDemand.txt")[2])
                     
                   )))
  })
  
  
 observeEvent(input$ToggleTable1, {
    toggle("MaxSupplyPeakDemandTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("MaxSupplyCapacityTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  




  output$MaxSupplyPeakDemand.png <- downloadHandler(
    filename = "MaxSupplyPeakDemand.png",
  content = function(file) {
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec capacity and peak demand", skip = 14)
    
    Data <- subset(Data, nchar(Data$Year) == 7)[2:4]
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    PeakDemandCap <- Data
    
    PeakDemandCap <- arrange(PeakDemandCap, -row_number())
    
    PeakDemandCap$Year <-
      factor(PeakDemandCap$Year,
             levels = unique(PeakDemandCap$Year),
             ordered = TRUE)
    
    PeakDemandCap <- melt(PeakDemandCap, id.vars = "Year")
    
    
    PeakDemandCap$variable <-
      factor(
        PeakDemandCap$variable,
        levels = unique(PeakDemandCap$variable),
        ordered = TRUE
      )
    
    PeakDemandCap <- PeakDemandCap %>%
      group_by(Year) %>%
      mutate(pos = cumsum(value) - value / 2) %>%
      mutate(top = sum(value))
    
    plottitle <-
      "Maximum supply capacity and peak electricity demand"
    sourcecaption <- "Source: BEIS, National Grid"
    
    ChartColours <- c("#5d8be1", "#FF8500")
    BarColours <-
      c(
        "#225ea8",
        "#ef6548"
      )
    
    
    PeakDemandCapChart <- PeakDemandCap %>%
      ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
      scale_fill_manual(
        "variable",
        values = c(
          "Maximum supply capacity" = BarColours[1],
          "Peak electricity demand for the year" = BarColours[2]
        )
      ) +
      geom_bar(position = "dodge",
               stat = "identity",
               width = .8) +
      geom_text(position = position_dodge(width = .8),
                aes(
                  y = value + 20,
                  fill = variable,
                  label = paste(format(round(value, digits = 0), big.mark = ","), "MW"),
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
                  label = ifelse(Year == max(Year), as.character(variable), ""),
                  hjust = 0
                ),
                fontface = 2,
                colour =  "white",
                family = "Century Gothic",
                size = 4) +
      annotate(
        "text",
        x = PeakDemandCap$Year,
        y = -.01,
        label = PeakDemandCap$Year,
        family = "Century Gothic",
        fontface = 2,
        colour =  ChartColours[1],
        size = 3,
        hjust = 1.05
      )
    
    PeakDemandCapChart
    
    
    PeakDemandCapChart <-
      StackedBars(PeakDemandCapChart,
                  PeakDemandCap,
                  plottitle,
                  sourcecaption,
                  ChartColours)
    
    PeakDemandCapChart <-
      PeakDemandCapChart +
      labs(subtitle = paste(max(PeakDemandCap$Year), "-", min(PeakDemandCap$Year))) +
      ylim(-400, 12800)+
      coord_flip()
    
    PeakDemandCapChart
    
    ggsave(
      file,
      plot = PeakDemandCapChart,
      width = 17.5,
      height = 12,
      units = "cm",
      dpi = 300
    )
    
  }
) 
  
  output$MaxSupplyCapacity.png <- downloadHandler(
    filename = "MaxSupplyCapacity.png",
    content = function(file) {

      Data <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Elec capacity and peak demand", col_names = TRUE, 
          skip = 37)[c(1:11)]
      
      names(Data)[1:2] <- c("Year", "Period")
      
      Data <- Data[which(Data$Year >= 2010),]
      
      Data[3:11] %<>% lapply(function(x) as.numeric(as.character(x)))
      
      Data <- as_tibble(Data)
      
      MaxSupply <- Data[2:11]
      
      names(MaxSupply)[1] <- "Year"
      
      MaxSupply <- MaxSupply[c(1, (ncol(MaxSupply) - 1):2)]
      
      MaxSupply <- arrange(MaxSupply,-row_number())
      
      MaxSupply$Year <-
        factor(MaxSupply$Year,
               levels = unique(MaxSupply$Year),
               ordered = TRUE)
      
      MaxSupply <- melt(MaxSupply, id.vars = "Year")
      
      
      MaxSupply$variable <-
        factor(MaxSupply$variable,
               levels = unique(MaxSupply$variable),
               ordered = TRUE)
      
      MaxSupply <- MaxSupply %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Total maximum non-intermittent supply capacity\nby source"
      sourcecaption <- "Source: BEIS, National Grid"
      
      ChartColours <- c("#5d8be1", "#FF8500")
      BarColours <-
        c(
          "#034e7b",
          "#0570b0",
          "#3690c0",
          "#74a9cf",
          "#a6bddb",
          "#d0d1e6",
          "#bdbdbd",
          "#969696"
        )
      
      
      MaxSupplyChart <- MaxSupply %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "CCGT" = BarColours[1],
            "Coal" = BarColours[2],
            "Nuclear" = BarColours[3],
            "Large Hydro" = BarColours[4],
            "Pumped Storage" = BarColours[5],
            "Diesel" = BarColours[6],
            "Moyle Interconnector" = BarColours[7],
            "Secure import capability of GB Transmission Network into Scotland" = BarColours[8],
            "Total maximum supply capacity" = "White"
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = MaxSupply$Year,
          y = -.01,
          label = ifelse(MaxSupply$Year == "z", "", str_wrap(MaxSupply$Year, width = 20)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
          size = 3,
          hjust = 1.05
        ) +
        geom_text(
          aes(
            x = 10.3,
            y = 13000 * (.5 / 8),
            label = "CCGT"
          ),
          fontface = 2,
          colour =  BarColours[1],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 10.3,
            y = 13000 * (1.5 / 8),
            label = "Coal"
          ),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 10.3,
            y = 13000 * (2.5 / 8),
            label = "Nuclear"
          ),
          fontface = 2,
          colour =  BarColours[3],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 10.3,
            y = 13000 * (3.5 / 8),
            label = "Large\nHydro"
          ),
          fontface = 2,
          colour =  BarColours[4],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 10.3,
            y = 13000 * (4.5 / 8),
            label = "Pumped\nStorage"
          ),
          fontface = 2,
          colour =  BarColours[5],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 10.3,
            y = 13000 * (5.5 / 8),
            label = "Diesel"
          ),
          fontface = 2,
          colour =  BarColours[6],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 10.3,
            y = 13000 * (6.5 / 8),
            label = "Moyle\nInterconnector"
          ),
          fontface = 2,
          colour =  BarColours[7],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 10.3,
            y = 13000 * (7.5 / 8),
            label = "Secure\nImport"
          ),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 10.8,
              y = 13000 * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = 13000 * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = MaxSupply$Year ,
            y = MaxSupply$top,
            label = paste(format(
              round(MaxSupply$top, digits = 0), big.mark = ","
            ), "MW")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = -0.1,
          size = 3
        ) +
        geom_text(
          aes(
            x = 9.7 ,
            y = (MaxSupply$top - MaxSupply$value[which(MaxSupply$variable == "Moyle Interconnector")] - MaxSupply$value[which(
              MaxSupply$variable == "Secure import capability of GB Transmission Network into Scotland"
            )]) * .5,
            label = ifelse(MaxSupply$Year == max(MaxSupply$Year),
                           paste("Total Scottish generation:",format(
                             round((
                               MaxSupply$top - MaxSupply$value[which(MaxSupply$variable == "Moyle Interconnector")] - MaxSupply$value[which(
                                 MaxSupply$variable == "Secure import capability of GB Transmission Network into Scotland"
                               )]
                             ) , digits = 0), big.mark = ","
                           ), "MW"),
                           " ")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 0.35 ,
            y = (MaxSupply$top - MaxSupply$value[which(MaxSupply$variable == "Moyle Interconnector")] - MaxSupply$value[which(
              MaxSupply$variable == "Secure import capability of GB Transmission Network into Scotland"
            )]) * .5,
            label = ifelse(MaxSupply$Year == min(MaxSupply$Year),
                           paste("Total Scottish generation:",format(
                             round((
                               MaxSupply$top - MaxSupply$value[which(MaxSupply$variable == "Moyle Interconnector")] - MaxSupply$value[which(
                                 MaxSupply$variable == "Secure import capability of GB Transmission Network into Scotland"
                               )]
                             ) , digits = 0), big.mark = ","
                           ), "MW"),
                           " ")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_segment(
          x = 0.52,
          xend = 0.52,
          y = 0,
          yend = ifelse(
            MaxSupply$Year == min(MaxSupply$Year),
            MaxSupply$top - MaxSupply$value[which(MaxSupply$variable == "Moyle Interconnector")] - MaxSupply$value[which(
              MaxSupply$variable == "Secure import capability of GB Transmission Network into Scotland"
            )],
            0
          ),
          colour =  ChartColours[1]
        ) +
        geom_segment(
          x = 9.47,
          xend = 9.47,
          y = 0,
          yend = ifelse(
            MaxSupply$Year == max(MaxSupply$Year),
            MaxSupply$top - MaxSupply$value[which(MaxSupply$variable == "Moyle Interconnector")] - MaxSupply$value[which(
              MaxSupply$variable == "Secure import capability of GB Transmission Network into Scotland"
            )],
            0
          ),
          colour =  ChartColours[1]
        )
      MaxSupplyChart
      
      
      MaxSupplyChart <-
        StackedBars(MaxSupplyChart,
                    MaxSupply,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      MaxSupplyChart <-
        MaxSupplyChart +
        labs(subtitle = paste(max(MaxSupply$Year), "-", min(MaxSupply$Year))) +
        coord_flip() +
        ylim(-450, 12950)
      
      MaxSupplyChart
      
      ggsave(
        file,
        plot = MaxSupplyChart,
        width = 16,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  
}
    
    