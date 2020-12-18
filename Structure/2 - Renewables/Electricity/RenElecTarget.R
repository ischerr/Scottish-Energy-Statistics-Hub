require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



RenElecTargetOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Target",
    fluidRow(column(8,
                    h3("Share of renewable electricity in gross final consumption", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenElecTargetSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecTarget.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenElecTargetPlot")),
    plotlyOutput(ns("RenElecTargetPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Calculation",
             fluidRow(column(8,
                             h3("Renewable electricity target calculation", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('GrossConsumptionSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GrossConsumption.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("GrossConsumptionPlot")),
             plotlyOutput(ns("GrossConsumptionPlot"), height = "500px")%>% withSpinner(color="#39ab2c"),
             HTML("<blockquote><p>Note: the calculation above is based on 2018 data as it is the most recent year where final data is available</p></blockquote>"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RenElecTargetTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISRenElec", "BEISElecGen"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISRenElec"),
        SourceLookup("BEISElecGen")
        
      )
    )
  )
}




###### Server ######
RenElecTarget <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecTarget.R")

  
  output$RenElecTargetSubtitle <- renderText({
    
    RenElec <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable elec target", col_names = FALSE, 
                          skip = 15)
    RenElec <- tail(RenElec[c(1,4)], -1)
    
    names(RenElec) <- c("Year", "Renewables")
    RenElec %<>% lapply(function(x) as.numeric(as.character(x)))
    RenElec <- as.data.frame(RenElec)
    
    paste("Scotland,", min(RenElec$Year),"-", max(RenElec$Year))
  })
  
  output$RenElecTargetPlot <- renderPlotly  ({
    
    RenElec <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable elec target", col_names = FALSE, 
                          skip = 15)
    RenElec <- tail(RenElec[c(1,4)], -1)
    
    names(RenElec) <- c("Year", "Renewables")
    RenElec <- merge(RenElec, data.frame(Year = 2020, Renewables = NA, Tgt = 1), all = T)
    RenElec %<>% lapply(function(x) as.numeric(as.character(x)))
    RenElec <- as.data.frame(RenElec)
    ### variables
    ChartColours <- c("#39ab2c", "#FF8500")
    sourcecaption = "Source: BEIS"
    plottitle = "Share of renewable electricity in\ngross electricity consumption"
    
    RenElec$Year <- paste0("01/01/", RenElec$Year)
    

    RenElecBar <- read_excel("Structure/CurrentWorking.xlsx", 
                             sheet = "Renewable elec by fuel", col_names = TRUE, 
                             skip = 12)
    
    RenElecBar <- arrange(RenElecBar, -row_number())
    
    RenElecBar <- distinct(RenElecBar, Year, .keep_all = TRUE)
    
    RenElecBar$Year <- paste0("01/01/", RenElecBar$Year)
    
    RenElecBar <- RenElecBar[which(RenElecBar$Year == max(RenElec[which(RenElec$Renewables >0),]$Year)),]
    
    names(RenElecBar)[2:3] <- c("Onshore", "Offshore")
    
    RenElecBar[2:10] %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecBar$Other <- RenElecBar$Total - RenElecBar$Onshore -RenElecBar$Offshore - RenElecBar$Hydro
    
    RenElecBar <- as_tibble(RenElecBar[c(1,2,3,4,11,10)])
    
    RenElecFigure <- RenElec[which(RenElec$Year == max(RenElec[which(RenElec$Renewables > 0 ),]$Year)),]$Renewables
    
    RenElecBar$Onshore <- RenElecBar$Onshore / RenElecBar$Total * RenElecFigure
    
    RenElecBar$Offshore <-  RenElecBar$Offshore / RenElecBar$Total * RenElecFigure
    
    RenElecBar$Hydro <-  RenElecBar$Hydro / RenElecBar$Total * RenElecFigure
    
    RenElecBar$Other <- RenElecBar$Other / RenElecBar$Total * RenElecFigure
    
    RenElecBar$Total <- NULL
    
   RenElec<- merge(RenElec, RenElecBar, all = TRUE)
   
   RenElec$Year <- dmy(RenElec$Year)
   
   RenElec <- arrange(RenElec, Year)
   
   BarColours <- c("#c7e9b4",
                   "#41b6c4",
                   "#225ea8",
                   "#253494")
    
    p <-  plot_ly(RenElec,x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "Renewables",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Progress: ",
                  percent(RenElec$Renewables, accuracy = 0.1),
                  "\nYear: ",
                  format(RenElec$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(RenElec[which(RenElec$Renewables > 0 | RenElec$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewable Electricity",
        text = paste0(
          "Progress: ",
          percent(RenElec[which(RenElec$Renewables > 0 | RenElec$Renewables < 0),][-1,]$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(RenElec[which(RenElec$Renewables > 0 | RenElec$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(
        data = RenElec,
        x = ~ Year,
        y = ~ Tgt,
        name = "Target",
        legendgroup = "2",
        text = paste0(
          "Target: ",
          percent(RenElec$Tgt, accuracy = 0.1),
          "\nYear: ",
          format(RenElec$Year, "%Y")
        ),
        hoverinfo = 'text',
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond",
                      color = ChartColours[2])
      ) %>%
      add_trace(x = ~Year, 
                type = 'bar', 
                y = ~Onshore, 
                width = 2592000000*3, #Milliseconds in a month multiplied by desired month width
                name = "Onshore", 
                legendgroup = "3",
                text = paste0(
                  "Onshore: ",
                  percent(RenElec$Onshore, accuracy = 0.1),
                  "\nYear: ",
                  format(RenElec$Year, "%Y")
                ),
                marker = list(color = BarColours[4]),
                hoverinfo = "text")   %>% 
      
      add_trace(x = ~Year, 
                type = 'bar', 
                y = ~Offshore, 
                width = 2592000000*3, #Milliseconds in a month multiplied by desired month width
                name = "Offshore", 
                legendgroup = "4",
                text = paste0(
                  "Offshore: ",
                  percent(RenElec$Offshore, accuracy = 0.1),
                  "\nYear: ",
                  format(RenElec$Year, "%Y")
                ),
                marker = list(color = BarColours[3]),
                hoverinfo = "text")   %>% 
      
      add_trace(x = ~Year, 
                type = 'bar', 
                y = ~Hydro,  
                width = 2592000000*3, #Milliseconds in a month multiplied by desired month width
                name = "Hydro", 
                legendgroup = "5",
                text = paste0(
                  "Hydro: ",
                  percent(RenElec$Hydro, accuracy = 0.1),
                  "\nYear: ",
                  format(RenElec$Year, "%Y")
                ),
                marker = list(color = BarColours[2]),
                hoverinfo = "text")   %>% 
      
      add_trace(x = ~Year, 
                type = 'bar', 
                y = ~Other,  
                width = 2592000000*3, #Milliseconds in a month multiplied by desired month width
                name = "Other", 
                legendgroup = "6",
                text = paste0(
                  "Other: ",
                  percent(RenElec$Other, accuracy = 0.1),
                  "\nYear: ",
                  format(RenElec$Year, "%Y")
                ),
                marker = list(color = BarColours[1]),
                hoverinfo = "text")   %>% 

      layout(
        barmode = 'stack',
        bargap = 0,
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(RenElec$Year)-100, max(RenElec$Year)+100)),
        yaxis = list(
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
    
    
    
  })
  
  
  output$RenElecTargetTable = renderDataTable({
    
    RenElec <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable elec target", col_names = FALSE, 
                          skip = 15)
    RenElec <- tail(RenElec, -1)
    
    names(RenElec) <- c("Year","Renewable Electricity (GWh)", "Gross Electricity Consumption (GWh)", "% Progress")

    RenElec %<>% lapply(function(x) as.numeric(as.character(x)))
    RenElec <- as_tibble(RenElec)
    
    datatable(
      RenElec[c(1,2,4)],
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
        title = "Renewable Electricity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Electricity',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Electricity')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(3, 1) %>% 
      formatRound(2, 0)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenElecTarget.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("RenElecTargetTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenElecTarget.png <- downloadHandler(
    filename = "RenElecTarget.png",
    content = function(file) {

      RenElec <- read_excel("Structure/CurrentWorking.xlsx", 
                            sheet = "Renewable elec target", col_names = FALSE, 
                            skip = 15)
      RenElec <- tail(RenElec[c(1,4)], -1)
      
      names(RenElec) <- c("Year", "Renewables")
      RenElec <- merge(RenElec, data.frame(Year = 2020, Renewables = NA, Tgt = 1), all = T)
      RenElec %<>% lapply(function(x) as.numeric(as.character(x)))
      RenElec <- as.data.frame(RenElec)
      ### variables
      ChartColours <- c("#39ab2c", "#FF8500")
      sourcecaption = "Source: BEIS"
      plottitle = "Share of renewable electricity in\ngross electricity consumption"
      
      RenElecChart <-
        TargetChartSide(RenElec, plottitle, sourcecaption, ChartColours)
      
      
      
      RenElecChart
      
      
      
      
      RenElecBar <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Renewable elec by fuel", col_names = TRUE, 
                               skip = 12)
      
      RenElecBar <- arrange(RenElecBar, -row_number())
      
      RenElecBar <- distinct(RenElecBar, Year, .keep_all = TRUE)
      
      RenElecBar <- RenElecBar[which(RenElecBar$Year == max(RenElec[which(RenElec$Renewables >0),]$Year)),]
      
      names(RenElecBar)[2:3] <- c("Onshore", "Offshore")
      
      RenElecBar %<>% lapply(function(x)
        as.numeric(as.character(x)))
      
      RenElecBar$Other <- RenElecBar$Total - RenElecBar$Onshore -RenElecBar$Offshore - RenElecBar$Hydro
      
      RenElecBar <- as_tibble(RenElecBar[c(1,2,3,4,11,10)])
      
      RenElecFigure <- RenElec[which(RenElec$Year == max(RenElec[which(RenElec$Renewables > 0 ),]$Year)),]$Renewables
      
      RenElecBar$Onshore <- RenElecBar$Onshore / RenElecBar$Total * RenElecFigure
      
      RenElecBar$Offshore <-  RenElecBar$Offshore / RenElecBar$Total * RenElecFigure
      
      RenElecBar$Hydro <-  RenElecBar$Hydro / RenElecBar$Total * RenElecFigure
      
      RenElecBar$Other <- RenElecBar$Other / RenElecBar$Total * RenElecFigure
      
      RenElecBar$Total <- NULL
      
      dataMax <- RenElecBar
      
      dataMax$Renewables <- RenElecFigure
      
      RenElecBar <- melt(RenElecBar, id = "Year")
      
      BarColours <- c("#c7e9b4",
                      "#41b6c4",
                      "#225ea8",
                      "#253494")
      
      RenElecChart <- RenElecChart +
        geom_bar(
          data = RenElecBar,
          aes(
            x = RenElecBar$Year,
            y = RenElecBar$value,
            fill = forcats::fct_rev(RenElecBar$variable)
          ),
          stat = "identity",
          width = 0.3
        ) +
        scale_fill_manual(values = BarColours) +
        geom_text(
          label = paste0("Other: ", round(
            as.numeric(dataMax$Other[1]) * 100, digits = 1
          ), "%"),
          aes(
            x = mean(RenElecBar$Year) + 1,
            y = dataMax$Onshore+dataMax$Offshore+dataMax$Hydro+(dataMax$Other*.5),
            hjust = 0
          ),
          colour = BarColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          label = paste0("Hydro: ", round(as.numeric(dataMax$Hydro[1]) *
                                            100, digits = 1), "%"),
          aes(
            x = mean(RenElecBar$Year) + 1,
            y = dataMax$Onshore+dataMax$Offshore+(dataMax$Hydro*.5),
            hjust = 0
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          label = paste0(
            "Offshore Wind: ",
            round(as.numeric(dataMax$Offshore[1]) * 100, digits = 1),
            "%"
          ),
          aes(
            x = mean(RenElecBar$Year) + 1,
            y = dataMax$Onshore + (.5*dataMax$Offshore),
            hjust = 0
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          label = paste0(
            "Onshore Wind: ",
            round(as.numeric(dataMax$Onshore[1]) * 100, digits = 1),
            "%"
          ),
          aes(
            x = mean(RenElecBar$Year) + 1,
            y = dataMax$Onshore*.5,
            hjust = 0
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        )+
        geom_point(
          data = dataMax,
          aes(
            x = dataMax$Year,
            y = Renewables,
            colour = "Renewables",
            show_guide = FALSE,
            size = 4
          )
        ) +
        geom_line(
          aes(
            y = Renewables,
            colour = "Renewables",
            label = Percentage
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        xlim(1999,2027)+
        ylim(-0.01, 1)
      
      ggsave(
        file,
        plot = RenElecChart,
        width = 16,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$GrossConsumptionSubtitle <- renderText({
    
      paste("Scotland, 2019")
  })
  
  output$GrossConsumptionPlot <- renderPlotly  ({
    
    GrossConsumption <- read_excel("Processed Data/TestConsumption.xlsx")
    
    GrossConsumptionPlotData <- GrossConsumption[c(1,3),]
    
    GrossConsumptionPlotData$Type <- as.numeric(rownames(GrossConsumptionPlotData))
    
    ChartColours <- c("#39ab2c", "#FF8500", "#74c476")
    BarColours <-
      c(
        "#045a8d",
        "#238b45",
        "#d7301f",
        "#f46d43",
        "#fdae61",
        "#fee08b",
        "#abdda4",
        "#016c59",
        "#3288bd",
        "#5e4fa2"
        
      )
    
    
    p <- plot_ly(data = GrossConsumptionPlotData, y = ~ Type) %>%
      add_trace(
        data = GrossConsumptionPlotData,
        x = ~ Consumption,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Consumption",
        text = paste0(" ",format(round(GrossConsumptionPlotData$Consumption, digits = 0), big.mark = ","), " GWh\n"), #The \n at the end forces it to adopt the correct font and formatting. I don't know why, possibly something to do with HTML
        hoverinfo = 'none',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = GrossConsumptionPlotData,
        x = ~ `Renewable`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Renewable",
        text = paste0("Renewable\n", format(round(GrossConsumptionPlotData$`Renewable`, digits = 0), big.mark = ","), " GWh"),
        hoverinfo = 'none',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      )  %>%
      add_trace(
        data = GrossConsumptionPlotData,
        x = ~ `Non-renewable`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Non-renewable",
        text = paste0("Non-renewable\n", format(round(GrossConsumptionPlotData$`Non-renewable`, digits = 0), big.mark=","), " GWh"),
        hoverinfo = 'none',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      )  %>%
      add_annotations(
        ax = max(GrossConsumption$Consumption)- min(GrossConsumption$`Exports`),
        x = max(GrossConsumption$Consumption),
        ay = 1.5,
        y = 1.5,
        xref = "x", yref = "y",
        axref = "x", ayref = "y",
        showlegend = FALSE ,
        arrowhead = 4,
        arrowsize = 1,
        arrowcolor = BarColours[8],
        hoverinfo = 'none',
        legendgroup = 11,
        text = "",
        name = "Exports",
        line = list(
          arrowhead = 1,
          width = 3,
          color = BarColours[8],
          dash = "none"
        )
      ) %>%
      add_trace(
        mode = 'text',
        x =max(GrossConsumption$Consumption) - (min(GrossConsumption$`Exports`)/2),
        y = 1.5,
        xref = "x", yref = "y",
        showlegend = FALSE ,
        hoverinfo = 'none',
        legendgroup = 10,
        text = paste0("Net Exports\n\n", format(round(min(GrossConsumption$`Exports`),0), big.mark = ",")," GWh"),
        name = paste("Exports"),
        marker = list(
          size = 100,
          opacity = 0
        ),
        showarrow = F,
        textfont = list(
          size = 20,
          color = BarColours[8]
        )
      ) %>%
      add_trace(
        mode = 'text',
        x = (max(GrossConsumption$Renewable)/2),
        y = 1.5,
        xref = "x", yref = "y",
        showlegend = FALSE ,
        hoverinfo = 'none',
        legendgroup = 10,
        text = paste0("\u00F7"),
        name = paste("Exports"),
        marker = list(
          size = 500,
          opacity = 0
        ),
        showarrow = F,
        textfont = list(
          size = 35,
          color = BarColours[8]
        )
      ) %>%
      add_annotations(
        mode = 'text',
        x = (max(GrossConsumption$Renewable)/2),
        y = 0.45,
        xref = "x", yref = "y",
        showlegend = FALSE ,
        hoverinfo = 'none',
        legendgroup = 10,
        text = paste0("<b>",
                      percent(max(GrossConsumption$Renewable)/(max(GrossConsumptionPlotData$Consumption)), .1),
                      "</b>\nequivalent of Scotland's own electricity\ndemand from renewable sources"
        ),
        name = paste("Exports"),
        marker = list(
          size = 500,
          opacity = 0
        ),
        showarrow = F,
        bordercolor="#c7c7c7",
        borderwidth=2,
        borderpad=4,
        bgcolor=ChartColours[3],
        font = list(
          color = "white"
        )
      ) %>%
      add_annotations(
        mode = 'text',
        x = (max(GrossConsumption$Renewable)/2),
        y = 2.45,
        xref = "x", yref = "y",
        showlegend = FALSE ,
        hoverinfo = 'none',
        legendgroup = 10,
        text = paste0("<b>",
                      percent(max(GrossConsumption$Renewable)/(max(GrossConsumptionPlotData$Renewable)+max(GrossConsumptionPlotData$`Non-renewable`)), .1),
                      "</b> of electricity generated\ncomes from renewable sources"
        ),
        name = paste("Exports"),
        marker = list(
          size = 500,
          opacity = 0
        ),
        showarrow = F,
        bordercolor="#c7c7c7",
        borderwidth=2,
        borderpad=4,
        bgcolor=ChartColours[3],
        font = list(
          color = "white"
        )
      ) %>%
      add_annotations(
        mode = 'text',
        x = (max(GrossConsumption$Consumption)*1.55),
        y = 1,
        xref = "x", yref = "y",
        showlegend = FALSE ,
        hoverinfo = 'none',
        legendgroup = 10,
        text = paste0("The denominator is\ngross consumption:\ngeneration minus net exports"
        ),
        name = paste("Consumption"),
        marker = list(
          size = 500,
          opacity = 0
        ),
        showarrow = F,
        bordercolor="#c7c7c7",
        borderwidth=2,
        borderpad=4,
        bgcolor=ChartColours[3],
        font = list(
          color = "white"
        )
      ) %>%
      add_annotations(
        mode = 'text',
        x = .4,
        y = -.1,
        xref = "paper", yref = "y",
        showlegend = FALSE ,
        hoverinfo = 'none',
        legendgroup = 10,
        text = "This does not mean that 23.3% of Scottish electricity demand is\nfrom non-renewable sources. Due to the way it is calculated,\nshare of renewable electricity in gross consumption can exceed 100%.",
        name = paste("Consumption"),
        marker = list(
          size = 500,
          opacity = 0
        ),
        showarrow = F,
        font = list(
          color = BarColours[8]
        )
      ) %>%
      layout(
        barmode = 'stack',
        showlegend = FALSE,
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(
          title = "",
          showgrid = FALSE,
          ticktext = list("<b>Gross\nConsumption</b>", "<b>Electricity Generation\nfuel mix</b>"),
          tickvals = list(1, 2),
          tickmode = "array",
          range = c(-.1,2.65)
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          range = c(0,70000),
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F) %>% 
      onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")
    
    
    p
    
    
  })
  
  
  output$GrossConsumptionTable = renderDataTable({
    
    GrossConsumption <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(GrossConsumption) <- c("Year", "Agriculture", "Business", "Energy Supply", "Industrial Processes", "International Aviation and Shipping", "Forestry (Carbon Sink)", "Public",  "Residential", "Domestic Transport", "Waste Management" )
    
    GrossConsumption <- GrossConsumption[c(1,10,3,2,4,9,6,11,8,5,7)]
    
    GrossConsumption$`Total Emissions` <- rowSums(GrossConsumption[2:11])
    
    datatable(
      GrossConsumption,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        scrollX = TRUE,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "Sources of Scottish Greenhouse Gas Emissions (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Sources of Scottish Greenhouse Gas Emissions (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Sources of Scottish Greenhouse Gas Emissions (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:12, 1) %>% 
      formatStyle(11, fontStyle = "italic")
  })
  
  output$GrossConsumption.png <- downloadHandler(
    filename = "GrossConsumption.png",
    content = function(file) {
      
      GrossConsumption <- read_excel("Processed Data/TestConsumption.xlsx")
      
      GrossConsumption$Type <- c("Gross Consumption", "Net exports", "Electricity generation fuel mix")
      
      GrossConsumptionPlotData <- GrossConsumption[c(1,3),]
      
      GrossConsumptionPlotData$Type <- as.numeric(rownames(GrossConsumptionPlotData))
      
      GrossConsumptionYear <- 2019
      
      ChartColours <- c("#39ab2c", "#FF8500", "#74c476")
      BarColours <-
        c(
          "#045a8d",
          "#238b45",
          "#d7301f",
          "#f46d43",
          "#fdae61",
          "#fee08b",
          "#abdda4",
          "#016c59",
          "#3288bd",
          "#5e4fa2"
          
        )
      
      
      GrossConsumption$Type <-
        factor(GrossConsumption$Type,
               levels = unique(GrossConsumption$Type),
               ordered = TRUE)
      
      GrossConsumption <- melt(GrossConsumption, id.vars = "Type")
      
      GHGCarbonSink <- min(GrossConsumption[which(GrossConsumption$variable == "Exports"),]$value)
      
      
      GrossConsumption <- subset(GrossConsumption, GrossConsumption$Type != "Net exports" )
      GrossConsumption$variable <-
        factor(
          GrossConsumption$variable,
          levels = unique(rev(GrossConsumption$variable)),
          ordered = TRUE
        )
      
      GrossConsumption <- GrossConsumption %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Renewable electricity target calculation"
      sourcecaption <- "Source: BEIS"
      
      length <-max(GrossConsumption$top)
      
      GrossConsumptionChart <- GrossConsumption %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Renewable" = BarColours[2],
            "Non-renewable" = BarColours[3],
            "Exports" = ChartColours[1],
            "Consumption" = BarColours[1]
          )
        ) +
        geom_bar(stat = "identity",
                 width = .4) +
        geom_text(
          aes(
            x = Type,
            y = ifelse(top > 0, length*-.08,7),
            label = ifelse(GrossConsumption$variable != "Consumption", "", str_wrap(GrossConsumption$Type, width = 13))
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        ) + 
        geom_text(
          aes(
            x = Type,
            y = pos,
            label = ifelse(value > 1.5 & Type == "Electricity generation fuel mix", paste0(variable, "\n",format(round(value, digits = 0), big.mark = ","), " GWh"), ""
            )),
          family = "Century Gothic",
          fontface = 2,
          colour =  "white"
        ) + 
        geom_text(
          aes( x = Type,
               y = top + ifelse(top > 0, length*.02,-1),
               label = ifelse(GrossConsumption$variable != "Consumption", "", paste(format(round(top, digits = 0), big.mark = ","), "GWh")),
               hjust = ifelse(top > 0, 0, 1)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        ) +
        geom_segment(
          x = 1.44,
          xend = 1.44,
          y = GrossConsumption[which(GrossConsumption$Type == "Electricity generation fuel mix" & GrossConsumption$variable == "Consumption"),]$top,
          yend = GrossConsumption[which(GrossConsumption$Type == "Electricity generation fuel mix" & GrossConsumption$variable == "Consumption"),]$top + GHGCarbonSink,
          colour =   BarColours[8],
          arrow = arrow(length = unit(0.4, "cm")),
          size = 1
        ) + 
        geom_text(
          aes( x = 1.6,
               y = GrossConsumption[which(GrossConsumption$Type == "Electricity generation fuel mix" & GrossConsumption$variable == "Consumption"),]$top + (GHGCarbonSink/2),
               label = paste("Net Exports: \n", format(round(GHGCarbonSink, digits = 0), big.mark = ","), "GWh")
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =   BarColours[8]
        ) +
        geom_label(
          aes( x = 2.4,
               y = GrossConsumption[which(GrossConsumption$Type == "Electricity generation fuel mix" & GrossConsumption$variable == "Renewable"),]$value / 2,
               label = paste(
                 percent(GrossConsumption[which(GrossConsumption$Type == "Electricity generation fuel mix" & GrossConsumption$variable == "Renewable"),]$value / GrossConsumption[which(GrossConsumption$Type == "Electricity generation fuel mix" & GrossConsumption$variable == "Renewable"),]$top,.1),
                 "of Scotland's electricity generation\nfuel mix coming from renewable sources"),
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  "white",
          fill = ChartColours[3]
        )  +
        geom_text(
          aes( x = 1.5,
               y = GrossConsumption[which(GrossConsumption$Type == "Electricity generation fuel mix" & GrossConsumption$variable == "Renewable"),]$value / 2,
               label = "\u00F7",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[2],
          size = 10
        )  +
        geom_label(
          aes( x = 0.5,
               y = GrossConsumption[which(GrossConsumption$Type == "Electricity generation fuel mix" & GrossConsumption$variable == "Renewable"),]$value / 2,
               label = paste0(percent(GrossConsumption[which(GrossConsumption$Type == "Electricity generation fuel mix" & GrossConsumption$variable == "Renewable"),]$value / GrossConsumption[which(GrossConsumption$Type == "Gross Consumption" & GrossConsumption$variable == "Consumption"),]$value, .1), "\nequivalent of Scotland's own electricity\ndemand from renewable sources") ,
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  "white",
          fill = ChartColours[3]
        ) +
        geom_label(
          aes( x = 1,
               y = max(GrossConsumption[which(GrossConsumption$Type == "Gross Consumption"),]$value) *1.46,
               label = "The denominator is\ngross consumption:\ngeneration minus net exports",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  "white",
          fill = ChartColours[3]
        ) +
        geom_text(
          aes( x = 0,
               y = 48629/1.9,
               label = "This does not mean that 23.3% of Scottish electricity demand is\nfrom non-renewable sources. Due to the way it is calculated,\nshare of renewable electricity in gross consumption can exceed 100%.",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[8]
        ) +
        geom_text(
          aes( x = -0.3,
               y = 48629/2,
               label = " ",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[8]
        ) 
      
      GrossConsumptionChart
      
      
      GrossConsumptionChart <-
        StackedBars(GrossConsumptionChart,
                    GrossConsumption,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      GrossConsumptionChart <-
        GrossConsumptionChart +
        labs(subtitle = GrossConsumptionYear)+
        coord_flip()+ 
        ylim(length*-.12,length*1.15)
      
      GrossConsumptionChart
      
      
      
      ggsave(
        file,
        plot = GrossConsumptionChart,
        width = 27,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
}
