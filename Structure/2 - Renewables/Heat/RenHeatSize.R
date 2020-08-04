require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenHeatSizeOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Renewable heat capacity by size", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenHeatSizeSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenHeatSize.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenHeatSizePlot")),
    plotlyOutput(ns("RenHeatSizePlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10, h3("Data - Capacity (GW)", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RenHeatSizeTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("ESTRenHEat"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("ESTRenHeat")
        
      )
    )
  )
}




###### Server ######
RenHeatSize <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenHeatSize.R")

  
  output$RenHeatSizeSubtitle <- renderText({
    
    Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatSize.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)[1:3]
    
    paste("Scotland,", min(Data$Year),"-", max(Data$Year))
  })
  
  output$RenHeatSizePlot <- renderPlotly  ({
    
    Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatSize.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)[1:3]
    
    Data <- dcast(Data, Year ~ Technology)
    
    Data[2] <- NULL
    
    names(Data) <- c("Year", "Large", "Micro", "Small to medium", "Total", "Unknown")
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[c(1,2,4,3,6,5)]
    
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    
    Data <- Data[complete.cases(Data),]
    
    ChartColours <- c("39ab2c", "#FF8500")
    
    BarColours <-
      c("#006837",
        "#1a9850",
        "#66bd63",
        "#fee08b",
        "#fdae61",
        "#f46d43",
        "#d73027")
    
    Data$Year <- paste0("<b>",Data$Year,"</b>")
    
    p <- plot_ly(data = Data, y = ~ `Year`) %>%
      
      add_trace(
        data = Data,
        x = ~ `Large`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Large",
        text = paste0("Large: ", round(Data$`Large`, 3), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Small to medium`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Small to medium",
        text = paste0("Small to medium: ", round(Data$`Small to medium`, 3), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Micro`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Micro",
        text = paste0("Micro: ", round(Data$`Micro`, 3), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Unknown`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Unknown",
        text = paste0("Unknown: ", round(Data$`Unknown`, 3), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        x = (Data$Total) * 1.01,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste("<b>",format(round((Data$Total), digits = 3), big.mark = ","),"GW</b>"),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      ) %>%
      add_trace(
        x = (Data$Total) * 1.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste(" "),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
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
                     ticktext = as.list(Data$`Year`),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          ticksuffix = " GW",
          tickformat = "",
          title = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
  })
  
  output$RenHeatOutputSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 6)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- arrange(Data, -row_number())
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    RenHeatOutputTech <- Data
    
    paste("Scotland,", min(RenHeatOutputTech$Year),"-", max(RenHeatOutputTech$Year))
  })
  
  output$RenHeatOutputPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 6)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- arrange(Data, -row_number())
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    RenHeatOutputTech <- Data
    
    plottitle <- "Renewable heat output by technology type"
    sourcecaption <- "Source: EST"
    ChartColours <- c("#39ab2c", "#FF8500", "#FFFFFF")
    LineColours <- c("#fc4e2a","#feb24c","#fed976","#addd8e","#41ab5d")
    
    RenHeatOutputTech$Year <- paste0("01/01/",  RenHeatOutputTech$Year)
    
    RenHeatOutputTech$Year <- dmy(RenHeatOutputTech$Year)
    
    p <-  plot_ly(RenHeatOutputTech, x = ~ Year ) %>% 
      add_trace(y = ~ Biomass,
                name = "Biomass",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Biomass: ",
                  format(round(RenHeatOutputTech$Biomass, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(y = ~ CHP,
                name = "Biomass CHP",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Biomass CHP: ",
                  format(round(RenHeatOutputTech$CHP, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[2], dash = "none")
      ) %>% 
      add_trace(y = ~ Waste,
                name = "Energy from waste",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Energy from waste: ",
                  format(round(RenHeatOutputTech$Waste, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[3], dash = "none")
      ) %>% 
      add_trace(y = ~ Pumps,
                name = "Heat pumps",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "Heat pumps: ",
                  format(round(RenHeatOutputTech$Pumps, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[4], dash = "none")
      ) %>% 
      add_trace(y = ~ Solar,
                name = "Solar thermal",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "5",
                text = paste0(
                  "Solar thermal: ",
                  format(round(RenHeatOutputTech$Solar, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(RenHeatOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[5], dash = "none")
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`Biomass` != 0),], 1),
        x = ~ Year,
        y = ~ `Biomass`,
        name = "Biomass",
        legendgroup = "1",
        text = paste0(
          "Biomass: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Biomass` != 0),], 1)$Biomass, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Biomass` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`CHP` != 0),], 1),
        x = ~ Year,
        y = ~ `CHP`,
        name = "Biomass CHP",
        legendgroup = "2",
        text = paste0(
          "Biomass CHP: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`CHP` != 0),], 1)$CHP, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`CHP` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[2])
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`Waste` != 0),], 1),
        x = ~ Year,
        y = ~ `Waste`,
        name = "Energy from waste",
        legendgroup = "3",
        text = paste0(
          "Energy from waste: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Waste` != 0),], 1)$Waste, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Waste` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[3])
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`Pumps` != 0),], 1),
        x = ~ Year,
        y = ~ `Pumps`,
        name = "Heat pumps",
        legendgroup = "4",
        text = paste0(
          "Heat pumps: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Pumps` != 0),], 1)$Pumps, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Pumps` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[4])
      ) %>% 
      add_trace(
        data = tail(RenHeatOutputTech[which(RenHeatOutputTech$`Solar` != 0),], 1),
        x = ~ Year,
        y = ~ `Solar`,
        name = "Solar thermal",
        legendgroup = "5",
        text = paste0(
          "Solar thermal: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Solar` != 0),], 1)$Solar, big.mark = ","),
          " GWh\nYear: ",
          format(tail(RenHeatOutputTech[which(RenHeatOutputTech$`Solar` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[5])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(RenHeatOutputTech$Year)-100, max(RenHeatOutputTech$Year)+100)),
        yaxis = list(
          title = "GWh",
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
  
  
  output$RenHeatSizeTable = renderDataTable({
    
    RenHeatSize <- read_delim("Processed Data/Output/Renewable Heat/RenHeatSize.txt", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
    
    RenHeatSize <- melt(RenHeatSize, id.vars = c("Year", "Technology"))
    
    RenHeatSize$variable <- paste0(RenHeatSize$variable, " - ", RenHeatSize$Year)
    
    RenHeatSize$Year <- NULL
    
    RenHeatSize <- dcast(RenHeatSize, Technology ~ variable)
    
    RenHeatSize[RenHeatSize <= 0.01] <- "< 0.1"
    
    RenHeatSize <- RenHeatSize[c(2,4,3,1,6,5),]
    
    datatable(
      RenHeatSize[c(1,13,12,9,8,11,10)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Renewable heat output and capacity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Renewable heat output and capacity",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Renewable heat output and capacity")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      
      formatRound(c(4:7), 0)
  })
  
  output$RenHeatOutputTable = renderDataTable({
    
    RenHeatEnergyFromWaste <- read_delim("Processed Data/Output/Renewable Heat/RenHeatEnergyFromWaste.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)
    
    datatable(
      RenHeatEnergyFromWaste[c(2,3,5)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Renewable heat output by technology type (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Renewable heat output by technology type (GWh)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Renewable heat output by technology type (GWh)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2), 3) %>%
      formatRound(c(3), 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Heat/RenHeatSize.txt")[2])
                     
                   )))
  })
  
 observeEvent(input$ToggleTable1, {
    toggle("RenHeatSizeTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("RenHeatOutputTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenHeatSize.png <- downloadHandler(
    filename = "RenHeatSize.png",
    content = function(file) {

      Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatSize.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)[1:3]
      
      Data <- dcast(Data, Year ~ Technology)
      
      Data[2] <- NULL
      
      names(Data) <- c("Year", "Large", "Micro", "Small to medium", "Total", "Unknown")
      
      Data$Total <- NULL
      
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
      Data <- Data[complete.cases(Data),]
      
      DomesticEPC <- as_tibble(Data)
      
      DomesticEPC <-
        DomesticEPC[c(1, ncol(DomesticEPC):2)]
      
      DomesticEPC <-
        arrange(DomesticEPC,-row_number())
      
      DomesticEPC$Year <-
        factor(DomesticEPC$Year,
               levels = unique(DomesticEPC$Year))
      
      DomesticEPC <-
        melt(DomesticEPC, id.vars = "Year")
      
      
      DomesticEPC$variable <-
        factor(DomesticEPC$variable,
               levels = unique(DomesticEPC$variable))
      
      DomesticEPC <- DomesticEPC %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      length <- max(DomesticEPC$top)
      height <- max(as.numeric(as.character(DomesticEPC$Year))) - min(as.numeric(as.character(DomesticEPC$Year))) + 1
      
      plottitle <-
        "Renewable heat capacity by size"
      sourcecaption <- "Source: EST, SG"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c("#006837",
          "#1a9850",
          "#66bd63",
          "#fee08b",
          "#fdae61",
          "#f46d43",
          "#d73027")
      
      
      DomesticEPCChart <- DomesticEPC %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Large" = BarColours[1],
            "Small to medium"     = BarColours[2],
            "Micro"   = BarColours[3],
            "Unknown"   = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = DomesticEPC$Year,
          y = -.15,
          label = ifelse(
            DomesticEPC$Year == "z",
            "",
            str_wrap(DomesticEPC$Year, width = 8)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1]
        ) +
        geom_text(
          aes(x = height * 1.25,
              y = length * (0.5 / 4.2),
              label = "Large"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height * 1.25,
              y = length * (1.5 / 4.2),
              label = "Small to medium"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height * 1.25,
              y = length * (2.5 / 4.2),
              label = "Micro"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height * 1.25,
              y = length * (3.5 /4.2),
              label = "Unknown"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        annotate(
          "text",
          x = DomesticEPC$Year,
          y = DomesticEPC$top +.05,
          label = paste0(
            round(DomesticEPC$top, 3),
            " GW"
          ),
          family = "Century Gothic",
          hjust = 0,
          fontface = 2,
          colour = ChartColours[1]
        ) +
        geom_text(
          aes(x = height * 1.25,
              y = length * (4.5 / 4.2),
              label = "Total"),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5
        )+
        geom_text(
          aes(x = height *1.30,
              y = 1.05,
              label = " "),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5
        )
      
      DomesticEPCChart
      
      
      DomesticEPCChart <-
        StackedBars(
          DomesticEPCChart,
          DomesticEPC,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      DomesticEPCChart <-
        DomesticEPCChart +
        coord_flip() +
        labs(subtitle = paste("Scotland",
                              min(as.numeric(as.character(DomesticEPC$Year))),
                              "-",
                              max(as.numeric(as.character(DomesticEPC$Year)))
        )) +
        ylim(-.2, max(DomesticEPC$top)*1.1)
      
      DomesticEPCChart
      
      
      ggsave(
        file,
        plot = DomesticEPCChart,
        width = 19,
        height = 15.5,
        units = "cm",
        dpi = 300
      )
    }
  )



output$RenHeatOutput.png <- downloadHandler(
  filename = "RenHeatOutput.png",
  content = function(file) {
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 6)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- arrange(Data, -row_number())
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    RenHeatOutputTech <- Data
    
    plottitle <- "Renewable heat output by technology type"
    sourcecaption <- "Source: EST"
    ChartColours <- c("#39ab2c", "#FF8500", "#FFFFFF")
    LineColours <- c("#fc4e2a","#feb24c","#fed976","#addd8e","#41ab5d")
    
    RenHeatOutputTechChart <-
      RenHeatOutputTech %>%  ggplot(aes(x = Year), family = "Century Gothic") +
      
      ### Line of Values
      geom_line(
        aes(y = Biomass,
            label = Biomass),
        colour = LineColours[1],
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Biomass,
          label = ifelse(Year == min(Year), paste(format(round(Biomass, digits = 0), big.mark = ","), "GWh"), ""),
          hjust = 1,
          vjust= 0,
          fontface = 2
        ),
        colour = LineColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+1.2,
          y = Biomass,
          label = ifelse(Year == max(Year), paste0("Biomass\n",format(round(Biomass, digits = 0), big.mark = ","), " GWh"), ""),
          fontface = 2
        ),
        colour = LineColours[1],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = Biomass,
          label = Biomass,
          show_guide = FALSE
        ), 
        colour = LineColours[1],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = CHP,
            
            label = CHP),
        size = 1.5,
        colour = LineColours[2],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = CHP,
          label = ifelse(Year == min(Year), paste(round(CHP, digits = 0),"GWh"), ""),
          hjust = 1,
          vjust = 1,
          fontface = 2
        ),
        colour = LineColours[2],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+1.2,
          y = CHP,
          label = ifelse(Year == max(Year), paste0("Biomass CHP\n",round(CHP, digits = 0), " GWh"), ""),
          fontface = 2,
        ),
        colour = LineColours[2],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = CHP,
          label = CHP,
          show_guide = FALSE
        ),
        colour = LineColours[2],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Waste,
            label = Waste),
        size = 1.5,
        colour = LineColours[3],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Waste,
          label = ifelse(Year == min(Year), paste(round(Waste, digits = 0),"GWh"), ""),
          hjust = 1,
          vjust = -.9,
          fontface = 2
        ),
        colour = LineColours[3],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+1.2,
          y = Waste,
          label = ifelse(Year == max(Year), paste0("Energy from waste\n",round(Waste, digits = 0), " GWh"), ""),
          fontface = 2,
          vjust =-.3
        ),
        colour = LineColours[3],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = Waste,
          label = Waste,
          show_guide = FALSE
        ),
        colour = LineColours[3],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Pumps,
            label = Pumps),
        size = 1.5,
        colour = LineColours[4],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Pumps,
          label = ifelse(Year == min(Year), paste(round(Pumps, digits = 0), "GWh"), ""),
          hjust = 1,
          vjust= 0,
          fontface = 2
        ),
        colour = LineColours[4],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+1.2,
          y = Pumps,
          label = ifelse(Year == max(Year), paste0("Heat pumps\n", round(Pumps, digits = 0)," GWh"), ""),
          vjust = .5,
          fontface = 2
        ),
        colour = LineColours[4],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = Pumps,
          label = Pumps,
          show_guide = FALSE
        ),
        colour = LineColours[4],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Solar,
            label = Solar),
        size = 1.5,
        colour = LineColours[5],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Solar,
          label = ifelse(Year == min(Year), paste(round(Solar, digits = 0),"GWh"), ""),
          hjust = 1,
          vjust = 0,
          fontface = 2
        ),
        colour = LineColours[5],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+1.2,
          y = Solar,
          label = ifelse(Year == max(Year), paste0("Solar thermal\n",format(round(Solar, digits = 0), big.mark = ",")," GWh"), ""),
          fontface = 2,
          vjust = 0
        ),
        colour = LineColours[5],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RenHeatOutputTech, 1),
        aes(
          x = Year,
          y = Solar,
          label = Solar,
          show_guide = FALSE
        ),
        colour = LineColours[5],
        size = 4,
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
    
    
    RenHeatOutputTechChart
    
    RenHeatOutputTechChart <-
      LinePercentChart(RenHeatOutputTechChart,
                       RenHeatOutputTech,
                       plottitle,
                       sourcecaption,
                       ChartColours)
    
    
    RenHeatOutputTechChart

    
    RenHeatOutputTechChart <- RenHeatOutputTechChart +
      ylim(-.008, max(RenHeatOutputTech$Biomass))+
      xlim(min(RenHeatOutputTech$Year-1.2), max(RenHeatOutputTech$Year+2.1))
    ggsave(
      file,
      plot = RenHeatOutputTechChart,
      width = 16,
      height = 16,
      units = "cm",
      dpi = 300
    )
    
  }
)
}
    
    