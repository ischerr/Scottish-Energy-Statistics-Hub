require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



RenHeatTechOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
    tabPanel("Tech",
    fluidRow(column(8,
                    h3("Renewable heat by technology type", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenHeatTechSubtitle')), style = "color: #39ab2c;"),
                    selectInput(ns("MeasureSelect"), "Measure:", c("Capacity", "Generation", "Number of Installations"), selected = "Generation", multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL)
                      ),
             column(
               4, align = 'right', style = 'padding:15px;',
               downloadButton(ns('RenHeatTech.png'), 'Download Graph', style="float:right"),
             )),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenHeatTechPlot")),
    plotlyOutput(ns("RenHeatTechPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Size",
                fluidRow(column(8,
                                h3("Renewable heat by installation size", style = "color: #39ab2c;  font-weight:bold"),
                                h4(textOutput(ns('RenHeatSizeSubtitle')), style = "color: #39ab2c;"), 
                                selectInput(ns("MeasureSelect2"), "Measure:", c("Capacity", "Generation", "Number of Installations"), selected = "Generation", multiple = FALSE,
                                            selectize = TRUE, width = NULL, size = NULL),
                ),
                column(
                  4, style = 'padding:15px;',
                  downloadButton(ns('RenHeatSize.png'), 'Download Graph', style="float:right")
                )),
                
                tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
                #dygraphOutput(ns("RenHeatTechPlot")),
                plotlyOutput(ns("RenHeatSizePlot"))%>% withSpinner(color="#39ab2c"),
                tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ,
    tabPanel("Local Authorities",
             fluidRow(column(8,
                             h3("Renewable Heat by Local Authority", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenHeatLASubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenHeatLA.png'), 'Download Graph', style="float:right")
             )),
             fluidRow(column(6,selectInput(ns("CategorySelect"), "Category:", c("Output (GWh)", "Operational Capacity (GW)"), selected = "Output (GWh)", multiple = FALSE,
                                           selectize = TRUE, width = NULL, size = NULL))),
             leafletOutput(ns("RenHeatLA"), height = "675px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Tech",
    fluidRow(
    column(10, h3("Data - Renewable heat by technology type", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RenHeatTechTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Size",
             fluidRow(
               column(10, h3("Data - Renewable heat by installation size", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenHeatSizeTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
        tabPanel("Local Authority",
             fluidRow(
               column(10, h3("Data - Renewable Heat Output and Capacity by Local Authority", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenHeatLATable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("ESTRenHeat"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("ESTRenHeat"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("ESTRenHeat")
        
      )
    )
  )
}




###### Server ######
RenHeatTech <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenHeatTech.R")

  observe({
    
    RenHeatDropdown$Measure <- input$MeasureSelect
  })
  
   observe({
     RenHeatDropdown$Measure <- input$MeasureSelect2
   })
  
  observe(
    {
      updateSelectInput(session, 'MeasureSelect', selected = RenHeatDropdown$Measure)
      updateSelectInput(session, 'MeasureSelect2', selected = RenHeatDropdown$Measure)
    }
  )


  
  output$RenHeatTechSubtitle <- renderText({
    
    Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatCapOutput.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)[1:3]
    
    paste("Scotland,", min(Data$Year),"-", max(Data$Year))
  })
  
  output$RenHeatTechPlot <- renderPlotly  ({
    

    if (RenHeatDropdown$Measure == "Capacity"){
      x <- 3
      unit <- " GW"
      }
    
    if (RenHeatDropdown$Measure == "Generation"){
      x <- 5
      unit <- " GWh"
      }
    
    if (RenHeatDropdown$Measure == "Number of Installations"){
      x <- 7
      unit <- ""
      }
    
    Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatCapOutput.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    Data <- Data[c(1,2,x)]
    
    Data <- dcast(Data, Year ~ Technology)
    
    names(Data) <- c("Year", "Biomass", "CHP", "Biomethane", "Waste", "Pumps", "Solar", "Total")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
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
        x = ~ `Biomass`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Biomass",
        text = paste0("Biomass: ", format(round(Data$`Biomass`, 3), big.mark = ","), unit),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `CHP`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Biomass CHP",
        text = paste0("Biomass CHP: ", format(round(Data$`CHP`, 3), big.mark = ","), unit),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Biomethane`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Biomethane",
        text = paste0("Biomethane: ", format(round(Data$`Biomethane`, 3), big.mark = ","), unit),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Waste`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Energy from waste",
        text = paste0("Energy from waste: ", format(round(Data$`Waste`, 3), big.mark = ","), unit),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Pumps`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Heat pumps",
        text = paste0("Heat pumps: ", format(round(Data$`Pumps`, 3), big.mark = ","), unit),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Solar`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Solar thermal",
        text = paste0("Solar thermal: ", format(round(Data$`Solar`, 3), big.mark = ","), unit),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        x = (Data$Total) * 1.01,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste0("<b>",format(round((Data$Total), digits = 3), big.mark = ","), unit, "</b>"),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      ) %>%
      add_trace(
        x = (Data$Total) * 1.2,
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
          ticksuffix = unit,
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
  
  output$RenHeatTechTable = renderDataTable({
    
    RenHeatCapOutput <- read_delim("Processed Data/Output/Renewable Heat/RenHeatCapOutput.txt", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
    
    RenHeatCapOutput <- melt(RenHeatCapOutput, id.vars = c("Year", "Technology"))
    
    RenHeatCapOutput$variable <- paste0(RenHeatCapOutput$variable, " - ", RenHeatCapOutput$Year)
    

    RenHeatCapOutput$Year <- NULL
    
    RenHeatCapOutput <- dcast(RenHeatCapOutput, Technology ~ variable)
    
    datatable(
      RenHeatCapOutput[c(1,13,12,09,08,11,10)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Renewable heat by technology type",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Renewable heat by technology type",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Renewable heat by technology type")
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
  
  output$RenHeatTech.png <- downloadHandler(
    filename = "RenHeatTech.png",
    content = function(file) {

      if (RenHeatDropdown$Measure == "Capacity"){
        x <- 3
        
        unit <- " GW"
        
        plottitle <-
          "Renewable heat capacity by technology type"
      }

      if (RenHeatDropdown$Measure == "Generation"){
        x <- 5
        
        unit <- " GWh"
        
        plottitle <-
          "Renewable heat generation by technology type"
      }

      if (RenHeatDropdown$Measure == "Number of Installations"){
        x <- 7
        
        unit <- ""
        
        plottitle <-
          "Number of Renewable heat installations\nby technology type"
      }
      
      Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatCapOutput.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)
      
      
      
      Data <- Data[c(1,2,x)]
      
      Data <- dcast(Data, Year ~ Technology)
      
      names(Data) <- c("Year", "Biomass", "CHP", "Biomethane", "Waste", "Pumps", "Solar", "Total")
      
      TotalData <- Data[c(1,8)]
      
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
      
      
      DomesticEPC <-
        DomesticEPC[c(1, ncol(DomesticEPC):2)]
      
      DomesticEPC <-
        arrange(DomesticEPC,-row_number())
      
      
      DomesticEPC$variable <-
        factor(DomesticEPC$variable,
               levels = rev(unique(DomesticEPC$variable)))
      
      DomesticEPC <- DomesticEPC %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      DomesticEPC <- merge(DomesticEPC, TotalData)
      
      length <- max(DomesticEPC$top)
      
      
      height <- max(as.numeric(as.character(DomesticEPC$Year))) - min(as.numeric(as.character(DomesticEPC$Year))) + 1
    
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
            "Biomass"    = BarColours[1],
            "CHP"        = BarColours[2],
            "Biomethane" = BarColours[3],
            "Waste"      = BarColours[4],
            "Pumps"      = BarColours[5],
            "Solar"      = BarColours[6]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = DomesticEPC$Year,
          y = -length*0.075,
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
          aes(x = height * 1.3,
              y = length * (0.5 / 6)*.95,
              label = "Biomass"),
          fontface = 2,
          colour = BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height * 1.3,
              y = length * (1.5 / 6)*.95,
              label = "Biomass\nCHP"),
          fontface = 2,
          colour = BarColours[2],
        ) +
        geom_text(
          aes(x = height * 1.3,
              y = length * (2.5 / 6)*.95,
              label = "Biomethane"),
          fontface = 2,
          colour = BarColours[3],
        ) +
        geom_text(
          aes(x = height * 1.3,
              y = length * (3.5 / 6)*.95,
              label = "Energy\nfrom waste"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height * 1.3,
              y = length * (4.5 /6)*.95,
              label = "Heat\npumps"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height * 1.3,
              y = length * (5.5 / 6)*.95,
              label = "Solar\n thermal"),
          fontface = 2,
          colour = BarColours[6],
          family = "Century Gothic",
          hjust = 0.5
          
        ) +
        annotate(
          "text",
          x = DomesticEPC$Year,
          y = DomesticEPC$top + length*0.015,
          label = paste0(
            format(round(DomesticEPC$Total, 3), big.mark =","),
            unit
          ),
          family = "Century Gothic",
          hjust = 0,
          fontface = 2,
          colour = ChartColours[1]
        ) +
        geom_text(
          aes(x = height * 1.3,
              y = length * (6.5 / 6)*.95,
              label = "Total"),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5
        )+
        geom_text(
          aes(x = height * 1.40,
              y = length * 2,
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
        ylim(-length*0.1, length*1.15)
      
      DomesticEPCChart
      
      
      ggsave(
        file,
        plot = DomesticEPCChart,
        width = 19,
        height =10.5,
        units = "cm",
        dpi = 300
      )
    }
  )


  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Heat/RenHeatTech.txt")[2])
                     
                   )))
  })
  
 observeEvent(input$ToggleTable1, {
    toggle("RenHeatTechTable")
  })
  
 observeEvent(input$ToggleTable2, {
   toggle("RenHeatSizeTable")
 })
 
 observeEvent(input$ToggleTable3, {
   toggle("RenHeatOutputTable")
 })
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenHeatSizeSubtitle <- renderText({
  
  Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatSize.txt", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)[1:3]
  
  paste("Scotland,", min(Data$Year),"-", max(Data$Year))
})
  
  output$RenHeatSizePlot <- renderPlotly  ({
  
  if (RenHeatDropdown$Measure == "Capacity"){
    x <- 3
    unit <- " GW"
  }
  
  if (RenHeatDropdown$Measure == "Generation"){
    x <- 5
    unit <- " GWh"
  }
  
  if (RenHeatDropdown$Measure == "Number of Installations"){
    x <- 7
    unit <- ""
  }
  
  Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatSize.txt", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)
  
  Data <- Data[c(1,2,x)]
  
  Data <- dcast(Data, Year ~ Technology)
  
  names(Data) <- c("Year", "Bioenergy","Large", "Micro", "Small to medium", "Total", "Unknown")
  
  Data %<>% lapply(function(x) as.numeric(as.character(x)))
  
  Data <- Data[c(1, 3, 5, 4, 2, 7 ,6)]
  
  Data <- as_tibble(Data)
  
  Data[is.na(Data)] <- 0
  
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
      text = paste0("Large: ", format(round(Data$`Large`, 3), big.mark = ","), unit),
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
      text = paste0("Small to medium: ", format(round(Data$`Small to medium`, 3), big.mark = ","), unit),
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
      text = paste0("Micro: ", format(round(Data$`Micro`, 3), big.mark = ","), unit),
      hoverinfo = 'text',
      marker = list(color = BarColours[3]),
      legendgroup = 3
    ) %>%
    add_trace(
      data = Data,
      x = ~ `Bioenergy`,
      type = 'bar',
      width = 0.7,
      orientation = 'h',
      name = "Bioenergy",
      text = paste0("Bioenergy: ", format(round(Data$`Bioenergy`, 3), big.mark = ","), unit),
      hoverinfo = 'text',
      marker = list(color = BarColours[4]),
      legendgroup = 4
    ) %>%
    add_trace(
      data = Data,
      x = ~ `Unknown`,
      type = 'bar',
      width = 0.7,
      orientation = 'h',
      name = "Unknown",
      text = paste0("Unknown: ", format(round(Data$`Unknown`, 3), big.mark = ","), unit),
      hoverinfo = 'text',
      marker = list(color = BarColours[5]),
      legendgroup = 5
    ) %>%
    add_trace(
      x = (Data$Total) * 1.01,
      showlegend = FALSE,
      type = 'scatter',
      mode = 'text',
      text = paste0("<b>",format(round((Data$Total), digits = 3), big.mark = ","), unit, "</b>"),
      textposition = 'middle right',
      textfont = list(color = ChartColours[1]),
      hoverinfo = 'skip',
      marker = list(
        size = 0.00001
      )
    ) %>%
    add_trace(
      x = (Data$Total) * 1.2,
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
        ticksuffix = unit,
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
  
  output$RenHeatSize.png <- downloadHandler(
  filename = "RenHeatSize.png",
  content = function(file) {
    
    if (RenHeatDropdown$Measure == "Capacity"){
      x <- 3
      
      unit <- " GW"
      
      plottitle <-
        "Renewable heat capacity by installation size"
    }
    
    if (RenHeatDropdown$Measure == "Generation"){
      x <- 5
      
      unit <- " GWh"
      
      plottitle <-
        "Renewable heat generation by installation size"
    }
    
    if (RenHeatDropdown$Measure == "Number of Installations"){
      x <- 7
      
      unit <- ""
      
      plottitle <-
        "Number of Renewable heat installations\nby installation size"
    }
    

    
    Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatSize.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    Data <- Data[c(1,2,x)]
    
    Data <- dcast(Data, Year ~ Technology)
    
    names(Data) <- c("Year", "Bioenergy","Large", "Micro", "Small to medium", "Total", "Unknown")
    
    TotalData <- Data[c(1,6)]
    
    
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[c(1, 3, 5, 4, 2, 7 ,6)]
    
    Data[7] <- NULL
    
    Data <- as_tibble(Data)
    
    Data[is.na(Data)] <- 0
    
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
    
    
    DomesticEPC <-
      DomesticEPC[c(1, ncol(DomesticEPC):2)]
    
    DomesticEPC <-
      arrange(DomesticEPC,-row_number())
    
    
    DomesticEPC$variable <-
      factor(DomesticEPC$variable,
             levels = rev(unique(DomesticEPC$variable)))
    
    DomesticEPC <- DomesticEPC %>%
      group_by(Year) %>%
      mutate(pos = cumsum(value) - value / 2) %>%
      mutate(top = sum(value))
    
    DomesticEPC <- merge(DomesticEPC, TotalData)
    
    length <- max(DomesticEPC$top)
    
    
    height <- max(as.numeric(as.character(DomesticEPC$Year))) - min(as.numeric(as.character(DomesticEPC$Year))) + 1

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
          "Bioenergy"   = BarColours[4],
          "Unknown"   = BarColours[5]
        )
      ) +
      geom_bar(stat = "identity", width = .8) +
      annotate(
        "text",
        x = DomesticEPC$Year,
        y = -length*0.075,
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
        aes(x = height * 1.3,
            y = length * (0.5 / 5)*.95,
            label = "Large"),
        fontface = 2,
        colour = BarColours[1],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = height * 1.3,
            y = length * (1.5 / 5)*.95,
            label = "Small to\nmedium"),
        fontface = 2,
        colour = BarColours[2],
      ) +
      geom_text(
        aes(x = height * 1.3,
            y = length * (2.5 / 5)*.95,
            label = "Micro"),
        fontface = 2,
        colour = BarColours[3],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = height * 1.3,
            y = length * (3.5 /5)*.95,
            label = "Bioenergy"),
        fontface = 2,
        colour = BarColours[4],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = height * 1.3,
            y = length * (4.5 / 5)*.95,
            label = "Unknown"),
        fontface = 2,
        colour = BarColours[5],
        family = "Century Gothic",
        hjust = 0.5
        
      ) +
      annotate(
        "text",
        x = DomesticEPC$Year,
        y = DomesticEPC$top + length*0.015,
        label = paste0(
          format(round(DomesticEPC$Total, 3), big.mark =","),
          unit
        ),
        family = "Century Gothic",
        hjust = 0,
        fontface = 2,
        colour = ChartColours[1]
      ) +
      geom_text(
        aes(x = height * 1.3,
            y = length * (5.5 / 5)*.95,
            label = "Total"),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      )+
      geom_text(
        aes(x = height * 1.40,
            y = length * 2,
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
      ylim(-length*0.1, length*1.15)
    
    DomesticEPCChart
    
    
    ggsave(
      file,
      plot = DomesticEPCChart,
      width = 19,
      height =10.5,
      units = "cm",
      dpi = 300
    )
    
  }
)
  output$RenHeatSizeTable = renderDataTable({
  
  RenHeatSize <- read_delim("Processed Data/Output/Renewable Heat/RenHeatSize.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)
  
  RenHeatSize <- melt(RenHeatSize, id.vars = c("Year", "Technology"))
  
  RenHeatSize$variable <- paste0(RenHeatSize$variable, " - ", RenHeatSize$Year)
  
  RenHeatSize$Year <- NULL
  
  RenHeatSize <- dcast(RenHeatSize, Technology ~ variable)
  
  RenHeatSize[RenHeatSize <= 0.01] <- "< 0.1"
  
  datatable(
    RenHeatSize[c(1,13,12,09,08,11,10)],
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
  
  output$RenHeatLASubtitle <- renderText({
    
    Data <- read_delim("Processed Data/Output/Renewable Heat/RenHeatSize.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)[1:3]
    
    paste("Scotland,",  max(Data$Year))
  })
  
  output$RenHeatLA <- renderLeaflet({
    
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
    
    ### Add Simplified shape back to the Shapefile
    LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")
    
    LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
    ############ RENEWABLE ELECTRICITY ################################################
    
    Category =  as.character(input$CategorySelect)
    
    RenHeatLA <- read_delim("Processed Data/Output/Renewable Heat/RenHeatLA.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(RenHeatLA)[1:2] <- c("LAName", "CODE")
    
    RenHeatLA <- RenHeatLA[which(RenHeatLA$variable == Category),]
    
    RenHeatLA$Prop <- percent(RenHeatLA$value / RenHeatLA[which(RenHeatLA$LAName == "TOTAL"),]$value,.1)
    
    if(Category == "Output (GWh)"){Unit <- " GWh"}
    if(Category == "Operational Capacity (GW)"){Unit <- " GW"}

    RenHeatLA$Content <- paste0("<b>",RenHeatLA$LAName, "</b><br/>", RenHeatLA$variable[1], ":<br/><em>", format(round(RenHeatLA$value, digits = 1), big.mark = ","),Unit,"<br/>", RenHeatLA$Prop, "</em>" )
    
    RenHeatLA$Hover <- paste0(RenHeatLA$LAName, " - ", format(round(RenHeatLA$value, digits = 1), big.mark = ","), Unit, " - ", RenHeatLA$Prop)
    

    if(Category == "Output (GWh)"){
      RenHeatLA[which(RenHeatLA$value == 0),]$Content <- paste0("<b>",RenHeatLA[which(RenHeatLA$value == 0),]$LAName, "</b><br/>", RenHeatLA[which(RenHeatLA$value == 0),]$variable[1], ":<br/><em>", "<10 GWh","<br/>", RenHeatLA[which(RenHeatLA$value == 0),]$Prop, "</em>" )
           
           RenHeatLA[which(RenHeatLA$value == 0),]$Hover <- paste0(RenHeatLA[which(RenHeatLA$value == 0),]$LAName, " - ", "<10 GWh", " - ", RenHeatLA[which(RenHeatLA$value == 0),]$Prop)
    }
    
    if(Category == "Operational Capacity (GW)"){
      RenHeatLA[which(RenHeatLA$value == 0),]$Content <- paste0("<b>",RenHeatLA[which(RenHeatLA$value == 0),]$LAName, "</b><br/>", RenHeatLA[which(RenHeatLA$value == 0),]$variable[1], ":<br/><em>", "<1 GW","<br/>", RenHeatLA[which(RenHeatLA$value == 0),]$Prop, "</em>" )
           
           RenHeatLA[which(RenHeatLA$value == 0),]$Hover <- paste0(RenHeatLA[which(RenHeatLA$value == 0),]$LAName, " - ", "<1 GW", " - ", RenHeatLA[which(RenHeatLA$value == 0),]$Prop)

    }

    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    RenHeatLA <- RenHeatLA[order(RenHeatLA$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      merge(LA, RenHeatLA)
    
    
    pal <- colorNumeric(
      palette = "Greens",
      domain = LAMap$value)
    
    l <-leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(value),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~value,
                         title = paste0(RenHeatLA$variable[1], ""),
                         opacity = 1
      ) 
    
    l
    
  }) 
  
  output$RenHeatLA.png <- downloadHandler(
    filename = "RenHeatLA.png",
    content = function(file) {if(as.character(input$CategorySelect) == "Output (GWh)"){
      file.copy(("Structure/2 - Renewables/Heat/RenHeatOutputMap.png"), file)
    }
      if(as.character(input$CategorySelect) == "Operational Capacity (GW)"){
        file.copy(("Structure/2 - Renewables/Heat/RenHeatCapMap.png"), file)
      }
      #"Carbon Saving Target (CERO)"       "ECO measures per 1,000 households" "Affordable Warmth (HHCRO)"         "ECO measures installed"            "Carbon Savings Community (CSCO)"
    }
    
  )
  
  
  output$RenHeatLATable = renderDataTable({
    
    RenHeatLA <- read_delim("Processed Data/Output/Renewable Heat/RenHeatLA.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(RenHeatLA)[1:2] <- c("Name", "LA Code")
    
    RenHeatLA <- as_tibble(dcast(RenHeatLA, Name + `LA Code` ~ variable))
    
    RenHeatLA <- RenHeatLA[c(1,2,6,5,4,3)]
    
    RenHeatLA <- RenHeatLA[order(RenHeatLA$`LA Code`),]
    
    RenHeatLA <- RenHeatLA[c(1:32,34,33),]
    
    datatable(
      RenHeatLA,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Renewable heat output and capacity by Local Authority",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Renewable heat output and capacity by Local Authority",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Renewable heat output and capacity by Local Authority")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      
      formatRound(c(3), 0) %>% 
      formatRound(5,3) %>% 
      formatPercentage(c(4,6),1)
  })

}

