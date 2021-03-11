require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



OilGasOutputsOutput <- function(id) {
  ns <- NS(id)
  tagList(
     fluidRow(column(8,
                             h3("Outputs from oil and gas", style = "color: #126992;  font-weight:bold"),
                             h4(textOutput(ns('OilGasOutputsSubtitle')), style = "color: #126992;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('OilGasOutputs.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
             #dygraphOutput(ns("OilGasOutputsPlot")),
             plotlyOutput(ns("OilGasOutputsPlot"), height = "600px")%>% withSpinner(color="#126992"),
             tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #126992;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
  fluidRow(
    column(10, h3("Data - Outputs from oil and gas", style = "color: #126992;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("OilGasOutputsTable"))%>% withSpinner(color="#126992"))),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
  fluidRow(
    column(2, HTML("<p><strong>Last Updated:</strong></p>")),
    column(2,
           UpdatedLookup(c("BEISSubNatEnergy",
                           "BEISSubNatElec",
                           "BEISRHI",
                           "SESHEnergyBalance"))),
    column(1, align = "right",
           HTML("<p><strong>Reason:</strong></p>")),
    column(7, align = "right", 
           p("Regular updates")
    )),
  fluidRow(p(" ")),
  fluidRow(
    column(2, HTML("<p><strong>Update Expected:</strong></p>")),
    column(2,
           DateLookup(c("BEISSubNatEnergy",
                      "BEISSubNatElec",
                      "BEISRHI",
                      "SESHEnergyBalance"))),
    column(1, align = "right",
           HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("BEISSubNatElec"),
        SourceLookup("BEISRHI"),
        SourceLookup("SESHEnergyBalance")
        
      )
    )
  )
}




###### Server ######
OilGasOutputs <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("OilGasOutputs.R")

  
  output$OilGasOutputsSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Outputs from oil & gas", col_names = TRUE, 
        skip = 11)
    
    names(Data) <- c("Year", "Domestic", "Exports", "Transformation")
    
    paste("Scotland,", min(Data$Year),"-", max(Data$Year))
  })
  
  output$OilGasOutputsPlot <- renderPlotly  ({
    
    
    ChartColours <- c("#126992", "#FF8500")
    BarColours <- c("#034e7b", "#0570b0", "#969696", "#f46d43", "#d73027")
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Outputs from oil & gas", col_names = TRUE, 
        skip = 11)
    
    names(Data) <- c("Year", "Domestic", "Exports", "Transformation")
    
    Data$YearFormat <- paste0("<b>",Data$Year, "</b>")
    
    p <-  plot_ly(Data, y = ~ YearFormat ) %>%  
      add_trace(x = ~ `Domestic`, 
                orientation = 'h',
                name = "Consumed domestically",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  "Consumed domestically: ", percent(Data$Domestic, 0.1),"\n",
                  "Year: ", Data$Year, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[1])
      ) %>% 
      add_trace(x = ~ `Exports`, 
                orientation = 'h',
                name = "Exports",
                type = 'bar',
                legendgroup = "2",
                text = paste0(
                  "Exports: ", percent(Data$Exports, 0.1),"\n",
                  "Year: ", Data$Year, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[2])
      ) %>% 
      add_trace(x = ~ `Transformation`, 
                orientation = 'h',
                name = "Transformation and losses",
                type = 'bar',
                legendgroup = "3",
                text = paste0(
                  "Transformation: ", percent(Data$Transformation, 0.1),"\n",
                  "Year: ", Data$Year, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
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
  
  output$OilGasOutputsTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Outputs from oil & gas", col_names = TRUE, 
        skip = 11)
    
    names(Data) <- c("Year", "Consumed domestically", "Exports", "Transformation and losses")
    
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
        title = "Outputs from oil and gas",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Outputs from oil and gas",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Outputs from oil and gas")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:4, 1)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/7 - Oil Gas/OilGasOutputs.txt")[2])
                     
                   )))
  })
  
  
 observeEvent(input$ToggleTable1, {
    toggle("OilGasOutputsTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$OilGasOutputs.png <- downloadHandler(
    filename = "OilGasOutputs.png",
  content = function(file) {
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Outputs from oil & gas", skip = 11, col_names = TRUE)[c(1,4,3,2)]
    
    names(Data) <- c("Year", "Losses", "Exports", "Domestic")
    
    OilGasOutputs <- Data
    
    OilGasOutputs <-
      OilGasOutputs[order(OilGasOutputs$Year),]
    
    OilGasOutputs <- melt(OilGasOutputs, id.vars = "Year")
    
    
    OilGasOutputs$variable <-
      factor(OilGasOutputs$variable,
             levels = unique(OilGasOutputs$variable))
    
    OilGasOutputs <- OilGasOutputs %>%
      group_by(Year) %>%
      mutate(pos = cumsum(value) - value / 2) %>%
      mutate(top = sum(value))
    
    plottitle <-
      "Outputs from oil and gas"
    sourcecaption <- "Source: BEIS, SG, HMRC"
    
    ChartColours <- c("#126992", "#FF8500")
    BarColours <- c("#034e7b", "#0570b0", "#969696", "#f46d43", "#d73027")
    
    
    OilGasOutputsChart <- OilGasOutputs %>%
      ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
      scale_fill_manual(
        "variable",
        values = c(
          "Domestic" = BarColours[1],
          "Exports" = BarColours[2],
          "Losses" = BarColours[3]
        )
      ) +
      geom_bar(stat = "identity", width = .8) +
      geom_text(
        aes(
          y = 0 - .04,
          label = ifelse(variable == "Domestic", Year, ""),
          color = ChartColours[2]
        ),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        y = OilGasOutputs$top - OilGasOutputs$pos,
        label =
          ifelse(
            OilGasOutputs$Year == min(OilGasOutputs$Year) |
              OilGasOutputs$Year ==  max(OilGasOutputs$Year),
            percent(OilGasOutputs$value, 0.1),
            ""
          ),
        hjust = ifelse(OilGasOutputs$value > .05, .5,-0.1),
        family = "Century Gothic",
        fontface = 2,
        color = ifelse(OilGasOutputs$value > .05, "white", BarColours[4])
      ) +
      geom_text(
        aes(x = 2003.9,
            y = 0,
            label = "Consumed\ndomestically"),
        hjust = 0,
        family = "Century Gothic",
        fontface = 2,
        color = BarColours[1]
      ) +
      geom_text(
        aes(x = 2003.9,
            y = 0.5,
            label = "Exports"),
        family = "Century Gothic",
        fontface = 2,
        color = BarColours[2]
      )+
      geom_text(
        aes(x = 2003.9,
            y = 1,
            label = "Transformation\nand losses"),
        hjust = 1,
        family = "Century Gothic",
        fontface = 2,
        color = BarColours[3]
      )
    
    
    OilGasOutputsChart
    
    
    OilGasOutputsChart <-
      BaselineChart(
        OilGasOutputsChart,
        OilGasOutputs,
        plottitle,
        sourcecaption,
        ChartColours
      )
    
    OilGasOutputsChart <-
      OilGasOutputsChart +
      xlim(max(OilGasOutputs$Year) + .5,
           min(OilGasOutputs$Year) - 1.1) +
      coord_flip()
    
    OilGasOutputsChart
    
    ggsave(
      file,
      plot = OilGasOutputsChart,
      width = 20,
      height = 13,
      units = "cm",
      dpi = 300
    )
    
  }
) 
  
  
}
    
    