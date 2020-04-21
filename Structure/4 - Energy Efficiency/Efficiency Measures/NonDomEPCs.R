require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

NonDomEPCsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Average energy efficiency levels of non-domestic properties, by type", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('NonDomEPCsSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('NonDomEPCs.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("NonDomEPCsPlot")),
    plotlyOutput(ns("NonDomEPCsPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
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
      column(12, dataTableOutput(ns("NonDomEPCsTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
      column(2, p("Next update:")),
      column(2,
             DateLookup(c("SGNonDomBase"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("SGNonDomBase")
        
      )
    )
  )
}




###### Server ######
NonDomEPCs <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("NonDomEPCs.R")

  
  output$NonDomEPCsSubtitle <- renderText({
    
    paste("Scotland, 2017")
  })
  
  output$NonDomEPCsPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-domestic EPC",
      col_names = FALSE,
      skip = 12
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "C or Better"
    
    Data <- Data[-1,]
    
    Data <- Data[c(2:9,1)]
    
    Data[2:9] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[is.na(Data)] <- 0
    
    Data[2,1] <- " "
    
    Data$`Building type` <- paste("<b>",str_wrap(Data$`Building type`, 20),"</b>")
    
    DataTail <- tail(Data,1)
    
    DataLatest <- Data[nrow(Data)-1,]
    
    ChartColours <- c("#34d1a3", "#FF8500")
    
    BarColours <-
      c("#006837",
        "#1a9850",
        "#66bd63",
        "#fee08b",
        "#fdae61",
        "#f46d43",
        "#d73027")
    
    p <- plot_ly(data = Data, y = ~ `Building type`) %>%
      
      add_trace(
        data = Data,
        x = ~ `A`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "A",
        text = paste0("A: ", percent(Data$`A`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `B`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "B",
        text = paste0("B: ", percent(Data$`B`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `C`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "C",
        text = paste0("C: ", percent(Data$`C`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `D`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "D",
        text = paste0("D: ", percent(Data$`D`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        x = ~ `E`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "E",
        text = paste0("E: ", percent(Data$`E`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = Data,
        x = ~ `F`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "F",
        text = paste0("F: ", percent(Data$`F`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = Data,
        x = ~ `G`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "G",
        text = paste0("G: ", percent(Data$`G`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[7]),
        legendgroup = 7
      ) %>%
      
      add_trace(
        data = Data,
        x = ~ 1.1 ,
        showlegend = TRUE,
        name = 'C or better',
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[2]),
        text =  paste0("<b>", ifelse(Data$`C or Better` > 0, percent(Data$`C or Better`, accuracy = 0.1), " "), "</b>"),
        legendgroup = 8
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
                     ticktext = as.list(Data$`Building type`),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
  })
  
  
  output$NonDomEPCsTable = renderDataTable({
    
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-domestic EPC",
      col_names = FALSE,
      skip = 12
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "C or Better"
    
    Data <- Data[-1,]
    
    Data <- Data[c(2:9,1)]
    
    Data[2:9] %<>% lapply(function(x) as.numeric(as.character(x)))
    
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
        title = "Average energy efficiency levels of non-domestic properties, by type",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average energy efficiency levels of non-domestic properties, by type',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average energy efficiency levels of non-domestic properties, by type')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:9, 1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Efficiency Measures/NonDomEPCs.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("NonDomEPCsTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$NonDomEPCs.png <- downloadHandler(
    filename = "NonDomEPCs.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Non-domestic EPC", skip = 12,  col_names = TRUE)[c(2:9,1)]
      
      Data[2,1] <- "z"
      
      names(Data) <- c("Type", "A or better", "B", "C", "D", "E", "F", "G", "Total")
      
      HouseholdEnergyEfficiency <- Data
      
      HouseholdEnergyEfficiency <-
        HouseholdEnergyEfficiency[c(1, ncol(HouseholdEnergyEfficiency):2)]
      
      HouseholdEnergyEfficiency <-
        arrange(HouseholdEnergyEfficiency,-row_number())
      
      HouseholdEnergyEfficiency$Type <-
        factor(HouseholdEnergyEfficiency$Type,
               levels = unique(HouseholdEnergyEfficiency$Type))
      
      HouseholdEnergyEfficiency <-
        melt(HouseholdEnergyEfficiency, id.vars = "Type")
      
      
      HouseholdEnergyEfficiency$variable <-
        factor(HouseholdEnergyEfficiency$variable,
               levels = unique(HouseholdEnergyEfficiency$variable))
      
      HouseholdEnergyEfficiency <- HouseholdEnergyEfficiency %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Average energy efficiency levels of\nnon-domestic properties, by type"
      sourcecaption <- "Source: SG"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <-
        c("#006837",
          "#1a9850",
          "#66bd63",
          "#fee08b",
          "#fdae61",
          "#f46d43",
          "#d73027")
      
      
      HouseholdEnergyEfficiencyChart <- HouseholdEnergyEfficiency %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "A or better" = BarColours[1],
            "B" = BarColours[2],
            "C" = BarColours[3],
            "D" = BarColours[4],
            "E" = BarColours[5],
            "F" = BarColours[6],
            "G" = BarColours[7],
            "Total" = "White"
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = HouseholdEnergyEfficiency$Type,
          y = -.01,
          label = ifelse(
            HouseholdEnergyEfficiency$Type == "z",
            "",
            str_wrap(HouseholdEnergyEfficiency$Type, width = 20)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1],
          size = 3,
          hjust = 1
        ) +
        geom_text(
          aes(x = 11,
              y = 0.5 * (1 / 7),
              label = "A"),
          fontface = 2,
          colour = BarColours[1],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 11,
              y = 1.5 * (1 / 7),
              label = "B"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 11,
              y = 2.5 * (1 / 7),
              label = "C"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 11,
              y = 3.5 * (1 / 7),
              label = "D"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 11,
              y = 4.5 * (1 / 7),
              label = "E"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 11,
              y = 5.5 * (1 / 7),
              label = "F"),
          fontface = 2,
          colour = BarColours[6],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 11,
              y = 6.5 * (1 / 7),
              label = "G"),
          fontface = 2,
          colour = BarColours[7],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        annotate(
          "text",
          x = HouseholdEnergyEfficiency$Type,
          y = 1.05,
          label = ifelse(
            HouseholdEnergyEfficiency$Type == "z",
            "",
            percent(HouseholdEnergyEfficiency$value[which(HouseholdEnergyEfficiency$variable == "Total")])
          ),
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1],
          size = 3
        ) +
        geom_text(
          aes(x = 11,
              y = 1.05,
              label = "C\nor better"),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        )
      
      
      HouseholdEnergyEfficiencyChart
      
      
      HouseholdEnergyEfficiencyChart <-
        StackedBars(
          HouseholdEnergyEfficiencyChart,
          HouseholdEnergyEfficiency,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      HouseholdEnergyEfficiencyChart <-
        HouseholdEnergyEfficiencyChart +
        coord_flip() +
        labs(subtitle = "2017") +
        ylim(-.2, 1.06)
      
      HouseholdEnergyEfficiencyChart
      
      ggsave(
        file,
        plot = HouseholdEnergyEfficiencyChart,
        width = 17.5,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
