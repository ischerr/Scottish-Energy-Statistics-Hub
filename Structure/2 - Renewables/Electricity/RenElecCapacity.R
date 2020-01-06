require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenElecCapacityOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Installed capacity",
    fluidRow(column(8,
                    h3("Operational renewable capacity by planning stage", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenElecCapacitySubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecOperational.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenElecCapacityPlot")),
    plotlyOutput(ns("RenElecCapacityPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Pipeline renewable capacity by plannng stage",
             fluidRow(column(8,
                             h3("Pipeline renewable capacity", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecPipelineCapSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecPipelineCap.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecCapacityPlot")),
             plotlyOutput(ns("RenElecPipelineCapPlot"), height = "200px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Operational Capacity",
               fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RenElecCapacityTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Pipeline Capacity",
             fluidRow(
               column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecPipelineCapTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
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
RenElecCapacity <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecCapacity.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RenElecCapacitySubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable elec capacity", col_names = TRUE,
        skip = 15)
    
    Data <- Data[c(2,4)]
    
    names(Data) <- c("Year", "Capacity")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    paste(min(Data$Year),"-", max(Data$Year))
  })

  output$RenElecCapacityPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable elec capacity", col_names = TRUE,
        skip = 15)
    
    Data <- Data[c(2,4)]
    
    names(Data) <- c("Year", "Capacity")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    ElecCapOperational <- Data[order(Data$Year),]
    
    ### variables
    ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
    LineColours <- c("#39ab2c", "#238b45", "#a1d99b")
    sourcecaption = "Source: BEIS"
    plottitle = "Opertional renewable capacity"
    
    
    
    p <-  plot_ly(data = ElecCapOperational,
                  x = ~ Year ) %>% 
      add_trace(y = ~ `Capacity`,
                name = "Prepayment",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Capacity: ",
                  round(ElecCapOperational$`Capacity`, digits = 1),
                  " GW\nAs of: ",
                  format(ElecCapOperational$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecCapOperational[which(ElecCapOperational$`Capacity` > 0 | ElecCapOperational$`Capacity` < 0),], 1),
        x = ~ Year,
        y = ~ `Capacity`,
        name = "Capacity",
        text = paste0(
          "Capacity: ",
          round(ElecCapOperational[which(ElecCapOperational$`Capacity` > 0 | ElecCapOperational$`Capacity` < 0),][-1,]$`Capacity`, digits = 1),
          " GW\nAs of: ",
          format(ElecCapOperational[which(ElecCapOperational$`Capacity` > 0 | ElecCapOperational$`Capacity` < 0),][-1,]$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(ElecCapOperational$Year)-1
                               , max(ElecCapOperational$Year)+1)),
        yaxis = list(
          title = "GW",
          tickformat = "",
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
  
  
  output$RenElecCapacityTable = renderDataTable({
    
    RenElecCapacity <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable elec capacity", col_names = TRUE, 
                          skip = 15)
    
    RenElecCapacity <- RenElecCapacity[c(2,4)]
    
    
    
    names(RenElecCapacity)[1] <- c("Quarter")
    
    datatable(
      RenElecCapacity,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(RenElecCapacity)-2, 'desc')),
        title = "Operational Capacity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Operational Capacity',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Operational Capacity')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecCapacity), 1)
  })
  
  
  output$RenElecPipelineCapTable = renderDataTable({
    
    RenElecCapacity <- read_excel("Structure/CurrentWorking.xlsx",
                             sheet = "Renewable elec pipeline", col_names = TRUE, 
                             skip = 29,)
    
    names(RenElecCapacity)[1] <- "Quarter"
    
    datatable(
      RenElecCapacity,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Pipeline Capacity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline Capacity',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline Capacity')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecCapacity), 1)
  })
  
 output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecCapacity.txt")[2])
                    
                  )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("RenElecCapacityTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("RenElecPipelineCapTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenElecOperational.png <- downloadHandler(
    filename = "RenElecOperational.png",
    content = function(file) {
      
      
      RenElecOperational <- read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable elec capacity",
        col_names = TRUE,
        skip = 15
      )
      
      
      RenElecOperational <- RenElecOperational[c(2, 4)]
      
      names(RenElecOperational) <- c("Year", "Total")
      
      RenElecOperational$Total <-
        as.numeric(RenElecOperational$Total)
      
      RenElecOperational$Year <-
        as.yearqtr(RenElecOperational$Year, format = "%Y Q%q")
      
      RenElecOperational <-
        RenElecOperational[order(RenElecOperational$Year), ]
      
      RenElecOperational <-
        RenElecOperational[complete.cases(RenElecOperational), ]
      ### variables
      ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
      LineColours <- c("#39ab2c", "#238b45", "#a1d99b")
      sourcecaption = "Source: BEIS"
      plottitle = "Operational renewable capacity"
      
      #RenElecOperational$CavityPercentage <- PercentLabel(RenElecOperational$Cavity)
      
      
      RenElecOperationalChart <- RenElecOperational %>%
        ggplot(aes(x = Year,
                   y = Total)) +
        geom_line(aes(),
                  size = 1.5,
                  colour = LineColours[1],
                  family = "Century Gothic") +
        geom_point(
          data = tail(RenElecOperational, 1),
          aes(x = Year,
              y = Total),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            label = ifelse(Year == min(Year), paste(round(Total, digits = 1), "MW"), ""),
            show_guide = FALSE
          ),
          fontface = 2,
          hjust = 1.1,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .3,
            label = ifelse(Year == max(Year), paste(round(Total, digits = 1), "MW"), ""),
            show_guide = FALSE
          ),
          fontface = 2,
          hjust = -0.1,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
      geom_text(
        aes(
          y = 0,
          label = ifelse(
            Year == min(Year) |
              Year == max(Year),
            format(Year, format = "%Y Q%q"),
            ""
          ),
          hjust = 0.5,
          vjust = 1.5,
          colour = ChartColours[1],
          fontface = 2
        ),
        family = "Century Gothic"
      )
      
      
      RenElecOperationalChart
      
      
      RenElecOperationalChart <-
        StackedArea(
          RenElecOperationalChart,
          RenElecOperational,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      
      RenElecOperationalChart <- RenElecOperationalChart +
        xlim(min(as.numeric(RenElecOperational$Year) - 2), max(as.numeric(RenElecOperational$Year) + 3))
      
      RenElecOperationalChart
      
      
      ggsave(
        file,
        plot = RenElecOperationalChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenElecPipelineCapPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec pipeline", col_names = TRUE,
                       skip = 29, n_max = 1)
    
   names(Data)[1] <- c("Type")
    
    Data[2:5]%<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecPipelineCap <- as_tibble(Data)
    
    RenElecPipelineCap <- arrange(RenElecPipelineCap,-row_number())
    
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#31a354",
        "#0868ac",
        "#43a2ca",
        "#7bccc4",
        "#a6bddb",
        "#d0d1e6",
        "#bdbdbd",
        "#969696"
      )
    
    
    RenElecPipelineCap$Type <- paste0("<b>", RenElecPipelineCap$Type, "</b>"  )
    
      p <- plot_ly(data = RenElecPipelineCap, y = ~ Type) %>%

      add_trace(
        data = RenElecPipelineCap,
        x = ~ `Under Construction`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Under Construction",
        text = paste0("Under Construction: ", round(RenElecPipelineCap$`Under Construction`, digits = 1), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = RenElecPipelineCap,
        x = ~ `Awaiting Construction`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Awaiting Construction",
        text = paste0("Awaiting Construction: ", round(RenElecPipelineCap$`Awaiting Construction`, digits = 1), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      
      add_trace(
        data = RenElecPipelineCap,
        x = ~ `In Planning`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "In Planning",
        text = paste0("In Planning: ", round(RenElecPipelineCap$`In Planning`, digits = 1), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
        add_trace(
          data = RenElecPipelineCap,
          y = ~Type,
          x = ~ (RenElecPipelineCap$`Under Construction` + RenElecPipelineCap$`Awaiting Construction` + RenElecPipelineCap$`In Planning`) + 0.3,
          showlegend = FALSE,
          type = 'scatter',
          mode = 'text',
          text = paste("<b>",format(round((RenElecPipelineCap$`Under Construction` + RenElecPipelineCap$`Awaiting Construction` + RenElecPipelineCap$`In Planning`), digits = 1), big.mark = ","),"GW</b>"),
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
        yaxis = list(
          title = "",
          showgrid = FALSE,
          tickmode = "array"
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = FALSE,
          showticklabels = FALSE,
          range = c(0,15),
          zeroline = FALSE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F)
    
      p
  })
  
  output$RenElecPipelineCapSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec pipeline", col_names = TRUE,
                       skip = 29, n_max = 1)
    Quarter <- substr(Data[1,1], 8,8)
    
    Quarter <- as.numeric(Quarter)*3
    
    Year <- substr(Data[1,1], 1,4)
    
        paste("Scotland,", month.name[Quarter], Year)
  })
  
  output$RenElecPipelineCap.png <- downloadHandler(
    filename = "RenElecPipelineCap.png",
    content = function(file) {
      
      Data2 <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Renewable elec pipeline", col_names = TRUE,
                         skip = 15, n_max = 13)
      
      Data2 <- Data2[c(1,4,3,2)]
      
      Data2 <- Data2[complete.cases(Data2),]
      
      names(Data2) <- c("Type", "Under Construction", "Awaiting Construction", "In Planning")
      
      PipelineTotal <- tail(Data2, 1)
      
      PipelineTotal <- arrange(PipelineTotal,-row_number())
      
      PipelineTotal$Type <-
        factor(PipelineTotal$Type,
               levels = unique(PipelineTotal$Type),
               ordered = TRUE)
      
      PipelineTotal <- melt(PipelineTotal, id.vars = "Type")
      
      
      PipelineTotal$variable <-
        factor(PipelineTotal$variable,
               levels = rev(unique(PipelineTotal$variable)),
               ordered = TRUE)
      
      PipelineTotal$value <- as.numeric(PipelineTotal$value) /1000
      
      PipelineTotal <- PipelineTotal %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Pipeline renewable capacity by planning stage"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#31a354",
          "#0868ac",
          "#43a2ca",
          "#7bccc4",
          "#a6bddb",
          "#d0d1e6",
          "#bdbdbd",
          "#969696"
        )
      
      
      PipelineTotalChart <- PipelineTotal %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Under Construction" = BarColours[2],
            "Awaiting Construction" = BarColours[3],
            "In Planning" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .4) +
        geom_text(
          aes(
            x = Type,
            y = -200,
            label = Type,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 1
        ) +
        geom_text(
          aes(
            x = Type,
            y = top+1.600  ,
            label = paste0(format(sprintf("%.1f", round(top,digits = 1)),big.mark = ","), "\nGW"),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Type,
            y = pos  ,
            label = paste0(format(round(value,digits = 1),big.mark = ",", trim = TRUE), "\nGW"),
            fontface = 2
          ),
          colour = "white",
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 1.4,
            y = PipelineTotal$pos[which(PipelineTotal$variable == "Under Construction")],
            label = "Under\nConstruction",
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic",
          size = 3.5
        ) +
        geom_text(
          aes(
            x = 1.4,
            y = PipelineTotal$pos[which(PipelineTotal$variable == "Awaiting Construction")],
            label = "Awaiting\nConstruction",
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic",
          size = 3.5
        ) +
        geom_text(
          aes(
            x = 1.4,
            y = PipelineTotal$pos[which(PipelineTotal$variable == "In Planning")],
            label = "In\nPlanning",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic",
          size = 3.5
        ) +
        geom_text(
          aes(
            x = 1.7,
            y = (3.5/4) * 15,
            label = " ",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        )
      # # geom_text(
      # #   aes(
      # #     x = 4.77,
      # #     y = .45/2,
      # #     label = "Petroleum\nproducts",
      # #     fontface = 2
      # #   ),
      # #   colour = BarColours[1],
      # #   family = "Century Gothic"
      # # ) +
      # # geom_text(
      # #   aes(
      # #     x = 4.7,
      # #     y = (.33/2)+.45,
      # #     label = "Gas",
      # #     fontface = 2
      # #   ),
      # #   colour = BarColours[2],
      # #   family = "Century Gothic"
      # # ) +
      # # geom_text(
      # #   aes(
      # #     x = 4.7,
      # #     y = (.22/2)+.33+.45,
      # #     label = "Other\nfuels",
      # #     fontface = 2
      # #   ),
      # #   colour = BarColours[3],
      # #   family = "Century Gothic"
      # # )+
      # # geom_text(
      # #   aes(
      # #     x = 5.05,
      # #     y = .5,
      # #     label = "",
      # #     fontface = 2
      # #   ),
      # #   colour = "black",
      # #   family = "Century Gothic"
      # # )
      
      
      
      PipelineTotalChart
      
      
      PipelineTotalChart <-
        StackedBars(PipelineTotalChart,
                    PipelineTotal,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      PipelineTotalChart <-
        PipelineTotalChart +
        labs(subtitle = "Scotland, September 2019") +
        ylim(-2, max(PipelineTotal$top)+1.700)+
        coord_flip()
      
      PipelineTotalChart
      
      ggsave(
        file,
        plot = PipelineTotalChart,
        width = 17.5,
        height = 8,
        units = "cm",
        dpi = 300
      )
    }
  )
}
