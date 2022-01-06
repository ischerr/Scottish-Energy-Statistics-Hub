require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



RenElecPipelineOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
    tabPanel("Pipeline renewable capacity",
             fluidRow(column(8,
                             h3("Pipeline renewable capacity by planning stage", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecPipelineCapSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecPipelineCap.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecCapacityPlot")),
             plotlyOutput(ns("RenElecPipelineCapPlot"), height = "200px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    
    tabPanel("Pipeline capacity tech",
             fluidRow(column(8,
                             h3("Pipeline renewable capacity by technology", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecPipelineSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecPipeline.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecPipelinePlot")),
             plotlyOutput(ns("RenElecPipelinePlot"), height = "500px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(10, h3("Scottish extract of Planning Table", style = "color: #39ab2c;  font-weight:bold")),
      column(2, style = "padding:15px",  downloadButton(ns("PlanningTable"), "Planning Table", style = "float:right; "))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
    tabPanel("Pipeline capacity",
             fluidRow(
               column(10, h3("Data - Pipeline capacity (MW)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecPipelineTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Pipeline projects",
             fluidRow(
               column(10, h3("Data - Pipeline projects", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable7"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecProjectsTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("LA pipeline capacity",
             fluidRow(
               column(10, h3("Data - Pipeline capacity by local authority  (MW)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable6"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecPipelineLATable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Pipeline capacity time series",
             fluidRow(
               column(10, h3("Data - Pipeline capacity time series (GW)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable5"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecPipelineTimeTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("BEISREPD"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("BEISREPD"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("BEISREPD")
        
      )
    )
  )
}

###### Server ######
RenElecPipeline <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecCapacity.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###

  

  
 output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecPipeline.txt")[2])
                    
                  )))
 })
 
  
  observeEvent(input$ToggleTable2, {
    toggle("RenElecBreakdownCapTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  

  
  observeEvent(input$ToggleTable, {
    toggle("RenElecFuelCapTable")
  })

  
  output$RenElecPipelineSubtitle <- renderText({
    
    Data <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelineTimeSeries.csv")
    paste("Scotland,", Data$Quarter[1])
  })
  
  output$RenElecPipelinePlot <- renderPlotly  ({
    
    RenElecPipeline <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelinebyTech.csv")
    
    RenElecPipeline <- arrange(RenElecPipeline, RenElecPipeline$Total)
    
    RenElecPipeline$Type <- paste0("<b>", RenElecPipeline$Type, "</b>")
    
    rownames(RenElecPipeline) <- NULL
    
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
    
    
    p <- plot_ly(data = RenElecPipeline, y = ~ Type) %>%
      
      add_trace(
        data = RenElecPipeline,
        x = ~ `Under Construction`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Under Construction",
        text = paste0("Under Construction: ", format(round(RenElecPipeline$`Under Construction`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = RenElecPipeline,
        x = ~ `Awaiting Construction`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Awaiting Construction",
        text = paste0("Awaiting Construction: ", format(round(RenElecPipeline$`Awaiting Construction`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      
      add_trace(
        data = RenElecPipeline,
        x = ~ `In Planning`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "In Planning",
        text = paste0("In Planning: ", format(round(RenElecPipeline$`In Planning`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = RenElecPipeline,
        y = ~ Type,
        x = ~ (RenElecPipeline$`Under Construction` + RenElecPipeline$`Awaiting Construction` + RenElecPipeline$`In Planning`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste("<b>",format(round((RenElecPipeline$`Under Construction` + RenElecPipeline$`Awaiting Construction` + RenElecPipeline$`In Planning`), digits = 0), big.mark = ","),"MW</b>"),
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
          # ticktext = list( "Landfill Gas",          
          #                  "Large Hydro",          
          #                  "Anaerobic Digestion",   
          #                  "Small Hydro",     
          #                  "Energy from waste"  ,   
          #                  "Biomass (co-firing)" ,  
          #                  "Solar Photovoltaics" ,  
          #                  "Shoreline wave / tidal",
          #                  "Wind Offshore",       
          #                  "Wind Onshore"),
          tickvals = list(0,1,2,3,4,5,6,7,8,9),
          tickmode = "array"
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = FALSE,
          showticklabels = FALSE,
          range = c(0,12000),
          zeroline = FALSE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F)
    
    p
  })
  
  output$RenElecPipelineTable = renderDataTable({
    
    RenElecPipeline <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelinebyTech.csv")[2:6]
    
    datatable(
      RenElecPipeline,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(RenElecPipeline)-1, 'desc')),
        title = "Pipeline renewable capacity by technology  (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline renewable capacity by technology  (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline renewable capacity by technology  (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecPipeline), 0)
  })
  
  output$RenElecProjectsTable = renderDataTable({
    
    RenElecPipeline <- read_excel("Structure/CurrentWorking.xlsx",
                                  sheet = "Renewable elec pipeline", col_names = TRUE, 
                                  skip = 13,
                                  n_max = 11)
    
    RenElecPipeline[5,c(2:5,8:9)] <- RenElecPipeline[5,c(2:5,8:9)] + RenElecPipeline[6,c(2:5,8:9)] +RenElecPipeline[8,c(2:5,8:9)] +RenElecPipeline[10,c(2:5,8:9)]
    
    RenElecPipeline[5,1] <- "Bioenergy and Waste"
    
    RenElecPipeline <- RenElecPipeline[-c(6,8,10),]
    
    
    names(RenElecPipeline)[1] <- "Tech"
    
    CurrentOnshore <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/CurrentOnshore.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    CurrentOffshore <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/CurrentOffshore.txt", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
    
    Turbines <- data.frame(
      Tech = c("Wind Onshore", "Wind Offshore"),
      Turbines = c(sum(CurrentOnshore$TurbineAmount), sum(CurrentOffshore$TurbineAmount))
    )
    
    names(Turbines) <- c("Tech", "Number of wind turbines")
    
    RenElecPipeline <- merge(RenElecPipeline, Turbines, all = TRUE)
    
    RenElecProjects <- RenElecPipeline[c(1,8,17,9)]
    
    datatable(
      RenElecProjects,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(RenElecProjects)-1, 'desc')),
        title = "Pipeline projects",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline projects',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline projects')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecProjects), 0)
  })
  
  output$RenElecPipelineLATable = renderDataTable({
    
    RenElecPipeline <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelineLA.csv")[c(1,2,3,4,6)]
    
    names(RenElecPipeline) <- c("Local Authority", "LA Code", "In Planning", "Awaiting COnstruction", "Under Construction")
    
    RenElecPipeline$Total <- RenElecPipeline$`In Planning` + RenElecPipeline$`Awaiting COnstruction` + RenElecPipeline$`Under Construction`
    
    datatable(
      RenElecPipeline,
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Pipeline renewable capacity by planning stage and local authority (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline renewable capacity by planning stage and local authority (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline renewable capacity by planning stage and local authority (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(3:ncol(RenElecPipeline), 0)
    
  })
  

  
  observeEvent(input$ToggleTable3, {
    toggle("RenElecPipelineTable")
  })
  
  observeEvent(input$ToggleTable7, {
    toggle("RenElecProjectsTable")
  })
  
  observeEvent(input$ToggleTable6, {
    toggle("RenElecPipelineLATable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("RenElecPipelineCapTable")
  })
  
  
  
  output$RenElecPipeline.png <- downloadHandler(
    filename = "RenElecPipeline.png",
    content = function(file) {
      
      RenElecPipeline <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelineTimeSeries.csv")
      
      RenElecPipeline <- head(RenElecPipeline, 1)
      
      Subtitle <- as.character(RenElecPipeline$Quarter)
      
      ### Load Packages and Functions
      Data2 <-  read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelinebyTech.csv")
      
      RenElecCapTech <- Data2[2:5]
      
      RenElecCapTech$Type <-
        factor(RenElecCapTech$Type,
               levels = unique(RenElecCapTech$Type),
               ordered = TRUE)
      
      RenElecCapTech <- melt(RenElecCapTech, id.vars = "Type")
      
      
      RenElecCapTech$variable <-
        factor(RenElecCapTech$variable,
               levels = rev(unique(RenElecCapTech$variable)),
               ordered = TRUE)
      
      RenElecCapTech <- RenElecCapTech %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Pipeline renewable capacity by technology"
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
      
      
      RenElecCapTechChart <- RenElecCapTech %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Operational" = BarColours[1],
            "Under Construction" = BarColours[2],
            "Awaiting Construction" = BarColours[3],
            "In Planning" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
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
            y = top+100  ,
            label = paste(format(round(top,digits = 0),big.mark = ","), "MW"),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0
        ) +
        
        geom_text(
          aes(
            x = 7.1,
            y = (2/2) * (max(RenElecCapTech$top) * .8) +(max(RenElecCapTech$top) *.1),
            label = "Under\nConstruction",
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 7.1,
            y = (1/2) * (max(RenElecCapTech$top) * .8) +(max(RenElecCapTech$top) *.1),
            label = "Awaiting\nConstruction",
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 7.1,
            y = (0/2) * (max(RenElecCapTech$top) * .8) +(max(RenElecCapTech$top) *.1),
            label = "In\nPlanning",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 8,
            y = (3.5/4) * 15000,
            label = " ",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        )
      
      
      RenElecCapTechChart
      
      
      RenElecCapTechChart <-
        StackedBars(RenElecCapTechChart,
                    RenElecCapTech,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      RenElecCapTechChart <-
        RenElecCapTechChart +
        labs(subtitle = Subtitle) +
        ylim(-4000, max(RenElecCapTech$top)+1800)+
        coord_flip()
      
      RenElecCapTechChart
      
      ggsave(
        file,
        plot = RenElecCapTechChart,
        width = 17.5,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
  )
  ######
  
  observeEvent(input$ToggleTable5, {
    toggle("RenElecPipelineTimeTable")
  })
  

  output$RenElecPipelineCapPlot <- renderPlotly  ({
    
    RenElecPipeline <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelineTimeSeries.csv")
    
    names(RenElecPipeline)[1] <- "Type"
    
    RenElecPipelineCap <- head(RenElecPipeline, 1)
    
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
        text = paste0("Under Construction: ", round(RenElecPipelineCap$`Under Construction`, digits = 0), " MW"),
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
        text = paste0("Awaiting Construction: ", round(RenElecPipelineCap$`Awaiting Construction`, digits = 0), " MW"),
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
        text = paste0("In Planning: ", round(RenElecPipelineCap$`In Planning`, digits = 0), " MW"),
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
        text = paste("<b>",format(round((RenElecPipelineCap$`Under Construction` + RenElecPipelineCap$`Awaiting Construction` + RenElecPipelineCap$`In Planning`), digits = 0), big.mark = ","),"MW</b>"),
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
          range = c(0,17000),
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
    
    RenElecPipeline <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelineTimeSeries.csv")
    
    RenElecPipeline <- head(RenElecPipeline, 1)
    
    paste("Scotland,", RenElecPipeline$Quarter)
  })
  
  output$RenElecPipelineCap.png <- downloadHandler(
    filename = "RenElecPipelineCap.png",
    content = function(file) {
      
      RenElecPipeline <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelineTimeSeries.csv")
      
      RenElecPipeline <- head(RenElecPipeline, 1)
      
      Subtitle <- as.character(RenElecPipeline$Quarter)
      
      PipelineTotal <- tibble(
        Type = "Total",
        `Under Construction` = RenElecPipeline$`Under Construction`,
        `Awaiting Construction`= RenElecPipeline$`Awaiting Construction`,
        `In Planning` = RenElecPipeline$`In Planning`
      )
      
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
      
      PipelineTotalChart
      
      
      PipelineTotalChart <-
        StackedBars(PipelineTotalChart,
                    PipelineTotal,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      PipelineTotalChart <-
        PipelineTotalChart +
        labs(subtitle = Subtitle) +
        ylim(-1, max(PipelineTotal$top)+1.700)+
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
  
  output$RenElecPipelineTimeTable = renderDataTable({
    
    RenElecCapacity <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelineTimeSeries.csv")
    
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
        title = "Pipeline Capacity Time Series (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline Capacity Time Series (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline Capacity Time Series (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecCapacity), 0)
  })
  
  
  output$PlanningTable <- downloadHandler(
    filename = "DataTable.csv",
    content = function(file){

      DataTable  <- read_csv("Processed Data/Output/REPD (Operational Corrections)/PipelineDataTable.csv")
      
      write.csv(DataTable, 
                file,
                row.names = FALSE)
    }
  )
  
}
