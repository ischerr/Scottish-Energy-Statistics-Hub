require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenElecPipelineOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Operational renewable capacity", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenElecPipelineSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecPipeline.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenElecPipelinePlot")),
    plotlyOutput(ns("RenElecPipelinePlot"), height = "500px")%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
      column(12, dataTableOutput(ns("RenElecPipelineTable"))%>% withSpinner(color="#39ab2c"))),
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
RenElecPipeline <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecPipeline.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RenElecPipelineSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec pipeline", col_names = TRUE,
                       skip = 29, n_max = 1)
    Quarter <- substr(Data[1,1], 8,8)
    
    Quarter <- as.numeric(Quarter)*3
    
    Year <- substr(Data[1,1], 1,4)
    
    paste("Scotland,", month.name[Quarter], Year)
  })

  output$RenElecPipelinePlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec pipeline", col_names = TRUE,
                       skip = 16, n_max = 10)[1:6]
    
    Data[2:5]%<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecPipeline <- as_tibble(Data)
    
    RenElecPipeline$Total <- RenElecPipeline$`Under Construction` + RenElecPipeline$`Awaiting Construction` + RenElecPipeline$`In Planning`
    
    names(RenElecPipeline)[1] <- "Type"
    
    # 
    # Data <- as.data.frame(t(Data))
    # 
    # names(Data) <-  as.character(unlist(Data[1,]))
    # names(Data)[1] <- "Type"
    # Data <- tail(Data,-1)
    # Data %<>% lapply(function(x) as.numeric(as.character(x)))
    # Data <- as.data.frame(Data)
    
    RenElecPipeline <- RenElecPipeline[which(RenElecPipeline$Total > 0),]
    
    RenElecPipeline <- arrange(RenElecPipeline, RenElecPipeline$Total)
    
    rownames(RenElecPipeline) <- NULL
    
    #RenElecPipeline$Type <- as.numeric(rownames(RenElecPipeline))
    
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
          range = c(0,9900),
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
    
    RenElecPipeline <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable elec pipeline", col_names = TRUE, 
                          skip = 16,
                          n_max = 11)
    
    RenElecPipeline <- RenElecPipeline[c(1:5)]
    
    
    
    names(RenElecPipeline)[1] <- c("Type")
    
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
        title = "Pipeline renewable capacity by technology",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline renewable capacity by technology',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline renewable capacity by technology')
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
  
  output$RenElecPipelineCapTable = renderDataTable({
    
    RenElecPipeline <- read_excel("Structure/CurrentWorking.xlsx",
                                  sheet = "Renewable elec pipeline", col_names = TRUE, 
                                  skip = 16,
                                  n_max = 10)
    
    RenElecPipeline <- RenElecPipeline[c(7:9)]
    
    
    
    names(RenElecPipeline)[1] <- c("Type")
    
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
        title = "Pipeline renewable capacity by planning stage",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline renewable capacity by planning stage',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline renewable capacity by planning stage')
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
  
 output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecPipeline.txt")[2])
                    
                  )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("RenElecPipelineTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("RenElecPipelineCapTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenElecPipeline.png <- downloadHandler(
    filename = "RenElecPipeline.png",
    content = function(file) {

      ### Load Packages and Functions
      Data2 <- read_excel("Structure/CurrentWorking.xlsx",
                         sheet = "Renewable elec pipeline", col_names = TRUE, 
                         skip = 16, n_max = 10)[1:5]
      
      names(Data2)[1] <- "Type"
      
      RenElecCapTech <- Data2[1:4]
      
      
      RenElecCapTech <- arrange(RenElecCapTech,-row_number())
      
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
            x = 11.1,
            y = (2/2) * (max(RenElecCapTech$top) * .8) +(max(RenElecCapTech$top) *.1),
            label = "Under\nConstruction",
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 11.1,
            y = (1/2) * (max(RenElecCapTech$top) * .8) +(max(RenElecCapTech$top) *.1),
            label = "Awaiting\nConstruction",
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 11.1,
            y = (0/2) * (max(RenElecCapTech$top) * .8) +(max(RenElecCapTech$top) *.1),
            label = "In\nPlanning",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 12,
            y = (3.5/4) * 15000,
            label = " ",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        )
      # geom_text(
      #   aes(
      #     x = 4.77,
      #     y = .45/2,
      #     label = "Petroleum\nproducts",
      #     fontface = 2
      #   ),
      #   colour = BarColours[1],
      #   family = "Century Gothic"
      # ) +
      # geom_text(
      #   aes(
      #     x = 4.7,
      #     y = (.33/2)+.45,
      #     label = "Gas",
      #     fontface = 2
      #   ),
      #   colour = BarColours[2],
      #   family = "Century Gothic"
      # ) +
      # geom_text(
      #   aes(
      #     x = 4.7,
      #     y = (.22/2)+.33+.45,
      #     label = "Other\nfuels",
      #     fontface = 2
      #   ),
      #   colour = BarColours[3],
      #   family = "Century Gothic"
      # )+
      # geom_text(
      #   aes(
      #     x = 5.05,
      #     y = .5,
      #     label = "",
      #     fontface = 2
      #   ),
      #   colour = "black",
      #   family = "Century Gothic"
      # )
      
      
      
      RenElecCapTechChart
      
      
      RenElecCapTechChart <-
        StackedBars(RenElecCapTechChart,
                    RenElecCapTech,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      RenElecCapTechChart <-
        RenElecCapTechChart +
        labs(subtitle = "Scotland, June 2019") +
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
  

  
}
