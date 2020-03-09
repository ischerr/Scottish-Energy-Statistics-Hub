require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

DistrictHeatOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("District heat networks", style = "color: #a3d65c;  font-weight:bold"),
                    h4(textOutput(ns('DistrictHeatSubtitle')), style = "color: #a3d65c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('DistrictHeat.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
    #dygraphOutput(ns("DistrictHeatPlot")),
    plotlyOutput(ns("DistrictHeatPlot"))%>% withSpinner(color="#a3d65c"),
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #a3d65c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
    tabsetPanel(
      tabPanel("District Heat Networks",
    fluidRow(
    column(10, h3("Data - District Heat Networks", style = "color: #a3d65c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("DistrictHeatTable"))%>% withSpinner(color="#a3d65c"))),
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    tabPanel("Sectors",
             fluidRow(
               column(10, h3("Data - Sectors of Scottish heat networks by network type and total capacity, generation and supply", style = "color: #a3d65c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("DistrictHeatSectorTable"))%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    tabPanel("Network type",
             fluidRow(
               column(10, h3("Data - Scottish heat networks by network type and total capacity, generation and supply", style = "color: #a3d65c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("DistrictHeatNetworkTypeTable"))%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    tabPanel("Tech",
             fluidRow(
               column(10, h3("Data - Scottish heat networks by technology and fuel type", style = "color: #a3d65c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("DistrictHeatNetworkTechTable"))%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    tabPanel("Customers",
             fluidRow(
               column(10, h3("Data - Scottish heat network customers by technology and fuel type", style = "color: #a3d65c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable5"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("DistrictHeatNetworkCustomersTable"))%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    tabPanel("Buildings",
             fluidRow(
               column(10, h3("Data - Scottish heat network customers by technology and fuel type", style = "color: #a3d65c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable6"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("DistrictHeatNetworkBuildingsTable"))%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    tabPanel("Heat supplied",
             fluidRow(
               column(10, h3("Data - Scottish heat network customers by technology and fuel type", style = "color: #a3d65c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable7"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("DistrictHeatNetworkHeatTable"))%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"))
    
    
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
        SourceLookup("BEISHeatNetwork")
        
      )
    )
  )
}




###### Server ######
DistrictHeat <- function(input, output, session) {
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("DistrictHeat.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$DistrictHeatSubtitle <- renderText({
    
    DistrictHeat <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = FALSE,
      skip = 11,
      n_max = 4
    )
    
    DistrictHeatYear <- tail(DistrictHeat, 2)[1,1]
    
    DistrictHeat <- as_tibble(t(DistrictHeat))
    
    DistrictHeat <- DistrictHeat[c(1, ncol(DistrictHeat) -1, ncol(DistrictHeat))]
    
    DistrictHeat <- DistrictHeat[complete.cases(DistrictHeat),]
    
    names(DistrictHeat) <- c("Type", "Latest", "Ambition")
    
    paste("Scotland,", DistrictHeatYear)
  })
  
  output$DistrictHeatPlot <- renderPlotly  ({
    
    DistrictHeat <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = FALSE,
      skip = 11,
      n_max = 4
    )
    
    DistrictHeatYear <- tail(DistrictHeat, 2)[1,1]
    
    DistrictHeat <- as_tibble(t(DistrictHeat))
    
    DistrictHeat <- DistrictHeat[c(1, ncol(DistrictHeat) -1, ncol(DistrictHeat))]
    
    DistrictHeat <- DistrictHeat[complete.cases(DistrictHeat),]
    
    names(DistrictHeat) <- c("Type", "Latest", "Ambition")
    
    DistrictHeat$Ambition <- as.numeric(DistrictHeat$Ambition) - as.numeric(DistrictHeat$Latest)
    
    DistrictHeat$Latest <- as.numeric(DistrictHeat$Latest)
    
    DistrictHeat$LatestProp <- DistrictHeat$Latest / (DistrictHeat$Latest + DistrictHeat$Ambition)
    
    DistrictHeat$AmbitionProp <- DistrictHeat$Ambition / (DistrictHeat$Latest + DistrictHeat$Ambition)
    
    DistrictHeat$Type <- str_wrap(paste0("<b>",DistrictHeat$Type,"</b>"), 28)
    
    p <-  plot_ly(DistrictHeat, 
                  y = ~Type, 
                  x = ~ `LatestProp`, 
                  type = 'bar', 
                  name = 'Latest',
                  hoverinfo = "text",
                  text = paste0("Latest: ", format(DistrictHeat$Latest, big.mark = ","), "\nProportion: ", percent(DistrictHeat$LatestProp, accuracy = 0.1)),
                  orientation = 'h',
                  marker = list(color = "#a3d65c")
    )%>%
      add_trace(x = ~ `AmbitionProp`,
                name = 'Ambition',
                text = paste0("Overall Ambition: ", format((DistrictHeat$Ambition + DistrictHeat$Latest) , big.mark = ","), "\n"),
                marker = list(color = "#ccebc5")
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.25,
        legend = list(font = list(color = "#a3d65c"),
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
          tickformat = "%",
          autorange = "reversed",
          showgrid = FALSE,
          zeroline = FALSE,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
  })
  
  
  output$DistrictHeatTable = renderDataTable({
    
    DistrictHeat <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = FALSE,
      skip = 11,
      n_max = 4
    )

    DistrictHeat <- DistrictHeat[c(1,2,4)]
    
    names(DistrictHeat) <- unlist(DistrictHeat[1,])
    
    names(DistrictHeat)[1] <- "Year"
    
    DistrictHeat <- DistrictHeat [-1,]
    
    datatable(
      DistrictHeat,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Heat Networks",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Heat Networks',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Heat Networks')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:3, 0)
  })
  
  observeEvent(input$ToggleTable1, {
    toggle("DistrictHeatTable")
  })
  
  output$DistrictHeatSectorTable = renderDataTable({
    
    DistrictHeat <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = TRUE,
      skip = 18,
      n_max = 4
    )
    
    names(DistrictHeat) <- c("Sector", "Network Type - District", "Network Type - Communal", "Network Type - Total", "Customers - Domestic", "Customers - Non-domestic", "Customers - Total", "Heat - Capacity (GW)", "Heat - Generation (GWh)", "Heat - Supplied (GWh)")
    
    datatable(
      DistrictHeat,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Sectors of Scottish heat networks by network type and total capacity, generation and supply",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Sectors of Scottish heat networks by network type and total capacity, generation and supply',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Sectors of Scottish heat networks by network type and total capacity, generation and supply')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:10, 0) %>% 
      formatRound(8,2) %>% 
      formatStyle(c(4,7), fontWeight = "bold")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("DistrictHeatSectorTable")
  })
  
  output$DistrictHeatNetworkTypeTable = renderDataTable({
    
    DistrictHeat <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = TRUE,
      skip = 26,
      n_max = 3
    )
    
    datatable(
      DistrictHeat,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Scottish heat networks by network type and total capacity, generation and supply",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish heat networks by network type and total capacity, generation and supply',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish heat networks by network type and total capacity, generation and supply')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:10, 0) %>% 
      formatRound(5,2) %>% 
      formatStyle(c(4), fontWeight = "bold")
    
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("DistrictHeatNetworkTypeTable")
  })
  
  output$DistrictHeatNetworkTechTable = renderDataTable({
    
    DistrictHeat <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = TRUE,
      skip = 34,
      n_max = 4
    )
    
    datatable(
      DistrictHeat,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Scottish heat networks by technology and fuel type",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish heat networks by technology and fuel type',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish heat networks by technology and fuel type')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:10, 0) %>% 
      formatStyle(c(8), fontWeight = "bold")
    
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("DistrictHeatNetworkTechTable")
  })
  
  output$DistrictHeatNetworkCustomersTable = renderDataTable({
    
    DistrictHeat <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = TRUE,
      skip = 44,
      n_max = 4
    )
    
    datatable(
      DistrictHeat,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Scottish heat network customers by technology and fuel type",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish heat network customers by technology and fuel type',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish heat network customers by technology and fuel type')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:10, 0) %>% 
      formatStyle(c(8), fontWeight = "bold")
    
  })
  
  observeEvent(input$ToggleTable5, {
    toggle("DistrictHeatNetworkCustomersTable")
  })
  
  output$DistrictHeatNetworkBuildingsTable = renderDataTable({
    
    DistrictHeat <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = TRUE,
      skip = 54,
      n_max = 4
    )
    
    datatable(
      DistrictHeat,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Buildings connected to Scottish heat networks by technology and fuel type",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Buildings connected to Scottish heat networks by technology and fuel type',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Buildings connected to Scottish heat networks by technology and fuel type')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:10, 0) %>% 
      formatStyle(c(8), fontWeight = "bold")
    
    
  })
  
  observeEvent(input$ToggleTable6, {
    toggle("DistrictHeatNetworkBuildingsTable")
  })
  
  output$DistrictHeatNetworkHeatTable = renderDataTable({
    
    DistrictHeat <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = TRUE,
      skip = 64,
      n_max = 4
    )
    
    datatable(
      DistrictHeat,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Heat supplied (GWh) by Scottish heat networks by technology and fuel type",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Heat supplied (GWh) by Scottish heat networks by technology and fuel type',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Heat supplied (GWh) by Scottish heat networks by technology and fuel type')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:10, 0) %>% 
      formatStyle(c(8), fontWeight = "bold")
    
    
    
  })
  
  observeEvent(input$ToggleTable7, {
    toggle("DistrictHeatNetworkHeatTable")
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/3 - Local Energy/DistrictHeat.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$DistrictHeat.png <- downloadHandler(
    filename = "DistrictHeat.png",
    content = function(file) {
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Heat networks", skip = 11, col_names = FALSE, n_max = 5)
      
      Data <- as_tibble(t(Data))
      
      Data <- Data[c(1, (ncol(Data)-1), (ncol(Data)))]
      names(Data) <- c("Type", "Current", "Ambition")
      
      Data <- Data[complete.cases(Data),]
      
      Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
      
      Data$Ambition <- Data$Ambition - Data$Current
      
      DistrictDistrictHeats <- Data
      
      DistrictDistrictHeatsProportions <- DistrictDistrictHeats
      
      DistrictDistrictHeatsProportions$Total <- DistrictDistrictHeatsProportions$Current+DistrictDistrictHeatsProportions$Ambition
      
      DistrictDistrictHeatsProportions$Current <- DistrictDistrictHeatsProportions$Current/DistrictDistrictHeatsProportions$Total
      
      DistrictDistrictHeatsProportions$Ambition <- DistrictDistrictHeatsProportions$Ambition/DistrictDistrictHeatsProportions$Total
      
      DistrictDistrictHeatsProportions$Total <- NULL
      
      DistrictDistrictHeatsProportions <- arrange(DistrictDistrictHeatsProportions,-row_number())
      
      DistrictDistrictHeatsProportions$Type <-
        factor(DistrictDistrictHeatsProportions$Type,
               levels = unique(DistrictDistrictHeatsProportions$Type),
               ordered = TRUE)
      
      DistrictDistrictHeatsProportions <- melt(DistrictDistrictHeatsProportions, id.vars = "Type")
      
      
      DistrictDistrictHeatsProportions$variable <-
        factor(DistrictDistrictHeatsProportions$variable,
               levels = rev(unique(DistrictDistrictHeatsProportions$variable)),
               ordered = TRUE)
      
      DistrictDistrictHeatsProportions <- DistrictDistrictHeatsProportions %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      DistrictDistrictHeats <- arrange(DistrictDistrictHeats,-row_number())
      
      DistrictDistrictHeats$Type <-
        factor(DistrictDistrictHeats$Type,
               levels = unique(DistrictDistrictHeats$Type),
               ordered = TRUE)
      
      DistrictDistrictHeats <- melt(DistrictDistrictHeats, id.vars = "Type")
      
      
      DistrictDistrictHeats$variable <-
        factor(DistrictDistrictHeats$variable,
               levels = rev(unique(DistrictDistrictHeats$variable)),
               ordered = TRUE)
      
      DistrictDistrictHeats <- DistrictDistrictHeats %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "District heat networks"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#a3d65c", "#ccebc5")
      
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
      
      
      DistrictDistrictHeatsProportionsChart <- DistrictDistrictHeatsProportions %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Current" = ChartColours[1],
            "Ambition" = ChartColours[2]
          )
        ) +
        geom_bar(stat = "identity", width = .7)+
        geom_text(
          aes(
            y = pos,
            label = ifelse(variable == "Current", format(DistrictDistrictHeats$value, big.mark = ","), ""),
            fontface = 2
          ),
          colour = "white",
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            y = 1.15,
            label = ifelse(variable == "Current", paste0("2020 Ambition:\n",format(DistrictDistrictHeats$top, big.mark = ",")), ""),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            y = -.18,
            label = ifelse(variable == "Current", str_wrap(as.character(DistrictDistrictHeats$Type), width = 15), ""),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      DistrictDistrictHeatsProportionsChart
      
      
      DistrictDistrictHeatsProportionsChart <-
        StackedBars(DistrictDistrictHeatsProportionsChart,
                    DistrictDistrictHeatsProportions,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      DistrictDistrictHeatsProportionsChart <-
        DistrictDistrictHeatsProportionsChart +
        labs(subtitle = "Scotland, 2018") +
        ylim(-.3,1.22)+
        coord_flip()
      
      DistrictDistrictHeatsProportionsChart
      
      ggsave(
        file,
        plot = DistrictDistrictHeatsProportionsChart,
        width = 17.5,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
