require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

HeatNetworkOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("District heat networks", style = "color: #a3d65c;  font-weight:bold"),
                    h4(textOutput(ns('HeatNetworkSubtitle')), style = "color: #a3d65c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('HeatNetwork.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
    #dygraphOutput(ns("HeatNetworkPlot")),
    plotlyOutput(ns("HeatNetworkPlot"))%>% withSpinner(color="#a3d65c"),
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #a3d65c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
    fluidRow(
    column(10, h3("Data", style = "color: #a3d65c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("HeatNetworkTable"))%>% withSpinner(color="#a3d65c"))),
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
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
HeatNetwork <- function(input, output, session) {
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("HeatNetwork.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$HeatNetworkSubtitle <- renderText({
    
    HeatNetwork <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = FALSE,
      skip = 11,
      n_max = 4
    )
    
    HeatNetworkYear <- tail(HeatNetwork, 2)[1,1]
    
    HeatNetwork <- as_tibble(t(HeatNetwork))
    
    HeatNetwork <- HeatNetwork[c(1, ncol(HeatNetwork) -1, ncol(HeatNetwork))]
    
    HeatNetwork <- HeatNetwork[complete.cases(HeatNetwork),]
    
    names(HeatNetwork) <- c("Type", "Latest", "Ambition")
    
    paste("Scotland,", HeatNetworkYear)
  })
  
  output$HeatNetworkPlot <- renderPlotly  ({
    
    HeatNetwork <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = FALSE,
      skip = 11,
      n_max = 4
    )
    
    HeatNetworkYear <- tail(HeatNetwork, 2)[1,1]
    
    HeatNetwork <- as_tibble(t(HeatNetwork))
    
    HeatNetwork <- HeatNetwork[c(1, ncol(HeatNetwork) -1, ncol(HeatNetwork))]
    
    HeatNetwork <- HeatNetwork[complete.cases(HeatNetwork),]
    
    names(HeatNetwork) <- c("Type", "Latest", "Ambition")
    
    HeatNetwork$Ambition <- as.numeric(HeatNetwork$Ambition) - as.numeric(HeatNetwork$Latest)
    
    HeatNetwork$Latest <- as.numeric(HeatNetwork$Latest)
    
    HeatNetwork$LatestProp <- HeatNetwork$Latest / (HeatNetwork$Latest + HeatNetwork$Ambition)
    
    HeatNetwork$AmbitionProp <- HeatNetwork$Ambition / (HeatNetwork$Latest + HeatNetwork$Ambition)
    
    HeatNetwork$Type <- str_wrap(paste0("<b>",HeatNetwork$Type,"</b>"), 28)
    
    p <-  plot_ly(HeatNetwork, 
                  y = ~Type, 
                  x = ~ `LatestProp`, 
                  type = 'bar', 
                  name = 'Latest',
                  hoverinfo = "text",
                  text = paste0("Latest: ", format(HeatNetwork$Latest, big.mark = ","), "\nProportion: ", percent(HeatNetwork$LatestProp, accuracy = 0.1)),
                  orientation = 'h',
                  marker = list(color = "#31a354")
    )%>%
      add_trace(x = ~ `AmbitionProp`,
                name = 'Ambition',
                text = paste0("Overall Ambition: ", format((HeatNetwork$Ambition + HeatNetwork$Latest) , big.mark = ","), "\n"),
                marker = list(color = "#addd8e")
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
  
  
  output$HeatNetworkTable = renderDataTable({
    
    HeatNetwork <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Heat networks",
      col_names = FALSE,
      skip = 11,
      n_max = 4
    )

    HeatNetwork <- HeatNetwork[c(1,2,4)]
    
    names(HeatNetwork) <- unlist(HeatNetwork[1,])
    
    names(HeatNetwork)[1] <- "Year"
    
    HeatNetwork <- HeatNetwork [-1,]
    
    datatable(
      HeatNetwork,
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
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/3 - Local Energy/HeatNetworks.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$HeatNetwork.png <- downloadHandler(
    filename = "HeatNetwork.png",
    content = function(file) {
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Heat networks", skip = 11, col_names = FALSE, n_max = 5)
      
      Data <- as_tibble(t(Data))
      
      Data <- Data[c(1, (ncol(Data)-1), (ncol(Data)))]
      names(Data) <- c("Type", "Current", "Ambition")
      
      Data <- Data[complete.cases(Data),]
      
      Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
      
      Data$Ambition <- Data$Ambition - Data$Current
      
      DistrictHeatNetworks <- Data
      
      DistrictHeatNetworksProportions <- DistrictHeatNetworks
      
      DistrictHeatNetworksProportions$Total <- DistrictHeatNetworksProportions$Current+DistrictHeatNetworksProportions$Ambition
      
      DistrictHeatNetworksProportions$Current <- DistrictHeatNetworksProportions$Current/DistrictHeatNetworksProportions$Total
      
      DistrictHeatNetworksProportions$Ambition <- DistrictHeatNetworksProportions$Ambition/DistrictHeatNetworksProportions$Total
      
      DistrictHeatNetworksProportions$Total <- NULL
      
      DistrictHeatNetworksProportions <- arrange(DistrictHeatNetworksProportions,-row_number())
      
      DistrictHeatNetworksProportions$Type <-
        factor(DistrictHeatNetworksProportions$Type,
               levels = unique(DistrictHeatNetworksProportions$Type),
               ordered = TRUE)
      
      DistrictHeatNetworksProportions <- melt(DistrictHeatNetworksProportions, id.vars = "Type")
      
      
      DistrictHeatNetworksProportions$variable <-
        factor(DistrictHeatNetworksProportions$variable,
               levels = rev(unique(DistrictHeatNetworksProportions$variable)),
               ordered = TRUE)
      
      DistrictHeatNetworksProportions <- DistrictHeatNetworksProportions %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      DistrictHeatNetworks <- arrange(DistrictHeatNetworks,-row_number())
      
      DistrictHeatNetworks$Type <-
        factor(DistrictHeatNetworks$Type,
               levels = unique(DistrictHeatNetworks$Type),
               ordered = TRUE)
      
      DistrictHeatNetworks <- melt(DistrictHeatNetworks, id.vars = "Type")
      
      
      DistrictHeatNetworks$variable <-
        factor(DistrictHeatNetworks$variable,
               levels = rev(unique(DistrictHeatNetworks$variable)),
               ordered = TRUE)
      
      DistrictHeatNetworks <- DistrictHeatNetworks %>%
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
      
      
      DistrictHeatNetworksProportionsChart <- DistrictHeatNetworksProportions %>%
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
            label = ifelse(variable == "Current", format(DistrictHeatNetworks$value, big.mark = ","), ""),
            fontface = 2
          ),
          colour = "white",
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            y = 1.15,
            label = ifelse(variable == "Current", paste0("2020 Ambition:\n",format(DistrictHeatNetworks$top, big.mark = ",")), ""),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            y = -.15,
            label = ifelse(variable == "Current", str_wrap(as.character(DistrictHeatNetworks$Type), width = 15), ""),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      DistrictHeatNetworksProportionsChart
      
      
      DistrictHeatNetworksProportionsChart <-
        StackedBars(DistrictHeatNetworksProportionsChart,
                    DistrictHeatNetworksProportions,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      DistrictHeatNetworksProportionsChart <-
        DistrictHeatNetworksProportionsChart +
        labs(subtitle = "Scotland, 2018") +
        ylim(-.25,1.22)+
        coord_flip()
      
      DistrictHeatNetworksProportionsChart
      
      ggsave(
        file,
        plot = DistrictHeatNetworksProportionsChart,
        width = 17.5,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
