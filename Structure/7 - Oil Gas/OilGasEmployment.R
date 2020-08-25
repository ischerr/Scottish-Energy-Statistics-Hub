require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



OilGasEmploymentOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Scottish Employment",
     fluidRow(column(8,
                             h3("Scottish Oil and gas employment", style = "color: #126992;  font-weight:bold"),
                             h4(textOutput(ns('OilGasEmploymentSubtitle')), style = "color: #126992;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('OilGasEmployment.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
             #dygraphOutput(ns("OilGasEmploymentPlot")),
             plotlyOutput(ns("OilGasEmploymentPlot"), height = "600px")%>% withSpinner(color="#126992"),
             tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;")),
     tabPanel("UK Regional Employment",
              fluidRow(column(8,
                              h3("UK Oil and gas employment by region", style = "color: #126992;  font-weight:bold"),
                              h4(textOutput(ns('OilGasEmploymentRegionSubtitle')), style = "color: #126992;")
              ),
              column(
                4, style = 'padding:15px;',
                downloadButton(ns('OilGasEmploymentRegionMap.png'), 'Download Graph', style="float:right")
              )),
              
              tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
              #dygraphOutput(ns("OilGasEmploymentPlot")),
              leafletOutput(ns("OilGasEmploymentRegionMapPlot"), height = "750px")%>% withSpinner(color="#126992"),
              tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #126992;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    tabsetPanel(
      tabPanel("Employment",
  fluidRow(
    column(10, h3("Data - Scottish Oil and gas employment", style = "color: #126992;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("OilGasEmploymentTable"))%>% withSpinner(color="#126992"))),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;")),
  tabPanel("Regional",
           fluidRow(
             column(10, h3("Data - UK Oil and gas employment by region", style = "color: #126992;  font-weight:bold")),
             column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
           ),
           fluidRow(
             column(12, dataTableOutput(ns("OilGasEmploymentUKTable"))%>% withSpinner(color="#126992"))),
           tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("OGUKWorkforce"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("OGUKWorkforce")
        
      )
    )
  )
}




###### Server ######
OilGasEmployment <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("OilGasEmployment.R")

  
  output$OilGasEmploymentSubtitle <- renderText({
    
    paste("Scotland, 2017")

      })
  
  output$OilGasEmploymentPlot <- renderPlotly  ({
    
    
    ChartColours <- c("#126992", "#FF8500")
    BarColours <- c("#034e7b", "#0570b0", "#969696", "#f46d43", "#d73027")
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Oil and gas employment", col_names = FALSE, 
        skip = 12,
        n_max = 6)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data <- Data[complete.cases(Data),]
    
    Data$YearFormat <- paste0("<b>", Data$Year, "</b>")
    
    p <-  plot_ly(Data, y = ~ YearFormat ) %>%  
      add_trace(x = ~ `Total employment - Scotland only`, 
                orientation = 'h',
                name = "Total employment - Scotland only",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  "Total employment - Scotland only: ", format(round(Data$`Total employment - Scotland only`, 0.1), big.mark = ","),"\n",
                  "Year: ", Data$Year, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[1])
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
                     tickformat = "",
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(
          title = "",
          tickformat = ",d",
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
  
  output$OilGasEmploymentTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Oil and gas employment", col_names = FALSE, 
        skip = 12,
        n_max = 6)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data[nrow(Data),1] <- Data[nrow(Data)-1,1] + 1
    
    names(Data) <- c("Year", "UK - Direct", "UK - Indirect", "UK - Induced", "UK - Total", "Total employment - Scotland only")
    
    Data<- Data[seq(dim(Data)[1],1),]
    
    datatable(
      Data[c(1,6,2:5)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Scottish Oil and gas employment",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Scottish Oil and gas employment",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Scottish Oil and gas employment")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:6, 0)
  })
  
  output$OilGasEmploymentRegionSubtitle <- renderText({
    
    paste("2018")
    
  })
  
  output$OilGasEmploymentRegionPlot <- renderPlotly  ({
    
    
    ChartColours <- c("#126992", "#FF8500")
    BarColours <- c("#034e7b", "#0570b0", "#969696", "#f46d43", "#d73027")
    
    OilGasEmployment <- read_excel("Structure/7 - Oil Gas/RegionalOilGasEmployment.xlsx")
    
    names(OilGasEmployment) <- c("Region", "Renewables")
    
    OilGasEmployment <- OilGasEmployment[order(OilGasEmployment$Renewables),]
    
    OilGasEmployment$RegionFormat <- paste0("<b>",OilGasEmployment$Region, "</b>")
    
    p <-  plot_ly(OilGasEmployment, y = ~ RegionFormat ) %>%  
      add_trace(x = ~ `Renewables`, 
                orientation = 'h',
                name = "Renewables",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  percent(OilGasEmployment$`Renewables`, 0.1),"\n",
                  OilGasEmployment$Region, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[1])
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
          ticktext = as.list(OilGasEmployment$`Region`),
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
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/7 - Oil Gas/OilGasEmployment.txt")[2])
                     
                   )))
  })
  
  
 observeEvent(input$ToggleTable1, {
    toggle("OilGasEmploymentTable")
  })
 
 observeEvent(input$ToggleTable2, {
   toggle("OilGasEmploymentUKTable")
 })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$OilGasEmployment.png <- downloadHandler(
    filename = "OilGasEmployment.png",
  content = function(file) {
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Oil and gas employment", skip = 12, col_names = FALSE)[c(1,6),]
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- c("Year", "Employment")
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data <- Data[complete.cases(Data), ]
    
    OilGasEmployment <- Data
    
    plottitle <-
      "Oil and gas employment"
    sourcecaption <- "Source: Oil and gas UK"
    
    ChartColours <- c("#126992", "#FF8500")
    BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
    
    
    OilGasEmploymentChart <- OilGasEmployment %>%
      ggplot(aes(x = Year, y = Employment), family = "Century Gothic") +
      geom_bar(stat = "identity", width = .8, fill = ChartColours[1]) +
      geom_text(
        aes(
          y = -1000,
          label = Year,
          hjust = 1
        ),
        family = "Century Gothic",
        colour = ChartColours[1],
        fontface = 2
      ) +
      geom_text(
        aes(
          y = Employment - 1000,
          label = format(Employment, big.mark = ","),
          hjust = 1
        ),
        family = "Century Gothic",
        colour = "white",
        fontface = 2
      )
    
    
    OilGasEmploymentChart
    
    
    OilGasEmploymentChart <-
      BaselineChart(OilGasEmploymentChart,
                    OilGasEmployment,
                    plottitle,
                    sourcecaption,
                    ChartColours)
    
    OilGasEmploymentChart <-
      OilGasEmploymentChart +
      scale_x_reverse()+
      coord_flip() +
      labs(subtitle = paste("Scotland,", min(OilGasEmployment$Year), "-", max(OilGasEmployment$Year)))+
      ylim(-4000,max(OilGasEmployment$Employment))
    
    
    OilGasEmploymentChart
    
    ggsave(
      file,
      plot = OilGasEmploymentChart,
      width = 16,
      height = 8.5,
      units = "cm",
      dpi = 300
    )
    
  }
) 
  
  
  output$OilGasEmploymentRegion.png <- downloadHandler(
    filename = "OilGasEmploymentRegion.png",
    content = function(file) {
      
      OilGasEmployment <- read_excel("Structure/7 - Oil Gas/RegionalOilGasEmployment.xlsx")
      
      names(OilGasEmployment) <- c("Region", "Renewables")
      
      OilGasEmployment <- OilGasEmployment[order(OilGasEmployment$Renewables),]
      ### variables
      ChartColours <- c("#126992", "#238b45", "#a1d99b")
      sourcecaption = "Source: Oil and Gas UK"
      plottitle = "Oil and gas employment by region"
      
      
      
      
      OilGasEmployment$Region <-
        factor(OilGasEmployment$Region, levels = OilGasEmployment$Region)
      
      OilGasEmploymentChart <-
        OilGasEmployment %>%  ggplot(aes(x = Region, y = Renewables)) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.18, .45) +
        geom_bar(stat = "identity", fill = ChartColours[1]) +
        coord_flip() +
        geom_text(
          y = -0.02,
          label = OilGasEmployment$Region,
          fontface = 2,
          family = "Century Gothic",
          hjust = 1.1,
          vjust = .5,
          color = ChartColours[1]
        ) +
        geom_text(
          y = OilGasEmployment$Renewables + 0.05,
          label = percent(OilGasEmployment$Renewables, accuracy = 1),
          fontface = 2,
          family = "Century Gothic",
          hjust = 1.1,
          vjust = .5,
          color = ChartColours[1]
        ) +
        theme(
          text = element_text(family = "Century Gothic")
          ,
          panel.background = element_rect(fill = "transparent") # bg of the panel
          ,
          plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
          ,
          legend.background = element_rect(fill = "transparent") # get rid of legend bg
          ,
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
          ,
          legend.title = ggplot2::element_blank()
          ,
          axis.text.x = element_blank()
          ,
          axis.text.y = element_blank()
          ,
          axis.title = ggplot2::element_blank()
          ,
          legend.text = element_text(colour = "black", family = "Century Gothic")
          ,
          axis.ticks = ggplot2::element_blank()
          ,
          panel.grid.major = ggplot2::element_blank()
          ,
          legend.position = "none"
          ,
          title = element_text(colour = ChartColours[1], size = 14)
          ,
          plot.title = ggplot2::element_text(face = "bold")
        ) + ### Label Plot
        labs(y = "Percentage", caption = sourcecaption) +
        labs(title = plottitle,
             face = "bold",
             subtitle = 2018) +
        ### 0 Axis
        
        #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
        
        
        ### Plot Borders
        annotate(
          geom = 'segment',
          x = Inf,
          xend = Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1.5
        ) +
        annotate(
          geom = 'segment',
          x = -Inf,
          xend = -Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1
        ) 
      
      OilGasEmploymentChart
      
      ggsave(
        file,
        plot =  OilGasEmploymentChart,
        width = 16,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
    }
  ) 
  
  output$OilGasEmploymentUKTable = renderDataTable({
    
    OilGasEmployment <- read_excel("Structure/7 - Oil Gas/RegionalOilGasEmployment.xlsx")
    
    names(OilGasEmployment) <- c("Region", "Renewables")
    
    datatable(
      OilGasEmployment,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Scottish Oil and gas employment",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Scottish Oil and gas employment",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Scottish Oil and gas employment")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2, 0)
  })
  
  output$OilGasEmploymentRegionMapPlot <- renderLeaflet({
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
    LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/NUTS1Simple.shp")
    
    LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
    ############ RENEWABLE ELECTRICITY ################################################
    
    AverageBillMap <- read_excel("Structure/7 - Oil Gas/RegionalOilGasEmploymentforMap.xlsx")
    
    AverageBillMap$Content <- paste0("<b>",AverageBillMap$Name, "</b><br/>Employment:<br/><em>", percent(AverageBillMap$Amount),"</em>" )
    
    AverageBillMap$Hover <- paste0(AverageBillMap$Name, " - ", percent(AverageBillMap$Amount))
    
    AverageBillMap$Amount <- AverageBillMap$Amount *100
    
    ### Change LA$nuts118cd to string
    LA$nuts118cd <- as.character(LA$nuts118cd)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$nuts118cd),]
    
    ### Order LAs in Data
    AverageBillMap <- AverageBillMap[order(AverageBillMap$nuts118cd),]
    
    ### Combine Data with Map data
    LAMap <-
      append_data(LA, AverageBillMap, key.shp = "nuts118cd", key.data = "nuts118cd")
    
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = LAMap$Amount)
    
    l <-leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(Amount),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~Amount, labFormat = labelFormat(suffix = "%"),
                         title = "Proportion employed",
                         opacity = 1
      ) 
    
    l
    
  })
    
  output$OilGasEmploymentRegionMap.png <- downloadHandler(
    filename = "OilGasEmploymentRegionMap.png",
    content = function(file) {
      
      writePNG(readPNG("Structure/7 - Oil Gas/OilGasEmploymentMap.png"), file) 
      
    }
  ) 

}
    
    