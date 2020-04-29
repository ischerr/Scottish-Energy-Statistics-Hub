require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RestrictedPPMOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
    tabPanel("Economy 7",
    fluidRow(column(8,
                    h3("Proportion of Economy 7 meters by local authority", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('RestrictedMeterSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RestrictedMeter.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #imageOutput(ns("RestrictedMeterPlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
    leafletOutput(ns("leaflettest"), height = "800px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Time Series",
             fluidRow(column(8,
                             h3("Proportion of restricted meters in Scotland", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('RestrictedMetersPropSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RestrictedMetersProp.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("RestrictedMeterPlot")),
             plotlyOutput(ns("RestrictedMetersPropPlot"))%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
   
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      tabPanel("Economy 7",
    fluidRow(
    column(10, h3("Data - Proportion of Economy 7 meters by local authority", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RestrictedMeterTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISElecPPM", "BEISElecMeter"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISElecMeter")
        
      )
    )
  )
}




###### Server ######
RestrictedPPM <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RestrictedPPM.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RestrictedMeterSubtitle <- renderText({
    
    paste("Scotland, 2018")
  
    })
  
  output$RestrictedMeterPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/5 - Consumers/RestrictedMeterOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$RestrictedMeterTable = renderDataTable({
    
    RestrictedMeter <- read_delim("Processed Data/Output/Restricted Meters/RestrictedMetersProp.txt", 
                                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(RestrictedMeter) <- names(read_delim("Processed Data/Output/Restricted Meters/RestrictedMeters.txt", 
                                               "\t", escape_double = FALSE, trim_ws = TRUE))
    
    RestrictedMeter[34,1] <- "All Scotland"

    datatable(
      RestrictedMeter[-33,],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Proportion of Economy 7 meters by local authority",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of Economy 7 meters by local authority',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of Economy 7 meters by local authority')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:100, 1)
  })
  
  output$PrepaymentMeterTable = renderDataTable({
    
    RestrictedMeter <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Restricted and PPM",
      skip = 15
    )[c(1,2,6,7)]
    
    names(RestrictedMeter) <- c("LA Code", "Local Authority", "Pre-payment Meters", "Proportion of total meters")
    
    datatable(
      RestrictedMeter[c(2,1,3,4)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Economy 7 meters by Local Authority",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Economy 7 meters by Local Authority',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Economy 7 meters by Local Authority')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(4, 1) %>% 
      formatRound(3, 0)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/RestrictedPPM.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("RestrictedMeterTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("PrepaymentMeterTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RestrictedMeter.png <- downloadHandler(
    filename = "RestrictedMeter.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/RestrictedMeterChart.png"), file) 
    }
  )
  
  output$PPMMeterSubtitle <- renderText({
    
    paste("Scotland, 2017")
    
  })
  
  output$PPMMeterPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    writePNG(readPNG("Structure/5 - Consumers/PPMMeterOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$PPMMeter.png <- downloadHandler(
    filename = "PPMMeter.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/PPMMeterChart.png"), file) 
    }
  )
  
  output$RestrictedMetersPropPlot <- renderPlotly  ({
    
    RestrictedMetersProp <- read_delim("Processed Data/Output/Restricted Meters/RestrictedMetersProp.txt", 
                                       "\t", escape_double = FALSE, col_names = FALSE, 
                                       trim_ws = TRUE)
    
    
    
    RestrictedMetersProp <- as_tibble(t(RestrictedMetersProp[which(RestrictedMetersProp[2] == "LA Code" | RestrictedMetersProp[2] == "S92000003"),]))
    
    RestrictedMetersProp <- tail(RestrictedMetersProp, -2)
    
    names(RestrictedMetersProp) <- c("Year", "Proportion")
    
    RestrictedMetersProp$Year <- as.numeric(as.character(RestrictedMetersProp$Year))  
    
    RestrictedMetersProp$Proportion <- as.numeric(as.character(RestrictedMetersProp$Proportion))
    
    plottitle <-
      "Coal Proportion (million tonnes)"
    sourcecaption <- "Source: BEIS"
    
    ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
    BarColours <-
      c(    "#0868ac","#43a2ca","#7bccc4"
      )
    
    p <-  plot_ly(RestrictedMetersProp,x = ~ Year ) %>% 
      add_trace(data = RestrictedMetersProp,
                x = ~ Year,
                y = ~ Proportion,
                name = "Proportion",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Proportion: ",
                  percent(RestrictedMetersProp$Proportion, accuracy = 0.1),
                  "\nYear: ",
                  paste(RestrictedMetersProp$Year)
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      )  %>% 
      add_trace(
        data = tail(RestrictedMetersProp[which(RestrictedMetersProp$Proportion > 0 | RestrictedMetersProp$Proportion < 0),], 1),
        x = ~ Year,
        y = ~ Proportion,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Proportion: ",
          percent(RestrictedMetersProp[which(RestrictedMetersProp$Proportion > 0 | RestrictedMetersProp$Proportion < 0),][-1,]$Proportion, accuracy = 0.1),
          "\nYear: ",
          paste(RestrictedMetersProp[which(RestrictedMetersProp$Proportion > 0 | RestrictedMetersProp$Proportion < 0),][-1,]$Year)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      )  %>%  
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "",
                     showgrid = FALSE),
        yaxis = list(
          title = "",
          tickformat = "%",
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
  
  output$RestrictedMetersProp.png <- downloadHandler(
    filename = "RestrictedMetersProp.png",
    content = function(file) {
      
      
      RestrictedMetersProp <- read_delim("Processed Data/Output/Restricted Meters/RestrictedMetersProp.txt", 
                                         "\t", escape_double = FALSE, col_names = FALSE, 
                                         trim_ws = TRUE)
      
      
      
      RestrictedMetersProp <- as_tibble(t(RestrictedMetersProp[which(RestrictedMetersProp[2] == "LA Code" | RestrictedMetersProp[2] == "S92000003"),]))
      
      RestrictedMetersProp <- tail(RestrictedMetersProp, -2)
      
      names(RestrictedMetersProp) <- c("Year", "Proportion")
      
      RestrictedMetersProp$Year <- as.numeric(as.character(RestrictedMetersProp$Year))  
      
      RestrictedMetersProp$Proportion <- as.numeric(as.character(RestrictedMetersProp$Proportion))
      
      plottitle <-
        "Proportion of Economy 7 meters in Scotland"
      sourcecaption <- "Source: BEIS"
      
      
      ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
      BarColours <-
        c(    "#0868ac","#43a2ca","#7bccc4"
        )
      
      
      RestrictedMetersPropChart <- RestrictedMetersProp %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
      geom_line(
        aes(
          y = Proportion,
          colour = ChartColours[2],
          label = percent(Proportion, 0.1)
        ),
        size = 1.5,
        family = "Century Gothic"
      ) +
        geom_text(
          aes(
            x = Year,
            y = Proportion,
            label = ifelse(Year == min(Year), percent(Proportion, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = 2.2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Proportion,
            label = ifelse(Year == max(Year), percent(Proportion, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(RestrictedMetersProp, 1),
          aes(
            x = Year,
            y = Proportion,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
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
      
      RestrictedMetersPropChart <-
        LinePercentChart(RestrictedMetersPropChart,
                         RestrictedMetersProp,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      RestrictedMetersPropChart
      
      ggsave(
        file,
        plot = RestrictedMetersPropChart,
        width = 12.5,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
      
})
  
  output$RestrictedMetersPropSubtitle <- renderText({
    
    RestrictedMetersProp <- read_delim("Processed Data/Output/Restricted Meters/RestrictedMetersProp.txt", 
                                       "\t", escape_double = FALSE, col_names = FALSE, 
                                       trim_ws = TRUE)
    
    
    
    RestrictedMetersProp <- as_tibble(t(RestrictedMetersProp[which(RestrictedMetersProp[2] == "LA Code" | RestrictedMetersProp[2] == "S92000003"),]))
    
    RestrictedMetersProp <- tail(RestrictedMetersProp, -2)
    
    names(RestrictedMetersProp) <- c("Year", "Proportion")
    
    RestrictedMetersProp$Year <- as.numeric(as.character(RestrictedMetersProp$Year))  
    
    paste0("Scotland, ", min(RestrictedMetersProp$Year)," - ",  max(RestrictedMetersProp$Year))
  })
  
  output$leaflettest <- renderLeaflet({
    
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
    
    print("MapsandGasGrid")
    
    # This is unlikely to change from 2012
    yearstart <- 2012
    
    ### Set the final year for the loop as the current year ###
    yearend <- format(Sys.Date(), "%Y")
    
    
    ### Add Simplified shape back to the Shapefile
    LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")
    
    LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
    ############ RENEWABLE ELECTRICITY ################################################
    
    EconomyMeter <- read_delim("Processed Data/Output/Restricted Meters/RestrictedMetersProp.txt", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)
    
    EconomyMeter <- EconomyMeter[c(1,2,ncol(EconomyMeter))]
    
    names(EconomyMeter) <- c("LocalAuthority", "CODE", "Meters")
    
    EconomyMeter <- EconomyMeter[which(substr(EconomyMeter$CODE, 1,3)== "S12"),]
    
    EconomyMeter$Content <- paste0("<b>",EconomyMeter$LocalAuthority, "</b><br/>Proportion of Economy 7 Meters:<br/><em>", percent(EconomyMeter$Meters, 0.1),"</em>" )
    
    EconomyMeter$Hover <- paste0(EconomyMeter$LocalAuthority, " - ", percent(EconomyMeter$Meters, 0.1))
    
    EconomyMeter$Meters <- EconomyMeter$Meters*100
    
    
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    EconomyMeter <- EconomyMeter[order(EconomyMeter$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      append_data(LA, EconomyMeter, key.shp = "CODE", key.data = "CODE")
  
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = LAMap$Meters)
    
    leaflet(LAMap) %>% 
      addProviderTiles("Thunderforest.TransportDark", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(Meters),
                  highlightOptions = list(color = "white", weight = 2,
                                                             bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~Meters,
                         title = "Proportion of<br/>Economy 7 Meters",
                         labFormat = labelFormat(suffix = "%"),
                         opacity = 1
      ) 
    
  }) 
  
  
}
