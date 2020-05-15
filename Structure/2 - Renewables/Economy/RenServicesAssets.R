require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenServicesAssetsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Services",
    fluidRow(column(8,
                    h3("Value of renewable energy services", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenServicesSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenServices.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenServicesAssetsPlot")),
    plotlyOutput(ns("RenServicesPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Assets",
             fluidRow(column(8,
                             h3("Value of renewable energy assets", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenAssetsSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenAssets.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenServicesAssetsPlot")),
             plotlyOutput(ns("RenAssetsPlot"))%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               fluidRow(
    column(10, h3("Data - Value of renewable energy services and assets", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RenServicesTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("ONSNatural"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("ONSNatural")
        
      )
    )
  )
}




###### Server ######
RenServicesAssets <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenServicesAssets.R")

  
  output$RenServicesSubtitle <- renderText({
    
    Renewables <- read_delim("Processed Data/Output/Services and assets/Renewables.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

    paste0("Scotland, ", min(Renewables$Year), " - ", max(Renewables$Year))
  })
  
  output$RenServicesPlot <- renderPlotly  ({
    
    
    Renewables <- read_delim("Processed Data/Output/Services and assets/Renewables.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

    ### variables
    ChartColours <- c("#39ab2c", "#1d91c0", "#7fcdbb", "#8da0cb")
    sourcecaption = "Source: ONS, SG"
    plottitle = "Value of renewable energy services"
    
    
    p <-  plot_ly(Renewables,x = ~ Year ) %>% 
      add_trace(data = Renewables,
                x = ~ Year,
                y = ~ Annual,
                name = "Services",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Value of renewable energy services: \u00A3",
                  round(Renewables$Annual, digits = 1),
                  " billion\nYear: ",
                  paste(Renewables$Year)
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      )  %>% 
      add_trace(
        data = tail(Renewables[which(Renewables$Annual > 0 | Renewables$Annual < 0),], 1),
        x = ~ Year,
        y = ~ Annual,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Value of renewable energy services: \u00A3",
          round(Renewables[which(Renewables$Annual > 0 | Renewables$Annual < 0),][-1,]$Annual, digits = 1),
          " billion\nYear: ",
          paste(Renewables[which(Renewables$Annual > 0 | Renewables$Annual < 0),][-1,]$Year)
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
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "",
                     showgrid = FALSE),
        yaxis = list(
          title = "\u00A3 Billion",
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
  
  output$RenServicesTable = renderDataTable({
    
    Renewables <- read_delim("Processed Data/Output/Services and assets/Renewables.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(Renewables) <- c("Year", "Value of renewable energy services (\u00A3 bn)", "Value of renewable energy assets (\u00A3 bn)")
    
    datatable(
      Renewables,
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
        title = "Value of renewable energy services and assets (\u00A3 billion)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Value of renewable energy services and assets (\u00A3 billion)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Value of renewable energy services and assets (\u00A3 billion)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:3), 1) 
  })
    output$RenServices.png <- downloadHandler(
    filename = "RenServicesAssets.png",
    content = function(file) {


      Renewables <- read_delim("Processed Data/Output/Services and assets/Renewables.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      ### variables
      ChartColours <- c("#39ab2c", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: ONS, SG"
      plottitle = "Value of renewable energy services"
      
      #Renewables$OilPercentage <- PercentLabel(Renewables$Oil)
      
      
      RenewablesChart <- Renewables %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Annual,
            colour = ChartColours[2],
            label = percent(Annual, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Annual,
            label = ifelse(Year == min(Year), paste0("\u00A3", format(round(Annual, digits = 1),nsmall = 1, trim = TRUE), " billion"), ""),
            hjust = 1.1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Annual,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(Annual, digits = 1),nsmall = 1, trim = TRUE), " billion"), ""),
            hjust = 0.5,
            vjust = -1.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(Renewables, 1),
          aes(
            x = Year,
            y = Annual,
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
                             Year == min(Year), paste0(Year), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      RenewablesChart <-
        LinePercentChart(RenewablesChart,
                         Renewables,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      RenewablesChart <- RenewablesChart +
        xlim(min(Renewables$Year)-1, max(Renewables$Year)+1)+
        ylim((max(Renewables$Annual)*-0.04), max(Renewables$Annual)*1.05)+
        labs(subtitle = paste0("Scotland, ",min(Renewables$Year), " - ", max(Renewables$Year)+1))
      
      RenewablesChart
      
      ggsave(
        file,
        plot =  RenewablesChart,
        width = 26,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
      
    }
  ) 
    
    observeEvent(input$ToggleTable, {
    toggle("RenServicesTable")
  })

    
    
    
    output$RenAssetsSubtitle <- renderText({
      
      Renewables <- read_delim("Processed Data/Output/Services and assets/Renewables.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      paste0("Scotland, ", min(Renewables$Year), " - ", max(Renewables$Year))
    })
    
    output$RenAssetsPlot <- renderPlotly  ({
      
      
      Renewables <- read_delim("Processed Data/Output/Services and assets/Renewables.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      ### variables
      ChartColours <- c("#39ab2c", "#1d91c0", "#7fcdbb", "#8da0cb")
      sourcecaption = "Source: ONS, SG"
      plottitle = "Value of renewable energy Assets"
      
      
      p <-  plot_ly(Renewables,x = ~ Year ) %>% 
        add_trace(data = Renewables,
                  x = ~ Year,
                  y = ~ Asset,
                  name = "Asset",
                  type = 'scatter',
                  mode = 'lines',
                  legendgroup = "1",
                  text = paste0(
                    "Value of renewable energy assets: \u00A3",
                    round(Renewables$Asset, digits = 1),
                    " billion\nYear: ",
                    paste(Renewables$Year)
                  ),
                  hoverinfo = 'text',
                  line = list(width = 6, color = ChartColours[1], dash = "none")
        )  %>% 
        add_trace(
          data = tail(Renewables[which(Renewables$Asset > 0 | Renewables$Asset < 0),], 1),
          x = ~ Year,
          y = ~ Asset,
          legendgroup = "1",
          name = "Total",
          text = paste0(
            "Value of renewable energy assets: \u00A3",
            round(Renewables[which(Renewables$Asset > 0 | Renewables$Asset < 0),][-1,]$Asset, digits = 1),
            " billion\nYear: ",
            paste(Renewables[which(Renewables$Asset > 0 | Renewables$Asset < 0),][-1,]$Year)
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
          legend = list(font = list(color = "#39ab2c"),
                        orientation = 'h'),
          hoverlabel = list(font = list(color = "white"),
                            hovername = 'text'),
          hovername = 'text',
          
          xaxis = list(title = "",
                       showgrid = FALSE),
          yaxis = list(
            title = "\u00A3 Billion",
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
    
    output$RenAssetsTable = renderDataTable({
      
      Renewables <- read_delim("Processed Data/Output/Services and assets/Renewables.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      names(Renewables) <- c("Year", "Value of renewable energy Assets (\u00A3 bn)", "Value of renewable energy assets (\u00A3 bn)")
      
      datatable(
        Renewables,
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
          title = "Oil and gas revenue (\u00A3 billion)",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Oil and gas revenue (\u00A3 billion)',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Oil and gas revenue (\u00A3 billion)')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = 10
        )
      ) %>%
        formatRound(c(2:3), 1) 
    })
    output$RenAssets.png <- downloadHandler(
      filename = "RenAssetsAssets.png",
      content = function(file) {
        
        
        Renewables <- read_delim("Processed Data/Output/Services and assets/Renewables.txt", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
        
        ### variables
        ChartColours <- c("#39ab2c", "#66c2a5", "#fc8d62", "#8da0cb")
        sourcecaption = "Source: ONS, SG"
        plottitle = "Value of renewable energy Assets"
        
        #Renewables$OilPercentage <- PercentLabel(Renewables$Oil)
        
        
        RenewablesChart <- Renewables %>%
          ggplot(aes(x = Year), family = "Century Gothic") +
          
          geom_line(
            aes(
              y = Asset,
              colour = ChartColours[2],
              label = percent(Asset, 0.1)
            ),
            size = 1.5,
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              y = Asset,
              label = ifelse(Year == min(Year), paste0("\u00A3", format(round(Asset, digits = 1),nsmall = 1, trim = TRUE), " billion"), ""),
              hjust = 1.1,
              colour = ChartColours[2],
              fontface = 2
            ),
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              y = Asset,
              label = ifelse(Year == max(Year), paste0("\u00A3", format(round(Asset, digits = 1),nsmall = 1, trim = TRUE), " billion"), ""),
              hjust = 0.5,
              vjust = -1.5,
              colour = ChartColours[2],
              fontface = 2
            ),
            family = "Century Gothic"
          ) +
          geom_point(
            data = tail(Renewables, 1),
            aes(
              x = Year,
              y = Asset,
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
                               Year == min(Year), paste0(Year), ""),
              hjust = 0.5,
              vjust = 1.5,
              fontface = 2
            ),
            colour = ChartColours[1],
            family = "Century Gothic"
          )
        
        
        RenewablesChart <-
          LinePercentChart(RenewablesChart,
                           Renewables,
                           plottitle,
                           sourcecaption,
                           ChartColours)
        
        RenewablesChart <- RenewablesChart +
          xlim(min(Renewables$Year)-1, max(Renewables$Year))+
          ylim((max(Renewables$Asset)*-0.04), max(Renewables$Asset)*1.05)+
          labs(subtitle = paste0("Scotland, ",min(Renewables$Year), " - ", max(Renewables$Year)+1))
        
        RenewablesChart
        
        ggsave(
          file,
          plot =  RenewablesChart,
          width = 26,
          height = 12,
          units = "cm",
          dpi = 300
        )
        
        
      }
    ) 
    
    observeEvent(input$ToggleTable, {
      toggle("RenAssetsTable")
    })
    
 output$Text <- renderUI({
   tagList(column(12,
                                   
                                     HTML(
                                       paste(readtext("Structure/2 - Renewables/Economy/RenServicesAssets.txt")[2])
                                     
                                   )))
 })
 
 
 
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  

}
