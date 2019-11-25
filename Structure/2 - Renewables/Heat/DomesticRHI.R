require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

DomesticRHIOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Capacity",
    fluidRow(column(8,
                    h3("Renewable heat capacity by technology type", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('DomesticRHISubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('DomesticRHI.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("DomesticRHIPlot")),
    plotlyOutput(ns("DomesticRHIPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Output",
             fluidRow(column(8,
                             h3("Renewable heat output by technology type", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('DomRHIInstallationsOutputSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('DomRHIInstallationsOutput.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("DomesticRHIPlot")),
             plotlyOutput(ns("DomRHIInstallationsOutputPlot"))%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Capacity",
    fluidRow(
    column(10, h3("Data - Capacity (GW)", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("DomesticRHITable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Output",
      fluidRow(
        column(10, h3("Data - Output (GWh)", style = "color: #39ab2c;  font-weight:bold")),
        column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
      ),
      fluidRow(
        column(12, dataTableOutput(ns("DomRHIInstallationsOutputTable"))%>% withSpinner(color="#39ab2c"))),
      tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
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
        SourceLookup("BEISFinalConsump"),
        SourceLookup("ETElecGen"),
        SourceLookup("ESTDomRHIInstallations")
        
      )
    )
  )
}




###### Server ######
DomesticRHI <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("DomesticRHI.R")

  
  output$DomesticRHISubtitle <- renderText({
    
    paste("Scotland, Apr 2014 - June 2019")
  })
  
  output$DomesticRHIPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Domestic RHI", 
        skip = 13, n_max = 5)
    
    Data <- Data[1:4,c(1,3,5)]
    
    names(Data) <- c("Tech", "Heat Paid For", "Installations recieving payment")
    
    Data$TechLabel <- Data$Tech
    
    Data$Tech <- paste0("<b>",Data$Tech,"</b>")
    
    Data$Tech <- str_wrap(Data$Tech, 16)
    
    p <-  plot_ly(Data, 
                  y = ~Tech, 
                  x = ~ `Heat Paid For`, 
                  type = 'bar', 
                  name = 'Heat Paid For',
                  hoverinfo = "text",
                  text = paste0("Heat paid for: ",percent(Data$`Heat Paid For`, accuracy = .1)),
                  orientation = 'h',
                  marker = list(color = "#31a354")
                  )%>%
      add_trace(x = ~`Installations recieving payment`, 
                name = 'Installations recieving payment',
                text = paste0("Installations recieving payment: ",percent(Data$`Installations recieving payment`, accuracy = .1)),
                marker = list(color = "#addd8e")
                ) %>% 
      layout(
        barmode = 'group',
        bargap = 0.25,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
                     showgrid = TRUE,
                     range = c(-0.01, max(Data$`Heat Paid For`)+0.1),
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
  
  output$DomRHIInstallationsOutputSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Domestic RHI", col_names = FALSE, 
        skip = 58)
  Data <- Data[complete.cases(Data),]
     
names(Data)[1] <- "Year"
    
    paste("Scotland,", format(min(Data$Year), "%b %Y"),"-", format(max(Data$Year), "%b %Y"))
  })
  
  output$DomRHIInstallationsOutputPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Domestic RHI", col_names = FALSE, 
        skip = 58)
    Data <- Data[complete.cases(Data),][c(1,3,5,7,9,11)]
    
    names(Data) <- c("Year", "Air Source", "Ground Source", "Biomass", "Solar Thermal", "Total")
    
    Data[2:ncol(Data)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    DomRHIInstallationsOutputTech <- Data[order(Data$Year),]
    
    LineColours <- c("#39ab2c","#ef3b2c","#fb6a4a","#fc9272","#fcbba1")
    
    p <-  plot_ly(DomRHIInstallationsOutputTech, x = ~ Year ) %>%  
      add_trace(y = ~ `Total`,
                name = "Total",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Total: ",
                  format(round(DomRHIInstallationsOutputTech$`Total`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(DomRHIInstallationsOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(y = ~ `Air Source`,
                name = "Air Source",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Air Source: ",
                  format(round(DomRHIInstallationsOutputTech$`Air Source`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(DomRHIInstallationsOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[2], dash = "none")
      ) %>% 
      add_trace(y = ~ `Ground Source`,
                name = "Ground Source",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Ground Source: ",
                  format(round(DomRHIInstallationsOutputTech$`Ground Source`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(DomRHIInstallationsOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[3], dash = "none")
      ) %>% 
      add_trace(y = ~ `Biomass`,
                name = "Biomass",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "Biomass: ",
                  format(round(DomRHIInstallationsOutputTech$`Biomass`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(DomRHIInstallationsOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[4], dash = "none")
      ) %>% 
      add_trace(y = ~ `Solar Thermal`,
                name = "Solar Thermal",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "5",
                text = paste0(
                  "Solar Thermal: ",
                  format(round(DomRHIInstallationsOutputTech$`Solar Thermal`, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(DomRHIInstallationsOutputTech$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[5], dash = "none")
      ) %>% 
      add_trace(
        data = tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Total` != 0),], 1),
        x = ~ Year,
        y = ~ `Total`,
        name = "Total",
        legendgroup = "1",
        text = paste0(
          "Total: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Total` != 0),], 1)$`Total`, big.mark = ","),
          " GWh\nYear: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Total` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>%
      add_trace(
        data = tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Air Source` != 0),], 1),
        x = ~ Year,
        y = ~ `Air Source`,
        name = "Air Source",
        legendgroup = "2",
        text = paste0(
          "Air Source: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Air Source` != 0),], 1)$`Air Source`, big.mark = ","),
          " GWh\nYear: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Air Source` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[2])
      ) %>% 
      add_trace(
        data = tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Ground Source` != 0),], 1),
        x = ~ Year,
        y = ~ `Ground Source`,
        name = "Ground Source",
        legendgroup = "3",
        text = paste0(
          "Ground Source: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Ground Source` != 0),], 1)$`Ground Source`, big.mark = ","),
          " GWh\nYear: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Ground Source` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[3])
      ) %>% 
      add_trace(
        data = tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Biomass` != 0),], 1),
        x = ~ Year,
        y = ~ `Biomass`,
        name = "Biomass",
        legendgroup = "4",
        text = paste0(
          "Biomass: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Biomass` != 0),], 1)$`Biomass`, big.mark = ","),
          " GWh\nYear: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Biomass` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[4])
      ) %>% 
      add_trace(
        data = tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Solar Thermal` != 0),], 1),
        x = ~ Year,
        y = ~ `Solar Thermal`,
        name = "Solar Thermal",
        legendgroup = "5",
        text = paste0(
          "Solar Thermal: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Solar Thermal` != 0),], 1)$`Solar Thermal`, big.mark = ","),
          " GWh\nYear: ",
          format(tail(DomRHIInstallationsOutputTech[which(DomRHIInstallationsOutputTech$`Solar Thermal` != 0),], 1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[5])
      ) %>% 
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
          title = "GWh",
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
  
  
  output$DomesticRHITable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 7)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar", "Total")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    DomesticRHI <- Data
    
    datatable(
      DomesticRHI,
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
        title = "Renewable heat capacity by technology type (GW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Renewable heat capacity by technology type (GW)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Renewable heat capacity by technology type (GW)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:7), 3)
  })
  
  output$DomRHIInstallationsOutputTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 7)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar", "Total")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- arrange(Data, -row_number())
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    DomRHIInstallationsOutputTech <- Data
    
    datatable(
      DomRHIInstallationsOutputTech,
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
        title = "Renewable heat output by technology type (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Renewable heat output by technology type (GWh)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Renewable heat output by technology type (GWh)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:7), 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Heat/DomesticRHI.txt")[2])
                     
                   )))
  })
  
 observeEvent(input$ToggleTable1, {
    toggle("DomesticRHITable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("DomRHIInstallationsOutputTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$DomesticRHI.png <- downloadHandler(
    filename = "DomesticRHI.png",
    content = function(file) {

      Data <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Renewable heat by tech type", col_names = FALSE, 
          skip = 13, n_max = 6)
      
      Data <- as.data.frame(t(Data))
      names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
      Data <- Data[complete.cases(Data),]
      
      DomRHIInstallationsCapTech <- Data
      
      plottitle <- "Renewable heat capacity by technology type"
      sourcecaption <- "Source: BEIS"
      ChartColours <- c("#39ab2c", "#FF8500", "#FFFFFF")
      LineColours <- c("#fc4e2a","#feb24c","#fed976","#addd8e","#41ab5d")
      
      DomRHIInstallationsCapTechChart <-
        DomRHIInstallationsCapTech %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Biomass,
              label = Biomass),
          colour = LineColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = Biomass,
            label = ifelse(Year == min(Year), paste(round(Biomass, digits = 3), "GW"), ""),
            hjust = 1,
            
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = Biomass,
            label = ifelse(Year == max(Year), paste0("Biomass\n",round(Biomass, digits = 3), " GW"), ""),
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(DomRHIInstallationsCapTech, 1),
          aes(
            x = Year,
            y = Biomass,
            label = Biomass,
            show_guide = FALSE
          ), 
          colour = LineColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = CHP,
              
              label = CHP),
          size = 1.5,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = CHP,
            label = ifelse(Year == min(Year), paste(round(CHP, digits = 3),"GW"), ""),
            hjust = 1,
            
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = CHP,
            label = ifelse(Year == max(Year), paste0("Biomass CHP\n",round(CHP, digits = 3), " GW"), ""),
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(DomRHIInstallationsCapTech, 1),
          aes(
            x = Year,
            y = CHP,
            label = CHP,
            show_guide = FALSE
          ),
          colour = LineColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Waste,
              label = Waste),
          size = 1.5,
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = Waste,
            label = ifelse(Year == min(Year), paste(sprintf("%.3f", round(Waste, digits = 3)),"GW"), ""),
            hjust = 1,
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = Waste,
            label = ifelse(Year == max(Year), paste0("Waste\n",round(Waste, digits = 3), " GW"), ""),
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(DomRHIInstallationsCapTech, 1),
          aes(
            x = Year,
            y = Waste,
            label = Waste,
            show_guide = FALSE
          ),
          colour = LineColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Pumps,
              label = Pumps),
          size = 1.5,
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = Pumps,
            label = ifelse(Year == min(Year), paste(round(Pumps, digits = 3), "GW"), ""),
            hjust = 1,
            vjust= -1.5,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = Pumps,
            label = ifelse(Year == max(Year), paste0("Heat pumps\n", round(Pumps, digits = 3)," GW"), ""),
            vjust = 0,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(DomRHIInstallationsCapTech, 1),
          aes(
            x = Year,
            y = Pumps,
            label = Pumps,
            show_guide = FALSE
          ),
          colour = LineColours[4],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Solar,
              label = Solar),
          size = 1.5,
          colour = LineColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.1,
            y = Solar,
            label = ifelse(Year == min(Year), paste(round(Solar, digits = 3),"GW"), ""),
            hjust = 1,
            vjust = -.8,
            fontface = 2
          ),
          colour = LineColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.8,
            y = Solar,
            label = ifelse(Year == max(Year), paste0("Solar thermal\n",round(Solar, digits = 3)," GW"), ""),
            fontface = 2
          ),
          colour = LineColours[5],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(DomRHIInstallationsCapTech, 1),
          aes(
            x = Year,
            y = Solar,
            label = Solar,
            show_guide = FALSE
          ),
          colour = LineColours[5],
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
      
      
      DomRHIInstallationsCapTechChart
      
      DomRHIInstallationsCapTechChart <-
        LinePercentChart(DomRHIInstallationsCapTechChart,
                         DomRHIInstallationsCapTech,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      DomRHIInstallationsCapTechChart

      
      DomRHIInstallationsCapTechChart <- DomRHIInstallationsCapTechChart +
        ylim(-.008, max(DomRHIInstallationsCapTech$Biomass))+
        xlim(min(DomRHIInstallationsCapTech$Year-.8), max(DomRHIInstallationsCapTech$Year+1.1))
      ggsave(
       file,
       plot = DomRHIInstallationsCapTechChart,
        width = 16,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )



output$DomRHIInstallationsOutput.png <- downloadHandler(
  filename = "DomRHIInstallationsOutput.png",
  content = function(file) {
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable heat by tech type", col_names = FALSE, 
        skip = 13, n_max = 6)
    
    Data <- as.data.frame(t(Data))
    names(Data) <- c("Year", "Biomass", "CHP", "Waste", "Pumps", "Solar")
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    Data <- as_tibble(Data)
    Data <- arrange(Data, -row_number())
    Data <- distinct(as_tibble(Data), Year, .keep_all = TRUE)
    Data <- Data[complete.cases(Data),]
    Data <- arrange(Data, -row_number())
    
    DomRHIInstallationsOutputTech <- Data
    
    plottitle <- "Renewable heat output by technology type"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500", "#FFFFFF")
    LineColours <- c("#fc4e2a","#feb24c","#fed976","#addd8e","#41ab5d")
    
    DomRHIInstallationsOutputTechChart <-
      DomRHIInstallationsOutputTech %>%  ggplot(aes(x = Year), family = "Century Gothic") +
      
      ### Line of Values
      geom_line(
        aes(y = Biomass,
            label = Biomass),
        colour = LineColours[1],
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Biomass,
          label = ifelse(Year == min(Year), paste(format(round(Biomass, digits = 0), big.mark = ","), "GWh"), ""),
          hjust = 1,
          vjust= 0,
          fontface = 2
        ),
        colour = LineColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = Biomass,
          label = ifelse(Year == max(Year), paste0("Biomass\n",format(round(Biomass, digits = 0), big.mark = ","), " GWh"), ""),
          fontface = 2
        ),
        colour = LineColours[1],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(DomRHIInstallationsOutputTech, 1),
        aes(
          x = Year,
          y = Biomass,
          label = Biomass,
          show_guide = FALSE
        ), 
        colour = LineColours[1],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = CHP,
            
            label = CHP),
        size = 1.5,
        colour = LineColours[2],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = CHP,
          label = ifelse(Year == min(Year), paste(round(CHP, digits = 0),"GWh"), ""),
          hjust = 1,
          vjust = 1,
          fontface = 2
        ),
        colour = LineColours[2],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = CHP,
          label = ifelse(Year == max(Year), paste0("Biomass CHP\n",round(CHP, digits = 0), " GWh"), ""),
          fontface = 2,
        ),
        colour = LineColours[2],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(DomRHIInstallationsOutputTech, 1),
        aes(
          x = Year,
          y = CHP,
          label = CHP,
          show_guide = FALSE
        ),
        colour = LineColours[2],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Waste,
            label = Waste),
        size = 1.5,
        colour = LineColours[3],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Waste,
          label = ifelse(Year == min(Year), paste(round(Waste, digits = 0),"GWh"), ""),
          hjust = 1,
          vjust = -.9,
          fontface = 2
        ),
        colour = LineColours[3],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = Waste,
          label = ifelse(Year == max(Year), paste0("Waste\n",round(Waste, digits = 0), " GWh"), ""),
          fontface = 2,
          vjust =-.3
        ),
        colour = LineColours[3],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(DomRHIInstallationsOutputTech, 1),
        aes(
          x = Year,
          y = Waste,
          label = Waste,
          show_guide = FALSE
        ),
        colour = LineColours[3],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Pumps,
            label = Pumps),
        size = 1.5,
        colour = LineColours[4],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Pumps,
          label = ifelse(Year == min(Year), paste(round(Pumps, digits = 0), "GWh"), ""),
          hjust = 1,
          vjust= 0,
          fontface = 2
        ),
        colour = LineColours[4],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = Pumps,
          label = ifelse(Year == max(Year), paste0("Heat pumps\n", round(Pumps, digits = 0)," GWh"), ""),
          vjust = .5,
          fontface = 2
        ),
        colour = LineColours[4],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(DomRHIInstallationsOutputTech, 1),
        aes(
          x = Year,
          y = Pumps,
          label = Pumps,
          show_guide = FALSE
        ),
        colour = LineColours[4],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Solar,
            label = Solar),
        size = 1.5,
        colour = LineColours[5],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year-.1,
          y = Solar,
          label = ifelse(Year == min(Year), paste(round(Solar, digits = 0),"GWh"), ""),
          hjust = 1,
          vjust = 0,
          fontface = 2
        ),
        colour = LineColours[5],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.8,
          y = Solar,
          label = ifelse(Year == max(Year), paste0("Solar thermal\n",format(round(Solar, digits = 0), big.mark = ",")," GWh"), ""),
          fontface = 2,
          vjust = 0
        ),
        colour = LineColours[5],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(DomRHIInstallationsOutputTech, 1),
        aes(
          x = Year,
          y = Solar,
          label = Solar,
          show_guide = FALSE
        ),
        colour = LineColours[5],
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
    
    
    DomRHIInstallationsOutputTechChart
    
    DomRHIInstallationsOutputTechChart <-
      LinePercentChart(DomRHIInstallationsOutputTechChart,
                       DomRHIInstallationsOutputTech,
                       plottitle,
                       sourcecaption,
                       ChartColours)
    
    
    DomRHIInstallationsOutputTechChart

    
    DomRHIInstallationsOutputTechChart <- DomRHIInstallationsOutputTechChart +
      ylim(-.008, max(DomRHIInstallationsOutputTech$Biomass))+
      xlim(min(DomRHIInstallationsOutputTech$Year-.8), max(DomRHIInstallationsOutputTech$Year+1.1))
    ggsave(
      file,
      plot = DomRHIInstallationsOutputTechChart,
      width = 16,
      height = 16,
      units = "cm",
      dpi = 300
    )
    
  }
)
}
    
    