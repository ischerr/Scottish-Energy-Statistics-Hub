require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ChargingPoints <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Vehicles licenced",
    fluidRow(column(8,
                    h3("Number of ultra low emission vehicles licenced", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('ULEVsSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ULEVs.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("ULEVsPlot")),
    plotlyOutput(ns("ULEVsPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("First time registrations",
             fluidRow(column(8,
                             h3("Proportion of ULEVs registered for the first time", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('ULEVRegOutputSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ULEVRegOutput.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("ULEVsPlot")),
             plotlyOutput(ns("ULEVRegOutputPlot"))%>% withSpinner(color="#39ab2c"),
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
      tabPanel("Vehicles licenced",
    fluidRow(
    column(10, h3("Data - Vehicles licenced", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ULEVsTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("First Time Registrations",
      fluidRow(
        column(10, h3("Data - All ULEVs registered for the first time in Scotland", style = "color: #39ab2c;  font-weight:bold")),
        column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
      ),
      fluidRow(
        column(12, dataTableOutput(ns("ULEVRegOutputTable"))%>% withSpinner(color="#39ab2c"))),
      tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("DFTLicenced", "DFTULEVs"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("DFTLicenced"),
        SourceLookup("DFTULEVs")
        
      )
    )
  )
}




###### Server ######
ChargingPoints <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ULEVs.R")

  
  output$ULEVsSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", 
        skip = 17)
    
    Data <- Data[c(1,3,4,2)]
    
    Data$Year <- as.yearqtr(Data$Year)
    
    paste("Scotland,", min(Data$Year), "-", max(Data$Year))
  })
  
  output$ULEVsPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", 
        skip = 17)
    
    Data <- Data[c(1,3,4,2)]
    
    Data$Year <- as.yearqtr(Data$Year)
    
    Data2 <- rbind(head(Data,1), tail(Data,1))
    
    ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
    
    p <-  plot_ly(Data, 
                  x = ~Year, 
                  y = ~ `Battery Electric Vehicles`, 
                  name = 'Battery Electric Vehicles',
                  type = 'scatter',
                  mode = 'none',
                  stackgroup = 'one',
                  fillcolor = ChartColours[1],
                  hoverinfo = "text",
                  text = paste0("Battery Electric Vehicles: ", Data$`Battery Electric Vehicles`, "\nAs Of: ", Data$Year)
    )%>%
      add_trace(
        y = ~ `Other ULEVs`, 
        name = 'Other ULEVs',
        fillcolor = ChartColours[3],
        hoverinfo = "text",
        text = paste0("Other ULEVs: ", Data$`Other ULEVs`, "\nAs Of: ", Data$Year)
      ) %>% 
      add_trace(
        x = Data2$Year,
        y = Data2$`All ULEVs` +  1500,
        name = "All ULEVs",
        mode = 'text',
        text = paste("<b>Total:\n", format(Data2$`All ULEVs`, big.mark = ","),"</b>"),
        stackgroup = 'two',
        showlegend = FALSE,
        fillcolor = 'rgba(26,150,65,0)'
      ) %>% 
      layout(
        barmode = 'group',
        bargap = 0.25,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'skip',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     showgrid = FALSE,
                     range = c(min(Data$Year)-.25, max(Data$Year)+.25)
                     
        ),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          zeroline = FALSE,
          rangemode = "tozero",
          range = c(0, max(Data$`All ULEVs`)+3000)
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
    
    
  })
  
  output$ULEVRegOutputSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", 
        skip = 17)
    
    Data <- Data[c(1,3,4,2)]
    
    Data$Year <- as.yearqtr(Data$Year)
    
    paste("Scotland,", min(Data$Year), "-", max(Data$Year))
  })
  
  output$ULEVRegOutputPlot <- renderPlotly  ({
    
    
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", col_names = TRUE, 
        skip = 18)
    
    Data <- Data[c(1,12)]
    
    names(Data) <- c("Year", "Proportion")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    LineColours <- c("#39ab2c","#ef3b2c","#fb6a4a","#fc9272","#fcbba1")
    
    ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
    
    ULEVRegOutput <- Data
    
    p <-  plot_ly(ULEVRegOutput, x = ~ Year ) %>%  
      add_trace(y = ~ `Proportion`,
                name = "Proportion",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Proportion: ",
                  percent(ULEVRegOutput$Proportion, accuracy = 0.1),
                  "\nYear: ",
                  format(ULEVRegOutput$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ULEVRegOutput[which(ULEVRegOutput$`Proportion` != 0),], 1),
        x = ~ Year,
        y = ~ `Proportion`,
        name = "Proportion",
        legendgroup = "1",
        text = paste0(
          "Proportion: ",
          percent(tail(ULEVRegOutput[which(ULEVRegOutput$`Proportion` != 0),], 1)$`Proportion`, accuracy = 0.1),
          "\nYear: ",
          format(tail(ULEVRegOutput[which(ULEVRegOutput$`Proportion` != 0),], 1)$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[1])
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
          title = "",
          tickformat = ".1%",
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
  
  
  output$ULEVsTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", col_names = TRUE, 
        skip = 17)
    
    Data <- Data[c(1:4,6:7)]
    
    ULEVs <- Data
    
    datatable(
      ULEVs,
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
        title = "Number of Ultra Low Emission Vehicles (ULEVs) licenced at end of year/quarter",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Number of Ultra Low Emission Vehicles (ULEVs) licenced at end of year/quarter",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Number of Ultra Low Emission Vehicles (ULEVs) licenced at end of year/quarter")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:6), 0) %>% 
      formatPercentage(6,2)
  })
  
  output$ULEVRegOutputTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", col_names = TRUE, 
        skip = 17)
    
    Data <- Data[c(1,10:12)]
    
    ULEVRegOutputTech <- Data
    
    datatable(
      ULEVRegOutputTech,
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
        title = "All ULEVs registered for the first time in Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "All ULEVs registered for the first time in Scotland",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "All ULEVs registered for the first time in Scotland")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:4), 0) %>% 
      formatPercentage(4,2)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Transport/ULEVs.txt")[2])
                     
                   )))
  })
  
 observeEvent(input$ToggleTable1, {
    toggle("ULEVsTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ULEVRegOutputTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ULEVs.png <- downloadHandler(
    filename = "ULEVs.png",
    content = function(file) {

      ElecVehicles <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "ULEVs", col_names = TRUE, 
                                 skip = 17)[c(1,3,4)]
      
      ElecVehicles$Year <-
        as.yearqtr(ElecVehicles$Year, format = "%Y Q%q")
      
      
      ElecVehiclesMin <- head(ElecVehicles, 1)
      ElecVehiclesMax <- tail(ElecVehicles, 1)
      
      ElecVehicles <- melt(ElecVehicles, id.vars = "Year")
      
      ElecVehicles <- ElecVehicles %>% mutate(variable = factor(variable),
                                              variable = factor(variable, levels = rev(levels(variable))))
      
      
      ### variables
      ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
      sourcecaption = "Source: DfT"
      plottitle = "Number of ultra low emission vehicles\nlicenced"
      
      #ElecVehicles$CavityPercentage <- PercentLabel(ElecVehicles$Cavity)
      
      
      ElecVehiclesChart <- ElecVehicles %>%
        ggplot(aes(
          x = Year,
          y = value,
          group = variable,
          fill = variable
        )) +
        scale_fill_manual(
          "variable",
          values = c(
            "Battery Electric Vehicles" = ChartColours[2],
            "Other ULEVs" = ChartColours[3]
          )
        ) +
        geom_area(posistion = "fill") +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              format(Year, format = "%Y Q%q"),
              ""
            ),
            hjust = ifelse(Year == min(Year), 0, 1),
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2,
            family = "Century Gothic"
          )
        ) +
        annotate(
          "text",
          x = ElecVehiclesMin$Year,
          y = ElecVehiclesMin$`Battery Electric Vehicles` * 0.5,
          label = format(ElecVehiclesMin$`Battery Electric Vehicles`,big.mark = ","),
          hjust = -.1,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMin$Year,
          y = (ElecVehiclesMin$`Other ULEVs` * 0.5) + ElecVehiclesMin$`Battery Electric Vehicles`,
          label = format(ElecVehiclesMin$`Other ULEVs`,big.mark = ","),
          hjust = -.1,
          vjust = -1,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMax$Year,
          y = ElecVehiclesMax$`Battery Electric Vehicles` * 0.5,
          label = format(ElecVehiclesMax$`Battery Electric Vehicles`, big.mark = ","),
          hjust = 1.1,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMax$Year,
          y = (ElecVehiclesMax$`Other ULEVs` * 0.5) + ElecVehiclesMax$`Battery Electric Vehicles`,
          label = format(ElecVehiclesMax$`Other ULEVs`, big.mark = ","),
          hjust = 1.1,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecVehicles$Year),
          y = (
            ElecVehiclesMax$`Battery Electric Vehicles` + ElecVehiclesMin$`Battery Electric Vehicles`
          ) * .25,
          label = "Battery Electric\nVehicles",
          hjust = .5,
          vjust = 1,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecVehicles$Year),
          y = ((ElecVehiclesMax$`Other ULEVs` + ElecVehiclesMin$`Other ULEVs`) *
                 .25
          ) + ((
            ElecVehiclesMax$`Battery Electric Vehicles` + ElecVehiclesMin$`Battery Electric Vehicles`
          ) * .5
          ),
          label = "Other ULEVs",
          hjust = .5,
          vjust = 8.1,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMin$Year,
          y = ElecVehiclesMin$`Battery Electric Vehicles` + ElecVehiclesMin$`Other ULEVs`,
          label = paste0(
            "Total: ",
            format(ElecVehiclesMin$`Battery Electric Vehicles` + ElecVehiclesMin$`Other ULEVs`, big.mark = ",")
          ),
          hjust = 0,
          vjust = -3,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMax$Year,
          y = ElecVehiclesMax$`Battery Electric Vehicles` + ElecVehiclesMax$`Other ULEVs`,
          label = paste0(
            "Total: ",
            format(ElecVehiclesMax$`Battery Electric Vehicles` + ElecVehiclesMax$`Other ULEVs`,big.mark = ",")
          ),
          hjust = 1.3,
          vjust = 1,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        )
      
      ElecVehiclesChart
      
      
      ElecVehiclesChart <-
        StackedArea(ElecVehiclesChart,
                    ElecVehicles,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      
      ElecVehiclesChart <- ElecVehiclesChart +
        ylim(-100, max(ElecVehiclesMax$`Battery Electric Vehicles`)+max(ElecVehiclesMax$`Other ULEVs`))
      


      
      ggsave(
        file,
        plot =  ElecVehiclesChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )



output$ULEVRegOutput.png <- downloadHandler(
  filename = "ULEVRegOutput.png",
  content = function(file) {
    

    ElecVehiclesRegistrations <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", col_names = TRUE, 
        skip = 18)[c(1,10,12)]
    
    names(ElecVehiclesRegistrations) <- c("Year", "Registrations", "Proportion")
    
    ElecVehiclesRegistrations$Year <-
      as.yearqtr(ElecVehiclesRegistrations$Year, format = "%Y Q%q")
    
        ElecVehiclesProportion <- ElecVehiclesRegistrations
        
        ElecVehiclesProportion$Year <-
          as.yearqtr(ElecVehiclesProportion$Year, format = "%Y Q%q")
        
        
        ### variables
        ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
        LineColours <- c("#39ab2c", "#238b45", "#a1d99b")
        sourcecaption = "Source: DfT"
        plottitle = "Proportion of ULEVs registered for\nthe first time"
        
        #ElecVehiclesProportion$CavityPercentage <- PercentLabel(ElecVehiclesProportion$Cavity)
        
        
        ElecVehiclesProportionChart <- ElecVehiclesProportion %>%
          ggplot(aes(x = Year,
                     y = Proportion)) +
          geom_line(aes(),
                    size = 1.5,
                    colour = LineColours[1],
                    family = "Century Gothic") +
          geom_point(
            data = tail(ElecVehiclesProportion, 1),
            aes(x = Year,
                y = Proportion),
            size = 4,
            colour = LineColours[1],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              label = ifelse(Year == min(Year), percent(Proportion, accuracy =  .1), ""),
              show_guide = FALSE
            ),
            fontface = 2,
            vjust = 2,
            colour = LineColours[1],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              label = ifelse(Year == max(Year), percent(Proportion, accuracy =  .1), ""),
              show_guide = FALSE
            ),
            hjust = -.4,
            fontface = 2,
            colour = LineColours[1],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              y = 0,
              label = ifelse(Year == min(Year) |
                               Year == max(Year), format(Year, format = "%Y Q%q"), ""),
              hjust = 0.5,
              vjust = 1.5,
              colour = ChartColours[1],
              fontface = 2
            ),
            family = "Century Gothic"
          )
        
        
        ElecVehiclesProportionChart
        
        
        ElecVehiclesProportionChart <-
          StackedArea(ElecVehiclesProportionChart,
                      ElecVehiclesProportion,
                      plottitle,
                      sourcecaption,
                      ChartColours)
        
        
        ElecVehiclesProportionChart <- ElecVehiclesProportionChart +
          xlim(min(as.numeric(ElecVehiclesProportion$Year)-.15),max(as.numeric(ElecVehiclesProportion$Year)+.3))
        
        ElecVehiclesProportionChart
        
        
        ggsave(
          file,
          plot =  ElecVehiclesProportionChart,
          width = 14,
          height = 16,
          units = "cm",
          dpi = 300
        )
        
    
  }
)
}
    
    