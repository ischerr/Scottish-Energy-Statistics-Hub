require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

SupplyEmissionsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Net source greenhouse gas emissions from the energy supply sector (MtCO2e)", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('SupplyEmissionsSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SupplyEmissions.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("SupplyEmissionsPlot")),
    plotlyOutput(ns("SupplyEmissionsPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("SupplyEmissionsTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
SupplyEmissions <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }

  print("SupplyEmissions.R")
  
    output$SupplyEmissionsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Energy supply emissions", skip = 12)
    
    Data <- as.data.frame(t(Data))
    
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    Data <- setDT(Data, keep.rownames = TRUE)[]
    names(Data) <- c("Year", "Greenhouse Gas", "Energy Supply", "Electricity Production", "Other Energy")
    Data[1,1] <- 1988
    
    Data<- rbind(Data, setNames(data.frame(1989,NA,NA,NA,NA),names(Data)))
    Data <- Data[order(Data$Year)]
    Data$Year <- as.numeric(as.character(Data$Year))
    Data$`Greenhouse Gas` <- as.numeric(as.character(Data$`Greenhouse Gas`))
    Data$`Energy Supply` <- as.numeric(as.character(Data$`Energy Supply`))
    Data$`Electricity Production` <- as.numeric(as.character(Data$`Electricity Production`))
    Data$`Other Energy` <- as.numeric(as.character(Data$`Other Energy`))
    
    
    
    SupplyEmissions <- Data
    ### variables
    
    paste("Scotland, 1990","-", max(SupplyEmissions$Year))
  })
  
  output$SupplyEmissionsPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Energy supply emissions", skip = 12)
    
    Data <- as.data.frame(t(Data))
    
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    Data <- setDT(Data, keep.rownames = TRUE)[]
    names(Data) <- c("Year", "Greenhouse Gas", "Energy Supply", "Electricity Production", "Other Energy")
    Data[1,1] <- 1988
    
    Data<- rbind(Data, setNames(data.frame(1989,NA,NA,NA,NA),names(Data)))
    Data <- Data[order(Data$Year)]
    Data$Year <- as.numeric(as.character(Data$Year))
    Data$`Greenhouse Gas` <- as.numeric(as.character(Data$`Greenhouse Gas`))
    Data$`Energy Supply` <- as.numeric(as.character(Data$`Energy Supply`))
    Data$`Electricity Production` <- as.numeric(as.character(Data$`Electricity Production`))
    Data$`Other Energy` <- as.numeric(as.character(Data$`Other Energy`))
    
    
    
    SupplyEmissions <- Data
    
    
    plottitle <- "Net source greenhouse gas emissions from the energy supply sector (MtCO2e)"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500")
    LineColours <- c( "#39ab2c","#006837", "#41ab5d", "#addd8e")
    
    SupplyEmissions$Year <- paste0("01/01/", SupplyEmissions$Year)
    
    SupplyEmissions$Year <- dmy(SupplyEmissions$Year)
    
    
    p <-  plot_ly(data = SupplyEmissions,
                  x = ~ Year ) %>% 
      add_trace(data = SupplyEmissions,
                x = ~ Year,
                y = ~ `Greenhouse Gas`,
                name = "Total greenhouse gas emissions",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Total greenhouse gas emissions: ",
                  round(SupplyEmissions$`Greenhouse Gas`, digits = 1),
                  " MtCO2e\nYear: ",
                  format(SupplyEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))],
        x = ~ Year,
        y = ~ `Greenhouse Gas`,
        name = "Total greenhouse gas emissions",
        text = paste0(
          "Total greenhouse gas emissions: ",
          round(SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))]$`Greenhouse Gas`, digits = 1),
          " MtCO2e\nYear: ",
          format(SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>% 
      
      add_trace(data = SupplyEmissions,
                x = ~ Year,
                y = ~ `Energy Supply`,
                name = "Total energy supply emissions",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Total energy supply emissions: ",
                  round(SupplyEmissions$`Energy Supply`, digits = 1),
                  " MtCO2e\nYear: ",
                  format(SupplyEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))],
        x = ~ Year,
        y = ~ `Energy Supply`,
        name = "Total energy supply emissions",
        text = paste0(
          "Total energy supply emissions: ",
          round(SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))]$`Energy Supply`, digits = 1),
          " MtCO2e\nYear: ",
          format(SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "2",
        marker = list(size = 18, 
                      color = LineColours[2])
      ) %>% 
      
      add_trace(data = SupplyEmissions,
                x = ~ Year,
                y = ~ `Electricity Production`,
                name = "Electricity production emissions",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Electricity production emissions: ",
                  round(SupplyEmissions$`Electricity Production`, digits = 1),
                  " MtCO2e\nYear: ",
                  format(SupplyEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))],
        x = ~ Year,
        y = ~ `Electricity Production`,
        name = "Electricity production emissions",
        text = paste0(
          "Electricity production emissions: ",
          round(SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))]$`Electricity Production`, digits = 1),
          " MtCO2e\nYear: ",
          format(SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "3",
        marker = list(size = 18, 
                      color = LineColours[3])
      ) %>% 
      
      add_trace(data = SupplyEmissions,
                x = ~ Year,
                y = ~ `Other Energy`,
                name = "Other energy emissions",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "Other energy emissions: ",
                  round(SupplyEmissions$`Other Energy`, digits = 1),
                  " MtCO2e\nYear: ",
                  format(SupplyEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[4], dash = "none")
      ) %>% 
      add_trace(
        data = SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))],
        x = ~ Year,
        y = ~ `Other Energy`,
        name = "Other energy emissions",
        text = paste0(
          "Other energy emissions: ",
          round(SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))]$`Other Energy`, digits = 1),
          " MtCO2e\nYear: ",
          format(SupplyEmissions[which(SupplyEmissions$Year %in% c(ymd("1988-01-01"),ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(SupplyEmissions$Year)))]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "4",
        marker = list(size = 18, 
                      color = LineColours[4])
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
                     showgrid = FALSE,
                     range = c(min(SupplyEmissions$Year)-100, max(SupplyEmissions$Year)+100)),
        yaxis = list(
          title = "MtCO2e",
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
  
  
  output$SupplyEmissionsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Energy supply emissions", skip = 12)
    
    Data <- as.data.frame(t(Data))
    
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    Data <- setDT(Data, keep.rownames = TRUE)[]
    names(Data) <- c("Year", "Greenhouse Gas", "Energy Supply", "Electricity Production", "Other Energy")
    Data[1,1] <- 1988
    
    Data<- rbind(Data, setNames(data.frame(1989,NA,NA,NA,NA),names(Data)))
    Data <- Data[order(Data$Year)]
    Data$Year <- as.numeric(as.character(Data$Year))
    Data$`Greenhouse Gas` <- as.numeric(as.character(Data$`Greenhouse Gas`))
    Data$`Energy Supply` <- as.numeric(as.character(Data$`Energy Supply`))
    Data$`Electricity Production` <- as.numeric(as.character(Data$`Electricity Production`))
    Data$`Other Energy` <- as.numeric(as.character(Data$`Other Energy`))
    
    Data$Year <- as.character(Data$Year)
    Data[1,1] <- " Baseline"
    Data$Year <- factor(Data$Year, ordered = TRUE)
    
    SupplyEmissions <- Data[complete.cases(Data)]
    
    
    plottitle <- "Net source greenhouse gas emissions from the energy supply sector (MtCO2e)"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500")
    LineColours <- c( "#39ab2c","#006837", "#41ab5d", "#addd8e")
    

    datatable(
      SupplyEmissions,
      extensions = 'Buttons',
     # container = sketch,
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "Net source greenhouse gas emissions",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Net source greenhouse gas emissions',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Net source greenhouse gas emissions')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatRound(2:5, 1)
  })
  
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Emissions/SupplyEmissions.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("SupplyEmissionsTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$SupplyEmissions.png <- downloadHandler(
    filename = "SupplyEmissions.png",
    content = function(file) {

      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Energy supply emissions", skip = 12)
      
      Data <- as.data.frame(t(Data))
      
      colnames(Data) <- as.character(unlist(Data[1,]))
      Data = Data[-1, ]
      Data <- setDT(Data, keep.rownames = TRUE)[]
      names(Data) <- c("Year", "Greenhouse Gas", "Energy Supply", "Electricity Production", "Other Energy")
      Data[1,1] <- 1988
      
      Data<- rbind(Data, setNames(data.frame(1989,NA,NA,NA,NA),names(Data)))
      Data <- Data[order(Data$Year)]
      Data$Year <- as.numeric(as.character(Data$Year))
      Data$`Greenhouse Gas` <- as.numeric(as.character(Data$`Greenhouse Gas`))
      Data$`Energy Supply` <- as.numeric(as.character(Data$`Energy Supply`))
      Data$`Electricity Production` <- as.numeric(as.character(Data$`Electricity Production`))
      Data$`Other Energy` <- as.numeric(as.character(Data$`Other Energy`))
      
      
      
      EnergySupplyEmissions <- Data
      
      
      plottitle <- "Net source greenhouse gas emissions from the energy supply sector (MtCO2e)"
      sourcecaption <- "Source: BEIS"
      ChartColours <- c("#39ab2c", "#FF8500")
      LineColours <- c( "#39ab2c","#006837", "#41ab5d", "#addd8e")
      
      EnergySupplyEmissionsChart <-
        EnergySupplyEmissions %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = `Greenhouse Gas`,
              
              label = `Greenhouse Gas`),
          size = 1.5,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Greenhouse Gas`,
            label = ifelse(Year %in% c(1988,1990,1995, 1998), round(`Greenhouse Gas`, digits= 1), ""),
            hjust = 0.5,
            vjust = 2,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+0.75,
            y = `Greenhouse Gas`,
            label = ifelse(Year == max(Year), round(`Greenhouse Gas`, digits= 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(c(1998,max(Year))),
            y = mean(`Greenhouse Gas`, na.rm = TRUE),
            label = "Total greenhouse gas emissions",
            hjust = 0.5,
            vjust = -3,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergySupplyEmissions, 1),
          aes(
            x = Year,
            y = `Greenhouse Gas`,
            label = round(`Greenhouse Gas`, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = EnergySupplyEmissions[which(EnergySupplyEmissions$Year %in% c(1988,1990,1995,1998)),],
          aes(
            x = Year,
            y = `Greenhouse Gas`,
            label = round(`Greenhouse Gas`, digits = 1),
            show_guide = FALSE
          ),
          size = 3,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Energy Supply`,
              
              label = `Energy Supply`),
          size = 1.5,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Energy Supply`,
            label = ifelse(Year %in% c(1988,1990,1995, 1998), round(`Energy Supply`, digits= 1), ""),
            hjust = 0.5,
            vjust = 2,
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+0.75,
            y = `Energy Supply`,
            label = ifelse(Year == max(Year), sprintf("%.1f", round(`Energy Supply`, digits= 1)), ""),
            hjust = 0.5,
            vjust = 0.1,
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(c(1998,max(Year))),
            y = mean(`Energy Supply`, na.rm = TRUE),
            label = "Total energy supply emissions",
            hjust = 0.5,
            vjust = -3,
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergySupplyEmissions, 1),
          aes(
            x = Year,
            y = `Energy Supply`,
            label = round(`Energy Supply`, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = EnergySupplyEmissions[which(EnergySupplyEmissions$Year %in% c(1988,1990,1995,1998)),],
          aes(
            x = Year,
            y = `Energy Supply`,
            label = round(`Energy Supply`, digits = 1),
            show_guide = FALSE
          ),
          size = 3,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Electricity Production`,
              
              label = `Electricity Production`),
          size = 1.5,
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Electricity Production`,
            label = ifelse(Year %in% c(1988,1990,1995, 1998), round(`Electricity Production`, digits= 1), ""),
            hjust = 0.5,
            vjust = 2,
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+0.75,
            y = `Electricity Production`,
            label = ifelse(Year == max(Year), round(`Electricity Production`, digits= 1), ""),
            hjust = 0.5,
            vjust = 0.3,
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(c(1998,max(Year))),
            y = mean(`Electricity Production`, na.rm = TRUE),
            label = "Electricity supply emissions",
            hjust = 0.5,
            vjust = 2.5,
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergySupplyEmissions, 1),
          aes(
            x = Year,
            y = `Electricity Production`,
            label = round(`Electricity Production`, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = EnergySupplyEmissions[which(EnergySupplyEmissions$Year %in% c(1988,1990,1995,1998)),],
          aes(
            x = Year,
            y = `Electricity Production`,
            label = round(`Electricity Production`, digits = 1),
            show_guide = FALSE
          ),
          size = 3,
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Other Energy`,
              
              label = `Other Energy`),
          size = 1.5,
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Other Energy`,
            label = ifelse(Year %in% c(1988,1990,1995, 1998), sprintf("%.1f", round(`Other Energy`, digits= 1)), ""),
            hjust = 0.5,
            vjust = 2,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+0.75,
            y = `Other Energy`,
            label = ifelse(Year == max(Year), round(`Other Energy`, digits= 1), ""),
            hjust = 0.5,
            vjust = 0.8,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(c(1998,max(Year))),
            y = mean(`Other Energy`, na.rm = TRUE),
            label = "Other energy supply emissions",
            hjust = 0.5,
            vjust = 1.8,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergySupplyEmissions, 1),
          aes(
            x = Year,
            y = `Other Energy`,
            label = round(`Other Energy`, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = EnergySupplyEmissions[which(EnergySupplyEmissions$Year %in% c(1988,1990,1995,1998)),],
          aes(
            x = Year,
            y = `Other Energy`,
            label = round(`Other Energy`, digits = 1),
            show_guide = FALSE
          ),
          size = 3,
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year %in% c(1990, 1995, 1998, max(Year)), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = 1988,
            y = 0,
            label = "Baseline",
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      EnergySupplyEmissionsChart <-
        LinePercentChart(EnergySupplyEmissionsChart,
                         EnergySupplyEmissions,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      EnergySupplyEmissionsChart
      
      EnergySupplyEmissionsChart <- EnergySupplyEmissionsChart +
        ylim(-3, max(EnergySupplyEmissions$`Greenhouse Gas`)) +
        labs(subtitle = paste("Scotland, 1990 -", max(EnergySupplyEmissions$Year)))
      ggsave(
        file,
        plot = EnergySupplyEmissionsChart,
        width = 30,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
}
