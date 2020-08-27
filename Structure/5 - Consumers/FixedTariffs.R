require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



FixedTariffsOutput <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    tabPanel("Electricity",
    fluidRow(column(8,
                    h3("Proportion of electricity customers on a fixed tariff", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('ElecPaymentsSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecPayments.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("ElecPaymentsPlot")),
    plotlyOutput(ns("ElecPaymentsPlot"))%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas",
             fluidRow(column(8,
                             h3("Proportion of gas customers on a fixed tariff", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('GasPaymentsSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasPayments.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("ElecPaymentsPlot")),
             plotlyOutput(ns("GasPaymentsPlot"))%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    
    tabsetPanel(
      tabPanel("Electricity",
    fluidRow(
    column(10, h3("Data - Proportion of electricity customers on a fixed tariff", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ElecPaymentsTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas",
             fluidRow(
               column(10, h3("Data - Proportion of gas customers on a fixed tariff", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasPaymentsTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISPaymentMethodElec", "BEISPaymentMethodGas"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISPaymentMethodElec"),
        SourceLookup("BEISPaymentMethodGas")
        
      )
    )
  )
}




###### Server ######
FixedTariffs <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("FixedTariffs.R")

  
  output$ElecPaymentsSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec payment methods",
        col_names = TRUE,
        skip = 12
      )
    
    names(Data)[1] <- "Year"
    
    Data$Year <- as.yearqtr(Data$Year)
    
    ElecPayments <- Data
    ### variables
    
    paste("Scotland,", min(ElecPayments$Year),"-", max(ElecPayments$Year))
  })
  
  output$ElecPaymentsPlot <- renderPlotly  ({
    Data <- read_delim("Processed Data/Output/Energy Bills/FixedTariffElectricity.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    names(Data)[1] <- "Year"
    
    Data$Year <- ymd(Data$Year)
    
    ElecPayments <- Data
    
    ### variables
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <-
      c(    "#0868ac","#43a2ca","#7bccc4"
      )
    sourcecaption = "Source: BEIS"
    plottitle = "Proportion of payment methods used\nfor Electricity bills"
    
    
    
    p <-  plot_ly(data = ElecPayments,
                  x = ~ Year ) %>% 
      add_trace(y = ~ `North Scotland`,
                name = "North Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "North Scotland: ",
                  percent(ElecPayments$`North Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(as.yearqtr(ElecPayments$Year), "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = BarColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecPayments[which(ElecPayments$`North Scotland` > 0 | ElecPayments$`North Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `North Scotland`,
        name = "North Scotland",
        text = paste0(
          "North Scotland: ",
          percent(ElecPayments[which(ElecPayments$`North Scotland` > 0 | ElecPayments$`North Scotland` < 0),][-1,]$`North Scotland`, accuracy = 1),
          "\nYear: ",
          format(as.yearqtr(ElecPayments[which(ElecPayments$`North Scotland` > 0 | ElecPayments$`North Scotland` < 0),][-1,]$Year), "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = BarColours[1])
      ) %>% 
      add_trace(data = ElecPayments,
                x = ~ Year,
                y = ~ `South Scotland`,
                name = "South Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "South Scotland: ",
                  percent(ElecPayments$`South Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(as.yearqtr(ElecPayments$Year), "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = BarColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecPayments[which(ElecPayments$`South Scotland` > 0 | ElecPayments$`South Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `South Scotland`,
        name = "South Scotland",
        legendgroup = "2",
        text = paste0(
          "South Scotland: ",
          percent(ElecPayments[which(ElecPayments$`South Scotland` > 0 | ElecPayments$`South Scotland` < 0),][-1,]$`South Scotland`, accuracy = 1),
          "\nYear: ",
          format(as.yearqtr(ElecPayments[which(ElecPayments$`South Scotland` > 0 | ElecPayments$`South Scotland` < 0),][-1,]$Year), "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = BarColours[2])
      ) %>% 
      add_trace(data = ElecPayments,
                x = ~ Year,
                y = ~ `Great Britain`,
                name = "Great Britain",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Great Britain: ",
                  percent(ElecPayments$`Great Britain`, accuracy = 1),
                  "\nYear: ",
                  format(as.yearqtr(ElecPayments$Year), "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = BarColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecPayments[which(ElecPayments$`Great Britain` > 0 | ElecPayments$`Great Britain` < 0),], 1),
        x = ~ Year,
        y = ~ `Great Britain`,
        name = "Great Britain",
        legendgroup = "3",
        text = paste0(
          "Great Britain: ",
          percent(ElecPayments[which(ElecPayments$`Great Britain` > 0 | ElecPayments$`Great Britain` < 0),][-1,]$`Great Britain`, accuracy = 1),
          "\nYear: ",
          format(as.yearqtr(ElecPayments[which(ElecPayments$`Great Britain` > 0 | ElecPayments$`Great Britain` < 0),][-1,]$Year), "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = BarColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE
        ),
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
  
  
  output$ElecPaymentsTable = renderDataTable({
    Data  <- read_delim("Processed Data/Output/Energy Bills/FixedTariffElectricity.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(Data) <- c("Year", "North Scotland", "South Scotland", "Great Britain")
    
    Data$Year <- ymd(Data$Year)
    
    Data$Year <- as.yearqtr(Data$Year)
    
    Data$Year <- format(Data$Year, "%Y Q%q")
    ElecPayments <- Data
    
    datatable(
      ElecPayments,
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
        title = "Proportion of electricity customers on a fixed tariff",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of electricity customers on a fixed tariff',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of electricity customers on a fixed tariff')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(2:4, 0) 
  })
  
  output$GasPaymentsSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Gas payment methods",
        col_names = TRUE,
        skip = 12
      )
    
    names(Data)[1] <- "Year"
    
    Data$Year <- as.yearqtr(Data$Year)
    
    GasPayments <- Data
    ### variables
    
    paste("Scotland,", min(GasPayments$Year),"-", max(GasPayments$Year))
  })
  
  output$GasPaymentsPlot <- renderPlotly  ({
    
    Data <- read_delim("Processed Data/Output/Energy Bills/FixedTariffGas.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    names(Data)[1] <- "Year"
    
    Data$Year <- ymd(Data$Year)

    ElecPayments <- Data
    
    ### variables
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <-
      c(    "#0868ac","#43a2ca","#7bccc4"
      )
    sourcecaption = "Source: BEIS"
    plottitle = "Proportion of payment methods used\nfor Gas bills"
    
    
    
    p <-  plot_ly(data = ElecPayments,
                  x = ~ Year ) %>% 
      add_trace(y = ~ `North Scotland`,
                name = "North Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "North Scotland: ",
                  percent(ElecPayments$`North Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(as.yearqtr(ElecPayments$Year), "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = BarColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecPayments[which(ElecPayments$`North Scotland` > 0 | ElecPayments$`North Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `North Scotland`,
        name = "North Scotland",
        text = paste0(
          "North Scotland: ",
          percent(ElecPayments[which(ElecPayments$`North Scotland` > 0 | ElecPayments$`North Scotland` < 0),][-1,]$`North Scotland`, accuracy = 1),
          "\nYear: ",
          format(as.yearqtr(ElecPayments[which(ElecPayments$`North Scotland` > 0 | ElecPayments$`North Scotland` < 0),][-1,]$Year), "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = BarColours[1])
      ) %>% 
      add_trace(data = ElecPayments,
                x = ~ Year,
                y = ~ `South Scotland`,
                name = "South Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "South Scotland: ",
                  percent(ElecPayments$`South Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(as.yearqtr(ElecPayments$Year), "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = BarColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecPayments[which(ElecPayments$`South Scotland` > 0 | ElecPayments$`South Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `South Scotland`,
        name = "South Scotland",
        legendgroup = "2",
        text = paste0(
          "South Scotland: ",
          percent(ElecPayments[which(ElecPayments$`South Scotland` > 0 | ElecPayments$`South Scotland` < 0),][-1,]$`South Scotland`, accuracy = 1),
          "\nYear: ",
          format(as.yearqtr(ElecPayments[which(ElecPayments$`South Scotland` > 0 | ElecPayments$`South Scotland` < 0),][-1,]$Year), "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = BarColours[2])
      ) %>% 
      add_trace(data = ElecPayments,
                x = ~ Year,
                y = ~ `Great Britain`,
                name = "Great Britain",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Great Britain: ",
                  percent(ElecPayments$`Great Britain`, accuracy = 1),
                  "\nYear: ",
                  format(as.yearqtr(ElecPayments$Year), "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = BarColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecPayments[which(ElecPayments$`Great Britain` > 0 | ElecPayments$`Great Britain` < 0),], 1),
        x = ~ Year,
        y = ~ `Great Britain`,
        name = "Great Britain",
        legendgroup = "3",
        text = paste0(
          "Great Britain: ",
          percent(ElecPayments[which(ElecPayments$`Great Britain` > 0 | ElecPayments$`Great Britain` < 0),][-1,]$`Great Britain`, accuracy = 1),
          "\nYear: ",
          format(as.yearqtr(ElecPayments[which(ElecPayments$`Great Britain` > 0 | ElecPayments$`Great Britain` < 0),][-1,]$Year), "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = BarColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE
        ),
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
  
  
  output$GasPaymentsTable = renderDataTable({
    
    Data  <- read_delim("Processed Data/Output/Energy Bills/FixedTariffGas.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(Data) <- c("Year", "North Scotland", "South Scotland", "Great Britain")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    Data$Year <- format(Data$Year, "%Y Q%q")
    ElecPayments <- Data
    
    datatable(
      ElecPayments,
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
        title = "Proportion of gas customers on a fixed tariff",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of gas customers on a fixed tariff',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of gas customers on a fixed tariff')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(2:4, 0) 
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/FixedTariffs.txt")[2])
                     
                   )))
  })
 
  observeEvent(input$ToggleTable, {
    toggle("ElecPaymentsTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("GasPaymentsTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  
  output$ElecPayments.png <- downloadHandler(
    filename = "ElecPayments.png",
    content = function(file) {
      ElectricityBillPaymentMethods  <- read_delim("Processed Data/Output/Energy Bills/FixedTariffElectricity.txt", 
                                                   "\t", escape_double = FALSE, trim_ws = TRUE)
      
      names(ElectricityBillPaymentMethods) <- c("Year", "North Scotland", "South Scotland", "Great Britain")
      
      ElectricityBillPaymentMethods$Year <- ymd(ElectricityBillPaymentMethods$Year)
      
      ElectricityBillPaymentMethods$Year <- as.yearqtr(ElectricityBillPaymentMethods$Year)
      
      ### variables
      ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
      
      BarColours <-
        c(    "#0868ac","#43a2ca","#7bccc4"
        )
      sourcecaption = "Source: BEIS"
      plottitle = "Proportion of electricity customers\non a fixed tariff"
      
      width = max(ElectricityBillPaymentMethods$Year) - min(ElectricityBillPaymentMethods$Year)
      
      #ElectricityBillPaymentMethods$`North Scotland`Percentage <- PercentLabel(ElectricityBillPaymentMethods$`North Scotland`)
      
      
      ElectricityBillPaymentMethodsChart <- ElectricityBillPaymentMethods %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = `North Scotland`,
              label = percent(`North Scotland`)),
          colour = BarColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - (width * 0.05),
            y = `North Scotland`,
            label = ifelse(Year == min(Year), percent(`North Scotland`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = -0.5,
            fontface = 2
          ),
          colour = BarColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + (width * 0.1),
            y = `North Scotland`,
            label = ifelse(Year == max(Year), percent(`North Scotland`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = -.5,
            fontface = 2
          ),
          colour = BarColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElectricityBillPaymentMethods, 1),
          aes(x = Year,
              y = `North Scotland`,
              show_guide = FALSE),
          colour = BarColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElectricityBillPaymentMethods$Year),
          y = mean(ElectricityBillPaymentMethods$`North Scotland`),
          label = "North Scotland",
          hjust = 0.5,
          vjust = -4,
          colour = BarColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `South Scotland`,
              label = paste0(`South Scotland` * 100, "%")),
          colour = BarColours[2],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - (width * 0.05),
            y = `South Scotland`,
            label = ifelse(Year == min(Year), percent(`South Scotland`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + (width * 0.1),
            y = `South Scotland`,
            label = ifelse(Year == max(Year), percent(`South Scotland`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = .5,
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElectricityBillPaymentMethods, 1),
          aes(x = Year,
              y = `South Scotland`,
              show_guide = FALSE),
          colour = BarColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElectricityBillPaymentMethods$Year),
          y = mean(ElectricityBillPaymentMethods$`South Scotland`),
          label = "South Scotland",
          hjust = 0.5,
          vjust = 5,
          colour = BarColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Great Britain`,
              label = paste0(`Great Britain` * 100, "%")),
          colour = BarColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - (width * 0.05),
            y = `Great Britain`,
            label = ifelse(Year == min(Year), percent(`Great Britain`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + (width * 0.1),
            y = `Great Britain`,
            label = ifelse(Year == max(Year), percent(`Great Britain`,  accuracy = 1), ""),
            hjust = 0.5,
            vjust = 0,
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElectricityBillPaymentMethods, 1),
          aes(x = Year,
              y = `Great Britain`,
              
              show_guide = FALSE),
          size = 4,
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElectricityBillPaymentMethods$Year),
          y = mean(ElectricityBillPaymentMethods$`Great Britain`),
          label = "Great Britain",
          hjust = 0.5,
          vjust = -2.5,
          colour = BarColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
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
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      ElectricityBillPaymentMethodsChart
      
      ElectricityBillPaymentMethodsChart <-
        StackedArea(ElectricityBillPaymentMethodsChart,
                    ElectricityBillPaymentMethods,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      
      ElectricityBillPaymentMethodsChart <- ElectricityBillPaymentMethodsChart +
        ylim(0 - (max(ElectricityBillPaymentMethods[2:4])*0.01),(max(ElectricityBillPaymentMethods[2:4])*1.15))
      
      ggsave(
        file,
        plot =  ElectricityBillPaymentMethodsChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$GasPayments.png <- downloadHandler(
    filename = "GasPayments.png",
    content = function(file) {
      
      GasBillPaymentMethods  <- read_delim("Processed Data/Output/Energy Bills/FixedTariffGas.txt", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)
      
      names(GasBillPaymentMethods) <- c("Year", "North Scotland", "South Scotland", "Great Britain")
      
      GasBillPaymentMethods$Year <- ymd(GasBillPaymentMethods$Year)
      
      GasBillPaymentMethods$Year <- as.yearqtr(GasBillPaymentMethods$Year)
      
      ### variables
      ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
      
      BarColours <-
        c(    "#0868ac","#43a2ca","#7bccc4"
        )
      sourcecaption = "Source: BEIS"
      plottitle = "Proportion of gas customers\non a fixed tariff"
      
      width = max(GasBillPaymentMethods$Year) - min(GasBillPaymentMethods$Year)
      
      #GasBillPaymentMethods$`North Scotland`Percentage <- PercentLabel(GasBillPaymentMethods$`North Scotland`)
      
      
      GasBillPaymentMethodsChart <- GasBillPaymentMethods %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = `North Scotland`,
              label = percent(`North Scotland`)),
          colour = BarColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - (width * 0.05),
            y = `North Scotland`,
            label = ifelse(Year == min(Year), percent(`North Scotland`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = -0.5,
            fontface = 2
          ),
          colour = BarColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + (width * 0.1),
            y = `North Scotland`,
            label = ifelse(Year == max(Year), percent(`North Scotland`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = -.5,
            fontface = 2
          ),
          colour = BarColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GasBillPaymentMethods, 1),
          aes(x = Year,
              y = `North Scotland`,
              show_guide = FALSE),
          colour = BarColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GasBillPaymentMethods$Year),
          y = mean(GasBillPaymentMethods$`North Scotland`),
          label = "North Scotland",
          hjust = 0.5,
          vjust = -6,
          colour = BarColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `South Scotland`,
              label = paste0(`South Scotland` * 100, "%")),
          colour = BarColours[2],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - (width * 0.05),
            y = `South Scotland`,
            label = ifelse(Year == min(Year), percent(`South Scotland`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + (width * 0.1),
            y = `South Scotland`,
            label = ifelse(Year == max(Year), percent(`South Scotland`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = .5,
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GasBillPaymentMethods, 1),
          aes(x = Year,
              y = `South Scotland`,
              show_guide = FALSE),
          colour = BarColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GasBillPaymentMethods$Year),
          y = mean(GasBillPaymentMethods$`South Scotland`),
          label = "South Scotland",
          hjust = 0.5,
          vjust = 3.5,
          colour = BarColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Great Britain`,
              label = paste0(`Great Britain` * 100, "%")),
          colour = BarColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - (width * 0.05),
            y = `Great Britain`,
            label = ifelse(Year == min(Year), percent(`Great Britain`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + (width * 0.1),
            y = `Great Britain`,
            label = ifelse(Year == max(Year), percent(`Great Britain`,  accuracy = 1), ""),
            hjust = 0.5,
            vjust = 0,
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GasBillPaymentMethods, 1),
          aes(x = Year,
              y = `Great Britain`,
              
              show_guide = FALSE),
          size = 4,
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GasBillPaymentMethods$Year),
          y = mean(GasBillPaymentMethods$`Great Britain`),
          label = "Great Britain",
          hjust = 0.5,
          vjust = -5.5,
          colour = BarColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
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
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      GasBillPaymentMethodsChart
      
      GasBillPaymentMethodsChart <-
        StackedArea(GasBillPaymentMethodsChart,
                    GasBillPaymentMethods,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      
      GasBillPaymentMethodsChart <- GasBillPaymentMethodsChart +
        ylim(0 - (max(GasBillPaymentMethods[2:4])*0.01),(max(GasBillPaymentMethods[2:4])*1.15))
      
      ggsave(
        file,
        plot =  GasBillPaymentMethodsChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
}
