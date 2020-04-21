require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

BillPaymentsOutput <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    tabPanel("Electricity",
    fluidRow(column(8,
                    h3("Proportion of payment methods used for electricity bills", style = "color: #68c3ea;  font-weight:bold"),
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
                             h3("Proportion of payment methods used for gas bills", style = "color: #68c3ea;  font-weight:bold"),
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
    column(10, h3("Data - Proportion of payment methods used for electricity bills", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ElecPaymentsTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas",
             fluidRow(
               column(10, h3("Data - Proportion of payment methods used for gas bills", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasPaymentsTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
      column(1,
             p("Next update:")),
      column(2,
             DateLookup(c("BEISPaymentMethodElec", "BEISPaymentMethodGas"))),
      column(1, align = "right",
             p("Sources:")),
      column(
        8,
        align = "right",
        SourceLookup("BEISPaymentMethodElec"),
        SourceLookup("BEISPaymentMethodGas")
        
      )
    )
  )
}




###### Server ######
BillPayments <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("BillPayments.R")

  
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
    ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: BEIS"
    plottitle = "Proportion of payment methods used\nfor electricity bills"

    
    
    p <-  plot_ly(data = ElecPayments,
                  x = ~ Year ) %>% 
      add_trace(y = ~ `Prepayment - Scotland`,
                name = "Prepayment",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Prepayment: ",
                  percent(ElecPayments$`Prepayment - Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(ElecPayments$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecPayments[which(ElecPayments$`Prepayment - Scotland` > 0 | ElecPayments$`Prepayment - Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `Prepayment - Scotland`,
        name = "Prepayment",
        text = paste0(
          "Prepayment: ",
          percent(ElecPayments[which(ElecPayments$`Prepayment - Scotland` > 0 | ElecPayments$`Prepayment - Scotland` < 0),][-1,]$`Prepayment - Scotland`, accuracy = 1),
          "\nYear: ",
          format(ElecPayments[which(ElecPayments$`Prepayment - Scotland` > 0 | ElecPayments$`Prepayment - Scotland` < 0),][-1,]$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = ElecPayments,
                x = ~ Year,
                y = ~ `Standard Credit - Scotland`,
                name = "Standard Credit",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Standard Credit: ",
                  percent(ElecPayments$`Standard Credit - Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(ElecPayments$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecPayments[which(ElecPayments$`Standard Credit - Scotland` > 0 | ElecPayments$`Standard Credit - Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `Standard Credit - Scotland`,
        name = "Standard Credit",
        legendgroup = "2",
        text = paste0(
          "Standard Credit: ",
          percent(ElecPayments[which(ElecPayments$`Standard Credit - Scotland` > 0 | ElecPayments$`Standard Credit - Scotland` < 0),][-1,]$`Standard Credit - Scotland`, accuracy = 1),
          "\nYear: ",
          format(ElecPayments[which(ElecPayments$`Standard Credit - Scotland` > 0 | ElecPayments$`Standard Credit - Scotland` < 0),][-1,]$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_trace(data = ElecPayments,
                x = ~ Year,
                y = ~ `Direct Debit - Scotland`,
                name = "Direct Debit",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Direct Debit: ",
                  percent(ElecPayments$`Direct Debit - Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(ElecPayments$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecPayments[which(ElecPayments$`Direct Debit - Scotland` > 0 | ElecPayments$`Direct Debit - Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `Direct Debit - Scotland`,
        name = "Direct Debit",
        legendgroup = "3",
        text = paste0(
          "Direct Debit: ",
          percent(ElecPayments[which(ElecPayments$`Direct Debit - Scotland` > 0 | ElecPayments$`Direct Debit - Scotland` < 0),][-1,]$`Direct Debit - Scotland`, accuracy = 1),
          "\nYear: ",
          format(ElecPayments[which(ElecPayments$`Direct Debit - Scotland` > 0 | ElecPayments$`Direct Debit - Scotland` < 0),][-1,]$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
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
                     showgrid = FALSE,
                     range = c(min(ElecPayments$Year)-1
                               , max(ElecPayments$Year)+1)),
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
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Elec payment methods",
        col_names = TRUE,
        skip = 12
      )
    
    names(Data)[1] <- "Year"
    
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
        title = "Electricity Bill Payment Methods",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity Bill Payment Methods',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity Bill Payment Methods')
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
    ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: BEIS"
    plottitle = "Proportion of payment methods used\nfor Gastricity bills"
    
    
    
    p <-  plot_ly(data = GasPayments,
                  x = ~ Year ) %>% 
      add_trace(y = ~ `Prepayment - Scotland`,
                name = "Prepayment",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Prepayment: ",
                  percent(GasPayments$`Prepayment - Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(GasPayments$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(GasPayments[which(GasPayments$`Prepayment - Scotland` > 0 | GasPayments$`Prepayment - Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `Prepayment - Scotland`,
        name = "Prepayment",
        text = paste0(
          "Prepayment: ",
          percent(GasPayments[which(GasPayments$`Prepayment - Scotland` > 0 | GasPayments$`Prepayment - Scotland` < 0),][-1,]$`Prepayment - Scotland`, accuracy = 1),
          "\nYear: ",
          format(GasPayments[which(GasPayments$`Prepayment - Scotland` > 0 | GasPayments$`Prepayment - Scotland` < 0),][-1,]$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = GasPayments,
                x = ~ Year,
                y = ~ `Standard Credit - Scotland`,
                name = "Standard Credit",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Standard Credit: ",
                  percent(GasPayments$`Standard Credit - Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(GasPayments$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(GasPayments[which(GasPayments$`Standard Credit - Scotland` > 0 | GasPayments$`Standard Credit - Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `Standard Credit - Scotland`,
        name = "Standard Credit",
        legendgroup = "2",
        text = paste0(
          "Standard Credit: ",
          percent(GasPayments[which(GasPayments$`Standard Credit - Scotland` > 0 | GasPayments$`Standard Credit - Scotland` < 0),][-1,]$`Standard Credit - Scotland`, accuracy = 1),
          "\nYear: ",
          format(GasPayments[which(GasPayments$`Standard Credit - Scotland` > 0 | GasPayments$`Standard Credit - Scotland` < 0),][-1,]$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_trace(data = GasPayments,
                x = ~ Year,
                y = ~ `Direct Debit - Scotland`,
                name = "Direct Debit",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Direct Debit: ",
                  percent(GasPayments$`Direct Debit - Scotland`, accuracy = 1),
                  "\nYear: ",
                  format(GasPayments$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(GasPayments[which(GasPayments$`Direct Debit - Scotland` > 0 | GasPayments$`Direct Debit - Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `Direct Debit - Scotland`,
        name = "Direct Debit",
        legendgroup = "3",
        text = paste0(
          "Direct Debit: ",
          percent(GasPayments[which(GasPayments$`Direct Debit - Scotland` > 0 | GasPayments$`Direct Debit - Scotland` < 0),][-1,]$`Direct Debit - Scotland`, accuracy = 1),
          "\nYear: ",
          format(GasPayments[which(GasPayments$`Direct Debit - Scotland` > 0 | GasPayments$`Direct Debit - Scotland` < 0),][-1,]$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
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
                     showgrid = FALSE,
                     range = c(min(GasPayments$Year)-1
                               , max(GasPayments$Year)+1)),
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
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Gas payment methods",
        col_names = TRUE,
        skip = 12
      )
    
    names(Data)[1] <- "Year"
    
    Data$Year <- as.yearqtr(Data$Year)
    
    Data$Year <- format(Data$Year, "%Y Q%q")
    GasPayments <- Data
    
    datatable(
      GasPayments,
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
        title = "Gas Bill Payment Methods",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Gas Bill Payment Methods',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Gas Bill Payment Methods')
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
                     paste(readtext("Structure/5 - Consumers/BillPayments.txt")[2])
                     
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

      ElecBillPaymentMethods <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Elec payment methods",
          col_names = TRUE,
          skip = 12
        )
      
      names(ElecBillPaymentMethods) <- c("Year", "Prepayment", "Credit", "Direct Debit")
      
      ElecBillPaymentMethods$Year <- as.yearqtr(ElecBillPaymentMethods$Year)
      
      ### variables
      ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = "Proportion of payment methods used\nfor electricity bills"
      
      #ElecBillPaymentMethods$`Prepayment`Percentage <- PercentLabel(ElecBillPaymentMethods$`Prepayment`)
      
      
      ElecBillPaymentMethodsChart <- ElecBillPaymentMethods %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = `Prepayment`,
              label = percent(`Prepayment`)),
          colour = ChartColours[2],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .45,
            y = `Prepayment`,
            label = ifelse(Year == min(Year), percent(`Prepayment`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .8,
            y = `Prepayment`,
            label = ifelse(Year == max(Year), percent(`Prepayment`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = 1,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElecBillPaymentMethods, 1),
          aes(x = Year,
              y = `Prepayment`,
              show_guide = FALSE),
          colour = ChartColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecBillPaymentMethods$Year),
          y = mean(ElecBillPaymentMethods$`Prepayment`),
          label = "Prepayment",
          hjust = 0.5,
          vjust = 2,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Credit`,
              label = paste0(`Credit` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .45,
            y = `Credit`,
            label = ifelse(Year == min(Year), percent(`Credit`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .8,
            y = `Credit`,
            label = ifelse(Year == max(Year), percent(`Credit`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = -0.2,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElecBillPaymentMethods, 1),
          aes(x = Year,
              y = `Credit`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecBillPaymentMethods$Year),
          y = mean(ElecBillPaymentMethods$`Credit`),
          label = "Credit",
          hjust = 0.5,
          vjust = -.5,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Direct Debit`,
              label = paste0(`Direct Debit` * 100, "%")),
          colour = ChartColours[4],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .45,
            y = `Direct Debit`,
            label = ifelse(Year == min(Year), percent(`Direct Debit`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .8,
            y = `Direct Debit`,
            label = ifelse(Year == max(Year), percent(`Direct Debit`,  accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElecBillPaymentMethods, 1),
          aes(x = Year,
              y = `Direct Debit`,
              
              show_guide = FALSE),
          size = 4,
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecBillPaymentMethods$Year),
          y = mean(ElecBillPaymentMethods$`Direct Debit`),
          label = "Direct Debit",
          hjust = 0.5,
          vjust = 2.5,
          colour = ChartColours[4],
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
      
      ElecBillPaymentMethodsChart
      
      ElecBillPaymentMethodsChart <-
        StackedArea(ElecBillPaymentMethodsChart,
                    ElecBillPaymentMethods,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ElecBillPaymentMethodsChart
      
      ggsave(
        file,
        plot =  ElecBillPaymentMethodsChart,
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
      
      GasBillPaymentMethods <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Gas payment methods",
          col_names = TRUE,
          skip = 12
        )
      
      names(GasBillPaymentMethods) <- c("Year", "Prepayment", "Credit", "Direct Debit")
      
      GasBillPaymentMethods$Year <- as.yearqtr(GasBillPaymentMethods$Year)
      
      ### variables
      ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = "Proportion of payment methods used\nfor gas bills"
      
      #GasBillPaymentMethods$`Prepayment`Percentage <- PercentLabel(GasBillPaymentMethods$`Prepayment`)
      
      
      GasBillPaymentMethodsChart <- GasBillPaymentMethods %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = `Prepayment`,
              label = percent(`Prepayment`)),
          colour = ChartColours[2],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .45,
            y = `Prepayment`,
            label = ifelse(Year == min(Year), percent(`Prepayment`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .8,
            y = `Prepayment`,
            label = ifelse(Year == max(Year), percent(`Prepayment`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = .5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GasBillPaymentMethods, 1),
          aes(x = Year,
              y = `Prepayment`,
              show_guide = FALSE),
          colour = ChartColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GasBillPaymentMethods$Year),
          y = mean(GasBillPaymentMethods$`Prepayment`),
          label = "Prepayment",
          hjust = 0.5,
          vjust = 2,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Credit`,
              label = paste0(`Credit` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .45,
            y = `Credit`,
            label = ifelse(Year == min(Year), percent(`Credit`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .8,
            y = `Credit`,
            label = ifelse(Year == max(Year), percent(`Credit`, accuracy = 1), ""),
            hjust = 0.5,
            vjust = .5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GasBillPaymentMethods, 1),
          aes(x = Year,
              y = `Credit`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GasBillPaymentMethods$Year),
          y = mean(GasBillPaymentMethods$`Credit`),
          label = "Credit",
          hjust = 0.5,
          vjust = -.5,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Direct Debit`,
              label = paste0(`Direct Debit` * 100, "%")),
          colour = ChartColours[4],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .45,
            y = `Direct Debit`,
            label = ifelse(Year == min(Year), percent(`Direct Debit`, accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .8,
            y = `Direct Debit`,
            label = ifelse(Year == max(Year), percent(`Direct Debit`,  accuracy = 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GasBillPaymentMethods, 1),
          aes(x = Year,
              y = `Direct Debit`,
              
              show_guide = FALSE),
          size = 4,
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(GasBillPaymentMethods$Year),
          y = mean(GasBillPaymentMethods$`Direct Debit`),
          label = "Direct Debit",
          hjust = 0.5,
          vjust = 2.5,
          colour = ChartColours[4],
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
      
      
      GasBillPaymentMethodsChart
      
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
