require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EUBillOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Electricity",
    fluidRow(column(8,
                    h3("Total price (Pence per kWh) of Electricity Bills in EU 15, including breakdown.", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('EUBillElecSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EUBillElec.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("EUBillElecPlot")),
    plotlyOutput(ns("EUBillElecPlot"), height =  "900px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas",
             fluidRow(column(8,
                             h3("Total price (Pence per kWh) of gas Bills in EU 15, including breakdown.", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('EUBillGasSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EUBillGas.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("EUBillGasPlot")),
             plotlyOutput(ns("EUBillGasPlot"), height =  "900px")%>% withSpinner(color="#68c3ea"),
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
    column(10, h3("Data - Electricity", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EUBillElecTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas",
             fluidRow(
               column(10, h3("Data - Gas", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EUBillGasTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
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
EUBill <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EUBill.R")

  
  output$EUBillElecSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump sector",
      col_names = FALSE,
      skip = 12,
      n_max = 7
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:7] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland, 2017")
  })
  
  output$EUBillElecPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Bill prices in EU",
      col_names = TRUE,
      skip = 14
    )[c(1,3,4, 5)]
    
    names(Data) <- c("Country", "PreTax", "Tax", "Total")
    
    Data[2:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[complete.cases(Data),]
    
    Data$Country <- paste0("<b>", str_wrap(Data$Country, 5),"</b>") 
    
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <- c("#253494", "#1d91c0")
    
    p <- plot_ly(data = Data, y = ~ Country) %>%
      
      add_trace(
        data = Data,
        x = ~ `PreTax`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Price (excluding tax)",
        text = paste0("Price (excluding tax):", format(round(Data$`PreTax`, digits = 2), big.mark = ","), ""),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Tax`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Tax",
        text = paste0("Tax Component: ", format(round(Data$`Tax`, digits = 2), big.mark = ","), ""),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        y = ~ Country,
        x = ~ (Data$`PreTax` + Data$`Tax`) + 0.1,
        name = "Total Cost",
        legendgroup = 3,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`PreTax` >0, paste0("<b>",format(round((Data$`PreTax` + Data$`Tax`), digits = 2), big.mark = ","),"</b>")," "),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      )  %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$Country),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          range = c(0,33)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  output$EUBillElecTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Bill prices in EU",
      col_names = TRUE,
      skip = 14
    )[c(1,3,4, 5)]
    
    names(Data) <- c("Country", "Price (excluding tax)", "Tax component", "Total Cost
")
    
    Data[2:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[complete.cases(Data),]
    
    datatable(
      Data,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Total price (Pence per kWh) of Electricity Bills in EU 15, including breakdown",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Total price (Pence per kWh) of Electricity Bills in EU 15, including breakdown',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Total price (Pence per kWh) of Electricity Bills in EU 15, including breakdown')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:4, 2)%>% 
      formatStyle(c(4), fontWeight = 'bold')
  })
  
  output$EUBillElec.png <- downloadHandler(
    filename = "EUBillElec.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Bill prices in EU", skip = 14,  col_names = TRUE)[c(1,3,4)
                                                                                    ]
      names(Data) <- c("Countries", "Price (excluding tax)", "Tax component")
      
      Data <- Data[complete.cases(Data),]
      
      EUFlagLookup <- read_csv("Structure/EUFlagLookup.csv")
      
      EUFlagLookup <- EUFlagLookup %>% mutate(Countries = replace(Countries, Countries == "U.K.", "United Kingdom")) %>% mutate(Countries = replace(Countries, Countries == "EU (28)", "EU (15)"))
      
      Data <- merge(Data, EUFlagLookup)
      
      Data <- Data[order(Data$`Price (excluding tax)` + Data$`Tax component`),]
      
      Data <- rbind(Data[which(Data$Flag != "eu"),], Data[which(Data$Flag == "eu"),])
      
      EUElecBills <- Data
      
      EUElecBills <- EUElecBills[c(1, ncol(EUElecBills):2)]
      
      EUElecBills <- arrange(EUElecBills,row_number())
      
      EUElecBills$Countries <-
        factor(EUElecBills$Countries, levels = unique(EUElecBills$Countries), ordered = TRUE)
      
      EUElecBills <- melt(EUElecBills, id =  c("Countries", "Flag"))
      
      
      EUElecBills$variable <-
        factor(EUElecBills$variable, levels = unique(EUElecBills$variable))
      
      EUElecBills <- EUElecBills %>%
        group_by(Countries) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Total price of electricity bills in EU 15, including breakdown\n(pence per kWh)"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#253494", "#1d91c0")
      
      
      EUElecBillsChart <- EUElecBills %>%
        ggplot(aes(x = Countries, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Price (excluding tax)" = BarColours[1],
                                     "Tax component" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = top+.3,
            label = sprintf("%.2f", round(top, digits = 2))
          ),
          hjust = 0,
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            y = top- pos,
            label =sprintf("%.2f", round(value, digits = 2))
          ),
          fontface = 2,
          colour =  "white",
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            y = -2,
            label = Countries
          ),
          fontface = 2,
          hjust = 1,
          colour =  ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_flag(aes(
          y = -1 ,
          size = 50,
          country = Flag
        ),
        size = 7
        ) +
        geom_text(
          aes(x = 17,
              y = 11.07/2,
              label = "Price, excluding tax"),
          fontface = 2,
          colour =  BarColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = 17,
              y = 11.07+ (6.23/2),
              label = "Tax component"),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = 17.5,
              y = 11.07+ (6.23/2),
              label = " "),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic"
        )
      
      EUElecBillsChart
      
      
      EUElecBillsChart <-
        StackedBars(EUElecBillsChart,
                    EUElecBills,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      EUElecBillsChart
      
      EUElecBillsChart <-
        EUElecBillsChart +
        coord_flip() +
        labs(subtitle = "Jan 18 - June 18") +
        ylim(-7, max(EUElecBills$top) + 1)
      
      EUElecBillsChart
      
      ggsave(
        file,
        plot = EUElecBillsChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  ) 
  

 

  output$EUBillGasSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump sector",
      col_names = FALSE,
      skip = 12,
      n_max = 7
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:7] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland, 2017")
  })
  
  output$EUBillGasPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Bill prices in EU",
      col_names = TRUE,
      skip = 14
    ) [c(9,11,12,13)]
    
    names(Data) <- c("Country", "PreTax", "Tax", "Total")
    
    Data[2:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[!is.na(Data$Country),]
    
    Data$Country <- paste0("<b>", str_wrap(Data$Country, 5),"</b>") 
    
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <- c("#253494", "#1d91c0")
    
    p <- plot_ly(data = Data, y = ~ Country) %>%
      
      add_trace(
        data = Data,
        x = ~ `PreTax`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Price (excluding tax)",
        text = paste0("Price (excluding tax):", format(round(Data$`PreTax`, digits = 2), big.mark = ","), ""),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Tax`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Tax",
        text = paste0("Tax Component: ", format(round(Data$`Tax`, digits = 2), big.mark = ","), ""),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        y = ~ Country,
        x = ~ (Data$`PreTax` + Data$`Tax`) + 0.1,
        name = "Total Cost",
        legendgroup = 3,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`PreTax` >0, paste0("<b>",format(round((Data$`PreTax` + Data$`Tax`), digits = 2), big.mark = ","),"</b>")," "),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      )  %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$Country),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          range = c(0,13)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  output$EUBillGasTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Bill prices in EU",
      col_names = TRUE,
      skip = 14
    )[c(9,11,12,13)]
    
    names(Data) <- c("Country", "Price (excluding tax)", "Tax component", "Total Cost
")
    
    Data[2:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[complete.cases(Data),]
    
    datatable(
      Data,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Total price (Pence per kWh) of gas Bills in EU 15, including breakdown",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Total price (Pence per kWh) of gas Bills in EU 15, including breakdown',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Total price (Pence per kWh) of gas Bills in EU 15, including breakdown')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:4, 2)%>% 
      formatStyle(c(4), fontWeight = 'bold')
  })
  
  output$EUBillGas.png <- downloadHandler(
    filename = "EUBillGas.png",
    content = function(file) {
      
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Bill prices in EU", skip = 14, n_max = 15, col_names = TRUE)[c(9,11,12)]
      
      names(Data) <- c("Countries", "Price (excluding tax)", "Tax component")
      
      
      
      Data <- Data[complete.cases(Data),]
      
      EUFlagLookup <- read_csv("Structure/EUFlagLookup.csv")
      
      EUFlagLookup <- EUFlagLookup %>% mutate(Countries = replace(Countries, Countries == "U.K.", "United Kingdom")) %>% mutate(Countries = replace(Countries, Countries == "EU (28)", "EU (15)"))
      
      Data <- merge(Data, EUFlagLookup)
      
      Data <- Data[order(as.numeric(Data$`Price (excluding tax)`) + as.numeric(Data$`Tax component`)),]
      
      Data <- rbind(Data[which(Data$Flag != "eu"),], Data[which(Data$Flag == "eu"),])
      
      EUGasBills <- Data
      
      EUGasBills <- EUGasBills[c(1, ncol(EUGasBills):2)]
      
      EUGasBills <- arrange(EUGasBills,row_number())
      
      EUGasBills$Countries <-
        factor(EUGasBills$Countries, levels = unique(EUGasBills$Countries), ordered = TRUE)
      
      EUGasBills <- melt(EUGasBills, id =  c("Countries", "Flag"))
      
      
      EUGasBills$variable <-
        factor(EUGasBills$variable, levels = unique(EUGasBills$variable))
      
      EUGasBills <- EUGasBills %>%
        group_by(Countries) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Total price of gas bills in EU 15, including breakdown\n(pence per kWh)"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#253494", "#1d91c0")
      
      
      EUGasBillsChart <- EUGasBills %>%
        ggplot(aes(x = Countries, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Price (excluding tax)" = BarColours[1],
                                     "Tax component" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = top+.25,
            label = sprintf("%.2f", round(top, digits = 2))
          ),
          hjust = 0,
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            y = top- pos,
            label = ifelse(value >.5, sprintf("%.2f", round(value, digits = 2)), "")
          ),
          fontface = 2,
          colour =  "white",
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            y = -.6,
            label = Countries
          ),
          fontface = 2,
          hjust = 1,
          colour =  ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_flag(aes(
          y = -.3,
          size = 50,
          country = Flag
        ),
        size = 7
        ) +
        geom_text(
          aes(x = 16,
              y = 4.14/2,
              label = "Price, excluding tax"),
          fontface = 2,
          colour =  BarColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = 16,
              y = 4.14+ (1.70/2),
              label = "Tax component"),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = 16.5,
              y = 11.07+ (6.23/2),
              label = " "),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic"
        )
      
      EUGasBillsChart
      
      
      EUGasBillsChart <-
        StackedBars(EUGasBillsChart,
                    EUGasBills,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      EUGasBillsChart
      
      EUGasBillsChart <-
        EUGasBillsChart +
        coord_flip() +
        labs(subtitle = "Jan 18 - June 18") +
        ylim(-2.5, max(EUGasBills$top) + .3)
      
      EUGasBillsChart
      
      ggsave(
        file,
        plot = EUGasBillsChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  ) 
  
  
  
  observeEvent(input$ToggleTable, {
    toggle("EUBillElecTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EUBillGasTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
    output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/EUBill.txt")[2])
                     
                   )))
  })

}
