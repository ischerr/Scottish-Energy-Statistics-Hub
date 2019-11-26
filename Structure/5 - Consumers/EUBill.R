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
    fluidRow(column(8,
                    h3("Total final energy consumption by consuming sector", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('EUBillSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EUBill.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("EUBillPlot")),
    plotlyOutput(ns("EUBillPlot"), height =  "900px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EUBillTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
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

  
  output$EUBillSubtitle <- renderText({
    
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
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$EUBillPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Elec consump",
      col_names = FALSE,
      skip = 12
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2,1] <- "Baseline\n2005/2007"
    
    Data[3,1] <- ""
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    Data <- Data[-1,]
    
    Data$RowNumber <- as.numeric(rownames(Data))
    
    Data[is.na(Data)] <- 0
    
    DataTail <- tail(Data,1)
    
    DataLatest <- Data[nrow(Data)-1,]
    
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Domestic`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Domestic",
        text = paste0("Domestic: ", format(round(Data$`Domestic`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Non-domestic`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Non-domestic",
        text = paste0("Non-domestic: ", format(round(Data$`Non-domestic`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        y = ~ Year,
        x = ~ (Data$`Domestic` + Data$`Non-domestic`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Domestic` >0, paste("<b>",format(round((Data$`Domestic` + Data$`Non-domestic`), digits = 0), big.mark = ","),"MW</b>")," "),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      )  %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Domestic`)/2,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[1]),
        text = paste0("<b>", percent(DataTail$`Domestic`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x =  mean(DataLatest$`Domestic`) + (mean(DataLatest$`Non-domestic`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[3]),
        text =  paste0("<b>", percent(DataTail$`Non-domestic`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Total`)+ 1500,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = ChartColours[1]),
        text =  paste0("<b>", percent(DataTail$Total, accuracy = 0.1), "</b>")
      ) %>% 
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
                     ticktext = as.list(Data$Year),
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
          range = c(0,35000)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  
  output$EUBillTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Elec consump",
      col_names = FALSE,
      skip = 12
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[2,1] <- " Baseline\n2005/2007"
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
  
    Data <- Data[-1,]
    
    Data <- head(Data, -1)
    
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
        order = list(list(0, 'desc')),
        title = "Energy Consumption",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Energy Consumption',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Energy Consumption')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:4, 0)%>% 
      formatStyle(c(4), fontWeight = 'bold')
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/EUBill.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("EUBillTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EUBill.png <- downloadHandler(
    filename = "EUBill.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Bill prices in EU", skip = 14,  col_names = TRUE)[c(1,3,4)
                                                                                    ]
      names(Data) <- c("Countries", "Price (excluding tax)", "Tax component")
      
      Data <- Data[complete.cases(Data),]
      
      EUFlagLookup <- read_csv("J:/ENERGY BRANCH/Statistics/Energy Statistics Processing/Releases and Publications/Energy Statistics Database/Charts/Sections/EUFlagLookup.csv")
      
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
}
