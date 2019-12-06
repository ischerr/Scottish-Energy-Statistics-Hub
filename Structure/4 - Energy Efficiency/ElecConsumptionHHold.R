require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ElecConsumptionHHoldOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Average domestic electricity consumption", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('ElecConsumptionHHoldSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecConsumptionHHold.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("ElecConsumptionHHoldPlot")),
    plotlyOutput(ns("ElecConsumptionHHoldPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10, h3("Data", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ElecConsumptionHHoldTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
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
ElecConsumptionHHold <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ElecConsumptionHHold.R")

  
  output$ElecConsumptionHHoldSubtitle <- renderText({
    
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
  
  output$ElecConsumptionHHoldPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Elec consump household",
      col_names = FALSE,
      skip = 13
    )
    
    names(Data) <- c("Year", "Consumption")
    
    Data[1:2] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[1,1] <- "Baseline\n2005/2007"
    
    Data[2,1] <- " "
    
    Data <- head(Data, -3)
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    Data$RowNumber <- as.numeric(rownames(Data))
    
    Data[is.na(Data)] <- 0
    
    DataTail <- tail(Data,1)
    
    DataLatest <- Data[nrow(Data)-1,]
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Consumption`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Consumption",
        text = paste0("Consumption: ", format(round(Data$`Consumption`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = ChartColours[1]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        y = ~ Year,
        x = ~ Data$`Consumption` + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Consumption` >0, paste("<b>",format(round((Data$`Consumption`), digits = 0), big.mark = ","),"MW</b>")," "),
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
        x = mean(DataLatest$`Consumption`)/2,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = ChartColours[1]),
        text = paste0("<b>", percent(DataTail$`Consumption`, accuracy = 0.1), "</b>")
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
          range = c(0,6500)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  
  output$ElecConsumptionHHoldTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Elec consump household",
      col_names = FALSE,
      skip = 13
    )
    
    names(Data) <- c("Year", "Consumption")
    
    Data[1:2] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[1,1] <- " Baseline\n2005/2007"
    
    
    Data <- head(Data, -3)
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
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
      formatRound(2, 0)%>% 
      formatStyle(c(2), fontWeight = 'bold')
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/ElecConsumptionHHold.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("ElecConsumptionHHoldTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ElecConsumptionHHold.png <- downloadHandler(
    filename = "ElecConsumptionHHold.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Elec consump household", skip = 12, col_names = TRUE)
      
      Data[1,1] <- 2003
      
      names(Data) <- c("Year", "Consumption")
      
      Data <- Data[complete.cases(Data),]
      
      Data[nrow(Data),1] <- max(as.numeric(Data$Year),na.rm = TRUE)+1
      
      Data$Total <- Data$Consumption
      
      Data$Year <- as.numeric(Data$Year)
      
      ElecConsumptionHHold <- Data
      
      ElecConsumptionHHold <-
        ElecConsumptionHHold[order(-ElecConsumptionHHold$Year),]
      
      ElecConsumptionHHold <-
        melt(ElecConsumptionHHold, id.vars = "Year")
      
      FinalConsumptionMax <-
        subset(ElecConsumptionHHold,
               Year == max(ElecConsumptionHHold$Year))
      
      ElecConsumptionHHold <-
        subset(
          ElecConsumptionHHold,
          Year < max(ElecConsumptionHHold$Year) & variable != "Total"
        )
      
      ElecConsumptionHHold$variable <-
        factor(ElecConsumptionHHold$variable,
               levels = unique(ElecConsumptionHHold$variable))
      
      ElecConsumptionHHold <- ElecConsumptionHHold %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Average domestic electricity consumption"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
      
      
      ElecConsumptionHHoldChart <- ElecConsumptionHHold %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Consumption" = ChartColours[1],
            "Gas" = BarColours[2],
            "Electricity" = BarColours[3],
            "Bioenergy & wastes" = BarColours[4],
            "Coal" = BarColours[5],
            "Manufactured fuels" = BarColours[6]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = ElecConsumptionHHold$top/2,
          label = ifelse(
            ElecConsumptionHHold$value < 7000,
            paste0(format(
              round(ElecConsumptionHHold$top, digits = 0), big.mark = ","
            ), " kWh"),
            ""
          ),
          family = "Century Gothic",
          fontface = 2,
          color = "white"
        ) +
        geom_text(
          y = -750,
          label =   ifelse(
            ElecConsumptionHHold$value < 7000,
            ifelse(
              ElecConsumptionHHold$Year == 2003,
              "2005/2007\n(baseline)",
              ElecConsumptionHHold$Year
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        annotate(
          "text",
          x = max(ElecConsumptionHHold$Year) + 1.2,
          y = as.numeric(
            subset(
              ElecConsumptionHHold,
              Year == max(ElecConsumptionHHold$Year) &
                variable == "Consumption"
            )[1, 5]
          ) - as.numeric(
            subset(
              ElecConsumptionHHold,
              Year == max(ElecConsumptionHHold$Year) &
                variable == "Consumption"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Consumption")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(ElecConsumptionHHold$Year) + 1.2,
          y = as.numeric(
            subset(
              ElecConsumptionHHold,
              Year == max(ElecConsumptionHHold$Year) &
                variable == "Gas"
            )[1, 5]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Total")[1, 3]
          ), accuracy  = .1),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic",
          hjust = -.75
        ) + annotate(
          "text",
          x = max(ElecConsumptionHHold$Year) + 1.2,
          y = -700,
          label = "% Change\nfrom baseline",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      
      ElecConsumptionHHoldChart
      
      
      ElecConsumptionHHoldChart <-
        BaselineChart(
          ElecConsumptionHHoldChart,
          ElecConsumptionHHold,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      ElecConsumptionHHoldChart <-
        ElecConsumptionHHoldChart +
        coord_flip() +
        labs(subtitle = paste("Scotland, 2005 -", max(ElecConsumptionHHold$Year))) +
        ylim(-1000, max(ElecConsumptionHHold$top)) +
        xlim(max(ElecConsumptionHHold$Year) + 1.2, 2002)
      
      ElecConsumptionHHoldChart
      
      ggsave(
        file,
        plot = ElecConsumptionHHoldChart,
        width = 17,
        height = 15.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
