require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

FuelPovertyOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      
      tabPanel("Extreme Fuel Poverty",
               fluidRow(column(8,
                               h3("Rates of fuel poverty and extreme fuel poverty", style = "color: #68c3ea;  font-weight:bold"),
                               h4(textOutput(ns('ExtremeFuelPovertySubtitle')), style = "color: #68c3ea;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('ExtremeFuelPoverty.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
               #dygraphOutput(ns("ExtremeFuelPovertyPlot")),
               plotlyOutput(ns("ExtremeFuelPovertyPlot"), height =  "900px")%>% withSpinner(color="#68c3ea"),
               tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
      
      tabPanel("Proportions",
    fluidRow(column(8,
                    h3("Proportion of homes in fuel poverty by primary heating fuel and EPC band", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('FuelPovertyProportionSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('FuelPovertyProportion.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("FuelPovertyProportionPlot")),
    plotlyOutput(ns("FuelPovertyProportionPlot"), height =  "900px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      tabPanel("Fuel & EPC band",
               fluidRow(
                 column(10, h3("Data - Fuel & EPC band", style = "color: #68c3ea;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("ExtremeFuelPovertyTable"))%>% withSpinner(color="#68c3ea"))),
               tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
      tabPanel("Fuel Poverty Rates",
    fluidRow(
    column(8, h3("Data - Fuel Poverty Rates", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  downloadButton(ns('FuelPovertyProportionData.xlsx'), 'Download Data', style="float:right")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("FuelPovertyProportionTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("EPC Bands",
             fluidRow(
               column(10, h3("Data - Gas", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("FuelPovertySAPTable"))%>% withSpinner(color="#68c3ea"))),
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
FuelPoverty <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("FuelPoverty.R")

  
  output$FuelPovertyProportionSubtitle <- renderText({
    
    paste("Scotland, 2017")
  })
  
  output$FuelPovertyProportionPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Fuel poverty",
      col_names = FALSE,
      skip = 14,
      n_max = 11
    )[c(7,9)]
    
    names(Data) <- c("Type", "Proportion")
    
    Data[2,1] <- " "
    
    Data[7,1] <- "  "
    
    Data[is.na(Data)] <- 0
    
    Data$Type <- paste0("<b>", Data$Type, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <- c("#00441b", "#bcbddc","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
    
    p <- plot_ly(data = Data, y = ~ Type) %>%
      
      add_trace(
        data = Data,
        x = ~ `Proportion`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Proportion",
        text = paste0(Data$Type, ": ", format(percent(Data$`Proportion`, accuracy = 0.1), big.mark = ",")),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_annotations(
        x = 0.01,
        y = "<b> </b>",
        text = "<b><em>Primary heating fuel</em></b>",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "top",
        font = list(
          color = ChartColours[1]
        )
      ) %>% 
      add_annotations(
        x = 0.01,
        y = "<b>  </b>",
        text = "<b><em>EPC band</em></b>",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "top",
        font = list(
          color = ChartColours[1]
        )
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
                     ticktext = as.list(Data$Type),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
  })
  
  output$FuelPovertyProportionTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Fuel poverty",
      col_names = TRUE,
      skip = 13,
      n_max = 11
    )[7:13]
    
    names(Data) <- c("Type", "2017 - 000s", "2017 - %", "2017 - Sample", "2016 - 000s", "2016 - %", "2016 - Sample")
    
    Data[2,1] <- paste0("<em>", Data[2,1], "</em>")
    
    Data[7,1] <- paste0("<em>", Data[7,1], "</em>")
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Type'),
          th(colspan = 3, '2017'),
          th(colspan = 3, '2016')
        ),
        tr(
          lapply(rep(c('000s', '%', "Sample"), 2), th)
        )
      )
    ))
    
    datatable(
      Data,
      container = sketch,
      escape = FALSE,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Aggregate Energy Balance (thousand tonnes of oil equivalent)",
        dom = '',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Aggregate Energy Balance (thousand tonnes of oil equivalent)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Aggregate Energy Balance (thousand tonnes of oil equivalent)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%     
      formatPercentage(c(3,6), 0) 
  })

  output$FuelPovertyProportion.png <- downloadHandler(
    filename = "FuelPovertyProportion.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Fuel poverty", skip = 13, n_max = 11)[c(7,9)]
      
      Data[2,1] <- "y"
      Data[7,1] <- "z"
      
      names(Data) <- c("Type", "Percent")
      
      
      FuelPovertyBands <- Data
      
      FuelPovertyBands <- arrange(FuelPovertyBands,-row_number())
      
      FuelPovertyBands$Type <-
        factor(FuelPovertyBands$Type,
               levels = unique(FuelPovertyBands$Type))
      
      plottitle <-
        "Proportion of homes in fuel poverty by primary heating\nfuel and EPC band"
      sourcecaption <- "Source: SG"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#00441b", "#bcbddc","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
      
      
      FuelPovertyBandsChart <- FuelPovertyBands %>%
        ggplot(aes(x = Type, y = Percent), family = "Century Gothic") +
        geom_bar(stat = "identity", width = .8, fill = BarColours[2]) +
        geom_text(
          aes(
            y = - .015,
            label = ifelse(Type != "y" & Type != "z", as.character(Type), "")
          ),
          hjust = 1,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          aes(
            y = Percent + .07,
            label = ifelse(Type != "y" & Type != "z", percent(Percent), "")
          ),
          hjust = 1,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          aes(
            x = 4.7,
            y = 0,
            label = "EPC Band"
          ),
          hjust = 0,
          family = "Century Gothic",
          fontface = 4,
          color = ChartColours[1]
        ) +
        geom_text(
          aes(
            x = 9.7,
            y = 0,
            label = "Primary heating fuel"
          ),
          hjust = 0,
          family = "Century Gothic",
          fontface = 4,
          color = ChartColours[1]
        )
      
      
      FuelPovertyBandsChart
      
      
      FuelPovertyBandsChart <-
        StackedBars(FuelPovertyBandsChart,
                    FuelPovertyBands,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      FuelPovertyBandsChart <- FuelPovertyBandsChart +
        coord_flip() +
        ylim(-.055, max(FuelPovertyBands$Percent))+
        labs(subtitle = "Scotland, 2017")
      
      FuelPovertyBandsChart
      
      ggsave(
        file,
        plot = FuelPovertyBandsChart,
        width = 17,
        height = 15.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  ) 
  
  output$ExtremeFuelPovertySubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Fuel poverty",
      col_names = TRUE,
      skip = 13
    )
    
    Data <- Data[c(1,4,5)]
    
    Data <- Data[complete.cases(Data),]
    
    Data[2,1] <- "Oct-11" 
    
    names(Data) <- c("Year", "Total Fuel Poverty", "Extreme Fuel Poverty")
    
    paste("Scotland,", min(as.numeric(Data$Year), na.rm = TRUE ), "-", max(as.numeric(Data$Year), na.rm = TRUE))
  })
  
  output$ExtremeFuelPovertyPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Fuel poverty",
      col_names = TRUE,
      skip = 13
    )
    
    Data <- Data[c(1,4,5)]
    
    Data <- Data[complete.cases(Data),]
    
    Data[2,1] <- "Oct-11" 
    
    names(Data) <- c("Year", "Total Fuel Poverty", "Extreme Fuel Poverty")
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- paste0("<b>", str_wrap(Data$Year, 5),"</b>") 
    
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <- c( "#6a51a3", "#bcbddc")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Extreme Fuel Poverty`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Extreme Fuel Poverty",
        text = paste0("Extreme Fuel Poverty: ", format(percent(Data$`Extreme Fuel Poverty`, accuracy = 0.1), big.mark = ","), ""),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Total Fuel Poverty` - `Extreme Fuel Poverty`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Total Fuel Poverty",
        text = paste0("Total Fuel Poverty: ", format(percent(Data$`Total Fuel Poverty`, accuracy = 0.1), big.mark = ","), ""),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
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
          tickformat = "%",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2
        )
      ) %>% 
      config(displayModeBar = F)
    
    p

  })
  
  output$ExtremeFuelPovertyTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Fuel poverty",
      col_names = TRUE,
      skip = 13
    )[1:5]
    
    names(Data) <- c("Year", "Previously Published - Fuel Poverty", "Previously Published - Extreme Fuel Poverty", "Home Discount - Fuel Poverty", "Home Discount - Extreme Fuel Poverty")
    
    Data[2:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data[9,1] <- "Oct-11"
    
    Data <- Data[seq(dim(Data)[1],1),]
    
    datatable(
      Data[c(1,4,5,2,3)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Fuel poverty and extreme fuel poverty rates",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Fuel poverty and extreme fuel poverty rates',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Fuel poverty and extreme fuel poverty rates')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:5, 1)
  })
   
  output$ExtremeFuelPoverty.png <- downloadHandler(
    filename = "ExtremeFuelPoverty.png",
    content = function(file) {
      
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Fuel poverty", skip = 13)[c(1,5,6)]
      
      Data <- Data[complete.cases(Data),]
      
      Data[2,1] <- "Oct-11"
      
      names(Data) <- c("Type", "Extreme Fuel Poverty", "Fuel Poverty")
      
      FuelPoverty <- Data
      
      FuelPoverty <- FuelPoverty[c(1, ncol(FuelPoverty):2)]
      
      FuelPoverty <- arrange(FuelPoverty,-row_number())
      
      FuelPoverty$Type <-
        factor(FuelPoverty$Type, levels = unique(FuelPoverty$Type), ordered = TRUE)
      
      FuelPoverty <- melt(FuelPoverty, id.vars = "Type")
      
      
      FuelPoverty$variable <-
        factor(FuelPoverty$variable, levels = unique(FuelPoverty$variable))
      
      FuelPoverty <- FuelPoverty %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Rates of fuel poverty and extreme fuel poverty"
      sourcecaption <- "Source: SG"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#bcbddc", "#6a51a3")
      
      
      FuelPovertyChart <- FuelPoverty %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Fuel Poverty" = BarColours[1],
                                     "Extreme Fuel Poverty" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = FuelPoverty$Type,
          y = -.01,
          label = ifelse(
            FuelPoverty$Type == "z",
            "",
            str_wrap(FuelPoverty$Type, width = 20)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1],
          hjust = 1
        ) +
        annotate(
          "text",
          x = FuelPoverty$Type,
          y = FuelPoverty$top - FuelPoverty$pos,
          label = ifelse(
            FuelPoverty$variable == "Extreme Fuel Poverty",
            percent(FuelPoverty$value),
            ""
          ),
          family = "Century Gothic",
          fontface = 2,
          colour = "white"
        ) +
        annotate(
          "text",
          x = FuelPoverty$Type,
          y = FuelPoverty$top,
          label = percent(FuelPoverty$top),
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1],
          hjust = -0.1
        ) +
        annotate(
          "text",
          x = 8.7,
          y = (FuelPoverty$value[which(FuelPoverty$variable == "Extreme Fuel Poverty" & FuelPoverty$Type == "2011")]) / 2,
          label = "Extreme",
          family = "Century Gothic",
          fontface = 2,
          colour = BarColours[2]
        ) +
        annotate(
          "text",
          x = 8.7,
          y = mean(FuelPoverty$top),
          label = "Total",
          family = "Century Gothic",
          fontface = 2,
          hjust = 0,
          colour = BarColours[1]
        ) +
        annotate(
          "text",
          x = 9,
          y = 0,
          label = " ",
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1],
          hjust = -0.1
        )
      
      
      FuelPovertyChart
      
      
      FuelPovertyChart <-
        StackedBars(FuelPovertyChart,
                    FuelPoverty,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      FuelPovertyChart <-
        FuelPovertyChart +
        coord_flip() +
        labs(subtitle = paste("Scotland,", max(FuelPoverty$Type), "-", min(FuelPoverty$Type))) +
        ylim(-.035, max(FuelPoverty$top) + .025)
      
      FuelPovertyChart
      
      ggsave(
        file,
        plot = FuelPovertyChart,
        width = 16,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
      
    }
  ) 

  observeEvent(input$ToggleTable, {
    toggle("FuelPovertyProportionTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ExtremeFuelPovertyTable")
  })
  
  output$FuelPovertySAPTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Fuel poverty",
      col_names = TRUE,
      skip = 12,
      n_max = 7
    )[16:20]
    
    names(Data) <- substr(names(Data), 1, 4)
    
    names(Data)[1] <- "EPC Band"
    Data[2:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    
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
        title = "EPC Band (SAP 2012) distribution for fuel poor households (new definition)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'EPC Band (SAP 2012) distribution for fuel poor households (new definition)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'EPC Band (SAP 2012) distribution for fuel poor households (new definition)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:5, 1)
  })
  
  
  observeEvent(input$ToggleTable3, {
    toggle("FuelPovertySAPTable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
    output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/FuelPoverty.txt")[2])
                     
                   )))
  })
    
    output$FuelPovertyProportionData.xlsx <- downloadHandler(
      filename = "FuelPovertyProportionData.xlsx",
      content <- function(file) {
        file.copy("Structure/5 - Consumers/Fuel Poverty.xlsx", file)
      })  

}
