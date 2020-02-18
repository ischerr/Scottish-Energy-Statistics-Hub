require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

NonDomRHIOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Accredited Installations",
               fluidRow(column(8,
                               h3("Non-domestic RHI - Accredited Installations", style = "color: #39ab2c;  font-weight:bold"),
                               h4(textOutput(ns('NonDomRHIAccreditedInstallationsSubtitle')), style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('NonDomRHIAccreditedInstallations.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("NonDomRHIPlot")),
               plotlyOutput(ns("NonDomRHIAccreditedInstallationsPlot"))%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Accredited Capacity",
               fluidRow(column(8,
                               h3("Non-domestic RHI - Accredited Installation Capacity", style = "color: #39ab2c;  font-weight:bold"),
                               h4(textOutput(ns('NonDomRHInstallationCapacitySubtitle')), style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('NonDomRHIInstallationCap.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("NonDomRHIPlot")),
               plotlyOutput(ns("NonDomRHInstallationCapacityPlot"))%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Tech type",
    fluidRow(column(8,
                    h3("Proportion on non-domestic RHI heat generated, installed capacity and number of installations receiving payment by tariff", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('NonDomRHISubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('NonDomRHI.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("NonDomRHIPlot")),
    plotlyOutput(ns("NonDomRHIPlot"))%>% withSpinner(color="#39ab2c"),
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
      tabPanel("Applications",
               fluidRow(
                 column(10, h3("Data - Cumulative number and capacity  of applications", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("NonDomRHIAccreditedInstallationsTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      
      tabPanel("LA Installations",
               fluidRow(
                 column(10, h3("Data - Number of accredited installations by Local Authority", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("NonDomRHILATable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      
      tabPanel("Heat Generated, Installed Capacity & Installations Paid ",
    fluidRow(
    column(10, h3("Data - Heat generated, installed capacity and number of installations receiving payment", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("NonDomRHITable"))%>% withSpinner(color="#39ab2c"))),
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
        SourceLookup("BEISRHI")
        
      )
    )
  )
}




###### Server ######
NonDomRHI <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("NonDomRHI.R")

  
  output$NonDomRHISubtitle <- renderText({
    
    paste("Scotland, Nov 2011 - June 2019")
  })
  
  output$NonDomRHIPlot <- renderPlotly  ({
    
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#31a354",
        "#78c679",
        "#addd8e"
      )
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", 
        skip = 13, n_max = 5)
    
    Data <- Data[3:5,c(1,4,6,8)]
    
    names(Data) <- c("Tech", "Heat generated", "No. of Installations", "Capacity")
    
    Data2 <- data.frame("Other", 1 - sum(Data$`Heat generated`), 1 - sum(Data$`No. of Installations`), 1 - sum(Data$Capacity))
    
    names(Data2) <- names(Data)
    
    Data <- rbind(Data, Data2)
    
    Data$TechLabel <- Data$Tech
    
    Data$Tech <- paste0("<b>",Data$Tech,"</b>")
    
    Data$Tech <- str_wrap(Data$Tech, 24)
    
    p <-  plot_ly(Data, 
                  y = ~Tech, 
                  x = ~ `Heat generated`, 
                  type = 'bar', 
                  name = 'Heat generated',
                  hoverinfo = "text",
                  text = paste0("Heat generated: ",percent(Data$`Heat generated`, accuracy = .1)),
                  orientation = 'h',
                  marker = list(color = "#31a354")
                  )%>%
      add_trace(x = ~`No. of Installations`, 
                name = 'No. of Installations',
                text = paste0("No. of Installations: ",percent(Data$`No. of Installations`, accuracy = .1)),
                marker = list(color = "#78c679")
                ) %>% 
      add_trace(x = ~`Capacity`, 
                name = 'Capacity',
                text = paste0("Capacity: ",percent(Data$`Capacity`, accuracy = .1)),
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
                     range = c(-0.01, max(Data$`No. of Installations`)+0.1),
                     x = 0.5
                     
                     ),
        yaxis = list(
          title = "",
          tickformat = "%",
          categoryorder = "array",
          categoryarray = c(Data[4,1],Data[3,1],Data[2,1],Data[1,1]),
          autorange = "reversed",
          showgrid = FALSE,
          zeroline = FALSE,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
    
    
  })
  
  output$NonDomRHIAccreditedInstallationsSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", col_names = TRUE, 
        skip = 72)
  Data <- Data[complete.cases(Data),]
     
names(Data)[1] <- "Year"

    paste("Scotland,", format(min(Data$Year), "%b %Y"),"-", format(max(Data$Year), "%b %Y"))
  })
  
  output$NonDomRHIAccreditedInstallationsPlot <- renderPlotly  ({
    
    ChartColours <- c("#39ab2c", "#FF8500")
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", col_names = TRUE, 
        skip = 72)
    Data <- Data[complete.cases(Data),]
    
    names(Data)[1] <- "Year"
    
    NonDomRHIAccreditedInstallations <- Data
    
    NonDomRHIAccreditedInstallations <- NonDomRHIAccreditedInstallations[order(NonDomRHIAccreditedInstallations$Year),]
    
    LineColours <- c("#39ab2c","#ef3b2c","#fb6a4a","#fc9272","#fcbba1")
    
    p <-  plot_ly(NonDomRHIAccreditedInstallations, x = ~ Year ) %>%  
      add_trace(y = ~ `Accredited full applications`,
                name = "Accredited full applications",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Accredited full applications: ",
                  format(round(NonDomRHIAccreditedInstallations$`Accredited full applications`, digits = 0), big.mark = ","),
                  "\nYear: ",
                  format(NonDomRHIAccreditedInstallations$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(NonDomRHIAccreditedInstallations[which(NonDomRHIAccreditedInstallations$`Accredited full applications` != 0),], 1),
        x = ~ Year,
        y = ~ `Accredited full applications`,
        name = "Accredited full applications",
        legendgroup = "1",
        text = paste0(
          "Accredited full applications: ",
          format(tail(NonDomRHIAccreditedInstallations[which(NonDomRHIAccreditedInstallations$`Accredited full applications` != 0),], 1)$`Accredited full applications`, big.mark = ","),
          "\nYear: ",
          format(tail(NonDomRHIAccreditedInstallations[which(NonDomRHIAccreditedInstallations$`Accredited full applications` != 0),], 1)$Year, "%Y")
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
  
  output$NonDomRHInstallationCapacitySubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", col_names = TRUE, 
        skip = 72)
    Data <- Data[complete.cases(Data),]
    
    names(Data)[1] <- "Year"
    
    paste("Scotland,", format(min(Data$Year), "%b %Y"),"-", format(max(Data$Year), "%b %Y"))
  })
  
  output$NonDomRHInstallationCapacityPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", col_names = TRUE, 
        skip = 72)
    Data <- Data[complete.cases(Data),]
    
    names(Data)[1] <- "Year"
    
    NonDomRHInstallationCapacity <- Data
    
    NonDomRHInstallationCapacity <- NonDomRHInstallationCapacity[order(NonDomRHInstallationCapacity$Year),]
    
    ChartColours <- c("#39ab2c", "#FF8500")
    
    LineColours <- c("#39ab2c","#ef3b2c","#fb6a4a","#fc9272","#fcbba1")
    
    p <-  plot_ly(NonDomRHInstallationCapacity, x = ~ Year ) %>%  
      add_trace(y = ~ `Capacity of accredited full applications`,
                name = "Capacity of accredited full applications",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Capacity of accredited full applications: ",
                  format(round(NonDomRHInstallationCapacity$`Capacity of accredited full applications`, digits = 0), big.mark = ","),
                  " MW\nYear: ",
                  format(NonDomRHInstallationCapacity$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(NonDomRHInstallationCapacity[which(NonDomRHInstallationCapacity$`Capacity of accredited full applications` != 0),], 1),
        x = ~ Year,
        y = ~ `Capacity of accredited full applications`,
        name = "Capacity of accredited full applications",
        legendgroup = "1",
        text = paste0(
          "Capacity of accredited full applications: ",
          format(round(tail(NonDomRHInstallationCapacity[which(NonDomRHInstallationCapacity$`Capacity of accredited full applications` != 0),], 1)$`Capacity of accredited full applications`, digits = 0), big.mark = ","),
          " MW\nYear: ",
          format(tail(NonDomRHInstallationCapacity[which(NonDomRHInstallationCapacity$`Capacity of accredited full applications` != 0),], 1)$Year, "%Y")
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
          title = "MW",
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
  
  output$NonDomRHITable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", 
        skip = 13, n_max = 15)
    
    Data <- tail(Data, 13)
    
    Data[2] <- NULL
    
    names(Data)[c(3,5,7)] <- "Proportion"
    
    NonDomRHI <- Data
    
    datatable(
      NonDomRHI,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Non-domestic RHI heat generated, installed capacity and number of installations receiving payment by tariff, Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Non-domestic RHI heat generated, installed capacity and number of installations receiving payment by tariff, Scotland",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Non-domestic RHI heat generated, installed capacity and number of installations receiving payment by tariff, Scotland")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:7), 0) %>% 
      formatPercentage(c(3,5,7))
  })
  
  output$NonDomRHIAccreditedInstallationsTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", 
        skip = 72)

    names(Data)[1] <- c("Date")
    
    Data <- as_tibble(Data)
    
    Data$Date <- format(Data$Date, "%b %Y")
    
    NonDomRHIAccreditedInstallationsTech <- Data
    
    datatable(
      NonDomRHIAccreditedInstallationsTech,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Cumulative number and capacity  of non-domestic RHI applications and accredited applications, Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Cumulative number and capacity  of non-domestic RHI applications and accredited applications, Scotland",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Cumulative number and capacity  of non-domestic RHI applications and accredited applications, Scotland")
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
  
  output$NonDomRHILATable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", 
        skip = 35, n_max = 34)
    
    Data <- Data[c(2,1,3,4)]
    
    names(Data)[c(2,4)] <- c("LA Code", "Capacity (MW)")
    
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
        title = "Number of accredited installations by Local Authority",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Number of accredited installations by Local Authority",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Number of accredited installations by Local Authority")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(3:4), 1)
  
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Heat/NonDomRHI.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable1, {
    toggle("NonDomRHITable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("NonDomRHIAccreditedInstallationsTable")
  })

  observeEvent(input$ToggleTable3, {
    toggle("NonDomRHILATable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$NonDomRHI.png <- downloadHandler(
    filename = "NonDomRHI.png",
    content = function(file) {

      Data <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
          sheet = "Non-domestic RHI", col_names = FALSE, 
          skip = 15, n_max = 4)
      Data <- Data[c(1,8,6,4)]
      
      names(Data) <- c("Tech", "Capacity", "No. of Installations", "Heat generated")
      
      Data <- rbind(Data, c("Other", (1-colSums(Data[2])),(1-colSums(Data[3])),(1-colSums(Data[4]))))
      
      NonDomRHI <- Data
      
      NonDomRHI <- arrange(NonDomRHI, -row_number())
      
      NonDomRHI$Tech <-
        factor(NonDomRHI$Tech,
               levels = unique(NonDomRHI$Tech),
               ordered = TRUE)
      
      NonDomRHI <- melt(NonDomRHI, id.vars = "Tech")
      
      
      NonDomRHI$variable <-
        factor(
          NonDomRHI$variable,
          levels = unique(NonDomRHI$variable),
          ordered = TRUE
        )
      
      NonDomRHI$value <- as.numeric(NonDomRHI$value)
      NonDomRHI <- NonDomRHI %>%
        group_by(Tech) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Proportion on non-domestic RHI heat generated, installed capacity\nand number of installations receiving payment by tariff"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#31a354",
          "#78c679",
          "#addd8e"
        )
      
      
      NonDomRHIChart <- NonDomRHI %>%
        ggplot(aes(x = Tech, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Heat generated" = BarColours[1],
            "No. of Installations" = BarColours[2],
            "Capacity" = BarColours[3]
          )
        ) +
        geom_bar(position = "dodge",
                 stat = "identity",
                 width = .8) +
        geom_text(position = position_dodge(width = .8),
                  aes(
                    y = value + .01,
                    fill = variable,
                    label = percent(value, accuracy = 1),
                    hjust = 0
                  ),
                  fontface = 2,
                  colour =  ChartColours[1],
                  family = "Century Gothic") +
        geom_text(position = position_dodge(width = .8),
                  aes(
                    y = .01,
                    fill = variable,
                    label = ifelse(Tech == max(Tech), as.character(variable), ""),
                    hjust = 0
                  ),
                  fontface = 2,
                  colour =  "white",
                  family = "Century Gothic") +
        annotate(
          "text",
          x = NonDomRHI$Tech,
          y = -.07,
          label = ifelse(NonDomRHI$Tech == "z", "", str_wrap(NonDomRHI$Tech, width = 13)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        )
      
      NonDomRHIChart
      
      
      NonDomRHIChart <-
        StackedBars(NonDomRHIChart,
                    NonDomRHI,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      NonDomRHIChart <-
        NonDomRHIChart +
        labs(subtitle = "Scotland, Nov 2011 - June 2019") +
        ylim(-.1, .7)+
        coord_flip()
      
      NonDomRHIChart
      

      ggsave(
        file,
        plot = NonDomRHIChart,
        width = 20,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
  )



output$NonDomRHIAccreditedInstallations.png <- downloadHandler(
  filename = "NonDomRHIAccreditedInstallations.png",
  content = function(file) {
    
    RHINonDom <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", skip = 72)
    
    
    RHINonDom <- RHINonDom[c(1,3,5)]
    
    
    
    names(RHINonDom) <- c( "Year", "Applications", "Capacity")
    
    RHINonDom$Year <- ymd(RHINonDom$Year)
    
    RHINonDom[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    RHINonDom <- as.data.frame(RHINonDom)
    
    RHINonDom <- RHINonDom[order(RHINonDom$Year),]
    
    ### variables
    ChartColours <- c("#39ab2c","#ef3b2c","#fb6a4a","#fc9272","#fcbba1")
    sourcecaption = "Source: SG"
    plottitle = "Non-domestic RHI - Accredited Installations"
    
    #RHINonDom$OilPercentage <- PercentLabel(RHINonDom$Oil)
    
    
    RHINonDomChart <- RHINonDom %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      geom_line(
        aes(
          y = `Applications`,
          label = percent(`Applications`)
        ),
        colour = ChartColours[1],
        size = 1.5,
        family = "Century Gothic"
      )+ 
      geom_text(
        aes(
          x = Year-88,
          y = `Applications`,
          label = ifelse(Year == min(Year), format(`Applications`, big.mark = ","), ""),
          hjust = 0.5,
          vjust = 1,
          fontface = 2
        ),colour = ChartColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+101,
          y = `Applications`,
          label = ifelse(Year == max(Year), format(`Applications`, big.mark = ","), ""),
          hjust = 0.5,
          fontface = 2
        ),colour = ChartColours[1],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RHINonDom, 1),
        aes(
          x = Year,
          y = `Applications`,
          show_guide = FALSE
        ),colour = ChartColours[1],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = 0,
          label = ifelse(Year == max(Year) |
                           Year == min(Year), format(Year, format = "%b %Y"), ""),
          hjust = 0.5,
          vjust = 1.5,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )+
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.7,
        linetype = 2
      )
    
    
    RHINonDomChart <-
      DailyChart(RHINonDomChart,
                 RHINonDom,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    RHINonDomChart <- RHINonDomChart+
      coord_cartesian(xlim = c(min(RHINonDom$Year)-75, max(RHINonDom$Year)+110)) +
      ylim(-50,max(RHINonDom$Applications))
    
    
    RHINonDomChart
    
    
    ggsave(
      file,
      plot = RHINonDomChart,
      width = 15.5,
      height = 15,
      units = "cm",
      dpi = 300
    )
    
  }
)

output$NonDomRHIInstallationCap.png <- downloadHandler(
  filename = "NonDomRHIInstallationCap.png",
  content = function(file) {
    
    RHINonDomCap <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Non-domestic RHI", skip = 72)
    
    
    
    RHINonDomCap <- RHINonDomCap[c(1,3,5)]
    
    
    
    
    names(RHINonDomCap) <- c( "Year", "Applications", "Capacity")
    
    RHINonDomCap$Year <- ymd(RHINonDomCap$Year)
    
    RHINonDomCap[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    RHINonDomCap <- as.data.frame(RHINonDomCap)
    
    RHINonDomCap <- RHINonDomCap[order(RHINonDomCap$Year),]
    
    ### variables
    ChartColours <- c("#39ab2c","#ef3b2c","#fb6a4a","#fc9272","#fcbba1")
    sourcecaption = "Source: SG"
    plottitle = "Non-domestic RHI - Accredited Installation\nCapacity"
    
    #RHINonDomCap$OilPercentage <- PercentLabel(RHINonDomCap$Oil)
    
    
    RHINonDomCapChart <- RHINonDomCap %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      geom_line(
        aes(
          y = `Capacity`,
          label = percent(`Capacity`)
        ),
        colour = ChartColours[1],
        size = 1.5,
        family = "Century Gothic"
      )+ 
      geom_text(
        aes(
          x = Year-88,
          y = `Capacity`,
          label = ifelse(Year == min(Year), paste(round(Capacity, digits = 1), "\nMW"), ""),
          hjust = 0.5,
          vjust = 1,
          fontface = 2
        ),colour = ChartColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+101,
          y = `Capacity`,
          label = ifelse(Year == max(Year), paste(round(Capacity, digits = 1), "\nMW"), ""),
          hjust = 0.5,
          fontface = 2
        ),colour = ChartColours[1],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(RHINonDomCap, 1),
        aes(
          x = Year,
          y = `Capacity`,
          show_guide = FALSE
        ),colour = ChartColours[1],
        size = 4,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = 0,
          label = ifelse(Year == max(Year) |
                           Year == min(Year), format(Year, format = "%b %Y"), ""),
          hjust = 0.5,
          vjust = 1.5,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )+
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.7,
        linetype = 2
      )
    
    
    RHINonDomCapChart <-
      DailyChart(RHINonDomCapChart,
                 RHINonDomCap,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    RHINonDomCapChart <- RHINonDomCapChart+
      coord_cartesian(xlim = c(min(RHINonDomCap$Year)-75, max(RHINonDomCap$Year)+110)) +
      ylim(0,max(RHINonDomCap$Capacity))
    
    
    RHINonDomCapChart
    
    
    ggsave(
      file,
      plot = RHINonDomCapChart,
      width = 15.5,
      height = 15,
      units = "cm",
      dpi = 300
    )
    
    
    
  }
)
}
    
    