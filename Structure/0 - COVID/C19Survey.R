require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")

###### UI Function ######

source("Structure/Global.R")

C19SurveyOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Energy supply and bills",
    fluidRow(column(8,
                    h3("Survey on energy supply and energy bills during lockdown", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('C19SurveySubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19Survey.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("C19SurveyPlot")),
    plotlyOutput(ns("C19SurveyPlot"), height = "1000px")%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Household questions",
             fluidRow(column(8,
                             h3("Household questions", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19SurveyRollingSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19SurveyRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19SurveyPlot")),
             plotlyOutput(ns("C19SurveyRollingPlot"), height = "1200px")%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("T62",
             fluidRow(column(8,
                             h3("T62", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19SurveyT62Subtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19SurveyRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19SurveyPlot")),
             plotlyOutput(ns("C19SurveyT62Plot"), height = "500px")%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("T66",
             fluidRow(column(8,
                             h3("T66", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19SurveyT66Subtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19SurveyRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19SurveyPlot")),
             plotlyOutput(ns("C19SurveyT66Plot"), height = "300px")%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("T70",
             fluidRow(column(8,
                             h3("Which, if any, of the following would you use to describe information provided by your electricity and gas supplier?", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19SurveyT70Subtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19SurveyRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19SurveyPlot")),
             plotlyOutput(ns("C19SurveyT70Plot"), height = "500px")%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    )
    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:;background-color:#5d8be1;"),
    # fluidRow(
    # column(10, h3("Data - 12 month rolling average.", style = "color: #5d8be1;  font-weight:bold")),
    # column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    # ),
    # fluidRow(
    #   column(12, dataTableOutput(ns("C19SurveyTable"))%>% withSpinner(color="#5d8be1"))),
    # tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
      column(2, p(" ")),
      column(2,
             p(" ")),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("NGElecDemand")
        
      )
    )
  )
}




###### Server ######
C19Survey <- function(input, output, session) {

  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("C19Survey.R")

  output$C19SurveySubtitle <- renderText({
    
    paste("Scotland, April 2020")
  })
  
  output$C19SurveyPlot <- renderPlotly  ({
    
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    library(readxl)
    library(scales)
    library(tidyverse)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Hidden/Data/Survey.xlsx")
    
    Data$Question <- paste0("<b>", str_wrap(Data$Question,30), "</b>")
    
    Data$Total <- Data$`Strongly agree` + Data$`Tend to agree` + Data$`Neither agree nor disagree` + Data$`Tend to disagree` + Data$`Strongly disagree`
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#1a9850", "#a6d96a","#737373", "#bdbdbd", "#f46d43", "#d73027")
    
    p <- plot_ly(data = Data, y = ~ Question) %>%
      
      add_trace(
        data = Data,
        x = ~ `Strongly agree` / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Strongly agree",
        text = paste0("Strongly agree: ", percent(Data$`Strongly agree`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Tend to agree`  / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Tend to agree",
        text = paste0("Tend to agree: ", percent(Data$`Tend to agree`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Neither agree nor disagree`  / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Neither agree nor disagree",
        text = paste0("Neither agree nor disagree: ", percent(Data$`Neither agree nor disagree`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Tend to disagree`  / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Tend to disagree",
        text = paste0("Tend to disagree: ", percent(Data$`Tend to disagree`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Strongly disagree`  / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Strongly disagree",
        text = paste0("Strongly disagree: ", percent(Data$`Strongly disagree`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = Data,
        x = ~ 1.05 ,
        showlegend = TRUE,
        name = 'All agree',
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = ChartColours[1]),
        text =  paste0("<b>", percent(Data$`Strongly agree`+Data$`Tend to agree`, accuracy = 1), "</b>"),
        legendgroup = 8
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
                     ticktext = as.list(Data$`Question`),
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
  
  output$C19SurveyRollingSubtitle <- renderText({
    
    paste("Scotland, April 2020")
  })
  
  output$C19SurveyRollingPlot <- renderPlotly  ({
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    library(readxl)
    library(scales)
    library(tidyverse)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Hidden/Data/Survey.xlsx",
                       sheet = "T54")
    
    Data$Question <- paste0("<b>", str_wrap(Data$Question,40), "</b>")
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#2b8cbe", "#a6d96a","#737373", "#bdbdbd", "#f46d43", "#d73027")
    
    p <- plot_ly(data = Data, y = ~ Question) %>%
      
      add_trace(
        data = Data,
        x = ~ `Total`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Total",
        text = paste0("Total: ", percent(Data$`Total`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
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
                     ticktext = as.list(Data$`Question`),
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
  
  output$C19SurveyT62Subtitle <- renderText({
    
    paste("Scotland, April 2020")
  })
  
  output$C19SurveyT62Plot <- renderPlotly  ({
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    library(readxl)
    library(scales)
    library(tidyverse)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Hidden/Data/Survey.xlsx",
                       sheet = "T62")
    
    Data$Question <- paste0("<b>", str_wrap(Data$Question,40), "</b>")
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#2b8cbe", "#a6d96a","#737373", "#bdbdbd", "#f46d43", "#d73027")
    
    p <- plot_ly(data = Data, y = ~ Question) %>%
      
      add_trace(
        data = Data,
        x = ~ `Total`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Total",
        text = paste0("Total: ", percent(Data$`Total`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
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
                     ticktext = as.list(Data$`Question`),
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
  
  output$C19SurveyTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas (Gwh)", "Transport (GWh)", "Electricity (GWh)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19SurveyRolling <- Data[complete.cases(Data),]
    
    C19SurveyRolling <- C19SurveyRolling %>% 
      mutate(yr_mnth = format(Year, '%Y-%m')) %>% 
      group_by(yr_mnth) %>% 
      filter(Year == max(Year)) %>% 
      mutate(Year = format(Year, "%B %Y"))
    
    names(C19SurveyRolling)[1] <- "12 month ending"
    
    datatable(
      C19SurveyRolling,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = 10,
        searching = TRUE,
        fixedColumns = FALSE,
        columnDefs = list(list(visible=FALSE, targets=c(4))),
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(4, 'desc')),
        title = "Daily Demand - 12 month rolling average",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Daily Demand - 12 month rolling average',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Daily Demand - 12 month rolling average')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:5, 1) 
  })
  
  output$C19SurveyT66Subtitle <- renderText({
    
    paste("Scotland, April 2020")
  })
  
  output$C19SurveyT66Plot <- renderPlotly  ({
    
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    library(readxl)
    library(scales)
    library(tidyverse)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Hidden/Data/Survey.xlsx", sheet = "T66")
    
    Data$Question <- paste0("<b>", str_wrap(Data$Question,40), "</b>")
    
    Data$Total <- Data$`Yes` + Data$`Don't know` + Data$`No` 
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#1a9850", "#a6d96a","#737373", "#bdbdbd", "#f46d43", "#d73027")
    
    p <- plot_ly(data = Data, y = ~ Question) %>%
      
      add_trace(
        data = Data,
        x = ~ `Yes` / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Yes",
        text = paste0("Yes: ", percent(Data$`Yes`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Don't know`  / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Don't know",
        text = paste0("Don't know: ", percent(Data$`Don't know`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `No`  / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "No",
        text = paste0("No: ", percent(Data$`No`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = Data,
        x = ~ 1.05 ,
        showlegend = TRUE,
        name = 'All agree',
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = ChartColours[1]),
        text =  paste0("<b>", percent(Data$`Strongly agree`+Data$`Tend to agree`, accuracy = 0.1), "</b>"),
        legendgroup = 8
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
                     ticktext = as.list(Data$`Question`),
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
  
  output$C19SurveyT70Subtitle <- renderText({
    
    paste("Scotland, April 2020")
  })
  
  output$C19SurveyT70Plot <- renderPlotly  ({
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    library(readxl)
    library(scales)
    library(tidyverse)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Hidden/Data/Survey.xlsx",
                       sheet = "T70")
    
    Data$Question <- paste0("<b>", str_wrap(Data$Question,40), "</b>")
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#2b8cbe", "#a6d96a","#737373", "#bdbdbd", "#f46d43", "#d73027")
    
    p <- plot_ly(data = Data, y = ~ Question) %>%
      
      add_trace(
        data = Data,
        x = ~ `Total`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Total",
        text = paste0("Total: ", percent(Data$`Total`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
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
                     ticktext = as.list(Data$`Question`),
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
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/0 - COVID/C19Survey.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("C19SurveyTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$C19Survey.png <- downloadHandler(
    filename = "C19Survey.png",
    content = function(file) {
      
      print("Energy daily demand")
      ###### Daily Demand  #####
      
      # C19Survey <-
      #   read_csv(
      #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/C19Survey.csv"
      #   )
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "DailyDemandWorking")[c(1,2,4,3)]
      
      Data <- Data[complete.cases(Data),]
      
      names(Data) <- c("Year", "Gas", "Transport", "Electricity")
      
      Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
      
      C19Survey <- Data
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: Sheffield, National Grid, BEIS"
      plottitle = "Energy use in Scotland per day"
      
      #C19Survey$GasPercentage <- PercentLabel(C19Survey$Gas)
      
      
      C19SurveyChart <- C19Survey %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = Gas,
              label = Gas),
          colour = ChartColours[2],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(C19Survey$Year),
          y = max(C19Survey$Gas),
          label = "Gas",
          hjust = 0.5,
          vjust = 1,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Electricity,
              label = paste0(Electricity * 100, "%")),
          colour = ChartColours[3],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(C19Survey$Year),
          y = mean(C19Survey$Electricity),
          label = "Electricity",
          hjust = 0.5,
          vjust = 5.5,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Transport,
              label = paste0(Transport * 100, "%")),
          colour = ChartColours[4],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(C19Survey$Year),
          y = mean(C19Survey$Transport),
          label = "Transport",
          hjust = 0.5,
          vjust = 8,
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
              format(Year, format = "%b %Y"),
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )+
        annotate(
          "segment",
          x = C19Survey$Year[which(C19Survey$Gas == max(C19Survey$Gas))]-2,
          xend = C19Survey$Year[which(C19Survey$Gas == max(C19Survey$Gas))] - 30,
          y = max(C19Survey$Gas),
          yend = max(C19Survey$Gas),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = C19Survey$Year[which(C19Survey$Gas == max(C19Survey$Gas))] - 35,
          y = max(C19Survey$Gas),
          label = paste(round(max(C19Survey$Gas), digits = 0), "GWh"),
          hjust = 1,
          fontface = 2,
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        annotate(
          "segment",
          x = C19Survey$Year[which(C19Survey$Electricity == max(C19Survey$Electricity[which(C19Survey$Year > dmy("01/08/18"))]))]+2,
          xend = C19Survey$Year[which(C19Survey$Electricity == max(C19Survey$Electricity[which(C19Survey$Year > dmy("01/08/18"))]))] + 30,
          y = max(C19Survey$Electricity[which(C19Survey$Year > dmy("01/08/18"))]),
          yend = max(C19Survey$Electricity[which(C19Survey$Year > dmy("01/08/18"))]),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = C19Survey$Year[which(C19Survey$Electricity == max(C19Survey$Electricity[which(C19Survey$Year > dmy("01/08/18"))]))] + 35,
          y = max(C19Survey$Electricity[which(C19Survey$Year > dmy("01/08/18"))]),
          label = paste(round(max(C19Survey$Electricity[which(C19Survey$Year > dmy("01/08/18"))]), digits = 0), "GWh"),
          hjust = 0,
          fontface = 2,
          size = 4,
          colour = ChartColours[3],
          family = "Century Gothic"
        )
      
      
      C19SurveyChart
      
      C19SurveyChart <-
        DailyChart(C19SurveyChart,
                   C19Survey,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      C19SurveyChart <- C19SurveyChart +
        coord_cartesian(xlim = c(min(C19Survey$Year), max(C19Survey$Year)+130)) +
        
        ylim(-15, 352) +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      C19SurveyChart
      
      ggsave(
        file,
        plot =  C19SurveyChart,
        width = 30,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
)

output$C19SurveyRolling.png <- downloadHandler(
  filename = "C19SurveyRolling.png",
  content = function(file) {
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19SurveyRolling <- Data[complete.cases(Data),]
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day\n12 month rolling average"
    
    #C19SurveyRolling$GasPercentage <- PercentLabel(C19SurveyRolling$Gas)
    
    
    C19SurveyRollingChart <- C19SurveyRolling %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      
      geom_line(
        aes(y = Gas,
            label = Gas),
        colour = ChartColours[2],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(C19SurveyRolling$Year),
        y = max(C19SurveyRolling$Gas),
        label = "Gas Rolling Average",
        hjust = 0.5,
        vjust = 2,
        colour = ChartColours[2],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Electricity,
            label = paste0(Electricity * 100, "%")),
        colour = ChartColours[3],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(C19SurveyRolling$Year),
        y = mean(C19SurveyRolling$Electricity),
        label = "Electricity Rolling Average",
        hjust = 0.5,
        vjust = -1,
        colour = ChartColours[3],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Transport,
            label = paste0(Transport * 100, "%")),
        colour = ChartColours[4],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(C19SurveyRolling$Year),
        y = mean(C19SurveyRolling$Transport),
        label = "Transport Rolling Average",
        hjust = 0.5,
        vjust = -1,
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
            format(Year, format = "%b %Y"),
            ""
          ),
          hjust = 0.5,
          vjust = 1.5,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = C19SurveyRolling$Gas[which(C19SurveyRolling$Year == min(C19SurveyRolling$Year))],
          label = paste0(round(C19SurveyRolling$Gas[which(C19SurveyRolling$Year == min(C19SurveyRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = C19SurveyRolling$Gas[which(C19SurveyRolling$Year == max(C19SurveyRolling$Year))],
          label = paste0(round(C19SurveyRolling$Gas[which(C19SurveyRolling$Year == max(C19SurveyRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = C19SurveyRolling$Electricity[which(C19SurveyRolling$Year == min(C19SurveyRolling$Year))],
          label = paste0(round(C19SurveyRolling$Electricity[which(C19SurveyRolling$Year == min(C19SurveyRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = C19SurveyRolling$Electricity[which(C19SurveyRolling$Year == max(C19SurveyRolling$Year))],
          label = paste0(round(C19SurveyRolling$Electricity[which(C19SurveyRolling$Year == max(C19SurveyRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = C19SurveyRolling$Transport[which(C19SurveyRolling$Year == min(C19SurveyRolling$Year))],
          label = paste0(round(C19SurveyRolling$Transport[which(C19SurveyRolling$Year == min(C19SurveyRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = C19SurveyRolling$Transport[which(C19SurveyRolling$Year == max(C19SurveyRolling$Year))],
          label = paste0(round(C19SurveyRolling$Transport[which(C19SurveyRolling$Year == max(C19SurveyRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )
    
    C19SurveyRollingChart
    
    C19SurveyRollingChart <-
      DailyChart(C19SurveyRollingChart,
                 C19SurveyRolling,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    C19SurveyRollingChart <- C19SurveyRollingChart +
      coord_cartesian(xlim = c(min(C19SurveyRolling$Year)-30, max(C19SurveyRolling$Year)+30)) +
      ylim(-5,190)+
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.7,
        linetype = 2
      )
    
    
    C19SurveyRollingChart
    
    ggsave(
      file,
      plot =  C19SurveyRollingChart,
      width = 18,
      height = 12,
      units = "cm",
      dpi = 300
    )
  }
)

output$FullData <- downloadHandler(
  filename = "C19SurveyFullData.csv",
  content = function(file){
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,2,4,3)]
    
    names(Data) <- c("Year", "Gas (GWh)", "Transport (GWh)", "Electricity (GWh)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19Survey <- Data
    
    write.csv(C19Survey, 
              file,
              row.names = FALSE)
  }
)

}
