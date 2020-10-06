require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")

###### UI Function ######



C19SurveyOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Attitudes",
    fluidRow(column(8,
                    h3("Attitudes towards energy since lockdown", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('C19SurveySubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19Survey.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("C19SurveyPlot")),
    plotlyOutput(ns("C19SurveyPlot"), height = "1200px")%>% withSpinner(color="#5d8be1"),
    HTML("<blockquote><p>*note that sample size for prepayment customers is small, so
caution should be exercised on these findings.&nbsp;</p></blockquote>"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Change in Attitudes",
             fluidRow(column(8,
                             h3("Change in attitudes towards energy since lockdown", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19SurveyChangeSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19SurveyChange.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19SurveyPlot")),
             plotlyOutput(ns("C19SurveyChangePlot"), height = "1200px")%>% withSpinner(color="#5d8be1"),
             HTML("<blockquote><p>*note that sample size for prepayment customers is small, so
caution should be exercised on these findings.&nbsp;</p></blockquote>"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Energy Issues",
             fluidRow(column(8,
                             h3("Issues with energy", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19SurveyRollingSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19SurveyRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19SurveyPlot")),
             plotlyOutput(ns("C19SurveyRollingPlot"), height = "1200px")%>% withSpinner(color="#5d8be1"),
             HTML("<blockquote><p>*note that sample size for prepayment customers is small, so
caution should be exercised on these findings.&nbsp;</p></blockquote>"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Advice Sources",
             fluidRow(column(8,
                             h3("Sources of advice", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19SurveyT62Subtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19SurveyT62.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19SurveyPlot")),
             plotlyOutput(ns("C19SurveyT62Plot"), height = "500px")%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Information",
             fluidRow(column(8,
                             h3("Information from energy supplier", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19SurveyT66Subtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19SurveyT66.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19SurveyPlot")),
             plotlyOutput(ns("C19SurveyT66Plot"), height = "500px")%>% withSpinner(color="#5d8be1"),
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
    fluidRow(
      column(8, h3("Full Survey Results", style = "color: #5d8be1;  font-weight:bold")),
      column(2, style = "padding:15px",  downloadButton(ns("FullData"), "April Results", style = "float:right; ")),
      column(2, style = "padding:15px",  downloadButton(ns("FullData2"), "June Results", style = "float:right; "))
    ),
    tags$hr(style = "height:3px;border:none;color:;background-color:#5d8be1;"),
    tabsetPanel(
      tabPanel("Attitudes",
    fluidRow(
    column(10, h3("Data - Attitudes towards energy since lockdown.", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("C19SurveyTable"))%>% withSpinner(color="#5d8be1"),
             HTML("<blockquote><p>*note that sample size for prepayment customers is small, so
caution should be exercised on these findings.&nbsp;</p></blockquote>"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Change in Attitudes",
             fluidRow(
               column(10, h3("Data - Change in attitudes towards energy since lockdown.", style = "color: #5d8be1;  font-weight:bold")),
               
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("C19SurveyChangeTable"))%>% withSpinner(color="#5d8be1"),
                      HTML("<blockquote><p>*note that sample size for prepayment customers is small, so
caution should be exercised on these findings.&nbsp;</p></blockquote>"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Energy issues",
             fluidRow(
               column(10, h3("Data - Issues with energy.", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("C19SurveyRollingTable"))%>% withSpinner(color="#5d8be1"),
                      HTML("<blockquote><p>*note that sample size for prepayment customers is small, so
caution should be exercised on these findings.&nbsp;</p></blockquote>"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Advice Sources",
             fluidRow(
               column(10, h3("Data - Sources of advice.", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("C19SurveyT62Table"))%>% withSpinner(color="#5d8be1"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Information",
             fluidRow(
               column(10, h3("Data - Information from energy supplier.", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable5"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("C19SurveyT66Table"))%>% withSpinner(color="#5d8be1"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Views on Information",
             fluidRow(
               column(10, h3("Data - Views on information from energy supplier.", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable6"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("C19SurveyT70Table"))%>% withSpinner(color="#5d8be1"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
    fluidRow(
      column(2, p(" ")),
      column(2,
             p(" ")),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("Ofgem")
        
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
    
    paste("Scotland, June 2020")
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
    
    ChartColours <- c("#5d8be1", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#5d8be1", "#1d91c0","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx")
    
    Data2 <- Data[c(1,8)]
    
    Data[8] <- NULL
    
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
        opacity = 0.05,
        name = "Neither agree nor disagree",
        text = paste0("Neither agree nor disagree: ", percent(Data$`Neither agree nor disagree`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3] ),
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
        text =  paste0("<b>", percent(Data2$`Total Agree`, accuracy = 1), "</b>"),
        legendgroup = 8
      ) %>%
      add_annotations(
        y = -0.9,
        x = .5,
        xref = "paper",
        text = paste0("<b>",str_wrap("Thinking about your energy supply and energy bills since the Government advised people to socially distance themselves from each other. To what extent do you agree or disagree with each of the following statements:", 50), "</b>"),
        showarrow = FALSE
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
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx")
    
    names(Data)[1] <- "Thinking about your energy supply and energy bills since the Government advised people to socially distance themselves from each other. To what extent do you agree or disagree with each of the following statements:"
    
    datatable(
      Data,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = 10,
        searching = TRUE,
        fixedColumns = FALSE,
        columnDefs = list(list(visible=FALSE, targets=c(4))),
        autoWidth = TRUE,
        title = "Attitudes towards energy since lockdown",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Attitudes towards energy since lockdown',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Attitudes towards energy since lockdown')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:7, 0) 
  })
 
  output$C19SurveyRollingSubtitle <- renderText({
    
    paste("Scotland, April - June 2020")
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
    
    ChartColours <- c("#5d8be1", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#0570b0", "#74a9cf","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx",
                       sheet = "T54")
    
    Data$Question <- paste0("<b>", str_wrap(Data$Question,40), "</b>")
    
    ChartColours <- c("#34d1a3", "#FF8500")
    
    
    p <- plot_ly(data = Data, y = ~ Question) %>%
      
      add_trace(
        data = Data,
        x = ~ `June`,
        type = 'bar',
        orientation = 'h',
        name = "June",
        text = paste0("June: ", percent(Data$`June`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>% 
      add_trace(
        data = Data,
        x = ~ `April`,
        type = 'bar',
        orientation = 'h',
        name = "April",
        text = paste0("April: ", percent(Data$`April`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>% 
      add_annotations(
        y = -0.8,
        x = .5,
        xref = "paper",
        text = paste0("<b>",str_wrap("Which of the following have happened to you in your household?", 50), "</b>"),
        showarrow = FALSE
      ) %>% 
      layout(
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
  
  output$C19SurveyRollingTable = renderDataTable({
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T54")
    
    names(Data)[1] <- "Which of the following have happened to you in your household?"
    
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
        title = "Issues with energy",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Issues with energy',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Issues with energy')
        ),
        
        # customize the length menu
        lengthMenu = list( c(-1, 10, 20) # declare values
                           , c("All", 10, 20 ) # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(2:3, 0) 
  })
  
  
  output$C19SurveyChangeSubtitle <- renderText({
    
    paste("Scotland, April - June 2020")
  })
  
  output$C19SurveyChangePlot <- renderPlotly  ({
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    library(readxl)
    library(scales)
    library(tidyverse)
    
    ChartColours <- c("#5d8be1", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#0570b0", "#74a9cf","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx",
                       sheet = "T29B")
    
    Data$Question <- paste0("<b>", str_wrap(Data$Question,40), "</b>")
    
    ChartColours <- c("#34d1a3", "#FF8500")
    
    
    p <- plot_ly(data = Data, y = ~ Question) %>%
      
      add_trace(
        data = Data,
        x = ~ `June`,
        type = 'bar',
        orientation = 'h',
        name = "June",
        text = paste0("June: ", percent(Data$`June`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>% 
      add_trace(
        data = Data,
        x = ~ `April`,
        type = 'bar',
        orientation = 'h',
        name = "April",
        text = paste0("April: ", percent(Data$`April`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>% 
      add_annotations(
        y = -0.8,
        x = .5,
        xref = "paper",
        text = paste0("<b>",str_wrap("Proportion of people who agree with the following statements:", 50), "</b>"),
        showarrow = FALSE
      ) %>% 
      layout(
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
  
  output$C19SurveyChangeTable = renderDataTable({
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T29B")
    
    names(Data)[1] <- "Proportion of people who agree with the following statements:"
    
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
        title = " Change in attitudes towards energy since lockdown.",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = ' Change in attitudes towards energy since lockdown.',
            header = TRUE
          ),
          list(extend = 'csv',
               title = ' Change in attitudes towards energy since lockdown.')
        ),
        
        # customize the length menu
        lengthMenu = list( c(-1, 10, 20) # declare values
                           , c("All", 10, 20 ) # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(2:3, 0) 
  })
  
  
  
  
  output$C19SurveyT62Subtitle <- renderText({
    
    paste("Scotland, April - June 2020")
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
    
    ChartColours <- c("#5d8be1", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#5d8be1", "#1d91c0","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx",
                       sheet = "T62")
    
    Data$Question <- paste0("<b>", str_wrap(Data$Question,40), "</b>")
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#0570b0", "#74a9cf","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    p <- plot_ly(data = Data, y = ~ Question) %>%
      
      add_trace(
        data = Data,
        x = ~ `June`,
        type = 'bar',
        orientation = 'h',
        name = "June",
        text = paste0("June: ", percent(Data$`June`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>% 
      add_trace(
        data = Data,
        x = ~ `April`,
        type = 'bar',
        orientation = 'h',
        name = "April",
        text = paste0("April: ", percent(Data$`June`, accuracy = 1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>% 
      add_annotations(
        y = -0.8,
        x = .5,
        xref = "paper",
        text = paste0("<b>",str_wrap("Since social distancing started, have you contacted any of the following for information or advice?", 50), "</b>"),
        showarrow = FALSE
      ) %>% 
      layout(
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
  
  output$C19SurveyT62Table = renderDataTable({
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T62")
    
    names(Data)[1] <- "Since social distancing started, have you contacted any of the following for information or advice?"
    
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
        title = "Sources of advice",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Sources of advice',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Sources of advice')
        ),
        
        # customize the length menu
        lengthMenu = list( c(-1, 10, 20) # declare values
                           , c("All", 10, 20 ) # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(2:3, 0) 
  })
  
  output$C19SurveyT66Subtitle <- renderText({
    
    paste("Scotland, April - June 2020")
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
    
    ChartColours <- c("#5d8be1", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#5d8be1", "#1d91c0","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T66")
    
    Data <- Data[nrow(Data):1,]
    
    Data$Month <- paste0("<b>", str_wrap(Data$Month,40), "</b>")
    
    Data$Total <- Data$`Yes` + Data$`Don't know` + Data$`No` 
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#1a9850", "#a6d96a","#737373", "#bdbdbd", "#f46d43", "#d73027")
    
    p <- plot_ly(data = Data, y = ~ Month) %>%
      
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
        x = ~ `No`  / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "No",
        text = paste0("No: ", percent(Data$`No`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Don't know`  / Data$Total,
        type = 'bar',
        width = 0.7,
        opacity= 0.05,
        orientation = 'h',
        name = "Don't know",
        text = paste0("Don't know: ", percent(Data$`Don't know`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_annotations(
        y = -0.8,
        x = .5,
        xref = "paper",
        text = paste0("<b>",str_wrap("Have you received any information from your electricity and gas supplier, such as a letter in the post, email or text, about what they're doing to manage their operations and customer service while people are socially distancing?", 50), "</b>"),
        showarrow = FALSE
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
                     ticktext = as.list(Data$`Month`),
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
  
  output$C19SurveyT66Table = renderDataTable({
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T66")
    
    names(Data)[1] <- "Have you received any information from your electricity and gas supplier, such as a letter in the post, email or text, about what they're doing to manage their operations and customer service while people are socially distancing?"
    
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
        title = "Views on information from energy supplier",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Views on information from energy supplier',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Views on information from energy supplier')
        ),
        
        # customize the length menu
        lengthMenu = list( c(-1, 10, 20) # declare values
                           , c("All", 10, 20 ) # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(2:4, 0) 
  })
  
  output$C19SurveyT70Subtitle <- renderText({
    
    paste("Scotland, April - June 2020")
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
    
    ChartColours <- c("#5d8be1", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#5d8be1", "#1d91c0","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T70")
    
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
        x = ~ `No`  / Data$Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "No",
        text = paste0("No: ", percent(Data$`No`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Don't know`  / Data$Total,
        type = 'bar',
        width = 0.7,
        opacity= 0.05,
        orientation = 'h',
        name = "Don't know",
        text = paste0("Don't know: ", percent(Data$`Don't know`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
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
  
  output$C19SurveyT70Table = renderDataTable({
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T70")
    
    names(Data)[1] <- "The information I received was..."
    
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
        title = "Views on information from energy supplier",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Views on information from energy supplier',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Views on information from energy supplier')
        ),
        
        # customize the length menu
        lengthMenu = list( c(-1, 10, 20) # declare values
                           , c("All", 10, 20 ) # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(2:3, 0) 
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/0 - COVID/C19Survey.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable1, {
    toggle("C19SurveyTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("C19SurveyChangeTable")
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("C19SurveyRollingTable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("C19SurveyT62Table")
  })
  
  observeEvent(input$ToggleTable5, {
    toggle("C19SurveyT66Table")
  })
  
  observeEvent(input$ToggleTable6, {
    toggle("C19SurveyT70Table")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$C19Survey.png <- downloadHandler(
    filename = "C19Survey.png",
    content = function(file) {
      
      library(readr)
      library(ISOweek)
      library(lubridate)
      library(zoo)
      library(plotly)
      library(readxl)
      library(scales)
      library(tidyverse)
      
      ChartColours <- c("#5d8be1", "#1d91c0", "#7fcdbb", "#8da0cb")
      BarColours <- c("#5d8be1", "#1d91c0", "#bdbdbd", "#7fcdbb", "#8da0cb")
      
      Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx")
      
      Data2 <- Data[c(1,8)]
      
      Data[8] <- NULL
      
      Data$`Don't know` <- NULL
      
      Data$Year <- paste0(str_wrap(Data$Question,30))
      
      Data$Year <- factor(Data$Year,
                          levels = rev(unique(Data$Year)))
      
      BarColours <- c("#1a9850", "#a6d96a", "#bdbdbd", "#f46d43", "#d73027")
      
      SurveyChart <- Data[c(7,2:6)]
      
      SurveyChart <- melt(SurveyChart, id.vars = "Year")
      
      SurveyChart$variable <-
        factor(SurveyChart$variable,
               levels = rev(unique(SurveyChart$variable)))
      
      
      SurveyChart <- SurveyChart %>%
        group_by(Year) %>%
        mutate(top = sum(value)) %>%
        mutate(value2 = (value/top)) %>% 
        mutate(pos = cumsum(value2) - value2 / 2)
      
      plottitle <-
        "Attitudes towards energy since lockdown"
      sourcecaption <- "Source: Ofgem, Scottish Government"
      
      SurveyChartChart <- SurveyChart %>%
        ggplot(aes(x = Year, y = value2, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Strongly agree" = BarColours[1],
            "Tend to agree" = BarColours[2],
            "Neither agree nor disagree" = BarColours[3],
            "Tend to disagree" = BarColours[4],
            "Strongly disagree" = BarColours[5]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = -0.3,
            label = ifelse(variable == "Strongly agree", as.character(Year), ""),
            color = ChartColours[2]
          ),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = 7.7,
              y = 0.05,
              label = "Strongly\nAgree"),
          fontface = 2,
          colour = BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 7.7,
              y = 0.25,
              label = "Tend to\nagree"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 7.7,
              y = 0.5,
              label = "Neither agree\nnor disagree"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 7.7,
              y = 0.73,
              label = "Tend to\ndisagree"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 7.7,
              y = 0.91,
              label = "Strongly\ndisagree"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 8.3,
              y = (-.55+1.07)/2,
              label = str_wrap("Thinking about your energy supply and energy bills since the Government advised people to socially distance themselves from each other. To what extent do you agree or disagree with each of the following statements:", 90)),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 8.7,
              y = 0.5,
              label = " "),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        annotate(
          "text",
          x = SurveyChart$Year,
          y = 1.069,
          label = ifelse(
            SurveyChart$Year == "z",
            "",
            percent(Data2$`Total Agree`, 1)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1]
        ) +
        geom_text(
          aes(x = 7.7,
              y = 1.069,
              label = "Total\nAgree"),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5
        )+
        geom_text(
          y = SurveyChart$pos,
          label = ifelse(SurveyChart$value >0.05, percent(SurveyChart$value,1), ""),
          
          hjust = 0.5,
          family = "Century Gothic",
          fontface = 2,
          color = "white")
  
  
  
  SurveyChartChart
  
  
  SurveyChartChart <-
    StackedBars(
      SurveyChartChart,
      SurveyChart,
      plottitle,
      sourcecaption,
      ChartColours
    )
  
  SurveyChartChart <-
    SurveyChartChart +
    ylim(-.55,1.07) +
    coord_flip() +
    labs(subtitle = "Scotland, June 2020")
  
  SurveyChartChart
  
  ggsave(
    file,
    plot = SurveyChartChart,
    width = 20,
    height = 22,
    units = "cm",
    dpi = 300
  )
    }
)

output$C19SurveyRolling.png <- downloadHandler(
  filename = "C19Issues.png",
  content = function(file) {
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T54")
    
    Data <- arrange(Data, -row_number())
    
    Data$Question <-
      factor(Data$Question,
             levels = unique(Data$Question),
             ordered = TRUE)
    
    Data <- melt(Data, id.vars = "Question")
    
    
    Data$variable <-
      factor(
        Data$variable,
        levels = unique(Data$variable),
        ordered = TRUE
      )
    
    Data <- Data %>%
      group_by(Question) %>%
      mutate(pos = cumsum(value) - value / 2) %>%
      mutate(top = sum(value))
    
    
    ### variables
    ChartColours <- c("#5d8be1", "#238b45", "#a1d99b")
    BarColours <- c("#0570b0", "#74a9cf","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    sourcecaption = "Source: Ofgem, Scottish Government"
    plottitle = "Issues with energy"
    
    DataChart <- Data %>%
      ggplot(aes(x = Question, y = value, fill = variable, color = variable), family = "Century Gothic") +
      scale_fill_manual(
        "variable",
        values = c(
          "June" = BarColours[1],
          "April" = BarColours[2]
        )
      ) +
      scale_colour_manual(values=c(BarColours[2],BarColours[1] )) +
      geom_bar(position = "dodge",
               stat = "identity",
               width = .8) +
      ylim(-1.6,.9)+
      coord_flip() +
      geom_text(position = position_dodge(width = .8),
                aes(
                  y = value + .02,
                  fill = variable,
                  label = ifelse(Data$Question == Data$Question[11], paste0(variable,": ", percent(value, 1)), percent(value, 1)),
                  colour = variable,
                ),
                hjust = 0,
                fontface = 2,
                family = "Century Gothic",
                size = 3) +
      geom_text(
        y = -0.85,
        label = str_wrap(Data$Question, 55),
        fontface = 2,
        family = "Century Gothic",
        vjust = .5,
        color = ChartColours[1]
      ) +
      geom_text(
        aes(x = 11.8,
            y = (-1.6+.9)/2,
            label = str_wrap("Which of the following have happened to you in your household?", 90)),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 12.2,
            y = 0.29,
            label = " "),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      theme(
        text = element_text(family = "Century Gothic")
        ,
        panel.background = element_rect(fill = "transparent") # bg of the panel
        ,
        plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        ,
        legend.background = element_rect(fill = "transparent") # get rid of legend bg
        ,
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        ,
        legend.title = ggplot2::element_blank()
        ,
        axis.text.x = element_blank()
        ,
        axis.text.y = element_blank()
        ,
        axis.title = ggplot2::element_blank()
        ,
        legend.text = element_text(colour = "black", family = "Century Gothic")
        ,
        axis.ticks = ggplot2::element_blank()
        ,
        panel.grid.major = ggplot2::element_blank()
        ,
        legend.position = "none"
        ,
        title = element_text(colour = ChartColours[1], size = 14)
        ,
        plot.title = ggplot2::element_text(face = "bold")
      ) + ### Label Plot
      labs(y = "Percentage", caption = sourcecaption) +
      labs(title = plottitle,
           face = "bold",
           subtitle = "Scotland, April - June 2020") +
      ### 0 Axis
      
      #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
      
      
      ### Plot Borders
      annotate(
        geom = 'segment',
        x = Inf,
        xend = Inf,
        color = ChartColours[1],
        y = -Inf,
        yend = Inf,
        size = 1.5
      ) +
      annotate(
        geom = 'segment',
        x = -Inf,
        xend = -Inf,
        color = ChartColours[1],
        y = -Inf,
        yend = Inf,
        size = 1
      ) 
    
    DataChart
    
    DataChart <-
      DataChart +
      labs(subtitle = paste("Scotland, April - June 2020")) +
      ylim(-1.6,.9)+
      coord_flip()
    
    DataChart
    
    ggsave(
      file,
      plot = DataChart,
      width = 20,
      height = 26,
      units = "cm",
      dpi = 300
    )
  }
)

output$C19SurveyChange.png <- downloadHandler(
  filename = "C19Issues.png",
  content = function(file) {
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T29B")
    
    Data <- arrange(Data, -row_number())
    
    Data$Question <-
      factor(Data$Question,
             levels = unique(Data$Question),
             ordered = TRUE)
    
    Data <- melt(Data, id.vars = "Question")
    
    
    Data$variable <-
      factor(
        Data$variable,
        levels = unique(Data$variable),
        ordered = TRUE
      )
    
    Data <- Data %>%
      group_by(Question) %>%
      mutate(pos = cumsum(value) - value / 2) %>%
      mutate(top = sum(value))
    
    
    ### variables
    ChartColours <- c("#5d8be1", "#238b45", "#a1d99b")
    BarColours <- c("#0570b0", "#74a9cf","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    sourcecaption = "Source: Ofgem, Scottish Government"
    plottitle = "Change in attitudes towards energy since lockdown"
    
    DataChart <- Data %>%
      ggplot(aes(x = Question, y = value, fill = variable, color = variable), family = "Century Gothic") +
      scale_fill_manual(
        "variable",
        values = c(
          "June" = BarColours[1],
          "April" = BarColours[2]
        )
      ) +
      scale_colour_manual(values=c(BarColours[2],BarColours[1] )) +
      geom_bar(position = "dodge",
               stat = "identity",
               width = .8) +
      ylim(-1.3,.7)+
      coord_flip() +
      geom_text(position = position_dodge(width = .8),
                aes(
                  y = value + .02,
                  fill = variable,
                  label = ifelse(Data$Question == Data$Question[6], paste0(variable,": ", percent(value, 1)), percent(value, 1)),
                  colour = variable,
                ),
                hjust = 0,
                fontface = 2,
                family = "Century Gothic",
                size = 3) +
      geom_text(
        y = -0.7,
        label = str_wrap(Data$Question, 55),
        fontface = 2,
        family = "Century Gothic",
        vjust = .5,
        color = ChartColours[1]
      ) +
      geom_text(
        aes(x = 6.8,
            y = (-1.3+.7)/2,
            label = str_wrap("Proportion of people who agree with the following statements:", 90)),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 7.2,
            y = 0.29,
            label = " "),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      theme(
        text = element_text(family = "Century Gothic")
        ,
        panel.background = element_rect(fill = "transparent") # bg of the panel
        ,
        plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        ,
        legend.background = element_rect(fill = "transparent") # get rid of legend bg
        ,
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        ,
        legend.title = ggplot2::element_blank()
        ,
        axis.text.x = element_blank()
        ,
        axis.text.y = element_blank()
        ,
        axis.title = ggplot2::element_blank()
        ,
        legend.text = element_text(colour = "black", family = "Century Gothic")
        ,
        axis.ticks = ggplot2::element_blank()
        ,
        panel.grid.major = ggplot2::element_blank()
        ,
        legend.position = "none"
        ,
        title = element_text(colour = ChartColours[1], size = 14)
        ,
        plot.title = ggplot2::element_text(face = "bold")
      ) + ### Label Plot
      labs(y = "Percentage", caption = sourcecaption) +
      labs(title = plottitle,
           face = "bold",
           subtitle = "Scotland, April - June 2020") +
      ### 0 Axis
      
      #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
      
      
      ### Plot Borders
      annotate(
        geom = 'segment',
        x = Inf,
        xend = Inf,
        color = ChartColours[1],
        y = -Inf,
        yend = Inf,
        size = 1.5
      ) +
      annotate(
        geom = 'segment',
        x = -Inf,
        xend = -Inf,
        color = ChartColours[1],
        y = -Inf,
        yend = Inf,
        size = 1
      ) 
    
    DataChart
    
    DataChart <-
      DataChart +
      labs(subtitle = paste("Scotland, April - June 2020")) 
    
    DataChart
    
    ggsave(
      file,
      plot = DataChart,
      width = 20,
      height = 14,
      units = "cm",
      dpi = 300
    )
  }
)

output$C19SurveyT62.png <- downloadHandler(
  filename = "C19Advice.png",
  content = function(file) {
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T62")
    
    Data <- arrange(Data, -row_number())
    
    Data$Question <-
      factor(Data$Question,
             levels = unique(Data$Question),
             ordered = TRUE)
    
    Data <- melt(Data, id.vars = "Question")
    
    
    Data$variable <-
      factor(
        Data$variable,
        levels = unique(Data$variable),
        ordered = TRUE
      )
    
    Data <- Data %>%
      group_by(Question) %>%
      mutate(pos = cumsum(value) - value / 2) %>%
      mutate(top = sum(value))
    
    
    ### variables
    ChartColours <- c("#5d8be1", "#238b45", "#a1d99b")
    BarColours <- c("#0570b0", "#74a9cf","#737373", "#bdbdbd", "#7fcdbb", "#8da0cb")
    sourcecaption = "Source: Ofgem, Scottish Government"
    plottitle = "Sources of advice"
    
    DataChart <- Data %>%
      ggplot(aes(x = Question, y = value, fill = variable, color = variable), family = "Century Gothic") +
      scale_fill_manual(
        "variable",
        values = c(
          "June" = BarColours[1],
          "April" = BarColours[2]
        )
      ) +
      scale_colour_manual(values=c(BarColours[2],BarColours[1] )) +
      geom_bar(position = "dodge",
               stat = "identity",
               width = .8) +
      ylim(-1.6,1.01)+
      coord_flip() +
      geom_text(position = position_dodge(width = .8),
                aes(
                  y = value + .02,
                  fill = variable,
                  label = ifelse(Data$Question == Data$Question[4], paste0(variable,": ", percent(value, 1)), percent(value, 1)),
                  colour = variable,
                ),
                hjust = 0,
                fontface = 2,
                family = "Century Gothic",
                size = 3) +
      geom_text(
        y = -0.85,
        label = str_wrap(Data$Question, 55),
        fontface = 2,
        family = "Century Gothic",
        vjust = .5,
        color = ChartColours[1]
      ) +
      geom_text(
        aes(x = 4.8,
            y = (-1.6+1.01)/2,
            label = str_wrap("Which of the following have happened to you in your household?", 90)),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 5.2,
            y = 0.29,
            label = " "),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      theme(
        text = element_text(family = "Century Gothic")
        ,
        panel.background = element_rect(fill = "transparent") # bg of the panel
        ,
        plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        ,
        legend.background = element_rect(fill = "transparent") # get rid of legend bg
        ,
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        ,
        legend.title = ggplot2::element_blank()
        ,
        axis.text.x = element_blank()
        ,
        axis.text.y = element_blank()
        ,
        axis.title = ggplot2::element_blank()
        ,
        legend.text = element_text(colour = "black", family = "Century Gothic")
        ,
        axis.ticks = ggplot2::element_blank()
        ,
        panel.grid.major = ggplot2::element_blank()
        ,
        legend.position = "none"
        ,
        title = element_text(colour = ChartColours[1], size = 14)
        ,
        plot.title = ggplot2::element_text(face = "bold")
      ) + ### Label Plot
      labs(y = "Percentage", caption = sourcecaption) +
      labs(title = plottitle,
           face = "bold",
           subtitle = "Scotland, April - June 2020") +
      ### 0 Axis
      
      #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
      
      
      ### Plot Borders
      annotate(
        geom = 'segment',
        x = Inf,
        xend = Inf,
        color = ChartColours[1],
        y = -Inf,
        yend = Inf,
        size = 1.5
      ) +
      annotate(
        geom = 'segment',
        x = -Inf,
        xend = -Inf,
        color = ChartColours[1],
        y = -Inf,
        yend = Inf,
        size = 1
      ) 
    
    DataChart
    
    DataChart <-
      DataChart +
      labs(subtitle = paste("Scotland, April - June 2020")) 
    coord_flip()
    
    DataChart
    
    ggsave(
      file,
      plot = DataChart,
      width = 20,
      height = 12,
      units = "cm",
      dpi = 300
    )
  }
)

output$C19SurveyT66.png <- downloadHandler(
  filename = "C19Information.png",
  content = function(file) {
    
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    library(readxl)
    library(scales)
    library(tidyverse)
    
    ChartColours <- c("#5d8be1", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#5d8be1", "#1d91c0", "#bdbdbd", "#7fcdbb", "#8da0cb")
    
    Data <- read_excel("Structure/0 - COVID/Survey/Data/Survey.xlsx", sheet = "T66")
    
    Data <- Data[nrow(Data):1,]
    
    Data$Year <- paste0(str_wrap(Data$Month,30))
    
    Data$Year <- factor(Data$Year,
                        levels = rev(unique(Data$Year)))
    
    BarColours <- c("#1a9850", "#bdbdbd", "#d73027")
    
    SurveyChart <- Data[c(5,2:4)]
    
    SurveyChart <- melt(SurveyChart, id.vars = "Year")
    
    SurveyChart$variable <-
      factor(SurveyChart$variable,
             levels = rev(unique(SurveyChart$variable)))
    
    
    SurveyChart <- SurveyChart %>%
      group_by(Year) %>%
      mutate(top = sum(value)) %>%
      mutate(value2 = (value/top)) %>% 
      mutate(pos = cumsum(value2) - value2 / 2)
    
    plottitle <-
      "Information from energy supplier"
    sourcecaption <- "Source: Ofgem, Scottish Government"
    
    SurveyChartChart <- SurveyChart %>%
      ggplot(aes(x = Year, y = value2, fill = variable), family = "Century Gothic") +
      scale_fill_manual(
        "variable",
        values = c(
          "Yes" = BarColours[1],
          "Don't know" = BarColours[2],
          "No" = BarColours[3]
        )
      ) +
      geom_bar(stat = "identity", width = .8) +
      geom_text(
        aes(
          y = -0.1,
          label = ifelse(variable == "Yes", as.character(Year), ""),
          color = ChartColours[2]
        ),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(x = 2.6,
            y = (0.33/2),
            label = "Yes"),
        fontface = 2,
        colour = BarColours[1],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 2.6,
            y = .93+(.07/2),
            label = "Don't know"),
        fontface = 2,
        colour = BarColours[2],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 2.6,
            y = .33 + (.6/2),
            label = "No"),
        fontface = 2,
        colour = BarColours[3],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = (3.7-2.6)/2+2.7,
            y =  (-.11+1.01)/2,
            label = str_wrap("Have you received any information from your electricity and gas supplier, such as a letter in the post, email or text, about what they're doing to manage their operations and customer service while people are socially distancing?",80)),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 3.7,
            y = (-.11+1.01)/2,
            label = " "),
        fontface = 2,
        colour = BarColours[4],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      
      geom_text(
        y = SurveyChart$pos,
        label = ifelse(SurveyChart$value >0.05, percent(SurveyChart$value,1), ""),
        
        hjust = 0.5,
        family = "Century Gothic",
        fontface = 2,
        color = "white")
    
    
    
    SurveyChartChart
    
    
    SurveyChartChart <-
      StackedBars(
        SurveyChartChart,
        SurveyChart,
        plottitle,
        sourcecaption,
        ChartColours
      )
    
    SurveyChartChart <-
      SurveyChartChart +
      ylim(-.11,1.01) +
      coord_flip() +
      labs(subtitle = "Scotland, April - June 2020")
    
    SurveyChartChart
    
    ggsave(
      file,
      plot = SurveyChartChart,
      width = 20,
      height = 10,
      units = "cm",
      dpi = 300
    )
  }
)



output$FullData <- downloadHandler(
  filename <- function() {
    paste("SurveyResultsApril", "xls", sep=".")
  },
  
  content <- function(file) {
    file.copy("Structure/0 - COVID/Survey/Data/FullSurvey.xls", file)
  },
  contentType = NULL
)

output$FullData2 <- downloadHandler(
  filename <- function() {
    paste("SurveyResultsJune", "pdf", sep=".")
  },
  
  content <- function(file) {
    file.copy("Structure/0 - COVID/Survey/Data/JuneWave.pdf", file)
  },
  contentType = NULL
)

}
