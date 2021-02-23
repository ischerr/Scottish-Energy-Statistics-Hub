require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



ScheduleOutput <- function(id) {
  ns <- NS(id)
  tagList(
    
 
    fluidRow(column(8,
                    h3("Overall renewable energy target", style = "color: #4d4d4d;  font-weight:bold"),
                    h4("Total Scottish energy consumption from renewables", style = "color: #4d4d4d;")
    ),
    column(
      4, style = 'padding:15px;',
      actionButton(ns('RenEnTgtLink'), 'More Information', style="float:right")
    )),
    #dygraphOutput(ns("SchedulePlot")),
    htmlOutput(ns("Markdown"))%>% withSpinner(color="#4d4d4d"),
    tags$hr(style = "height:3px;border:none;color:#4d4d4d;background-color:#4d4d4d;"),
    
    #################################################################################
    
    
    
    
    
    
        
      )
}




###### Server ######
Schedule <- function(input, output, session, parent_session) {
  
  
  output$Markdown <-    output$mtcars_kable <- function() {
    require("readxl")
    d <- readxl::read_xlsx(path="Structure/Sources.xlsx")
    d$Source <- cell_spec(d$Source, "html", link = d$`Direct URL`)
    library(knitr)
    d2<- d[c(2,5,7)]
    kable(d2,  escape = FALSE) %>% row_spec(0,bold=TRUE)
  }
}
