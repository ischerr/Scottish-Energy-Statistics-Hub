require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



SourcesListOutput <- function(id) {
  ns <- NS(id)
  tagList(
    
    
    fluidRow(column(12,
                    h3("Department for Business, Energy & Industrial Strategy", style = "color: #4d4d4d;  font-weight:bold"),
                    h4("BEIS", style = "color: #4d4d4d;")
    )),
    #dygraphOutput(ns("SchedulePlot")),
    htmlOutput(ns("BEIS"))%>% withSpinner(color="#4d4d4d"),
    tags$hr(style = "height:3px;border:none;color:#4d4d4d;background-color:#4d4d4d;"),
    
    #################################################################################
    
    fluidRow(column(12,
                    h3("Department for Transport", style = "color: #4d4d4d;  font-weight:bold"),
                    h4("DfT", style = "color: #4d4d4d;")
    )),
    #dygraphOutput(ns("SchedulePlot")),
    htmlOutput(ns("DFT"))%>% withSpinner(color="#4d4d4d"),
    tags$hr(style = "height:3px;border:none;color:#4d4d4d;background-color:#4d4d4d;"),
    
    #################################################################################
    
    fluidRow(column(12,
                    h3("National Grid", style = "color: #4d4d4d;  font-weight:bold"),
                    
    )),
    #dygraphOutput(ns("SchedulePlot")),
    htmlOutput(ns("NG"))%>% withSpinner(color="#4d4d4d"),
    tags$hr(style = "height:3px;border:none;color:#4d4d4d;background-color:#4d4d4d;"),
    
    #################################################################################
    
    fluidRow(column(12,
                    h3("Office of Gas and Electricity Markets", style = "color: #4d4d4d;  font-weight:bold"),
                    h4("OFGEM", style = "color: #4d4d4d;")
    )),
    #dygraphOutput(ns("SchedulePlot")),
    htmlOutput(ns("OFGEM"))%>% withSpinner(color="#4d4d4d"),
    tags$hr(style = "height:3px;border:none;color:#4d4d4d;background-color:#4d4d4d;"),
    
    #################################################################################
    
    fluidRow(column(12,
                    h3("Scottish Government", style = "color: #4d4d4d;  font-weight:bold"),
                    h4("SG", style = "color: #4d4d4d;")
    )),
    #dygraphOutput(ns("SchedulePlot")),
    htmlOutput(ns("SG"))%>% withSpinner(color="#4d4d4d"),
    tags$hr(style = "height:3px;border:none;color:#4d4d4d;background-color:#4d4d4d;"),
    
    #################################################################################
    
    fluidRow(column(12,
                    h3("Other", style = "color: #4d4d4d;  font-weight:bold"),
    )),
    #dygraphOutput(ns("SchedulePlot")),
    htmlOutput(ns("Other"))%>% withSpinner(color="#4d4d4d"),
    tags$hr(style = "height:3px;border:none;color:#4d4d4d;background-color:#4d4d4d;"),
    
  )
}




###### Server ######
SourcesList <- function(input, output, session, parent_session) {
  
  
  output$BEIS <-    output$mtcars_kable <- function() {
    require("readxl")
    
    d <- readxl::read_xlsx(path="Structure/Sources.xlsx")
    d$Source <- substr(d$Source,7,str_length(d$Source))
    d <- d[order(d$Source),]
    d$`Last Updated` <- format(d$`Last Updated`, "%b %Y")
    d$`Update Expected` <- format(d$`Update Expected`, "%b %Y")
    d$Source <- cell_spec(d$Source, "html", link = d$`Direct URL`)
    d <- d[which(d$Category == "BEIS"),]
    library(knitr)
    d2<- d[c(2,5,7)]
    d2[is.na(d2)] <- "To Be Confirmed"
    kable(d2,  escape = FALSE) %>% row_spec(0,bold=TRUE) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  output$DFT <-    output$mtcars_kable <- function() {
    require("readxl")
    
    d <- readxl::read_xlsx(path="Structure/Sources.xlsx")
    d$Source <- substr(d$Source,6,str_length(d$Source))
    d <- d[order(d$Source),]
    d$`Last Updated` <- format(d$`Last Updated`, "%b %Y")
    d$`Update Expected` <- format(d$`Update Expected`, "%b %Y")
    d$Source <- cell_spec(d$Source, "html", link = d$`Direct URL`)
    d <- d[which(d$Category == "DFT"),]
    library(knitr)
    d2<- d[c(2,5,7)]
    d2[is.na(d2)] <- "To Be Confirmed"
    kable(d2,  escape = FALSE) %>% row_spec(0,bold=TRUE) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  output$NG <-    output$mtcars_kable <- function() {
    require("readxl")
    
    d <- readxl::read_xlsx(path="Structure/Sources.xlsx")
    d$Source <- substr(d$Source,16,str_length(d$Source))
    d <- d[order(d$Source),]
    d$`Last Updated` <- format(d$`Last Updated`, "%b %Y")
    d$`Update Expected` <- format(d$`Update Expected`, "%b %Y")
    d$Source <- cell_spec(d$Source, "html", link = d$`Direct URL`)
    d <- d[which(d$Category == "NG"),]
    library(knitr)
    d2<- d[c(2,5,7)]
    d2[is.na(d2)] <- "To Be Confirmed"
    kable(d2,  escape = FALSE) %>% row_spec(0,bold=TRUE) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  output$OFGEM <-    output$mtcars_kable <- function() {
    require("readxl")
    
    d <- readxl::read_xlsx(path="Structure/Sources.xlsx")
    d$Source <- substr(d$Source,8,str_length(d$Source))
    d <- d[order(d$Source),]
    d$`Last Updated` <- format(d$`Last Updated`, "%b %Y")
    d$`Update Expected` <- format(d$`Update Expected`, "%b %Y")
    d$Source <- cell_spec(d$Source, "html", link = d$`Direct URL`)
    d <- d[which(d$Category == "OFGEM"),]
    library(knitr)
    d2<- d[c(2,5,7)]
    d2[is.na(d2)] <- "To Be Confirmed"
    kable(d2,  escape = FALSE) %>% row_spec(0,bold=TRUE) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  output$SG <-    output$mtcars_kable <- function() {
    require("readxl")
    
    d <- readxl::read_xlsx(path="Structure/Sources.xlsx")
    d$Source <- substr(d$Source,22,str_length(d$Source))
    d <- d[order(d$Source),]
    d$`Last Updated` <- format(d$`Last Updated`, "%b %Y")
    d$`Update Expected` <- format(d$`Update Expected`, "%b %Y")
    d$Source <- cell_spec(d$Source, "html", link = d$`Direct URL`)
    d <- d[which(d$Category == "SG"),]
    library(knitr)
    d2<- d[c(2,5,7)]
    d2[is.na(d2)] <- "To Be Confirmed"
    kable(d2,  escape = FALSE) %>% row_spec(0,bold=TRUE) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  output$Other <-    output$mtcars_kable <- function() {
    require("readxl")
    
    d <- readxl::read_xlsx(path="Structure/Sources.xlsx")
    d$Source <- substr(d$Source,1,str_length(d$Source))
    d <- d[order(d$Source),]
    d$`Last Updated` <- format(d$`Last Updated`, "%b %Y")
    d$`Update Expected` <- format(d$`Update Expected`, "%b %Y")
    d$Source <- cell_spec(d$Source, "html", link = d$`Direct URL`)
    d <- d[which(d$Category == "Other"),]
    library(knitr)
    d2<- d[c(2,5,7)]
    d2[is.na(d2)] <- "To Be Confirmed"
    kable(d2,  escape = FALSE) %>% row_spec(0,bold=TRUE) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
}
