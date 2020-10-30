require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

GHGElecOutput <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
                     column(8,
                      h3("Electricity emissions", style = "color: #39ab2c;  font-weight:bold"),
                      h4(textOutput(ns('GHGElectricitySubtitle')), style = "color: #39ab2c;")
                     ),
                      column(
                           4, style = 'padding:15px;',
                           downloadButton(ns('GHGElectricity.png'), 'Download Graph', style="float:right")
                         )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("GHGElectricityPlot"))%>% withSpinner(color="#39ab2c"),
             HTML("<blockquote><p>*electricity emissions refer to the power stations, autogenerators, public sector combustion and miscellaneous industrial/commercial combustion categories in the greenhouse gas inventory</p></blockquote>"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(10, h3("Data - Electricity emissions (MtCO2e)", style = "color: #39ab2c;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("GHGTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("SGQNAS", "BEISSubNatEnergy", "BEISSubNatElec", "BEISSubNatGas", "BEISLocalRoad"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("SGQNAS"),
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("BEISSubNatElec"),
        SourceLookup("BEISSubNatGas"),
        SourceLookup("BEISLocalRoad")
        
        
      )
    )
  )
}




###### Server ######
GHGElec <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("GHGElec.R")


#####################################################################

      
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/GHGElec.txt")[2])
                     
                   )))
  })
  

  
  observeEvent(input$ToggleTable, {
    toggle("GHGTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  

}