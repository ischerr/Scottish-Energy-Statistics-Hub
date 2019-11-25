require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

TargetTrackerOutput <- function(id) {
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
    #dygraphOutput(ns("TargetTrackerPlot")),
    plotlyOutput(ns("RenEnTgtPlot"), height = "75px")%>% withSpinner(color="#4d4d4d"),
    tags$hr(style = "height:3px;border:none;color:#4d4d4d;background-color:#4d4d4d;"),
    
    #################################################################################
    
    fluidRow(column(8,
                    h3("Energy productivity target", style = "color: #800008;  font-weight:bold"),
                    h4("% change in gross value added achieved from the input of one gigawatt hour of energy from 2015", style = "color: #800008;")
    ),
    column(
      4, style = 'padding:15px;',
      actionButton(ns('EnProdTgtLink'), 'More Information', style="float:right")
    )),
    #dygraphOutput(ns("TargetTrackerPlot")),
    plotlyOutput(ns("EnProdTgtPlot"), height = "75px")%>% withSpinner(color="#800008"),
    tags$hr(style = "height:3px;border:none;color:#800008;background-color:#800008;"),
    
    ##################################################################################
    
     fluidRow(column(8,
                    h3("Renewable electricity target", style = "color: #31a354;  font-weight:bold"),
                    h4("Gross electricity consumption from renewables", style = "color: #31a354;")
    ),
             column(
               4, style = 'padding:15px;',
               actionButton(ns('RenElecTgtLink'), 'More Information', style="float:right")
             )),
    #dygraphOutput(ns("TargetTrackerPlot")),
    plotlyOutput(ns("RenElecTgtPlot"), height = "75px")%>% withSpinner(color="#31a354"),
    tags$hr(style = "height:3px;border:none;color:#31a354;background-color:#31a354;"),
    
    #################################################################################
    
    fluidRow(column(8,
                    h3("Emissions", style = "color: #ff6600;  font-weight:bold"),
                    h4("Reduction in adjusted emissions (based on EU-ETS) against 1990 baseline (MtCO2e)", style = "color: #ff6600;")
    ),
    column(
      4, style = 'margin-top: 10px;',
      actionButton(ns('EmissionsLink'), 'More Information', style="float:right")
    )),
    #dygraphOutput(ns("TargetTrackerPlot")),
    plotlyOutput(ns("EmissionsPlot"), height = "75px")%>% withSpinner(color="#ff6600"),
    tags$hr(style = "height:3px;border:none;color:#ff6600;background-color:#ff6600;"),
    
    #################################################################################
    
    fluidRow(column(8,
                    h3("Grid Intensity", style = "color: #ff6600;  font-weight:bold"),
                    h4("Amount of CO2 for each kWh of electricity generated (gCO2e/kWh)", style = "color: #ff6600;")
    ),
    column(
      4, style = 'margin-top: 10px;',
      actionButton(ns('GridIntensityLink'), 'More Information', style="float:right")
    )),
    #dygraphOutput(ns("TargetTrackerPlot")),
    plotlyOutput(ns("GridIntensityPlot"), height = "75px")%>% withSpinner(color="#ff6600"),
    tags$hr(style = "height:3px;border:none;color:#ff6600;background-color:#ff6600;"),
    
    
    #################################################################################
    
    fluidRow(column(8,
                    h3("Renewable heat target", style = "color: #ff6600;  font-weight:bold"),
                    h4("Non-electrical heat demand from renewables", style = "color: #ff6600;")
    ),
    column(
      4, style = 'margin-top: 10px;',
      actionButton(ns('RenHeatTgtLink'), 'More Information', style="float:right")
    )),
    #dygraphOutput(ns("TargetTrackerPlot")),
    plotlyOutput(ns("RenHeatTgtPlot"), height = "75px")%>% withSpinner(color="#ff6600"),
    tags$hr(style = "height:3px;border:none;color:#ff6600;background-color:#ff6600;"),
    
    #################################################################################
    
    fluidRow(column(8,
                    h3("Energy consumption target", style = "color: #297fff;  font-weight:bold"),
                    h4("Reduction in total energy consumption from 2005-07", style = "color: #297fff;")
    ),
    column(
      4, style = 'padding:15px;',
      actionButton(ns('EnConsTgtLink'), 'More Information', style="float:right")
    )),
    #dygraphOutput(ns("TargetTrackerPlot")),
    plotlyOutput(ns("EnConsTgtPlot"), height = "75px")%>% withSpinner(color="#297fff"),
    tags$hr(style = "height:3px;border:none;color:#297fff;background-color:#297fff;")
    
    
    
        
      )
}




###### Server ######
TargetTracker <- function(input, output, session, parent_session) {
  
  BarPercentage <- function(start, target, current){

    change = start - current
    
    distance = start - target 
    
    
    barpercentage <- change / distance
    
    percent(barpercentage)
    
    return(barpercentage)
  }
  TargetTrackerBar <- function(start, target, current, CurrentAnnotation, TargetAnnotation, Colour){
    
    x <- BarPercentage(start, target, current)
    
    
    plot_ly() %>% 
      add_trace(
        y = 1,
        x = ifelse(x > 1, 1, x),
        name = "bar1",
        type = "bar",
        hoverinfo = 'skip',
        text = paste0(CurrentAnnotation),
        orientation = 'h',
        marker = list(color = Colour)
      ) %>%
      add_trace(
        y = 1,
        x = 1 - ifelse(x > 1, 1, x),
        name = "bar2",
        type = "bar",
        hoverinfo = 'skip',
        text = TargetAnnotation,
        orientation = 'h',
        marker = list(color = Colour,
                      opacity= 0.4)
      ) %>%
      add_trace(
        y = 1.45,
        x = 1,
        name = "bar2",
        type = "scatter",
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0(TargetAnnotation),
        textposition = 'top left',
        orientation = 'h',
        textfont = list(color = Colour),
        marker = list(color = Colour,
                      opacity= 0)
      ) %>%
      add_trace(
        y = 1.45,
        x = 0,
        name = "bar2",
        type = "scatter",
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0(CurrentAnnotation),
        textposition = 'top right',
        orientation = 'h',
        textfont = list(color = Colour),
        marker = list(color = Colour,
                      opacity= 0)
      ) %>%
      layout(barmode = 'stack',
             margin = list(
               l = 0,
               r = 0,
               b = 1,
               t = 1
             ),
             xaxis = list(zeroline = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE,
                          range = c(0,1)),
             yaxis = list(zeroline = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE,
                          range = c(0.9, 2.05)
             ),
             showlegend = FALSE
      ) %>% 
      config(displayModeBar = F)
  }
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("TargetTracker.R")
  
  output$RenEnTgtPlot <- renderPlotly  ({
    
    start = 0
    target = .5
    current = .208
    CurrentAnnotation = paste0("<b>Current: ", percent(current, accuracy = 0.1), "</b>\n in 2017")
    TargetAnnotation = paste0("<b>Target: ", percent(target, accuracy = 1), "</b>\n by 2030")
    Colour = "#4d4d4d"
    
    TargetTrackerBar(start, target, current, CurrentAnnotation, TargetAnnotation, Colour)
    
  })

  observeEvent(input$RenEnTgtLink, {
    
    updateTabsetPanel(parent_session, "MainNav",
                      selected = "WholeSystem")
    
    updateNavbarPage(parent_session, inputId = "WholeSystem",
                     selected = "RenEnTgt")
    
    updateTabsetPanel(parent_session, "RenEnTgt",
                      selected = "RenEnTgt")
    
    print("Hi")
  })
  
  
  output$RenElecTgtPlot <- renderPlotly  ({
    
    start = 0
    target = 1
    current = .766
    CurrentAnnotation = paste0("<b>Current: ", percent(current, accuracy = 0.1), "</b>\n in 2018")
    TargetAnnotation = paste0("<b>Target: ", percent(target, accuracy = 1), "</b>\n by 2020")
    Colour = "#31a354"
    
    TargetTrackerBar(start, target, current, CurrentAnnotation, TargetAnnotation, Colour)
    
  })
  
  observeEvent(input$RenElecTgtLink, {
    
    updateTabsetPanel(parent_session, "MainNav",
                      selected = "RenLowCarbon")
    
    updateNavbarPage(parent_session, inputId = "RenLowCarbon",
                     selected = "RenElec")
    
    updateTabsetPanel(parent_session, "RenElec",
                      selected = "RenElecTarget")
    
    print("Hi")
  })
  
  ##########################################
  
  output$EmissionsPlot <- renderPlotly  ({
    
    start = 75.6878490587855
    target = 0
    current = 46.4103865830099
    CurrentAnnotation = paste0("<b>Current: ", round(current, digits = 0), " MtCO2e</b>\n in 2017")
    TargetAnnotation = paste0("<b>Target: ", round(target, digits = 0), " MtCO2e</b>\n by 2045")
    Colour = "#ff6600"
    
    TargetTrackerBar(start, target, current, CurrentAnnotation, TargetAnnotation, Colour)
    
  })
  
  observeEvent(input$EmissionsLink, {
    
    updateTabsetPanel(parent_session, "MainNav",
                      selected = "RenLowCarbon")
    
    updateNavbarPage(parent_session, inputId = "RenLowCarbon",
                     selected = "Emissions")
    
    updateTabsetPanel(parent_session, "Emissions",
                      selected = "EnSupplyEmissions")
    
    print("Hi")
  })
  
  ##########################################
  
  output$GridIntensityPlot <- renderPlotly  ({
    
    start = 389.8
    target = 50
    current = 24
    CurrentAnnotation = paste0("<b>Current: ", round(current, digits = 0), " gCO2e/kWh</b>\n in 2017")
    TargetAnnotation = paste0("<b>Target: ", round(target, digits = 0), " gCO2e/kWh</b>\n by 2020")
    Colour = "#ff6600"
    
    TargetTrackerBar(start, target, current, CurrentAnnotation, TargetAnnotation, Colour)
    
  })
  
  observeEvent(input$GridIntensityLink, {
    
    updateTabsetPanel(parent_session, "MainNav",
                      selected = "RenLowCarbon")
    
    updateNavbarPage(parent_session, inputId = "RenLowCarbon",
                     selected = "Emissions")
    
    updateTabsetPanel(parent_session, "Emissions",
                      selected = "GridEmissions")
    
    print("Hi")
  })
  
  #########################################
  
  output$RenHeatTgtPlot <- renderPlotly  ({
    
    start = 0
    target = .11
    current = .055
    CurrentAnnotation = paste0("<b>Current: ", percent(current, accuracy = 0.1), "</b>\n in 2017")
    TargetAnnotation = paste0("<b>Target: ", percent(target, accuracy = 1), "</b>\n by 2020")
    Colour = "#ff6600"
    
    TargetTrackerBar(start, target, current, CurrentAnnotation, TargetAnnotation, Colour)
    
  })
  
  observeEvent(input$RenHeatTgtLink, {
    
    updateTabsetPanel(parent_session, "MainNav",
                      selected = "RenLowCarbon")
    
    updateNavbarPage(parent_session, inputId = "RenLowCarbon",
                     selected = "RenHeat")
    
    updateTabsetPanel(parent_session, "RenHeat",
                      selected = "RenHeat")
    
    print("Hi")
  })
  
  #########################################
  
  output$EnConsTgtPlot <- renderPlotly  ({
    
    start = 0
    target = .12
    current = .118
    CurrentAnnotation = paste0("<b>Current: ", percent(current, accuracy = 0.1), "</b>\n in 2017")
    TargetAnnotation = paste0("<b>Target: ", percent(target, accuracy = 1), "</b>\n by 2020")
    Colour = "#297fff"
    
    TargetTrackerBar(start, target, current, CurrentAnnotation, TargetAnnotation, Colour)
    
  })
  
  observeEvent(input$EnConsTgtLink, {
    
    updateTabsetPanel(parent_session, "MainNav",
                      selected = "EnergyEfficiency")
    
    updateNavbarPage(parent_session, inputId = "EnergyEfficiency",
                     selected = "EnConsumptionTgt")
    
    updateTabsetPanel(parent_session, "EnConsumptionTgt",
                      selected = "EnConsumptionTgt")
    
    print("Hi")
  })
  
  
  
  
  
  output$EnProdTgtPlot <- renderPlotly  ({
    
    start = 0
    target = .3
    current = -.0015
    CurrentAnnotation = paste0("<b>Current: ", percent(current, accuracy = 0.1), "</b>\n in 2017")
    TargetAnnotation = paste0("<b>Target: ", percent(target, accuracy = 1), "</b>\n by 2030")
    Colour = "#800008"
    
    TargetTrackerBar(start, target, current, CurrentAnnotation, TargetAnnotation, Colour)
    
  })
  
  observeEvent(input$EnProdTgtLink, {
    
    updateTabsetPanel(parent_session, "MainNav",
                      selected = "WholeSystem")
    
    updateNavbarPage(parent_session, inputId = "WholeSystem",
                     selected = "EnProd")
    
    updateTabsetPanel(parent_session, "EnProd",
                      selected = "EnProd")
    
    print("Hi")
  })
  
}
