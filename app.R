source("packages.R")

js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"

dir.create('~/.fonts')
file.copy("www/GOTHIC.TTF", "~/.fonts")
system('fc-cache -f ~/.fonts')

### Create List of Scripts, including filepath ###
   SourceList <-
    list.files(
      "Structure",
      full.names = TRUE,
      recursive = TRUE,
      pattern = "\\.R$"
    )
  
  ### Pass Each list item to Source() command ###
  sapply(SourceList, source)

  observe_helpers()

  
  
server <- function(input, output, session) {
  

  
  rv = reactiveValues()

#Create a reactive value so the URL code only runs once, when the app first loads. This prevents the app from looping if navigated too quickly  
rv$URLLoaded <- 0
  

###Parse URL to switch to appropriate tab
observe({
  
  
  if (rv$URLLoaded == 0) {
  query <- parseQueryString(session$clientData$url_search)
  
  # Return tabsetPanel names from URL. 
  query1<- paste(query[1])
  query2<- paste(query[1])
  query3<- paste(query[2])
  query4<- paste(query[2])
  query5<- paste(query[3])
  
  
  
  updateTabsetPanel(session, "MainNav",
                    selected = query1)
  
  updateNavbarPage(session, inputId = query2,
                   selected = query3)
  
  updateTabsetPanel(session, query4,
                    selected = query5)
  
  
  
  #Set that the load tab from URL code has ran, so it does not run again.
  rv$URLLoaded <- 1
  
  #Diagnostic Output to check code only runs once.
  print(paste("Url Loaded", rv$URLLoaded))
  
  }
  })

observe({

  if(input$MainNav == "WholeSystem"){
    
  updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$WholeSystem), mode = "push")
    
    callModule(match.fun(input$WholeSystem), input$WholeSystem)
    
    }
  
  
  if(input$MainNav == "RenLowCarbon"){
    
    if(input$RenLowCarbon == "RenElec"){
      
        updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$RenLowCarbon,"&Chart=",input$RenElec), mode = "push")
      
      callModule(match.fun(input$RenElec), input$RenElec)
                 }
    
    if(input$RenLowCarbon == "RenHeat"){

        updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$RenLowCarbon,"&Chart=",input$RenHeat), mode = "push")
      
      callModule(match.fun(input$RenHeat), input$RenHeat)
      
      }
    
    if(input$RenLowCarbon == "RenTransport"){
      
        updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$RenLowCarbon,"&Chart=",input$RenTransport), mode = "push")
      
      callModule(match.fun(input$RenTransport), input$RenTransport)
      
      }
    
    if(input$RenLowCarbon == "LowCarbonEconomy"){

        updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$RenLowCarbon,"&Chart=",input$LowCarbonEconomy), mode = "push")
      
      callModule(match.fun(input$LowCarbonEconomy), input$LowCarbonEconomy)
      
      }
  
  
  }
  
  
  if(input$MainNav == "LocalEnergy"){

      updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$LocalEnergy), mode = "push")
    
    callModule(match.fun(input$LocalEnergy), input$LocalEnergy)
    
    }
  
  
  if(input$MainNav == "EnergyEfficiency"){
    
    if(input$EnergyEfficiency == "DemandReduction"){
      
      updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$EnergyEfficiency,"&Chart=",input$DemandReduction), mode = "push")
      
      callModule(match.fun(input$DemandReduction), input$DemandReduction)
    }
    
    if(input$EnergyEfficiency == "EfficiencyMeasures"){
      
      updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$EnergyEfficiency,"&Chart=",input$EfficiencyMeasures), mode = "push")
      
      callModule(match.fun(input$EfficiencyMeasures), input$EfficiencyMeasures)
      
    }
    
    }
  
  
  if(input$MainNav == "ConsumerEngagement"){

      updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$ConsumerEngagement), mode = "push")
    
    callModule(match.fun(input$ConsumerEngagement), input$ConsumerEngagement)
    
    }
  
  
  if(input$MainNav == "SystemSecurity"){

      updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$SystemSecurity), mode = "push")
    
    callModule(match.fun(input$SystemSecurity), input$SystemSecurity)
    
    }
  
  if(input$MainNav == "OilGas"){
   
      updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$OilGas), mode = "push")
    
    callModule(match.fun(input$OilGas), input$OilGas)
    
  }
  
  if(input$MainNav == "TargetTracker"){
    
    updateQueryString(paste0("?Section=",input$MainNav), mode = "push")
    
    callModule(match.fun(input$MainNav), input$MainNav, parent_session = session)
    
  }
  
    



}
)

  observeEvent(input$GoToTotalEnergyTab, {
    updateTabsetPanel(session, "MainNav",
                      selected = "WholeSystem")

   })
  
  observeEvent(input$GoToRenLowCarbonTab, {
    updateNavlistPanel(session, "MainNav",
                      selected = "RenLowCarbon")
  })
  
  observeEvent(input$GoToLocalEnergyTab, {
    updateTabsetPanel(session, "MainNav",
                      selected = "LocalEnergy")
  })
  
  observeEvent(input$GoToEnergyEfficiencyTab, {
    updateTabsetPanel(session, "MainNav",
                      selected = "EnergyEfficiency"
    )
  })
  
  observeEvent(input$GoToConsumerTab, {
    updateTabsetPanel(session, "MainNav",
                      selected = "ConsumerEngagement"
    )
  })
  observeEvent(input$GoToSystemSecurityTab, {
    updateTabsetPanel(session, "MainNav",
                      selected = "SystemSecurity"
    )
  })
  
  observeEvent(input$GoToOilGasTab, {
    updateTabsetPanel(session, "MainNav",
                      selected = "OilGas"
    )
  })
  
  observeEvent(input$GoToOtherTab, {
    updateTabsetPanel(session, "MainNav",
                      selected = "TargetTracker"
    )
  })
  
  
output$HomeTab <- renderUI({
  
  tagList(
    fluidRow(
  h1("Scottish Energy Statistics"),
  h3(paste(input$MainTab)),
  HTML("<h2>The Scottish Energy Statistics Hub is still in development. If you notice any errors, please email:  <a href='mailto:energystatistics@gov.scot?'>energystatistics@gov.scot</a></h2>"),
  p(
    "This database brings together energy statistics from a range of data sources including Scottish
                      Government, Eurostat and the Department for Business, Energy and Industrial Strategy (BEIS) of the
                      UK Government. In previous years, Scottish Energy Statistics have been published in the 'Energy in Scotland'
                      compendium."
  ),
  img(src = "MainPage.png", width = "100%")
  ),
  setZoom(id = "SetEffects"),
  setShadow(id = "SetEffects"),
  fluidRow(
    column(width = 3,
           actionLink(
             "GoToTotalEnergyTab",
             label = div(
               tags$h3("Whole System View of Energy", style = "color: black;"),
               tags$p(
                 " ",
                 style = "color: black;"
               ),
               img(src = "WholeSystem.svg", height = "55%"),
               style = "border: solid 2px #269356; height: 200px; width: 100%; text-align: center; padding: 5px; border-radius: 0px; ",
               id = "SetEffects"
             )
           )),
    column(width = 3,
           actionLink(
             "GoToRenLowCarbonTab",
             label = div(
               tags$h3("Renewables and Low Carbon", style = "color: black;"),
               tags$p(
                 " ",
                 style = "color: black;"
               ),
               img(src = "RenLowCarbon.svg", height = "55%"),
               style = "border: solid 2px #39AB2C; height: 200px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
               id = "SetEffects"
             )
           )),
    column(width = 3,
           actionLink(
             "GoToLocalEnergyTab",
             label = div(
               tags$h3("Local Energy Systems", style = "color: black;"),
               tags$p(
                 " ",
                 style = "color: black;"
               ),
               img(src = "Local.svg", height = "55%"),
               style = "border: solid 2px #A3D65C; height: 200px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
               id = "SetEffects"
             )
           )),
    column(width = 3,
           actionLink(
             "GoToEnergyEfficiencyTab",
             label = div(
               tags$h3("Energy Efficiency", style = "color: black;"),
               tags$p(
                 " ",
                 style = "color: black;"
               ),
               img(src = "EE.svg", height = "55%"),
               style = "border: solid 2px #34D1A3; height: 200px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
               id = "SetEffects"
             )
           )),
    style = "padding: 10px; margin-top: 20px;"
  ),
  fluidRow(
    column(width = 3,
           actionLink(
             "GoToConsumerTab",
             label = div(
               tags$h3("Consumer Engagement", style = "color: black;"),
               tags$p(
                 " ",
                 style = "color: black;"
               ),
               img(src = "Consumer.svg", height = "55%"),
               style = "border: solid 2px #68c3ea; height: 200px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
               id = "SetEffects"
             )
           )),
    column(width = 3,
           actionLink(
             "GoToSystemSecurityTab",
             label = div(
               tags$h3("System Security", style = "color: black;"),
               tags$p(
                 " ",
                 style = "color: black;"
               ),
               img(src = "System.svg", height = "55%"),
               style = "border: solid 2px #5d8be1; height: 200px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
               id = "SetEffects"
             )
           )), 
    column(width = 3,
           actionLink(
             "GoToOilGasTab",
             label = div(
               tags$h3("Oil and Gas", style = "color: black;"),
               tags$p(
                 " ",
                 style = "color: black;"
               ),
               img(src = "OilGas.svg", height = "55%"),
               style = "border: solid 2px #126992; height: 200px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
               id = "SetEffects"
             )
           )),
    column(width = 3,
           actionLink(
             "GoToOtherTab",
             label = div(
               tags$h3("Target Tracker", style = "color: black;"),
               tags$p(
                 " ",
                 style = "color: black;"
               ),
               img(src = "TargetTracker.svg", height = "55%"),
               style = "border: solid 2px #3f3f3f; height: 200px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
               id = "SetEffects"
             )
           )),
    style = "padding: 10px; margin-top: 20px;"
  )
  )
})

  
}



ui <- shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  includeCSS("style.css"),
  useShinyjs(),
  extendShinyjs(text = js_code, functions = 'browseURL'),
  title = "Scottish Energy Statistics Hub",
  tags$head(tags$link(rel = "shortcut icon", href = "https://www.gov.scot/favicon.ico")),
  #' tags$head(HTML("<meta charset='utf-8'>
  #'   <title>Emoji</title>
  #'   <style>
  #'       @font-face {
  #'           font-family: 'Century Gothic';
  #'           src: url('GOTHIC.ttf');
  #'       }
  #'       span {
  #'           font-family: 'Century Gothic';
  #'       }
  #'   </style>")),
  navbarPage(
    id = "MainNav",
    title = div(
      span(a(
        img(src = "Govscot_logo_white.png", height = 20), href = "https://www.gov.scot/"
      ), style = "padding-right:10px;"),
      span("Scottish Energy Statistics Hub", style = "font-family: 'Century Gothic'; font-weight: 400 ")
    ),
    tabPanel(
      ###### Section - Introduction #######
      value = "Home",
      title = tags$div(img(src = "HomeIcon.svg", height = "30px",   display= "block"), " Home", style = "font-family: 'Century Gothic'; font-weight: 400 "),
      uiOutput("HomeTab")%>% withSpinner(color="#3f3f3f")
      ),

    tabPanel(
      ###### Section - Whole System View of Energy #######
      value = "WholeSystem",
      title = tags$div(img(src = "WholeSystemIcon.svg", height = "30px",   display= "block"), " Whole System", style = "font-family: 'Century Gothic'; font-weight: 400 "),
      navlistPanel(id = "WholeSystem",
        widths = c(3, 8),

        tabPanel(title = "Renewable Energy Target",
                 value = "RenEnTgt",
                 RenEnTgtOutput("RenEnTgt")),
        tabPanel(title = "Energy Productivity",
                 value = "EnProd",
                 EnProdOutput("EnProd")),
         tabPanel(title = "Energy Consumption by Sector",
                  value = "EnConsumption",
                  EnConsumptionOutput("EnConsumption")),
        tabPanel(title = "Energy Balance",
                 value = "EnBalance",
                 EnBalanceOutput("EnBalance")),
        tabPanel(title = "Energy Economy",
                 value = "EnEconomy",
                 EnEconomyOutput("EnEconomy"))
      )
    ),
    ###### Section - Renewables and Low Carbon #######
    tabPanel(
      value = "RenLowCarbon",
      title = tags$div(img(src = "RenLowCarbonIcon.svg", height = "30px",   display= "block"), " Renewables & Low Carbon", style = "font-family: 'Century Gothic'; font-weight: 400 "),
      tabsetPanel(id = "RenLowCarbon",
        tabPanel(
        value = "RenElec",
        title = "Electricity",
      navlistPanel(id = "RenElec",
        widths = c(3, 8),
        tabPanel(title ="Renewable Electricity Target", 
                 value = "RenElecTarget",
                 RenElecTargetOutput("RenElecTarget")),
        tabPanel(title ="Electricity Generation", 
                 value = "ElecGen",
                 ElecGenOutput("ElecGen")),
        tabPanel(title ="Renewable Electricity generation", 
                 value = "RenElecGen",
                 RenElecGenOutput("RenElecGen")),
        tabPanel(title ="Renewable Electricity Capacity", 
                 value = "RenElecCapacity",
                 RenElecCapacityOutput("RenElecCapacity")),
        tabPanel(title ="Electricity Consumption by Fuel", 
                 value = "ElecConsumptionFuel",
                 ElecConsumptionFuelOutput("ElecConsumptionFuel")),
        tabPanel(title ="Renewable Electricity Sources", 
                 value = "RenElecSources",
                 RenElecSourcesOutput("RenElecSources")),
        tabPanel(title ="Grid Emissions", 
                 value = "GridEmissions",
                 GridEmissionsOutput("GridEmissions")),
        tabPanel(title ="Displaced Emissions", 
                 value = "DisplacedEmissions",
                 DisplacedEmissionsOutput("DisplacedEmissions"))
        )),
      tabPanel(
        value = "RenHeat",
        title = "Heat",
        navlistPanel(id = "RenHeat",
          widths = c(3, 8),
          tabPanel(title ="Renewable Heat Target", 
                   value = "RenHeat",
                   RenHeatOutput("RenHeat")),
          tabPanel(title ="Renewable Heat by Tech Type", 
                   value = "RenHeatTech",
                   RenHeatTechOutput("RenHeatTech")),
          tabPanel(title ="Domestic RHI", 
                   value = "DomesticRHI",
                   DomesticRHIOutput("DomesticRHI")),
          tabPanel(title ="Non-domestic RHI", 
                   value = "NonDomRHI",
                   NonDomRHIOutput("NonDomRHI"))
      )),
      tabPanel(
        value = "RenTransport",
        title = "Transport",
        navlistPanel(id = "RenTransport",
                     widths = c(3, 8),
                     tabPanel(title ="Biofuels in Transport", 
                              value = "Biofuels",
                              BiofuelsOutput("Biofuels")),
                     tabPanel(title ="ULEVs", 
                              value = "ULEVs",
                              ULEVsOutput("ULEVs"))
        )
      ),
      tabPanel(
        value = "LowCarbonEconomy",
        title = "Low Carbon Economy",
        navlistPanel(id = "LowCarbonEconomy",
                     widths = c(3, 8),
                     tabPanel(title ="Low Carbon Economy", 
                              value = "LowCarbonEconomy",
                              LowCarbonEconomyOutput("LowCarbonEconomy"))
      ))
    
    )),
    ###### Section - Innovative Local Energy #######
    tabPanel(
      value = "LocalEnergy",
      title = tags$div(img(src = "LocalIcon.svg", height = "30px",   display= "block"), " Local Energy", style = "font-family: 'Century Gothic'; font-weight: 400" ),
      navlistPanel(id = "LocalEnergy",
                   widths = c(3, 8),
                   tabPanel(title = "Community and Locally Owned Renewables",
                            value = "LocalRenewables",
                            LocalRenewablesOutput("LocalRenewables")),
                   tabPanel(title = "Combined heat and power",
                            value = "CHPStats",
                            CHPStatsOutput("CHPStats")),
                   tabPanel(title = "District Heat Networks",
                            value = "DistrictHeat",
                            DistrictHeatOutput("DistrictHeat"))
    )),
    ###### Section - Energy Efficiency #######
    tabPanel(
      value = "EnergyEfficiency",
      title = tags$div(img(src = "EEIcon.svg", height = "30px",   display= "block"), " Energy Efficiency" , style = "font-family: 'Century Gothic'; font-weight: 400 "),
      tabsetPanel(id = "EnergyEfficiency",
                  tabPanel(
                    value = "DemandReduction",
                    title = "Demand Reduction",
                    navlistPanel(id = "DemandReduction",
                   widths = c(3, 8),
                   tabPanel(title = "Energy Consumption Target",
                            value = "EnConsumptionTgt",
                            EnConsumptionTgtOutput("EnConsumptionTgt")),
                   tabPanel(title = "Energy Consumption",
                            value = "EnergyConsumption",
                            EnergyConsumptionOutput("EnergyConsumption")),
                   tabPanel(title = "Electricity Consumption ",
                            value = "ElecConsumption",
                            ElecConsumptionOutput("ElecConsumption")),
                   tabPanel(title = "Heat Consumption",
                            value = "HeatConsumption",
                            HeatConsumptionOutput("HeatConsumption")),
                   tabPanel(title = "Gas Consumption",
                            value = "GasConsumption",
                            GasConsumptionOutput("GasConsumption")),
                   tabPanel(title = "Household Energy Consumption",
                            value = "HHoldEnConsumption",
                            HHoldEnConsumptionOutput("HHoldEnConsumption")),
                   tabPanel(title = "Household Energy Intensity",
                            value = "HouseholdIntensity",
                            HouseholdIntensityOutput("HouseholdIntensity")),
                   tabPanel(title = "Energy productivity of industry and sevices",
                            value = "IndustrySevicesProductivity",
                            IndustrySevicesProductivityOutput("IndustrySevicesProductivity")),
                   tabPanel(title = "Transport Energy Consumption",
                            value = "TransportEnConsumption",
                            TransportEnConsumptionOutput("TransportEnConsumption"))
                   )),
                   tabPanel(
                     value = "EfficiencyMeasures",
                     title = "Energy Efficiency Measures",
                     navlistPanel(id = "EfficiencyMeasures",
                                  widths = c(3, 8),
                   tabPanel(title = "Domestic EPCs",
                            value = "DomEPCs",
                            DomEPCsOutput("DomEPCs")),
                   tabPanel(title = "Wall Insulation",
                            value = "WallInsulation",
                            WallInsulationOutput("WallInsulation")),
                   tabPanel(title = "Loft Insulation",
                            value = "LoftInsulation",
                            LoftInsulationOutput("LoftInsulation")),
                   tabPanel(title = "Boilers",
                            value = "Boilers",
                            BoilersOutput("Boilers")),
                   tabPanel(title = "ECO Measures",
                            value = "ECOMeasures",
                            ECOMeasuresOutput("ECOMeasures")),
                   tabPanel(title = "Non-domestic EPCs",
                            value = "NonDomEPCs",
                            NonDomEPCsOutput("NonDomEPCs"))
    ))
    )),
    ###### Section - Consumer Engagement and Protection #######
    tabPanel(
      value = "ConsumerEngagement",
      title = tags$div(img(src = "ConsumerIcon.svg", height = "30px",   display= "block"), " Consumer Engagement", style = "font-family: 'Century Gothic'; font-weight: 400 "),
      navlistPanel(id = "ConsumerEngagement",
                   widths = c(3, 8),
                   tabPanel(title = "Energy Bill Payment Methods",
                            value = "BillPayments",
                            BillPaymentsOutput("BillPayments")),
                   tabPanel(title = "Electricity Bill Prices",
                            value = "ElecBillPrices",
                            ElecBillPricesOutput("ElecBillPrices")),
                   tabPanel(title = "Gas Bill Prices",
                            value = "GasBillPrices",
                            GasBillPricesOutput("GasBillPrices")),
                   tabPanel(title = "Average Bill",
                            value = "AverageBillLA",
                            AverageBillLAOutput("AverageBillLA")),
                   tabPanel(title = "Dual Fuel Bill Breakdown",
                            value = "DualFuelBreakdown",
                            DualFuelBreakdownOutput("DualFuelBreakdown")),
                   tabPanel(title = "Fuel Poverty",
                            value = "FuelPoverty",
                            FuelPovertyOutput("FuelPoverty")),
                   tabPanel(title = "Vulnerable Consumers",
                            value = "Vulnerability",
                            VulnerabilityOutput("Vulnerability")),
                   tabPanel(title = "Primary Heating Fuel",
                            value = "PrimaryHeating",
                            PrimaryHeatingOutput("PrimaryHeating")),                   
                   tabPanel(title = "Households not on the Gas Grid",
                            value = "NonGasGrid",
                            NonGasGridOutput("NonGasGrid")),
                   tabPanel(title = "Energy Supplier Switching",
                            value = "EnSupplySwitch",
                            EnSupplySwitchOutput("EnSupplySwitch")),
                   tabPanel(title = "Market Structure",
                            value = "MarketStructure",
                            MarketStructureOutput("MarketStructure")),
                   tabPanel(title = "Energy Customers on Non-home Supplier",
                            value = "EnergyNonHome",
                            EnergyNonHomeOutput("EnergyNonHome")),
                   tabPanel(title = "Smart Meter Installations",
                            value = "SmartMeters",
                            SmartMetersOutput("SmartMeters")),
                   tabPanel(title = "Restricted and Prepayment meters",
                            value = "RestrictedPPM",
                            RestrictedPPMOutput("RestrictedPPM"))
    )),
    ###### Section - System Security and Flexibility #######
    tabPanel(
      value = "SystemSecurity",
      title = tags$div(img(src = "SystemIcon.svg", height = "30px",   display= "block"), " System Security", style = "font-family: 'Century Gothic'; font-weight: 400 "),
      navlistPanel(id = "SystemSecurity",
                   widths = c(3, 8),
                   tabPanel(title = "Daily Energy Demand",
                            value = "DailyDemand",
                            DailyDemandOutput("DailyDemand")),
                   tabPanel(title = "Maximum Supply Capacity and Peak Electricity Demand",
                            value = "MaxSupplyPeakDemand",
                            MaxSupplyPeakDemandOutput("MaxSupplyPeakDemand")),
                   tabPanel(title = "Electricity Imports and Exports",
                            value = "ElecImportsExports",
                            ElecImportsExportsOutput("ElecImportsExports")),
                   tabPanel(title = "Scottish Generation Meeting Demand",
                            value = "ScotGenDemand", 
                            ScotGenDemandOutput("ScotGenDemand")),
                   tabPanel(title = "Electricity Storage",
                            value = "ElecStorage",
                            ElecStorageOutput("ElecStorage")),
                   tabPanel(title = "Gas Security",
                            value = "GasSecurity",
                            GasSecurityOutput("GasSecurity"))
    )),
    ###### Section - System Security and Flexibility #######
    tabPanel(value = "OilGas",
      title = tags$div(img(src = "OilGasIcon.svg", height = "30px",   display= "block"), " Oil & Gas", style = "font-family: 'Century Gothic'; font-weight: 400 "),
      navlistPanel(id = "OilGas",
                   widths = c(3, 8),
                   tabPanel(title = "Primary Energy - Oil and Gas",
                            value = "PrimaryOilGas",
                            PrimaryOilGasOutput("PrimaryOilGas")),
                   tabPanel(title = "Scottish Oil and Gas Production",
                            value = "OilGasProd",
                            OilGasProdOutput("OilGasProd")),
                   tabPanel(title = "Oil and Gas Outputs",
                            value = "OilGasOutputs",
                            OilGasOutputsOutput("OilGasOutputs")),
                   tabPanel(title = "Oil and Gas Consumption",
                            value = "OilGasConsumption",
                            OilGasConsumptionOutput("OilGasConsumption")),
                   tabPanel(title = "Oil and Gas Exports",
                            value = "OilGasExports",
                            OilGasExportsOutput("OilGasExports")),
                   tabPanel(title = "Oil and Gas Sales Revenue",
                            value = "OilGasRevenue",
                            OilGasRevenueOutput("OilGasRevenue")),
                   tabPanel(title = "Oil and Gas GVA",
                            value = "OilGasGVA",
                            OilGasGVAOutput("OilGasGVA")),
                   tabPanel(title = "Oil and Gas Employment",
                            value = "OilGasEmployment",
                            OilGasEmploymentOutput("OilGasEmployment")),
                   tabPanel(title = "Coal Production",
                            value = "CoalProd",
                            CoalProdOutput("CoalProd"))
                   )),
                   ###### Section - System Security and Flexibility #######
                   tabPanel(value = "TargetTracker",
                            title = tags$div(img(src = "TargetIcon.svg", height = "30px",   display= "block"), " Target Tracker", style = "font-family: 'Century Gothic'; font-weight: 400 "),
                            TargetTrackerOutput("TargetTracker")
                            
                          
    )
  ),
  # FOOTER ##########################################################################################################################################
  fluidRow(
    br(),
    p(
    "Reload the page should you experience any issues."
  ),
  style = "text-align: center; outline: 0px;"),
           wellPanel(
             fluidRow(
               # FOOTER - WEBSITE
               column(
                 width = 3,
                 icon("chart-bar", lib = "font-awesome"),
                 strong("STATISTICS"),
                 p(
                   "Scottish Energy Statistics Website:"
                 ),
                 a(
                   "Latest energy statistics for Scotland",
                   href = "https://www2.gov.scot/Topics/Statistics/Browse/Business/Energy",
                   ""
                 )
               ),
               # FOOTER - ABOUT
               column(
                 width = 3,
                 icon("info", lib = "font-awesome"),
                 strong("ABOUT"),
                 p(
                   "Scottish Energy Strategy:"
                 ),
                 a(
                   "The future of energy in Scotland: Scottish energy strategy",
                   href = "https://www.gov.scot/publications/scottish-energy-strategy-future-energy-scotland-9781788515276/",
                   ""
                 )
               ),
               
               # FOOTER - CONTACT DETAILS
               column(
                 width = 3,
                 icon("at", lib = "font-awesome"),
                 strong("CONTACT DETAILS"),
                 p("Please send any feedback or questions to our email address."),
                 HTML('<a href="mailto:energystatistics@gov.scot?">energystatistics@gov.scot</a>')
               ),
                              # FOOTER - COPYRIGHT NOTICE
               column(
                 width = 3,
                 icon("copyright", lib = "font-awesome"),
                 strong("COPYRIGHT NOTICE"),
                 p(
                   "You may use or re-use this information (not including logos) free of charge in any format or medium, under the terms of the ",
                   a("Open Government Licence", href = "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"),
                   "."
                 )
               ),# FOOTER - EXTERNAL LINKS
               
             )
           ),
  titlePanel("", windowTitle = "Scottish Energy Statistics")
)) # Navbar page ends here) ###### UI End ######


enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)
