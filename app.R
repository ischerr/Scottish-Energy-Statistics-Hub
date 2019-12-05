source("packages.R")



#loadfonts(device = "win")

js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"
# 
dir.create('~/.fonts')
file.copy("www/GOTHIC.TTF", "~/.fonts")
system('fc-cache -f ~/.fonts')

# 
# font_import(pattern="Cent",prompt=FALSE)
# loadfonts(device="win")


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


  
server <- function(input, output, session) {
  

  observe_helpers()
  
  rv = reactiveValues()
  
  callModule(RenEnTgt, "RenEn")
  
  callModule(EnProd, "EnProd")
  
  callModule(EnergyEconomy, "EnergyEconomy")
  
  callModule(DailyDemand, "DailyDemand")
  
  callModule(RenElecTgt, "RenElec")
  
  callModule(RenHeatTgt, "RenHeat")
  
  callModule(EnConsumption, "EnConsumption")
  
  callModule(ElecGenLCFF, "ElecGenLCFF")
  
  callModule(ScotRenGen, "ScotRenGen")
  
  callModule(RenHeatTech, "RenHeatTech")
  
  callModule(Biofuels, "Biofuels")
  
  callModule(SupplyEmissions, "SupplyEmissions")
  
  callModule(AdjustedEmissions, "AdjustedEmissions")
  
  callModule(EmissionsDisplaced, "EmissionsDisplaced")
  
  callModule(GridEmissions, "GridEmissions")
  
  callModule(WallInsulation, "WallInsulation")
  
  callModule(BoilerImprovements, "BoilerImprovements")
  
  callModule(BillPayments, "BillPayments")
  
  callModule(HHoldEnMonitor, "HHoldEnMonitor")
  
  callModule(ElecGen, "ElecGen")
  
  callModule(PeakDay, "PeakDay")
  
  callModule(ImportsExports, "ImportsExports")
  
  callModule(ScotOwnGen, "ScotOwnGen")
  
  callModule(OilGasProd, "OilGasProd")
  
  callModule(OilGasExports, "OilGasExports")
  
  callModule(OilGasRevenue, "OilGasRevenue")
  
  callModule(OilGasGVA, "OilGasGVA")
  
  callModule(CoalProd, "CoalProd")
  
  callModule(RenEnEU, "RenEnEU")
  
  callModule(RenElecEU, "RenElecEU")
  
  callModule(EUWindHydro, "EUWindHydro")
  
  callModule(ElecGenByFuel, "ElecGenByFuel")
  
  callModule(ElecConsumpByFuel, "ElecConsumpByFuel")
  
  callModule(RenElecFuel, "RenElecFuel")
  
  callModule(RenElecCap, "RenElecCap")
  
  callModule(RenElecPipeline, "RenElecPipeline")
  
  callModule(RenHeatEU, "RenHeatEU")
  
  callModule(EnergyConsumptionEU, "EnergyConsumptionEU")
  
  callModule(EnConsumptionTgt, "EnConsumptionTgt")
  
  callModule(EnConsLA, "EnConsLA")
  
  callModule(EnergyBalance, "EnergyBalance")
  
  callModule(ElecLA, "ElecLA")
  
  callModule(DomesticRHI, "DomesticRHI")
  
  callModule(NonDomesticRHI, "NonDomesticRHI")
  
  callModule(ULEVs, "ULEVs")
  
  callModule(GHGEmissions, "GHGEmissions")
  
  callModule(LowCarbonEconomy, "LowCarbonEconomy")
  
  callModule(CommunityRenewables, "CommunityRenewables")
  
  callModule(CHP, "CHP")
  
  callModule(HeatNetwork, "HeatNetwork")
  
  callModule(EnComsumpSector, "EnComsumpSector")
  
  callModule(EnConsumpFuel, "EnConsumpFuel")
  
  callModule(ElecConsump, "ElecConsump")
  
  callModule(ElecConsumpHhold, "ElecConsumpHhold")
  
  callModule(ElecConsumptionLA, "ElecConsumptionLA")
  
  callModule(HeatConsump, "HeatConsump")
  
  callModule(GasConsump, "GasConsump")
  
  callModule(GasConsumpHhold, "GasConsumpHhold")
  
  callModule(GasConsumptionLA, "GasConsumptionLA")
  
  callModule(HouseholdEnergyConsumption, "HouseholdEnergyConsumption")
  
  callModule(TransportConsumpFuel, "TransportConsumpFuel")
  
  callModule(NonDomEPC, "NonDomEPC")
  
  callModule(LoftInsulation, "LoftInsulation")
  
  callModule(DomEPC, "DomEPC")

  callModule(ElecBillPrices, "ElecBillPrices")
  
  callModule(GasBillPrices, "GasBillPrices")
  
  callModule(DualFuelBreakdown, "DualFuelBreakdown")
  
  callModule(EUBill, "EUBill")
  
  callModule(ElecNonHome, "ElecNonHome")
  
  callModule(GasNonHome, "GasNonHome")
  
  callModule(FuelPoverty, "FuelPoverty")
  
  callModule(PrimaryHeating, "PrimaryHeating")
  
  callModule(NonGasGrid, "NonGasGrid")
  
  callModule(EnergyUseMonitor, "EnergyUseMonitor")
  
  callModule(SmartMeters, "SmartMeters")
  
  callModule(EnSupplySwitch, "EnSupplySwitch")
  
  callModule(MaxSupplyPeakDemand, "MaxSupplyPeakDemand")
  
  callModule(GasSecurity, "GasSecurity")
  
  callModule(TargetTracker, "TargetTracker", parent_session = session)
  
  
#Create a reactive value so the URL code only runs once, when the app first loads. This prevents the app from looping if navigated too quickly  
rv$URLLoaded <- 0
  
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
    
  updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$WholeSystem), mode = "push")}
  
  
  if(input$MainNav == "RenLowCarbon"){
    
    if(input$RenLowCarbon == "RenElec"){
      
        updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$RenLowCarbon,"&Chart=",input$RenElec), mode = "push")}
    
    if(input$RenLowCarbon == "RenHeat"){

        updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$RenLowCarbon,"&Chart=",input$RenHeat), mode = "push")}
    
    if(input$RenLowCarbon == "RenTransport"){
      
        updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$RenLowCarbon,"&Chart=",input$RenTransport), mode = "push")}
    
    if(input$RenLowCarbon == "Emissions"){

        updateQueryString(paste0("?Section=",input$MainNav,"&Subsection=",input$RenLowCarbon,"&Chart=",input$Emissions), mode = "push")}
  
  
  }
  
  
  if(input$MainNav == "LocalEnergy"){

      updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$LocalEnergy), mode = "push")}
  
  
  if(input$MainNav == "EnergyEfficiency"){

      updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$EnergyEfficiency), mode = "push")}
  
  
  if(input$MainNav == "ConsumerEngagement"){

      updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$ConsumerEngagement), mode = "push")}
  
  
  if(input$MainNav == "SystemSecurity"){

      updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$SystemSecurity), mode = "push")}
  
  if(input$MainNav == "OilGas"){
   
      updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$OilGas), mode = "push")}
  
    

if(input$MainNav == "TargetTracker"){
  
  updateQueryString(paste0("?Section=",input$MainNav,"&Chart=",input$TargetTracker), mode = "push")}

}
)
    

  
  # SourceList <-
  #   list.files(
  #     "Structure",
  #     full.names = TRUE,
  #     recursive = TRUE,
  #     pattern = "\\.R$"
  #   )
  # 
  # sapply(SourceList, source)
  
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
    js$browseURL("https://www2.gov.scot/Topics/Statistics/Browse/Business/Energy")
  })
  
  
output$HomeTab <- renderUI({
  
  tagList(
    fluidRow(
  h1("Scottish Energy Statistics"),
  h3(paste(input$MainTab)),
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
               tags$h3("Scottish Energy Statistics Website", style = "color: black;"),
               tags$p(
                 " ",
                 style = "color: black;"
               ),
               img(src = "SES.svg", height = "55%"),
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
  title = "SESHon",
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
      # fluidRow(
      #   h4("Instructions"),
      #   p(
      #     "The various charts and graphs relating to Scottish energy statistics can be explored under the 'Charts' tab.
      #                 The sources of the data are provided."
      #   ),
      #   p(
      #     "Note that it is possible to zoom in on the charts. A chart can be reset by double clicking on it. Maximise
      #                 the browser window to get the best experience."
      #   ),
      #   h4("Questions and Comments"),
      #   p(
      #     "We welcome any feedback on this Shiny App. For questions and feedback please get in touch at the following
      #                 email address:"
      #   ),
      #   p("Email - energystatistics@gov.scot")
      # ),
    tabPanel(
      ###### Section - Whole System View of Energy #######
      value = "WholeSystem",
      title = tags$div(img(src = "WholeSystemIcon.svg", height = "30px",   display= "block"), " Whole System", style = "font-family: 'Century Gothic'; font-weight: 400 "),
      navlistPanel(id = "WholeSystem",
        widths = c(3, 8),
        ###### Tab - Total Energy #######
        tabPanel(title = "Renewable Energy Target",
                 value = "RenEnTgt",
                 RenEnTgtOutput("RenEn")),
        ###### Tab - Energy Productivity #######
        tabPanel(title = "Renewable Energy EU",
                 value = "RenEnEU",
                 RenEnEUOutput("RenEnEU")),
        ###### Tab - Energy Productivity #######
        tabPanel(title = "Energy Productivity",
                 value = "EnProd",
                 EnProdOutput("EnProd")),
        ###### Tab - Energy Productivity #######
         tabPanel(title = "Energy Consumption by Sector",
                  value = "EnConsumption",
                  EnConsumptionOutput("EnConsumption")),
        ###### Tab - Energy Productivity #######
        tabPanel(title = "Energy Consumption by Local Authority",
                 value = "EnConsumptionLA",
                  EnConsLAOutput("EnConsLA")),
        ###### Tab - Energy Productivity #######
        tabPanel(title = "Energy Balance",
                 value = "EnBalance",
                 EnergyBalanceOutput("EnergyBalance")),
        ###### Tab - Energy Productivity #######
        tabPanel(title = "Energy Economy",
                 value = "EnEconomy",
                 EnergyEconomyOutput("EnergyEconomy"))
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
        ###### Tab - Daily Demmand #######
        tabPanel(title ="Renewable Electricity Target", 
                 value = "RenElecTarget",
                 RenElecTgtOutput("RenElec")),
        tabPanel(title ="Renewable Electricity EU", 
                 value = "RenElecEU",
                 RenElecEUOutput("RenElecEU")),
        tabPanel(title ="Electricity Generation - Low Carbon, Nuclear and Fossil Fuels", 
                 value = "ElecGenLCFF",
                 ElecGenLCFFOutput("ElecGenLCFF")),
        tabPanel(title ="Electricity Generation by Fuel", 
                 value = "ElecGenFuel",
                 ElecGenByFuelOutput("ElecGenByFuel")),
        tabPanel(title ="Electricity Consumption by Fuel", 
                 value = "ElecConsumptionFuel",
                 ElecConsumpByFuelOutput("ElecConsumpByFuel")),
        tabPanel(title ="Renewable Electricity by Fuel", 
                 value = "RenElecFuel",
                 RenElecFuelOutput("RenElecFuel")),
        tabPanel(title ="Scottish Renewables Generation", 
                 value = "ScotRenGen",
                 ScotRenGenOutput("ScotRenGen")),
        tabPanel(title ="Renewable Electricity by Local Authority", 
                 value = "RenElecLA",
                 ElecLAOutput("ElecLA")),
        tabPanel(title ="Wind and Hydro Generation - EU", 
                 value = "WindHydroEU",
                 EUWindHydroOutput("EUWindHydro")),
        tabPanel(title ="Renewable Electricity Capacity", 
                 value = "RenElecCapacity",
                 RenElecCapOutput("RenElecCap")),
        tabPanel(title ="Renewable Electricity Pipeline", 
                 value = "RenElecPipeline",
                 RenElecPipelineOutput("RenElecPipeline"))
        )),
      tabPanel(
        value = "RenHeat",
        title = "Heat",
        navlistPanel(id = "RenHeat",
          widths = c(3, 8),
          tabPanel(title ="Renewable Heat Target", 
                   value = "RenHeat",
                   RenHeatTgtOutput("RenHeat")),
          tabPanel(title ="Renewable Heat EU", 
                   value = "RenHeatEU",
                   RenHeatEUOutput("RenHeatEU")),
          tabPanel(title ="Renewable Heat by Tech Type", 
                   value = "RenHeatTech",
                   RenHeatTechOutput("RenHeatTech")),
          tabPanel(title ="Domestic RHI", 
                   value = "DomesticRHI",
                   DomesticRHIOutput("DomesticRHI")),
          tabPanel(title ="Non-domestic RHI", 
                   value = "NonDomRHI",
                   NonDomesticRHIOutput("NonDomesticRHI"))
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
        value = "Emissions",
        title = "Emissions",
        navlistPanel(id = "Emissions",
                     widths = c(3, 8),
                     tabPanel(title ="Scottish Greenhouse Gas Emissions", 
                              value = "GHGEmissions",
                              GHGEmissionsOutput("GHGEmissions")),
                     tabPanel(title ="Energy Supply Emissions", 
                              value = "EnSupplyEmissions",
                              SupplyEmissionsOutput("SupplyEmissions")),
                     tabPanel(title ="Adjusted Emissions", 
                              value = "AdjustedEmissions",
                              AdjustedEmissionsOutput("AdjustedEmissions")),
                     tabPanel(title ="Emissions displaced by Renewables", 
                              value = "DisplacedEmissions",
                              EmissionsDisplacedOutput("EmissionsDisplaced")),
                     tabPanel(title ="Grid Emissions", 
                              value = "GridEmissions",
                              GridEmissionsOutput("GridEmissions")),
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
                            CommunityRenewablesOutput("CommunityRenewables")),
                   tabPanel(title = "CHPStats",
                            value = "CHPStats",
                            CHPOutput("CHP")),
                   tabPanel(title = "District Heat Networks",
                            value = "DistrictHeat",
                            HeatNetworkOutput("HeatNetwork"))
    )),
    ###### Section - Energy Efficiency #######
    tabPanel(
      value = "EnergyEfficiency",
      title = tags$div(img(src = "EEIcon.svg", height = "30px",   display= "block"), " Energy Efficiency" , style = "font-family: 'Century Gothic'; font-weight: 400 "),
      navlistPanel(id = "EnergyEfficiency",
                   widths = c(3, 8),
                   tabPanel(title = "Energy Consumption Target",
                            value = "EnConsumptionTgt",
                            EnConsumptionTgtOutput("EnConsumptionTgt")),
                   tabPanel(title = "Energy Consumption EU",
                            value = "EnConsumptionEU",
                            EnergyConsumptionEUOutput("EnergyConsumptionEU")),
                   tabPanel(title = "Energy Consumption by Sector",
                            value = "EnConsumptionSector",
                            EnComsumpSectorOutput("EnComsumpSector")),
                   tabPanel(title = "Energy Consumption by Fuel",
                            value = "EnConsumptionFuel",
                            EnConsumpFuelOutput("EnConsumpFuel")),
                   tabPanel(title = "Electricity Consumption ",
                            value = "ElecConsumption",
                            ElecConsumpOutput("ElecConsump")),
                   tabPanel(title = "Electricty Consumption - Households",
                            value = "EnConsumptionHHold",
                            ElecConsumpHholdOutput("ElecConsumpHhold")),
                   tabPanel(title = "Electricity Consumption - Local Authorities",
                            value = "EConsumptionLA",
                            ElecConsumptionLAOutput("ElecConsumptionLA")),
                   tabPanel(title = "Heat Consumption",
                            value = "HeatConsumption",
                            HeatConsumpOutput("HeatConsump")),
                   tabPanel(title = "Gas Consumption",
                            value = "GasConsumption",
                            GasConsumpOutput("GasConsump")),
                   tabPanel(title = "GasConsumption - Households",
                            value = "GasConsumptionHHold",
                            GasConsumpHholdOutput("GasConsumpHhold")),
                   tabPanel(title = "Gas Consumption - LA",
                            value = "GasConsumptionLA",
                            GasConsumptionLAOutput("GasConsumptionLA")),
                   tabPanel(title = "Household Energy Consumption",
                            value = "HHoldEnConsumption",
                            HouseholdEnergyConsumptionOutput("HouseholdEnergyConsumption")),
                   tabPanel(title = "Transport Energy Consumption",
                            value = "TransportEnConsumption",
                            TransportConsumpFuelOutput("TransportConsumpFuel")),
                   tabPanel(title = "Domestic EPCs",
                            value = "DomEPCs",
                            DomEPCOutput("DomEPC")),
                   tabPanel(title = "Wall Insulation",
                            value = "WallInsulation",
                            WallInsulationOutput("WallInsulation")),
                   tabPanel(title = "Loft Insulation",
                            value = "LoftInsulation",
                            LoftInsulationOutput("LoftInsulation")),
                   tabPanel(title = "Boilers",
                            value = "Boilers",
                            BoilerImprovementsOutput("BoilerImprovements")),
                   tabPanel(title = "Non-domestic EPCs",
                            value = "NonDomEPCs",
                            NonDomEPCOutput("NonDomEPC"))
    )),
    ###### Section - Consumer Engagement and Protection #######
    tabPanel(
      value = "ConsumerEngagement",
      title = tags$div(img(src = "ConsumerIcon.svg", height = "30px",   display= "block"), " Consumer Engagement", style = "font-family: 'Century Gothic'; font-weight: 400 "),
      navlistPanel(id = "ConsumerEngagement",
                   widths = c(3, 8),
                   tabPanel(title = "Electricity Bill Prices",
                            value = "ElecBillPrices",
                            ElecBillPricesOutput("ElecBillPrices")),
                   tabPanel(title = "Energy Bill Payment Methods",
                            value = "BillPayment",
                            BillPaymentsOutput("BillPayments")),
                   tabPanel(title = "Gas Bill Prices",
                            value = "GasBillPrices",
                            GasBillPricesOutput("GasBillPrices")),
                   tabPanel(title = "Dual Fuel Bill Breakdown",
                            value = "DualFuelBreakdown",
                            DualFuelBreakdownOutput("DualFuelBreakdown")),
                   tabPanel(title = "EU Bill Prices",
                            value = "EUBill",
                            EUBillOutput("EUBill")),
                   tabPanel(title = "Electricity Customers on Non-home Supplier",
                            value = "ElecNonHome",
                            ElecNonHomeOutput("ElecNonHome")),
                   tabPanel(title = "Gas Customers on Non-home Supplier",
                            value = "GasNonHome",
                            GasNonHomeOutput("GasNonHome")),
                   tabPanel(title = "Fuel Poverty",
                            value = "FuelPoverty",
                            FuelPovertyOutput("FuelPoverty")),
                   tabPanel(title = "Primary Heating Fuel",
                            value = "PrimaryHeating",
                            PrimaryHeatingOutput("PrimaryHeating")),
                   tabPanel(title = "Households not on the Gas Grid",
                            value = "NonGasGrid",
                            NonGasGridOutput("NonGasGrid")),
                   tabPanel(title = "Energy Use Monitoring",
                            value = "EnergyUseMonitor",
                            EnergyUseMonitorOutput("EnergyUseMonitor")),
                   tabPanel(title = "Households with Energy Use Monitors",
                            value = "HHoldEnMonitor",
                            HHoldEnMonitorOutput("HHoldEnMonitor")),
                   tabPanel(title = "Smart Meter Installations",
                            value = "SmartMeters",
                            SmartMetersOutput("SmartMeters")),
                   tabPanel(title = "Energy Supplier Switching",
                            value = "EnSupplySwitch",
                            EnSupplySwitchOutput("EnSupplySwitch"))
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
                   tabPanel(title = "Electricity Generation",
                            value = "ElecGeneration",
                            ElecGenOutput("ElecGen")),
                   tabPanel(title = "Peak Electricity and Gas Demand days",
                            value = "PeakElecGas",
                            PeakDayOutput("PeakDay")),
                   tabPanel(title = "Electricity Imports and Exports",
                            value = "ElecImportsExports",
                            ImportsExportsOutput("ImportsExports")),
                   tabPanel(title = "Scottish Generation Meeting Demand",
                            value = "ScotGenDemand", 
                            ScotOwnGenOutput("ScotOwnGen")),
                   tabPanel(title = "Gas Security",
                            value = "GasSecurity",
                            GasSecurityOutput("GasSecurity")),
                   tabPanel(title = "Electricity Storage",
                            value = "ElecStorage")
    )),
    ###### Section - System Security and Flexibility #######
    tabPanel(value = "OilGas",
      title = tags$div(img(src = "OilGasIcon.svg", height = "30px",   display= "block"), " Oil & Gas", style = "font-family: 'Century Gothic'; font-weight: 400 "),
      navlistPanel(id = "OilGas",
                   widths = c(3, 8),
                   tabPanel(title = "Primary Energy - Oil and Gas",
                            value = "PrimaryOilGas"),
                   tabPanel(title = "Scottish Oil and Gas Production",
                            value = "ScotOilGas",
                            OilGasProdOutput("OilGasProd")),
                   tabPanel(title = "Oil and Gas Outputs",
                            value = "OilGasOutputs"),
                   tabPanel(title = "Oil and Gas Consumption",
                            value = "OilGasConsumption"),
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
                            value = "OilGasEmployment"),
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
               # FOOTER - ABOUT
               column(
                 width = 4,
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
               # FOOTER - COPYRIGHT NOTICE
               column(
                 width = 4,
                 icon("copyright", lib = "font-awesome"),
                 strong("COPYRIGHT NOTICE"),
                 p(
                   "You may use or re-use this information (not including logos) free of charge in any format or medium, under the terms of the ",
                   a("Open Government Licence", href = "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"),
                   "."
                 )
               ),
               # FOOTER - CONTACT DETAILS
               column(
                 width = 4,
                 icon("at", lib = "font-awesome"),
                 strong("CONTACT DETAILS"),
                 p("Please send any feedback or questions to our email address."),
                 HTML('<a href="mailto:energystatistics@gov.scot?">energystatistics@gov.scot</a>')
               ),
               # FOOTER - EXTERNAL LINKS
               
             )
           ),
  titlePanel("", windowTitle = "Scottish Energy Statistics")
)) # Navbar page ends here) ###### UI End ######


enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)