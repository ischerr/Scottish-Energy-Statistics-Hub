Overview <- read_excel(
  "Structure/CurrentWorking.xlsx",
  sheet = "Renewable energy target",
  col_names = FALSE,
  skip = 21,
  n_max = 23
)

Overview <- as_tibble(t(Overview))

names(Overview) <- unlist(Overview[1,])

Overview <- Overview[-1,]

Overview %<>% lapply(function(x)
  as.numeric(as.character(x)))

names(Overview)[1] <- "Year"

Overview[c(2,7,8,13,14,19,20)] <- NULL

ElectricityTarget <- Overview[1:5]

ElectricityTarget <- as_tibble(ElectricityTarget)

names(ElectricityTarget) <- c(
  "Year",
  "Renewable Generation (GWh)",
  "Gross Consumption (GWh)",
  "Renewable % of consumption",
  "% of all energy consumption"
  )

datatable(
  ElectricityTarget,
  extensions = 'Buttons',
  
  rownames = FALSE,
  options = list(
    paging = TRUE,
    pageLength = -1,
    searching = TRUE,
    fixedColumns = FALSE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'ltBp',
    buttons = list(
      list(extend = 'copy'),
      list(
        extend = 'excel',
        title = 'Electricity Summary',
        header = TRUE
      ),
      list(extend = 'csv',
           title = 'Electricity Summary')
    ),
    
    # customize the length menu
    lengthMenu = list( c(10, 20, -1) # declare values
                       , c(10, 20, "All") # declare titles
    ), # end of lengthMenu customization
    pageLength = 10
  )
) %>%
  formatRound(2:ncol(ElectricityTarget), 0) %>% 
  formatPercentage(c(4,5), 1) %>% 
  formatStyle(c(5), fontStyle = "italic")

HeatTarget <- as_tibble(Overview[c(1, 6:9)])


names(HeatTarget) <- c(
  "Year",
  "Renewable Heat (GWh)",
  "Heat Demand (non-electrical, GWh)",
  "% Renewable Heat",
  "% of all energy consumption"
)

datatable(
  HeatTarget,
  extensions = 'Buttons',
  
  rownames = FALSE,
  options = list(
    paging = TRUE,
    pageLength = -1,
    searching = TRUE,
    fixedColumns = FALSE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'ltBp',
    buttons = list(
      list(extend = 'copy'),
      list(
        extend = 'excel',
        title = 'Heat Summary',
        header = TRUE
      ),
      list(extend = 'csv',
           title = 'Heat Summary')
    ),
    
    # customize the length menu
    lengthMenu = list( c(10, 20, -1) # declare values
                       , c(10, 20, "All") # declare titles
    ), # end of lengthMenu customization
    pageLength = 10
  )
) %>%
  formatRound(2:ncol(HeatTarget), 0) %>% 
  formatPercentage(c(4,5), 1) %>% 
  formatStyle(c(5), fontStyle = "italic")


TransportTarget <- as_tibble(Overview[c(1, 10:13)])

names(TransportTarget) <- c(
  "Year",
  "Biofuels in Scotland estimate (GWh)",
  "Petroleum used for Transport (GWh)",
  "UK % of biofuels",
  "% of all energy consumption"
)

datatable(
  TransportTarget,
  extensions = 'Buttons',
  
  rownames = FALSE,
  options = list(
    paging = TRUE,
    pageLength = -1,
    searching = TRUE,
    fixedColumns = FALSE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'ltBp',
    buttons = list(
      list(extend = 'copy'),
      list(
        extend = 'excel',
        title = 'Transport Summary',
        header = TRUE
      ),
      list(extend = 'csv',
           title = 'Transport Summary')
    ),
    
    # customize the length menu
    lengthMenu = list( c(10, 20, -1) # declare values
                       , c(10, 20, "All") # declare titles
    ), # end of lengthMenu customization
    pageLength = 10
  )
) %>%
  formatRound(2:ncol(TransportTarget), 0) %>% 
  formatPercentage(c(4,5), 1) %>% 
  formatStyle(c(5), fontStyle = "italic")



TotalTarget <- as_tibble(Overview[c(1, 14:16)])

names(TotalTarget) <- c(
  "Year",
  "Total Renewable Energy (GWh)",
  "Total Energy Consumption (GWh)",
  "% of all energy consumption"
)


datatable(
  TotalTarget,
  extensions = 'Buttons',
  
  rownames = FALSE,
  options = list(
    paging = TRUE,
    pageLength = -1,
    searching = TRUE,
    fixedColumns = FALSE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'ltBp',
    buttons = list(
      list(extend = 'copy'),
      list(
        extend = 'excel',
        title = 'Total Renewables Summary',
        header = TRUE
      ),
      list(extend = 'csv',
           title = 'Total Renewables Summary')
    ),
    
    # customize the length menu
    lengthMenu = list( c(10, 20, -1) # declare values
                       , c(10, 20, "All") # declare titles
    ), # end of lengthMenu customization
    pageLength = 10
  )
) %>%
  formatRound(2:ncol(TotalTarget), 0) %>% 
  formatPercentage(c(4), 1) %>% 
  formatStyle(c(4), fontStyle = "italic")




