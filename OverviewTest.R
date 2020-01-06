Overview <- read_excel(
  "Structure/CurrentWorking.xlsx",
  sheet = "Renewable energy target",
  col_names = TRUE,
  skip = 21,
  n_max = 23
)

Overview[2:15]%<>% lapply(function(x)
  as.numeric(as.character(x)))

datatable(
  Overview,
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
        title = 'Overview',
        header = TRUE
      ),
      list(extend = 'csv',
           title = 'Overview')
    ),
    
    # customize the length menu
    lengthMenu = list( c(10, 20, -1) # declare values
                       , c(10, 20, "All") # declare titles
    ), # end of lengthMenu customization
    pageLength = 10
  )
) 

})