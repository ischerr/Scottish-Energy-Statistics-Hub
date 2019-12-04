if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load("shiny")                # For the app iteslf.
p_load("shinythemes")          # For the "cerulean" theme.
p_load("shinyhelper")          # Used for help modal boxes.
p_load("shinyEffects")         # Used for home page effects.
p_load("shinyanimate")         # Used for home page effects.
p_load("shinycssloaders")      # For loaders.
p_load("shinyjs")              # Various UI effects
p_load("magrittr")             # Dependency of other packages. Lets use the pipe operator (%>%").
p_load("data.table")           # Needed for function na.omit(").
p_load("DT")                   # For the interactive tables.
p_load("dygraphs")             # For the interactive graphs.
p_load("leaflet")              # For interactive maps.
p_load("RColorBrewer")         # Used to create a colour palette for the map.
p_load("rgdal")                # Needed to load shapfile for the map.
p_load("plyr")                 # Needed for function revalue("), for editing country names.
p_load("rmarkdown")            # Used for reports, especially important is function render(").
p_load("knitr")                # Takes Rmd. file and turns it into .md document which includes the R code and its output.
p_load("networkD3")            # Used to create the sankey diagram.
p_load("treemap")              # Used to create the static treemap.
p_load("dplyr")                # Used for data manipulation.
p_load("ggplot2")              # Used for plots in country and sector profiles.
p_load("leaflet.minicharts")   # Used to superimpose piecharts on a leaflet map.
p_load("plotly")               # Used for the streamgraph.
p_load("ggsci")                # Used for colour palettes.
p_load("ggflags")              # Expands ggplot with new geom for adding flags.
p_load("countrycode")          # Enables conversion from Common country names to ISO codes.
p_load("knitr")                # Used for sector definitions table
p_load("kableExtra")           # Used for styling the sector definitions table
p_load("extrafont")            # Extra fonts
p_load("readtext")
p_load("htmlwidgets")
p_load("readxl")
p_load("gganimate")
p_load("gifski")
p_load("readr")
p_load("maptools")
p_load("tmaptools")
p_load("tmap")
p_load("sf")
p_load("leaflet")
p_load("rgeos")
p_load("shinyWidgets")
p_load("readxl")
p_load("lubridate")
p_load("ggrepel")
p_load("raceland")
