library(pacman)
require("shiny")                # For the app iteslf.
require("shinythemes")          # For the "cerulean" theme.
require("shinyhelper")          # Used for help modal boxes.
require("shinyEffects")         # Used for home page effects.
require("shinyanimate")         # Used for home page effects.
require("shinycssloaders")      # For loaders.
require("shinyjs")              # Various UI effects
require("magrittr")             # Dependency of other packages. Lets use the pipe operator (%>%").
require("data.table")           # Needed for function na.omit(").
require("DT")                   # For the interactive tables.
require("dygraphs")             # For the interactive graphs.
require("leaflet")              # For interactive maps.
require("RColorBrewer")         # Used to create a colour palette for the map.
require("rgdal")                # Needed to load shapfile for the map.
require("plyr")                 # Needed for function revalue("), for editing country names.
require("rmarkdown")            # Used for reports, especially important is function render(").
require("knitr")                # Takes Rmd. file and turns it into .md document which includes the R code and its output.
require("networkD3")            # Used to create the sankey diagram.
require("treemap")              # Used to create the static treemap.
require("dplyr")                # Used for data manipulation.
require("ggplot2")              # Used for plots in country and sector profiles.
require("leaflet.minicharts")   # Used to superimpose piecharts on a leaflet map.
require("plotly")               # Used for the streamgraph.
require("ggsci")                # Used for colour palettes.
require("ggflags")              # Expands ggplot with new geom for adding flags.
require("countrycode")          # Enables conversion from Common country names to ISO codes.
require("knitr")                # Used for sector definitions table
require("kableExtra")           # Used for styling the sector definitions table
require("extrafont")            # Extra fonts
require("readtext")
require("htmlwidgets")
require("readxl")
require("gganimate")
require("gifski")
require("readr")
require("maptools")
require("tmaptools")
require("tmap")
require("sf")
require("leaflet")
require("rgeos")
require("shinyWidgets")
require("readxl")
require("lubridate")
require("ggrepel")
require("raceland")
require("freealg")
