library(rsconnect)

source("packages.R")
# Set the account info for deployment.
setAccountInfo(name   = Sys.getenv("shinyapps_name"),
               token  = Sys.getenv("shinyapps_token"),
               secret = Sys.getenv("shinyapps_secret"))

# Deploy the application to the development server
deployApp(appName = "Reorganised",
          account = Sys.getenv("shinyapps_name"),
          lint = TRUE)

