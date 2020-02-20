library(rsconnect)

source("packages.R")
# Set the account info for deployment.
# setAccountInfo(name   = Sys.getenv("shinyapps_name"),
#                token  = Sys.getenv("shinyapps_token"),
#                secret = Sys.getenv("shinyapps_secret"))
# 
# # Deploy the application.
# deployApp(appName = "Reorganised",
#           account = Sys.getenv("shinyapps_name"),
#           lint = TRUE)


setAccountInfo(name   = Sys.getenv("shinyapps_name2"),
               token  = Sys.getenv("shinyapps_token2"),
               secret = Sys.getenv("shinyapps_secret2"))

deployApp(appName = "Energy",
          account = Sys.getenv("shinyapps_name2"),
          lint = TRUE)

deployApp(appName = "sg-scottish-energy-statistics",
          account = Sys.getenv("shinyapps_name2"),
          lint = TRUE)

