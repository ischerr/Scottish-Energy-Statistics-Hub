setwd("J:/ENERGY BRANCH/Statistics/R Shiny/Reorganised")

rsconnect::setAccountInfo(name='ischerr', token='9739438703127EB0D7884424EC7E0483', secret='1mmueOj5gt2HsvEJsJUeyEK+Ik5AyAwRuUHNwjiM')

rsconnect::deployApp(appName = "sg-scottish-energy-statistics",
                     account = "ischerr")

