library(readr)
library(ISOweek)
library(lubridate)
library(zoo)
library(plotly)

DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)

DailyDemand$Date <- ymd(DailyDemand$Date)

DailyDemand$Year <-year(DailyDemand$Date)

DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]

DailyDemand$Month <-month(DailyDemand$Date)

DailyDemand$Week <- isoweek(DailyDemand$Date)

DailyDemand$Weekday <- weekdays(DailyDemand$Date)

DailyDemand$DayofYear <- yday(DailyDemand$Date)

DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "PostLockdown", "BeforeLockdown")

DailyDemandFromMarch <- DailyDemand[which(DailyDemand$Week >= 10 & DailyDemand$Week <= 51),]

DailyDemandFromMarch <- DailyDemandFromMarch[c(5,6,7,9,1,8,4)]

DailyDemandFromMarch <- DailyDemandFromMarch %>% group_by(Year) %>% mutate(id = row_number())


DailyDemandFromMarch  <- dcast(DailyDemandFromMarch, id ~ Year, value.var = 'Electricity')

DailyDemandFromMarch$Date <- ymd("2020/03/01") + DailyDemandFromMarch$id

DailyDemandFromMarch <- DailyDemandFromMarch[complete.cases(DailyDemandFromMarch),]

vline1 <- function(x = 0, color = "#02818a") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )}
  
  vline2 <- function(x = 0, color = "#67a9cf") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color, dash = "dash")
    )
}


p2 <-  plot_ly(DailyDemandFromMarch,x = ~ Date ) %>% 
  add_trace(data = DailyDemandFromMarch,
            x = ~ Date,
            y = ~ `2020`,
            name = "2020 Exports",
            type = 'scatter',
            mode = 'lines',
            legendgroup = "1",
            text = paste0(
              "2020: \u00A3",
              round(DailyDemandFromMarch$`2020`, digits = 1),
              " GWh\nDate: ",
              DailyDemandFromMarch$Date
            ),
            hoverinfo = 'text'
  ) %>% 
  add_trace(data = DailyDemandFromMarch,
            x = ~ Date,
            y = ~ `2019`,
            name = "2019 Exports",
            type = 'scatter',
            mode = 'lines',
            legendgroup = "2",
            text = paste0(
              "2019: \u00A3",
              round(DailyDemandFromMarch$`2020`, digits = 1),
              " GWh\nDate: ",
              DailyDemandFromMarch$Date
            ),
            hoverinfo = 'text'
  )  %>% 
  add_annotations(
    x = dmy("13/03/2020"),
    y = 1,
    yref = "paper",
    text = "<b>20/03</b>\nClosure of pubs, gyms\netc.",
    font = list(color = "#67a9cf",
                family = "Century Gothic"),
    textposistion = "bottom right",
    showarrow = FALSE
  ) %>% 
  add_annotations(
    x = dmy("29/03/2020"),
    y = 1,
    yref = "paper",
    text = "<b>23/03</b>\nLockdown",
    font = list(color = "#02818a",
                family = "Century Gothic"),
    textposistion = "bottom right",
    showarrow = FALSE
  ) %>% 
  layout(
    barmode = 'stack',
    shapes = list(vline1(dmy("23/03/2020")), vline2(dmy("20/03/2020"))),
    bargap = 0.66,
    legend = list(font = list(color = "#126992"),
                  orientation = 'h'),
    hoverlabel = list(font = list(color = "white"),
                      hovername = 'text'),
    hovername = 'text',
    
    xaxis = list(title = "",
                 showgrid = FALSE),
    yaxis = list(
      title = "GWh",
      tickformat = "",
      showgrid = TRUE,
      zeroline = TRUE,
      zerolinecolor = ChartColours[1],
      zerolinewidth = 2,
      rangemode = "tozero"
    )
  ) %>% 
  config(displayModeBar = F)
p2
