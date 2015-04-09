## app.R ##
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(survival)

# change the path to db to match your path, all other code should not 
# need to be changed
db <- src_sqlite("/home/jacob/documents/blog/sms/data/sms.db")

### NO CHANGES REQUIRED BELOW THIS LINE ###
messages <- tbl(db, sql("SELECT name, type, time FROM texts"))
contacts <- group_by(messages, name) %>% 
  summarize(n = n())
contacts <- filter(contacts, n >= 50) %>% 
  arrange(desc(n))
contacts <- collect(contacts)$name

header <- dashboardHeader(title = "Quantified Friends")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(selectInput("contact",
                label = "Friend's Name?",
                choices = contacts,
                selected = contacts[1])),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Are we friends (statistically)?", tabName = "survAnalysis", 
             icon = icon("comment-o")),
    menuItem("When do we talk?", tabName = "timing",
             icon = icon("clock-o")),
    menuItem("Source code", icon = icon("github"), 
             href = "https://github.com/rstudio/shinydashboard/",
             newtab = TRUE))
)


body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              infoBoxOutput("days", width = 3),
              infoBoxOutput("nSent", width = 3),
              infoBoxOutput("nReceived", width = 3),
              infoBoxOutput("balance", width = 3)
            ),
            fluidRow(
              tabBox(
                selected = "Time Series",
                tabPanel("Time Series", plotOutput("tsPlot")),
                tabPanel("Smoother", plotOutput("tsPlotSmooth")),
                width = 12)
            ),
            fluidRow(
              box(plotOutput("senderPlot"), width = 12)
            )
    ),
    # Second tab content
    tabItem(tabName = "survAnalysis",
            fluidRow(
              box(plotOutput("ttePlotByDirection"), width = 8),
              infoBoxOutput("mteMe", width = 4),
              infoBoxOutput("mteThem", width = 4),
              infoBoxOutput("mteToAnyone", width = 4),
              infoBoxOutput("mteFromAnyone", width = 4)
              #infoBoxOutput("nSent", width = 3),
              #infoBoxOutput("nReceived", width = 3)
            )
    ),
    # Third tab content
    tabItem(tabName = "timing",
            fluidRow(
              infoBoxOutput("mostCommonDay", width = 3),
              infoBoxOutput("leastCommonDay", width = 3),
              infoBoxOutput("mostCommonHour", width = 3),
              infoBoxOutput("leastCommonHour", width = 3)
            ),
            fluidRow(
              box(plotOutput("plotByDay")),
              box(plotOutput("plotByHour")))
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  output$plotByDay <- renderPlot({
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$wday <- time[["wday"]]
    messages$wday <- ifelse(messages$wday == 0,
                            "Sunday", 
                            ifelse(messages$wday == 1, 
                                   "Monday",
                                   ifelse(messages$wday == 2, 
                                          "Tuesday",
                                          ifelse(messages$wday == 3,
                                                 "Wednesday",
                                                 ifelse(messages$wday == 4,
                                                        "Thursday",
                                                        ifelse(messages$wday == 5,
                                                               "Friday",
                                                               "Saturday"))))))
    
    days <- group_by(messages, wday, type) %>% summarize(n = n())
    days$wday <- factor(days$wday, levels = c("Sunday", "Monday", "Tuesday",
                                              "Wednesday", "Thursday", "Friday",
                                              "Saturday"))
    days$sender <- ifelse(days$type == 1, "Me", "Friend")
    ggplot(days, aes(x = wday, y = n, fill = sender)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete("Day of Week") + 
      scale_y_continuous("Number of Messages") + 
      facet_grid(sender ~ .)
  })
  
  output$plotByHour <- renderPlot({
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$time <- time[["hour"]] + time[["min"]] / 60 + time[["sec"]] / 3600
    messages$timeR <- round(messages$time, 1)
    hours <- group_by(messages, timeR, type) %>% summarize(n = n())
    hours$sender <- ifelse(hours$type == 1, "Me", "Friend")
    ggplot(hours, aes(x = timeR, y = n, color = sender)) + 
      geom_line() +
      facet_grid(sender ~ .) + 
      scale_x_continuous("Time of Day", limits = c(0, 23.9)) + 
      scale_y_continuous("Number of Messages / 6 Minutes")
  })
  
  output$mostCommonDay <- renderInfoBox({
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$wday <- time[["wday"]]
    messages$wday <- ifelse(messages$wday == 0,
                            "Sunday", 
                            ifelse(messages$wday == 1, 
                                   "Monday",
                                   ifelse(messages$wday == 2, 
                                          "Tuesday",
                                          ifelse(messages$wday == 3,
                                                 "Wednesday",
                                                 ifelse(messages$wday == 4,
                                                        "Thursday",
                                                        ifelse(messages$wday == 5,
                                                               "Friday",
                                                               "Saturday"))))))

    days <- group_by(messages, wday) %>% summarize(n = n())
    day <- days$wday[which.max(days$n)]
    infoBox(
      "Most Common Day", day, icon = icon("calendar")
    )
  })
  
  output$leastCommonDay <- renderInfoBox({
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$wday <- time[["wday"]]
    messages$wday <- ifelse(messages$wday == 0,
                            "Sunday", 
                            ifelse(messages$wday == 1, 
                                   "Monday",
                                   ifelse(messages$wday == 2, 
                                          "Tuesday",
                                          ifelse(messages$wday == 3,
                                                 "Wednesday",
                                                 ifelse(messages$wday == 4,
                                                        "Thursday",
                                                        ifelse(messages$wday == 5,
                                                               "Friday",
                                                               "Saturday"))))))
    
    days <- group_by(messages, wday) %>% summarize(n = n())
    day <- days$wday[which.min(days$n)]
    infoBox(
      "Least Common Day", day, icon = icon("calendar")
    )
  })
  
  output$mostCommonHour <- renderInfoBox({
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$hour <- time[["hour"]]
    hours <- group_by(messages, hour) %>% summarize(n = n())
    hour <- hours$hour[which.max(hours$n)]
    amPm = "am"
    if (hour == 0) {
      hour = 12
    } else if (hour >= 12) {
      amPm == "pm"
      if (hour != 12) {
        hour = hour - 12
      }
    }
    infoBox(
      "Most Common Hour", paste0(hour, " ", amPm), icon = icon("clock-o"),
      color = "green"
    )
  })
  
  output$leastCommonHour <- renderInfoBox({
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$hour <- time[["hour"]]
    hours <- group_by(messages, hour) %>% summarize(n = n())
    hour <- hours$hour[which.min(hours$n)]
    amPm = "am"
    if (hour == 0) {
      hour = 12
    } else if (hour >= 12) {
      amPm == "pm"
      if (hour != 12) {
        hour = hour - 12
      }
    }
    infoBox(
      "Least Common Hour", paste0(hour, " ", amPm), icon = icon("clock-o"),
      color = "green"
    )
  })
  
  output$tsPlot <- renderPlot({
    messages <- tbl(db, sql("SELECT name, type, time FROM texts"))
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$month <- time[["mon"]] + 1
    messages$year <- time[["year"]] + 1900
    messages$day <- time[["mday"]]
    messages$week <- floor((time[["yday"]] / 7)) + 1
    mts <- group_by(messages, year, week) %>% 
      summarize(n = n())
    mts$date <- (mts$week - 1) / 52 + mts$year
    ggplot(mts, aes(x = date, y = n)) + 
      geom_line() + 
      geom_point() +
      scale_x_continuous("") + 
      scale_y_continuous("Pooled Message Volume / Week")
  })
  
  output$tsPlotSmooth <- renderPlot({
    messages <- tbl(db, sql("SELECT name, type, time FROM texts"))
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$month <- time[["mon"]] + 1
    messages$year <- time[["year"]] + 1900
    messages$day <- time[["mday"]]
    messages$week <- floor((time[["yday"]] / 7)) + 1
    mts <- group_by(messages, year, week) %>% 
      summarize(n = n())
    mts$date <- (mts$week - 1) / 52 + mts$year
    ggplot(mts, aes(x = date, y = n)) + 
      geom_line() + 
      geom_point() +
      scale_x_continuous("") + 
      scale_y_continuous("Pooled Message Volume / Week") + 
      geom_smooth()
  })
  
  output$senderPlot <- renderPlot({
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$month <- time[["mon"]] + 1
    messages$year <- time[["year"]] + 1900
    messages$day <- time[["mday"]]
    messages$week <- floor((time[["yday"]] / 7)) + 1
    mts <- group_by(messages, type, year, week) %>% 
      summarize(n = n())
    mts$sender <- ifelse(mts$type == 1, "Me", "Friend")
    mts$date <- (mts$week - 1) / 52 + mts$year
    ggplot(mts, aes(x = date, y = n, color = sender)) + 
      geom_line() + 
      geom_point() +
      scale_x_continuous("") + 
      scale_y_continuous("Message Volume / Week")
  })
  
  output$days <- renderInfoBox({
    messages <- collect(filter(messages, name == input$contact))
    time <- as.POSIXlt(messages$time, origin = "1970-01-01")
    messages$month <- time[["mon"]] + 1
    messages$year <- time[["year"]] + 1900
    messages$day <- time[["mday"]]
    days <- group_by(messages, month, day, year) %>% summarize(n = n())
    infoBox(
      "Days of Contact", nrow(days), icon = icon("calendar"),
      color = "purple"
    )
  })
  
  output$nSent <- renderInfoBox({
    messages <- collect(filter(messages, name == input$contact))
    n <- sum(messages$type)
    infoBox(
      "Texts Sent", n, icon = icon("send"),
      color = "green"
    )
  })
  
  output$nReceived <- renderInfoBox({
    messages <- collect(filter(messages, name == input$contact))
    n <- sum(messages$type == 0)
    infoBox(
      "Texts Received", n, icon = icon("reply"),
      color = "blue"
    )
  })
  
  output$balance <- renderInfoBox({
    messages <- collect(filter(messages, name == input$contact))
    n <- round(sum(messages$type == 1) / nrow(messages) * 100)
    balanceColor <- ifelse(n <= 40, "red", 
                           ifelse(n <= 45, "yellow",
                                  ifelse(n <= 55, "green",
                                         ifelse(n <= 60, "yellow", "red"))))
    balanceIcon = ifelse(n <= 40 | n > 60, "thumbs-down", "thumbs-up")
    infoBox(
      "Percent Sent", paste0(n, "%"), icon = icon(balanceIcon),
      color = balanceColor
    )
  })
  
  output$mteMe <- renderInfoBox({
    messages <- arrange(messages, name, time)
    messages <- filter(messages, name != "(Unknown)")
    messages <- collect(messages)
    messages$delta <- messages$time - c(NA, messages$time[1:(nrow(messages)-1)])
    messages$delta <- ifelse(messages$name == 
                               c(NA, messages$name[1:(nrow(messages)-1)]), 
                             messages$delta,
                             NA)
    messages$reply <- !is.na(messages$delta)
    messages$SurvObj <- Surv(messages$delta, messages$reply == 1)
    sent <- summary(survfit(SurvObj ~ name, data = messages[messages$type == 1, ], 
                    conf.type = "log-log"))
    dat <- data.frame(time = 
                         sent$time[grepl(input$contact, sent$strata)],
                       surv = 
                         sent$surv[grepl(input$contact, sent$strata)])
    mTime <- dat$time[which.min((dat$surv - 0.5)^2)]
    timeColor <- ifelse(mTime <= 600, "green", 
                        ifelse(mTime <= 3600, "yellow", "red"))
    infoBox(
      "Median Time to Reply (Me)", paste0(round(mTime/60, 1), " minutes"), 
      icon = icon("clock-o"),
      color = timeColor)
  })
  
  output$mteThem <- renderInfoBox({
    messages <- arrange(messages, name, time)
    messages <- filter(messages, name != "(Unknown)")
    messages <- collect(messages)
    messages$delta <- messages$time - c(NA, messages$time[1:(nrow(messages)-1)])
    messages$delta <- ifelse(messages$name == 
                               c(NA, messages$name[1:(nrow(messages)-1)]), 
                             messages$delta,
                             NA)
    messages$reply <- !is.na(messages$delta)
    messages$SurvObj <- Surv(messages$delta, messages$reply == 1)
    rec <- summary(survfit(SurvObj ~ name, data = messages[messages$type == 0, ], 
                            conf.type = "log-log"))
    dat <- data.frame(time = 
                        rec$time[grepl(input$contact, rec$strata)],
                      surv = 
                        rec$surv[grepl(input$contact, rec$strata)])
    mTime <- dat$time[which.min((dat$surv - 0.5)^2)]
    timeColor <- ifelse(mTime <= 600, "green", 
                        ifelse(mTime <= 3600, "yellow", "red"))
    infoBox(
      "Median Time to Reply (Friend)", paste0(round(mTime/60, 1), " minutes"), 
      icon = icon("clock-o"),
      color = timeColor)
  })
  
  output$mteToAnyone <- renderInfoBox({
    messages <- arrange(messages, name, time)
    messages <- filter(messages, name != "(Unknown)")
    messages <- collect(messages)
    messages$delta <- messages$time - c(NA, messages$time[1:(nrow(messages)-1)])
    messages$delta <- ifelse(messages$name == 
                               c(NA, messages$name[1:(nrow(messages)-1)]), 
                             messages$delta,
                             NA)
    messages$reply <- !is.na(messages$delta)
    messages$SurvObj <- Surv(messages$delta, messages$reply == 1)
    sent <- summary(survfit(SurvObj ~ name, data = messages[messages$type == 1, ], 
                            conf.type = "log-log"))
    dat <- data.frame(time = 
                        sent$time[grepl(input$contact, sent$strata) == FALSE],
                      surv = 
                        sent$surv[grepl(input$contact, sent$strata) == FALSE])
    mTime <- dat$time[which.min((dat$surv - 0.5)^2)]
    timeColor <- ifelse(mTime <= 600, "green", 
                        ifelse(mTime <= 3600, "yellow", "red"))
    infoBox(
      "Typical Median Time to Reply (Me)", paste0(round(mTime/60, 1), " minutes"), 
      icon = icon("clock-o"),
      color = timeColor)
  })
  
  output$mteFromAnyone <- renderInfoBox({
    messages <- arrange(messages, name, time)
    messages <- filter(messages, name != "(Unknown)")
    messages <- collect(messages)
    messages$delta <- messages$time - c(NA, messages$time[1:(nrow(messages)-1)])
    messages$delta <- ifelse(messages$name == 
                               c(NA, messages$name[1:(nrow(messages)-1)]), 
                             messages$delta,
                             NA)
    messages$reply <- !is.na(messages$delta)
    messages$SurvObj <- Surv(messages$delta, messages$reply == 1)
    rec <- summary(survfit(SurvObj ~ name, data = messages[messages$type == 0, ], 
                            conf.type = "log-log"))
    dat <- data.frame(time = 
                        rec$time[grepl(input$contact, rec$strata) == FALSE],
                      surv = 
                        rec$surv[grepl(input$contact, rec$strata) == FALSE])
    mTime <- dat$time[which.min((dat$surv - 0.5)^2)]
    timeColor <- ifelse(mTime <= 600, "green", 
                        ifelse(mTime <= 3600, "yellow", "red"))
    infoBox(
      "Typical Median Time to Reply (Everyone)", paste0(round(mTime/60, 1), " minutes"), 
      icon = icon("clock-o"),
      color = timeColor)
  })
  
  output$ttePlotByDirection <- renderPlot({
    messages <- arrange(messages, name, time)
    messages <- filter(messages, name != "(Unknown)")
    messages <- collect(messages)
    messages$delta <- messages$time - c(NA, messages$time[1:(nrow(messages)-1)])
    messages$delta <- ifelse(messages$name == 
                               c(NA, messages$name[1:(nrow(messages)-1)]), 
                             messages$delta,
                             NA)
    messages$reply <- !is.na(messages$delta)
    messages$SurvObj <- Surv(messages$delta, messages$reply == 1)
    sent <- survfit(SurvObj ~ name, data = messages[messages$type == 1, ], 
                 conf.type = "log-log")
    sent <- summary(sent)
    sentTable <- data.frame(time = sent$time, nrisk = sent$n.risk, 
                        nevent = sent$n.event, 
                        surv = sent$surv, friend = sent$strata, 
                        survUpper = sent$upper, survLower = sent$lower,
                        sent = "Sent")
    rec <- survfit(SurvObj ~ name, data = messages[messages$type == 0, ], 
                    conf.type = "log-log")
    rec <- summary(rec)
    recTable <- data.frame(time = rec$time, nrisk = rec$n.risk, 
                           nevent = rec$n.event, 
                           surv = rec$surv, friend = rec$strata, 
                           survUpper = rec$upper, survLower = rec$lower,
                           sent = "Received")
    table <- rbind(sentTable, recTable)
    table$focus <- grepl(input$contact, table$friend, fixed = TRUE)
    table$time <- table$time / 60
    ggplot() + 
      geom_line(data = filter(table, focus == 0, sent == "Received"), 
                aes(x = time, y = surv, group = friend), 
                linetype = 1, alpha = 0.2) + 
      geom_line(data = filter(table, focus == 1, sent == "Received"), 
                aes(x = time, y = surv), 
                linetype = 1, color = "red", size = 1) + 
      geom_line(data = filter(table, focus == 0, sent == "Sent"), 
                aes(x = time, y = surv, group = friend), 
                linetype = 2, alpha = 0.2) + 
      geom_line(data = filter(table, focus == 1, sent == "Sent"), 
                aes(x = time, y = surv), 
                linetype = 2, color = "red", size = 1) + 
      scale_x_log10("Time / Minutes",
                    breaks = c(1, 5, 10, 15, 25, 50,
                               100, 200, 500, 1000, 5000))
  })
}

shinyApp(ui, server)
