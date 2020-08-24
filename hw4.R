getwd()
setwd('/Users/Katherine/R')
library(shiny)
library(openxlsx)
library(ggplot2)
library(DMwR)
library(ggthemes)
library(quantmod)

COVIDdata <- read.csv('COVID-19_1.csv')
COVIDdata$date <- as.Date(COVIDdata$date)

#######1#######

ui1 <- fluidPage(
  titlePanel('COVID-19 cases v.s. Time'),
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      plotOutput(outputId = "Plot"),
    )
  )
)

server1 <- function(input, output){
  output$Plot <- renderPlot({
    ggplot(data =COVIDdata, aes(x = date, y = positiveIncrease, group=1))+ geom_line()+ scale_x_date(date_breaks = "30 day")+ theme_grey()
    })
}

shinyApp(ui1, server1)

#######2#######

getSymbols("AAPL", warnings=F, from='2020-02-01',to='2020-06-01')

ui2 <- fluidPage(
  titlePanel('stockprice v.s. Time'),
  sidebarLayout(
    
    sidebarPanel(
    ),
    mainPanel(
      plotOutput(outputId = "Plot"),
    )
  )
)

server2 <- function(input, output){
  output$Plot <- renderPlot({
    chartSeries(AAPL, theme = chartTheme("white"), type = "line", TA = NULL)
  })
}

shinyApp(ui2, server2)


######3######

stock <- read.xlsx('stockid.xlsx')

stockdata <- data.frame(date=1:82)
stockdata[, 1] <- read.xlsx('time.xlsx')

covid19_data_raw <- read.csv('COVID-19.csv', header = TRUE)
covid19_data <- covid19_data_raw[1:183,c(1, 20, 23)]



for(i in 2:5){
  setSymbolLookup(TEMP=list(name=stock$id[i]))
  getSymbols("TEMP", warnings=F, from='2020-02-01',to='2020-06-01')
  TEMP <- as.data.frame(TEMP)
  stockdata[, i] <- TEMP[, 2]
  colnames(stockdata)[i] <- substring(colnames(TEMP)[1], 1, nchar(colnames(TEMP)[1])-5)
}

stockdata <- as.data.frame(stockdata)
stockdata_2 <- stockdata
stockdata_2$deathIncrease <- c(1:82)
stockdata_2$positiveIncrease <- c(1:82)


for(i in 1:nrow(stockdata_2)){
  date1 <- stockdata_2[i, 1]
  for(j in 1:nrow(covid19_data)){
    date2 <- covid19_data[j, 1]
    if(date1 == date2){
      stockdata_2$deathIncrease[i] <- covid19_data$deathIncrease[j]
      stockdata_2$positiveIncrease[i] <- covid19_data$positiveIncrease[j]
    }
  }
}

ui3 <- fluidPage(
  titlePanel('COVID-19 cases v.s. stockprice'),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "COVID",
                  label = "Choose a COVID-19 data:",
                  choices = c("DeathIncrease", "PositiveIncrease")),
      
      selectInput(inputId = "stock",
                  label = "Choose a stock:",
                  choices = c("AAPL", "MSFT", "AMZN", "GOOG"))
      
    ),
    mainPanel(
      plotOutput(outputId = "Plot"),
    )
  )
)

server3 <- function(input, output){
  
  covid <- reactive({
    switch(input$COVID, "DeathIncrease" = 6, "PositiveIncrease" = 7)
  })
  
  stockthis <- reactive({
    switch(input$stock, "AAPL" = 2, "MSFT" = 3, "AMZN" =4, "GOOG"=5)
  })
  
  
  thisdat <- reactive({
    this <- data.frame(date=stockdata_2[,1], covidthis= stockdata_2[,covid()], stockthis=stockdata_2[,stockthis()])
    return(this)
  })
  
  output$Plot <- renderPlot({
    ggplot(data =thisdat(), aes(x = stockthis , y = covidthis))+ geom_line()+ theme_grey()+xlab(input$stock)+ylab(input$COVID)
  })
}

shinyApp(ui3, server3)

 
