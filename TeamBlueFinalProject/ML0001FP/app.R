#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(corrgram)
library(GGally)
library(ggthemes) 
library(DMwR)
library(gridExtra)
library(rattle)
library(readxl)
library(cluster)
library(sqldf)
library(knitr)
library(xgboost)
library(outliers)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  headerPanel('Property Value Prediction Using Clustering and Linear Regression - Automated Valuation Model (AVM)'),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #Selector for file upload
      fileInput('datafile', 'Choose CSV file',
                accept=c('text/csv', 'text/comma-separated-values,text/plain'))   
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Purpose & Instructions", verbatimTextOutput("Purpose")),
        tabPanel(
          "Current Trend", 
          verbatimTextOutput("Trend") ,
          htmlOutput("text"),
          plotOutput("PriceTrend",height= "800px",width = "1000px"),
          tableOutput("view")
        ), 
        tabPanel("Predict Price",
                 sidebarLayout(
                   sidebarPanel(
                     div(
                       id = "form",
                       
                       selectInput("borough_type", "Borough",
                                   c("1",  "2", "3", "4")),
                       selectInput("NEIGHBORHOOD_type", "NEIGHBORHOOD",
                                   c("1",  "2", "3", "4")),
                       selectInput("building_category", "Building Category",
                                   c("1",  "2", "3", "4")),
                       selectInput("tax_class", "Tax Class",
                                   c("1",  "2", "3", "4")),
                       textInput("zip_code", "Zip Code"),
                       actionButton("submit", "Calculate Price", class = "btn-primary")
                     )
                   ),
                   mainPanel(
                     htmlOutput("predictedprice")
                   )
                 )
                 )
                 ,
        tabPanel("Summary", plotOutput("summary",height= "1000px",width = "800px")),
        tabPanel("Disclaimer", verbatimTextOutput("Disclaimer"))
      )
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Purpose <- renderText({
    "App Description"
  })
  
  output$Trend <- renderText({
    "Average Market Price for last one year"
  })
  
  output$predictedprice <- renderUI({
    
    HTML(paste("Predicted Price Value Here", "Summary Text Here", sep = '<br/>'))
  })
  
  output$text <- renderUI({
    str1 <- paste("", summary(filedata()$Sale.Price))
    str2 <- paste("Like to add some information",
                  input$range[1], "to", input$range[2])
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
  output$PriceTrend <- renderPlot({
    #hist(filedata()$Sale.Price)
    
    # draw the histogram with the specified number of bins
   # hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(filedata(), n = 10)
  })
  
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) return(NULL)
    # User has not uploaded a file yet
    
    data<- read.csv(infile$datapath)
    
    data <- as_data_frame(data[,-1])
    
    #Remove any home with facilities in its class
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'FACILITIES',]
    
    #Remove any commerical
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'COMMERCIAL',]
    
    #Remove office
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'OFFICE',]
    
    #Remove HOTEL
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'HOTEL',]
    
    #Remove Vacant Land
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'VACANT LAND',]
    
    #Remove Other
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'OTHER',]
    
    #Remove PARK
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'PARK',]
    
    #Remove WAREHOUSE
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'WAREHOUSE',]
    
    #Remove STORE
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'STORE',]
    
    #Remove FACTORY
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'FACTOR',]
    
    #Remove ASYLUM
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'ASYLUM',]
    
    #Remove RENTAL
    data <- data[!data$BUILDING.CLASS.CATEGORY %like% 'RENTAL',]
    
    data$BUILDING.CLASS.CATEGORY<-substring(data$BUILDING.CLASS.CATEGORY,3)
    
    data$BUILDING.CLASS.CATEGORY <- as.character(data$BUILDING.CLASS.CATEGORY)
    data$BUILDING.CLASS.CATEGORY <- ifelse(data$BUILDING.CLASS.CATEGORY %like% 'CONDO', "CONDO",data$BUILDING.CLASS.CATEGORY)
    data$BUILDING.CLASS.CATEGORY <- ifelse(data$BUILDING.CLASS.CATEGORY %like% 'COOP', "COOP",data$BUILDING.CLASS.CATEGORY)
    data$BUILDING.CLASS.CATEGORY <- as.factor(data$BUILDING.CLASS.CATEGORY)
    
    data$BOROUGH <- ifelse(data$BOROUGH==1,'Manhattan',data$BOROUGH)
    data$BOROUGH <- ifelse(data$BOROUGH==2,'Bronx',data$BOROUGH)
    data$BOROUGH <- ifelse(data$BOROUGH==3,'Brooklyn',data$BOROUGH)
    data$BOROUGH <- ifelse(data$BOROUGH==4,'Queens',data$BOROUGH)
    data$BOROUGH <- ifelse(data$BOROUGH==5,'Staten Island',data$BOROUGH)
    
    data$EASE.MENT<-NULL
    
    data$BUILDING.AGE<- 2018 - data$YEAR.BUILT
    
    data <- unique(data)
    
    fac<-c(1,3,10,17)
    data[fac]<-lapply(data[fac],factor)
    chr<-c(8,9)
    data[chr]<-lapply(data[chr],as.character)
    num<-c(14,15)
    data[num]<-lapply(data[num],as.numeric)
    dt<-c(20)
    #data[dt]<-lapply(data[dt],as.Date)
    
    
    
    #Remove sale prices with 0s and NAs, and with very low and high values (outliers)
    
    data <- data[data$SALE.PRICE != 0,]
    data <- data[!is.na(data$SALE.PRICE),]
    
    data_rm_outliers <- data;
    
    remove_outliers <- function(x,lbound) {
      qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
      H <- 1.5 * IQR(x, na.rm = TRUE)
      y=x
      y[x < (qnt[1] - H/lbound)] = -999999
      y[x > (qnt[2] + H)] = -999999
      y
      
    }
    
    #data$SALE.PRICE <- remove_outliers(data_rm_outliers$SALE.PRICE,4)
    
    #data <- data[data$SALE.PRICE!=-999999,]
    
    #data$GROSS.SQUARE.FEET[data$GROSS.SQUARE.FEET == 0] <- NA
    
    #For YEAR.BUILT, we replace any house with the year built before 1776 to NA's.
    
    #data$YEAR.BUILT[data$YEAR.BUILT <= 1776] <- NA
    
    #data <- data[,!colnames(data) %in% c('EASE.MENT','X','TOTAL.UNITS')]
    
    
    #borough_list <- c('Manhattan','Queens','Bronx','Brooklyn','Staten Island')
    
    #for (i in borough_list) {
    # Gross <- median(data$GROSS.SQUARE.FEET[data$BOROUGH==i & !is.na(data$GROSS.SQUARE.FEET)])
    #  data$GROSS.SQUARE.FEET[data$BOROUGH==i & is.na(data$GROSS.SQUARE.FEET)] <- Gross
    
    #  Land <- median(data$LAND.SQUARE.FEET[data$BOROUGH==i & !is.na(data$LAND.SQUARE.FEET)])
    #  data$LAND.SQUARE.FEET[data$BOROUGH==i & is.na(data$LAND.SQUARE.FEET)] <- Land
    #  
    #  Year_Built <- median(data$YEAR.BUILT[data$BOROUGH==i & !is.na(data$YEAR.BUILT)])
    #  data$YEAR.BUILT[data$BOROUGH==i & is.na(data$YEAR.BUILT)] <- Year_Built
    #}
    
    #data_rm_outliers_gsq_feet <- data
    
    #data$GROSS.SQUARE.FEET <- remove_outliers(data_rm_outliers_gsq_feet$GROSS.SQUARE.FEET,4) 
    
    
    #data <- data[data$GROSS.SQUARE.FEET!=-999999,]
    
    #data_rm_outliers_lsq_feet <- data
    
    #data$LAND.SQUARE.FEET[data$LAND.SQUARE.FEET!=0] <-remove_outliers(data_rm_outliers_lsq_feet$LAND.SQUARE.FEET[data_rm_outliers_lsq_feet$LAND.SQUARE.FEET!=0],1)
    
    
    #data <- data[data$LAND.SQUARE.FEET!=-999999,]
    
    data
    
    
    
  })   
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

