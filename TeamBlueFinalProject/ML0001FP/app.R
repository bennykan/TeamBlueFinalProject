#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)
library(digest)
library(DT)
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
library(scales)
library(gbm)

LTable <- read.csv("lookuptable.csv")

graph_table <- read.csv("graphfile.csv")

graph_table$SALE.DATE <- as.Date(graph_table$SALE.DATE)

graph_table$YearMonth <- as.Date(graph_table$YearMonth)

LTable$ZIPCODE <- LTable$ZIP.CODE


fieldsMandatory <- c("zip_code","Neighborhood", "building_category")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

shinyApp(
  ui = fluidPage(
    theme = shinytheme("spacelab"),
    tags$head(
      tags$style(HTML("
                      .shiny-output-error-validation {
                      color: red;
                      }
                      "))
      ),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    
    titlePanel("Property Value Prediction Using GBM - Automated Valuation Model (AVM)"),
    
    tabsetPanel(
      tabPanel("Predict Price",
        sidebarLayout(
          sidebarPanel(
            div(
              
              id = "form",
              
              selectInput("zip_code", labelMandatory("Zip Code"), LTable$ZIP.CODE[LTable$ZIP.CODE>0]),
              selectInput("Neighborhood", labelMandatory("Neighborhood"), levels(LTable$NEIGHBORHOOD)),
              selectInput("building_category",labelMandatory("Building Category") ,""),
              numericInput("gross_sqft", 'Living Area in Sqft',1500),
              numericInput("land_sqft", 'Lot SiZe in Sqft',20000),
              numericInput("building_age", "Building Age (Years)",25),
              actionButton("submit", "Calculate Price", class = "btn-primary")
              
            )
          ),
          mainPanel(
            HTML("<p><h1>Housing Price Predictor</h1></p></br><h4 style='color:red;'>Provide value for the required fields to predict the price </h4>"),
            htmlOutput("predictedprice") ,
            HTML("</br>"),
            plotOutput("PriceTrend",height= "500px",width = "500px")
          )
        )
      ),
      tabPanel("About",
               HTML("<p>Write up about the app</p>")
               ),
      tabPanel("Disclaimer", 
               HTML("<p>This App is jointly submitted by Tyler Blakeley, Benjamin Kan, Mohammad Islam, Avijeet Singh </p>")
               )
      
    )
    
    
  ),
  server = function(input, output, session) {
    
    house_value <- eventReactive(input$submit, {
      model <- readRDS("gbmFit1.rds")
      
      validate(
        need(input$zip_code, 'Select Your zip code'),
        need(input$Neighborhood != '', 'Select your Neighborhood.'),
        need(input$building_category != '', 'Select your Housing Type')
      )
      
      
      
      
      
      data_up <- filter(LTable,ZIP.CODE == input$zip_code & NEIGHBORHOOD == input$Neighborhood & BUILDING.CLASS.CATEGORY == input$building_category)
      
      
      data_modelling <- data.frame(BLOCK = data_up$BLOCK,LOT = data_up$LOT,LAND.SQUARE.FEET = input$land_sqft,GROSS.SQUARE.FEET = input$gross_sqft,SALE.PRICE = 0,BUILDING.AGE = input$building_age,BOROUGH = data_up$ENCOD_BOROUGH,NEIGHBORHOOD = data_up$ENCOD_NEIGHBORHOOD,BUILDING.CLASS.CATEGORY = data_up$ENCOD_BUILDING.CLASS.CATEGORY,ZIP.CODE = data_up$ENCOD_ZIP.CODE,data.SALE.DATE = as.Date('2018-01-01'))
      
      predict_data <- predict(model, data_modelling)
      
      predict_data <- round(10^predict_data,0)
      
      predict_data
    })
    
    
    plot_histo <- eventReactive(input$submit, {
      test <- filter(graph_table,NEIGHBORHOOD==input$Neighborhood)
      
    
    })
    output$predictedprice <- renderUI({
      

      #validate(
       # need(input$zip_code, 'Select Your zip code'),
        #need(input$Neighborhood != '', 'Select your Neighborhood.'),
        #need(input$building_category != '', 'Select your Housing Type')
      #)
      
      HTML(paste("<h1> Predicted valuation is <span style='color:green;font-weight:bold;padding:10px 10px;size:24px'>",house_value()," USD</span><br><br> Neighborhood Price Trend </br></h1>", "", sep = ''))
      
      
    })
    
    output$PriceTrend <- renderPlot({
      
      
      ggplot(data = plot_histo(),
             aes(YearMonth, SALE.PRICE)) +
        stat_summary(fun.y = median, geom = "bar") +
        scale_y_continuous(labels = comma)+
        theme_classic() + labs(title = plot_histo()$NEIGHBORHOOD)+scale_x_date(labels = date_format("%Y-%m"))
      
      
      
    })
    
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })   
    
    
    observe({
      x <- input$zip_code
      
      if (is.null(x))
        x <- character(0)
      # Can also set the label and select items
      updateSelectInput(session, "Neighborhood",
                        choices = filter(LTable,ZIP.CODE == x)$NEIGHBORHOOD,
                        selected = tail(x, 1)
      )
     })
    
    observe({
      
      
      y <- input$Neighborhood
      
      if (is.null(y))
        y <- character(0)
      # Can also set the label and select items
      updateSelectInput(session, "building_category",
                        choices = filter(LTable,ZIP.CODE == input$zip_code & NEIGHBORHOOD == y)$BUILDING.CLASS.CATEGORY,
                        selected = tail(y, 1)
      )
      
    })
    
    
  }
)