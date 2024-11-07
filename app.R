library(shiny)
library(randomForest)
library(ggplot2)
library(caret)
library(lubridate)
library(tidyverse)

options(shiny.maxRequestSize = 30*1024^2)  # Set to 30 MB or adjust as needed

# Define UI
ui <- fluidPage(
  titlePanel("NYC Taxi Fare Prediction"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Taxi Data CSV", accept = c(".csv")),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      plotOutput("predictionPlot"),
      verbatimTextOutput("modelMetrics")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Load and process the data
  data <- reactive({
    req(input$file)
    taxi_data <- read.csv(input$file$datapath)
    
    # Data preprocessing steps
    taxi_data$tpep_pickup_datetime <- as.POSIXct(taxi_data$tpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
    taxi_data$tpep_dropoff_datetime <- as.POSIXct(taxi_data$tpep_dropoff_datetime, format = "%Y-%m-%d %H:%M:%S")
    
    # Create pickup hour and pickup day if not present
    taxi_data$pickup_hour <- hour(taxi_data$tpep_pickup_datetime)
    taxi_data$pickup_day <- weekdays(taxi_data$tpep_pickup_datetime)
    
    taxi_data
  })
  
  # Generate predictions based on the data
  predictions <- eventReactive(input$predict, {
    taxi_data <- data()
    
    # Check if all required columns are available
    required_cols <- c("fare_amount", "trip_distance", "passenger_count", "pickup_hour", "pickup_day")
    if (!all(required_cols %in% colnames(taxi_data))) {
      stop("Data is missing one or more required columns.")
    }
    
    # Train a random forest model
    rf_model <- randomForest(fare_amount ~ trip_distance + passenger_count + pickup_hour + pickup_day, data = taxi_data, ntree = 100)
    
    # Predict fare amount
    predict(rf_model, newdata = taxi_data)
  })
  
  # Plot predictions
  output$predictionPlot <- renderPlot({
    predictions <- predictions()
    ggplot(data.frame(Predicted = predictions), aes(x = Predicted)) +
      geom_histogram(bins = 30, fill = "blue", color = "white") +
      labs(title = "Predicted Fare Amounts", x = "Predicted Value", y = "Frequency")
  })
  
  # Display model metrics
  output$modelMetrics <- renderPrint({
    predictions <- predictions()
    actuals <- data()$fare_amount
    
    rmse <- sqrt(mean((predictions - actuals)^2))
    r_squared <- 1 - sum((predictions - actuals)^2) / sum((actuals - mean(actuals))^2)
    
    cat("R-squared:", r_squared)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
