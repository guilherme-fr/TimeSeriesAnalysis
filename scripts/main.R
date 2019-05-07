
pacman::p_load(RMySQL, dplyr, chron, lubridate, ggfortify, padr, forecast)
source("scripts/utils.R")
source("scripts/plots.R")

granularity <- "month"

start_ts = c(2007, 1)

start_train <- c(2007, 1)
if (granularity == "day") {
  end_train <- c(2009, 365)  
} else if (granularity == "week") {
  end_train <- c(2009, 52)
} else if (granularity == "month") {
  end_train <- c(2009, 12)
}

start_test <- c(2010, 1)
end_test <- NULL

forecast_algorithm <- "arima"

conn <- dbConnect(MySQL(), 
                  user = "deepAnalytics",
                  password = "Sqltask1234!",
                  dbname = "dataanalytics2018",
                  host = "data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com")

dataBaseNames <- c("yr_2007", "yr_2008", "yr_2009", "yr_2010")

if (is.null(energy_data)) {
  energy_data <- data.frame()
  for (dataBase in dataBaseNames) {
    partial_data <- dbGetQuery(conn, paste("SELECT * FROM", dataBase, sep = " "))
    partial_data <- featureEngineering(partial_data)
    energy_data <- rbind(energy_data, partial_data)
  }
}

partial_data <- NULL

time_series <- totalEnergyConsumTimeSeries(energy_data, granularity, start = start_ts)

time_series_dec <- stl(time_series, s.window = "periodic")
time_series_relative_variance <- apply(time_series_dec$time.series, 2, var) / var(time_series)
print(paste("Relative variance"))

time_series_split <- splitTrainTestTimeSeries(time_series, 
                                     start_train = start_train, 
                                     end_train = end_train, 
                                     start_test = start_test, 
                                     end_test = end_test)

model <- NULL
forecast <- NULL
if (forecast_algorithm == "hw") {
  model <- HoltWinters(time_series_split$train)
  forecast <- forecast:::forecast.HoltWinters(model, h = length(time_series_split$test))
} else if (forecast_algorithm == "arima") {
  model <- auto.arima(time_series_split$train)
  forecast <- forecast:::forecast.Arima(model, h = length(time_series_split$test))
}

model_accuracy <- accuracy(forecast, time_series_split$test)
autoplot(time_series) + autolayer(forecast$mean)
