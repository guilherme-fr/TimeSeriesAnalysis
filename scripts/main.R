
pacman::p_load(RMySQL, dplyr, chron, lubridate, ggplot2, ggfortify, padr, forecast, tseries)
source("scripts/utils.R")
source("scripts/plots.R")


if (!exists("energy_data") || is.null(energy_data)) {
  cat("Energy data not in memory. Trying to obtain it from the data base.\n")
  conn <- dbConnect(MySQL(), 
                    user = "deepAnalytics",
                    password = "Sqltask1234!",
                    dbname = "dataanalytics2018",
                    host = "data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com")
  
  dataBaseNames <- c("yr_2007", "yr_2008", "yr_2009", "yr_2010")
  energy_data <- data.frame()
  for (dataBase in dataBaseNames) {
    cat(paste("Downloading the '", dataBase, "' data...\n", sep = ""))
    partial_data <- dbGetQuery(conn, paste("SELECT * FROM", dataBase, sep = " "))
    cat(paste("Download of '", dataBase, "' completed successfully\n", sep = ""))
    
    cat(paste("Applying feature engineering in the '", dataBase, "' data...\n", sep = ""))
    partial_data <- featureEngineering(partial_data)
    cat(paste("Feature engineering of '", dataBase, "' data finished\n\n", sep = ""))
    
    energy_data <- rbind(energy_data, partial_data)
  }
  partial_data <- NULL #Memory cleaning
}

cat("Choose the granularity of the data by typing its number option: ")
cat(" (1) Days\n (2) Weeks\n (3) Months\n")
user_input_granularity <- readline()
user_input_granularity <- as.integer(user_input_granularity)
granularity <- NULL
frequency <- NULL
start_ts = c(2007, 1)
start_train <- c(2007, 1)
start_test <- c(2010, 1)
end_train <- NULL
end_test <- NULL
if (user_input_granularity == 1) {
  granularity <- "day"
  frequency <- 365
  end_train <- c(2009, 365)
} else if (user_input_granularity == 2) {
  granularity <- "week"
  frequency <- 52
  end_train <- c(2009, 52)
} else if (user_input_granularity == 3) {
  granularity <- "month"
  frequency <- 12
  end_train <- c(2009, 12)
} else {
  #TODO Invalid option. Should throw an error message and show the options again
}


time_series <- totalEnergyConsumTimeSeries(energy_data, granularity, start = start_ts)
time_series_dec <- stl(time_series, s.window = "periodic")
time_series_relative_variance <- apply(time_series_dec$time.series, 2, var) / var(time_series)

autoplot(time_series_dec) #Shows graphs of the time series and its components
ggtsdisplay(time_series)  #Shows the time series and its ACF and PACF graphs
cat("\nRelative variance of the time series components: ")
print(time_series_relative_variance)

adf_test <- adf.test(time_series)
cat("\nADF Test: \n")
print(adf_test)

cat("\nDo you want to make any transformation in the data?")
cat(" (1) YES\n (2) NO\n")
user_input_do_transform <- readline()
user_input_do_transform <- as.integer(user_input_do_transform)

if (user_input_do_transform == 1) {
  #TODO Show possible transformations
} else if (user_input_do_transform == 2) {
  #TODO Move on with the execution flow
} else {
  #TODO Invalid option. Should throw an error message and show the options again
}

time_series_split <- splitTrainTestTimeSeries(time_series, 
                                              start_train = start_train, 
                                              end_train = end_train, 
                                              start_test = start_test, 
                                              end_test = end_test)

cat("\nChoose the forecast algorithm: ")
cat(" (1) ARIMA\n (2) Holt Winters\n")
user_input_algorithm <- readline()
user_input_algorithm <- as.integer(user_input_algorithm)

model <- NULL
forecast <- NULL
if (user_input_algorithm == 1) {
  #TODO Show ARIMA options
  cat("\nSelect the type of ARIMA: ")
  cat(" (1) Auto Arima\n (2) Non seasonal ARIMA\n (3) Seasonal ARIMA\n")
  user_input_arima_type <- readline()
  user_input_arima_type <- as.integer(user_input_arima_type)
  
  if (user_input_arima_type == 1) {
    #TODO Run auto arima
    model <- auto.arima(time_series_split$train)
    forecast <- forecast:::forecast.Arima(model, h = length(time_series_split$test))
  } else if (user_input_arima_type == 2) {
    #TODO Show Non seasonal ARIMA options
    
  } else if (user_input_arima_type == 3) {
    #TODO Show seasonal ARIMA options
    
  } else {
    #TODO Invalid option. Should throw an error message and show the options again
  }
  
} else if (user_input_algorithm == 2) {
  #TODO Run Holt Winters
  model <- HoltWinters(time_series_split$train)
  forecast <- forecast:::forecast.HoltWinters(model, h = length(time_series_split$test))
} else {
  #TODO Invalid option. Should throw an error message and show the options again
}

model_accuracy <- accuracy(forecast, time_series_split$test)
cat("\nModel accuracy: \n")
print(model_accuracy)

checkresiduals(forecast)

autoplot(time_series) + autolayer(forecast$mean)



