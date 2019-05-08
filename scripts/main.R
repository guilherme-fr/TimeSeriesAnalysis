
pacman::p_load(RMySQL, dplyr, chron, lubridate, ggfortify, padr, forecast, tseries)
source("scripts/utils.R")
source("scripts/plots.R")

options(warn=-1)
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

cat("What do you want to analyze?\n")
cat(" (1) Energy\n (2) Cost")
user_input_analysis <- readline()
user_input_analysis <- as.integer(user_input_analysis)

cat("Do you want to forecast your data by: \n")
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

time_series <- NULL
y_label_ts <- NULL
title_ts <- NULL
if (user_input_analysis == 1) {
  time_series <- totalEnergyConsumTimeSeries(energy_data, granularity, start = start_ts)  
  y_label_ts <- "Energy (kWh)"
  title_ts <- "Energy Comsumption Time Series"
} else if (user_input_analysis == 2) {
  time_series <- totalCostTimeSeries(energy_data, granularity, start = start_ts)
  y_label_ts <- "Euros"
  title_ts <- "Energy Cost Time Series"
} else {
  
}

time_series_dec <- stl(time_series, s.window = "periodic")
time_series_relative_variance <- apply(time_series_dec$time.series, 2, var) / var(time_series)

autoplot(time_series_dec) #Shows graphs of the time series and its components
ggtsdisplay(time_series)  #Shows the time series and its ACF and PACF graphs

time_series_split <- splitTrainTestTimeSeries(time_series, 
                                              start_train = start_train, 
                                              end_train = end_train, 
                                              start_test = start_test, 
                                              end_test = end_test)

cat(paste("\nHow many ", granularity, "s do you want to forecast?\n", sep = ""))
user_input_forecast_length <- readline()
user_input_forecast_length <- as.numeric(user_input_forecast_length)

model <- NULL
forecast <- NULL
forecast_length = length(time_series_split$test) + user_input_forecast_length

model_arima <- auto.arima(time_series_split$train)
forecast_arima <- forecast:::forecast.Arima(model_arima, h = forecast_length)

model_hw <- HoltWinters(time_series_split$train)
forecast_hw <- forecast:::forecast.HoltWinters(model_hw, h = forecast_length)

print(autoplot(time_series, size = 1.3, main = title_ts) + 
        autolayer(forecast_arima$mean, size = 1.3, series = "ARIMA") + 
        autolayer(forecast_hw$mean, size = 1.3, series = "Holt Winters") +
        ylab(y_label_ts) + xlab("Year") +
        guides(colour = guide_legend(title = "Forecast Algorithm"))
      )
  
options(warn=0)
