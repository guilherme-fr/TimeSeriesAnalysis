
pacman::p_load(RMySQL, dplyr, chron, lubridate, ggfortify, padr, forecast, tseries)
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

cat("What do you want to analyze?\n")
cat(" (1) Energy\n (2) Cost")
user_input_analysis <- readline()
user_input_analysis <- as.integer(user_input_analysis)

cat("Choose the granularity of the data by typing its number option: \n")
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
data_time_series <- NULL
if (user_input_granularity == 1) {
  granularity <- "day"
  frequency <- 365
  end_train <- c(2009, 365)
  if (user_input_analysis == 1) {
    data_time_series <- totalEnergyConsumByDay(energy_data)
    data_time_series <- data_time_series$Total_energy_day
  } else if (user_input_analysis == 2) {
    data_time_series <- totalCostByDay(energy_data)
    data_time_series <- data_time_series$Total_cost_day
  } else {
    
  }
} else if (user_input_granularity == 2) {
  granularity <- "week"
  frequency <- 52
  end_train <- c(2009, 52)
  if (user_input_analysis == 1) {
    data_time_series <- totalEnergyConsumByWeek(energy_data)
    data_time_series <- data_time_series$Total_energy_week
  } else if (user_input_analysis == 2) {
    data_time_series <- totalCostByWeek(energy_data)
    data_time_series <- data_time_series$Total_cost_week
  } else {
    
  }
} else if (user_input_granularity == 3) {
  granularity <- "month"
  frequency <- 12
  end_train <- c(2009, 12)
  if (user_input_analysis == 1) {
    data_time_series <- totalEnergyConsumByMonth(energy_data)
    data_time_series <- data_time_series$Total_energy_month
  } else if (user_input_analysis == 2) {
    data_time_series <- totalCostByMonth(energy_data)
    data_time_series <- data_time_series$Total_cost_month
  } else {
    
  }
} else {
  #TODO Invalid option. Should throw an error message and show the options again
}

y_label_ts <- NULL
if (user_input_analysis == 1) {
  y_label_ts <- "Energy (kWh)"
  title_ts <- "Energy Consumption Time Series"
} else if (user_input_analysis == 2) {
  y_label_ts <- "Euros"
  title_ts <- "Energy Cost Time Series"
} else {
  
}

time_series <- toTimeSeries(data_time_series, granularity, start = start_ts)

time_series_dec <- stl(time_series, s.window = "periodic")
time_series_relative_variance <- apply(time_series_dec$time.series, 2, var) / var(time_series)

autoplot(time_series_dec) #Shows graphs of the time series and its components
ggtsdisplay(time_series)  #Shows the time series and its ACF and PACF graphs
cat("\nRelative variance of the time series components: \n")
print(time_series_relative_variance)

adf_test <- adf.test(time_series)
cat("\nADF Test: \n")
print(adf_test)

cat("\n#####################################################\n")

cat("\nDo you want to make any transformation in the data?\n")
cat(" (1) YES\n (2) NO\n")
user_input_do_transform <- readline()
user_input_do_transform <- as.integer(user_input_do_transform)

did_transformaction <- FALSE
if (user_input_do_transform == 1) {
  did_transformaction <- TRUE
  data_transformed <- data_time_series
  while (user_input_do_transform == 1) {
    #TODO Show possible transformations
    cat("\nSelect the transformation: \n")
    cat(" (1) Simple Diff\n (2) Seasonal Diff\n (3) Log")
    user_input_transformation <- readline()
    user_input_transformation <- as.integer(user_input_transformation)
    
    if (user_input_transformation == 1) {
      data_transformed <- diff(data_transformed)
    } else if (user_input_transformation == 2) {
      data_transformed <- diff(data_transformed, lag = frequency)
    } else if (user_input_transformation == 3) {
      data_transformed <- log(data_transformed)
    } else {
      
    }
    
    time_series <- toTimeSeries(data_transformed, granularity, start = start_ts)
    
    time_series_dec <- stl(time_series, s.window = "periodic")
    time_series_relative_variance <- apply(time_series_dec$time.series, 2, var) / var(time_series)
    
    autoplot(time_series_dec) #Shows graphs of the time series and its components
    ggtsdisplay(time_series)  #Shows the time series and its ACF and PACF graphs
    cat("\nRelative variance of the time series components: \n")
    print(time_series_relative_variance)
    
    adf_test <- adf.test(time_series)
    cat("\nADF Test: \n")
    print(adf_test)
    
    cat("\n#####################################################\n")
    
    cat("\nDo you want to make any transformation in the data?\n")
    cat(" (1) YES\n (2) NO\n")
    user_input_do_transform <- readline()
    user_input_do_transform <- as.integer(user_input_do_transform)
  }
  
} else if (user_input_do_transform == 2) {
  #TODO Move on with the execution flow
} else {
  #TODO Invalid option. Should throw an error message and show the options again
}

if (did_transformaction) {
  cat("\nDo you want to keep the transformations in the data?\n")
  cat(" (1) YES\n (2) NO\n")
  user_input_keep_transf <- readline()
  user_input_keep_transf <- as.integer(user_input_keep_transf)
  
  if (user_input_keep_transf == 2) {
    time_series <- toTimeSeries(data_time_series, granularity, start = start_ts)
  }
}

time_series_split <- splitTrainTestTimeSeries(time_series, 
                                              start_train = start_train, 
                                              end_train = end_train, 
                                              start_test = start_test, 
                                              end_test = end_test)

cat("\nChoose the forecast algorithm: \n")
cat(" (1) ARIMA\n (2) Holt Winters\n")
user_input_algorithm <- readline()
user_input_algorithm <- as.integer(user_input_algorithm)

model <- NULL
forecast <- NULL
forecast_length = length(time_series_split$test) + 2
if (user_input_algorithm == 1) {
  #TODO Show ARIMA options
  cat("\nSelect the type of ARIMA: \n")
  cat(" (1) Auto Arima\n (2) Non seasonal ARIMA\n (3) Seasonal ARIMA\n")
  user_input_arima_type <- readline()
  user_input_arima_type <- as.integer(user_input_arima_type)
  
  if (user_input_arima_type == 1) {
    #TODO Run auto arima
    model <- auto.arima(time_series_split$train)
    forecast <- forecast:::forecast.Arima(model, h = forecast_length)
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
  forecast <- forecast:::forecast.HoltWinters(model, h = forecast_length)
} else {
  #TODO Invalid option. Should throw an error message and show the options again
}

cat("\nModel Metrics:\n")
print(model)

model_accuracy <- accuracy(forecast, time_series_split$test)
cat("\nModel accuracy: \n")
print(model_accuracy)

cat("\nCheck Residuals: \n")
checkresiduals(forecast)

line_width = 1.3
print(autoplot(time_series, size = line_width, main = title_ts) + 
        autolayer(forecast$mean, size = line_width, series = "Forecast") + 
        ylab(y_label_ts) + xlab("Year") +
        guides(colour = guide_legend(title = "")) +
        theme(text = element_text(size = 15))
)
