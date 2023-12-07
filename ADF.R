library(dplyr)
library(readxl)
library(magrittr)
library(tidyr)
library(zoo)
library(tseries)


## Import data
time_series <- ts_d_US


## Implement a function that realizes the ADF test and then reports reults

perform_adf_test <- function(column, variable_name = "") {
  result <- adf.test(column)
  
  cat("---------------------------------\n")
  cat("Test Statistic:", result$statistic, "\n")
  cat("P-value:", result$p.value, "\n")
  cat("Critical Values:", paste(result$critical, collapse = ", "), "\n")
  
  if (result$p.value < 0.05) {
    cat("Conclusion: Reject the null hypothesis. The time series is stationary.\n")
  } else {
    cat("Conclusion: Fail to reject the null hypothesis. The time series may not be stationary.\n")
  }
}


## Apply the test to each column of the data_frame

apply(time_series, 2, perform_adf_test)