library(dplyr)
library(readxl)
library(magrittr)
library(tidyr)
library(zoo)


d1 <- read_excel("2023-11-07 - OECD short term indicators.xlsx")

ts_d1 <- ts.plot(d1)
for (i in 1:ncol(ts_d1)) {
  ts.plot(ts_df[, i], gpars = list(main = colnames(ts_d1)[i]))
}
