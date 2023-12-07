library(dplyr)
library(readxl)
library(magrittr)
library(tidyr)
library(zoo)


d_US <- read_excel("2023-12-07_BDD_clean.xlsx", sheet="US_quarter", skip=1)
d_France <- read_excel("2023-12-07_BDD_clean.xlsx", sheet="FR_quarter", skip=1)
d_Germany <- read_excel("2023-12-07_BDD_clean.xlsx", sheet="GER_quarter", skip=1)

d_US <- row.names(d_US$Quarter)

ts_d_US <- ts(d_US, start =c(1990, 01), frequency = 4)
ts.plot(ts_d_US)


for (i in 1:ncol(ts_d_US)) {
  ts.plot(ts_d_US[, i], gpars = list(main = colnames(ts_d_US)[i], ylab=""))
}

stationary_US <- sapply(ts_d_US, diff)

for (i in 1:ncol(stationary_US)) {
  ts.plot(stationary_US[, i], gpars = list(main = colnames(stationary_US)[i], ylab=""))
}
