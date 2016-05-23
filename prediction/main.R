#Load source files
source("time_to_unix.R")
source("time_series_creation.R")
source("utility_functions.R")
source("time_series_predictions.R")

library(forecast)
library(parallel)

# Reading the dataset from "generateDataset()" and obtaining of time series (TS) of the acumulate time detection of cars.
dataset_UNIX_name=data_time_unix("Datasets/pasosFull.csv",c(3,4))

# Preprocessing of the sensor data obtained during the last two months.
TS_dataset_name=process_sensors(dataset_UNIX_name,60*60*24*7*8,60)

# Obtains predictions 1-step-ahead for different structs of data
predict_test(TS_dataset_name)

