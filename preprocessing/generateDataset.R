library(dplyr)
library(readr)
library(Hmisc)
library(plyr)
require(arules)
require(lubridate)

#Load data
dispositivos <- read_csv("Datasets/dispositivos.csv")
pasos <- read_csv("Datasets/pasos.csv")
sensores <- read_csv("Datasets/sensores.csv")

#Show decimals
op <- options(digits.secs=3)
options(scipen=4)

#Add detection time variable
pasosTfiltered <- transform(pasos, tiempo = pasos$tfin - pasos$tinicio)

#Change column names
colnames(dispositivos)[1] <- "idDisp"
colnames(pasosTfiltered)[2] <- "idDisp"

#Merge data
pasosTDisp <- merge(pasosTfiltered, dispositivos, by="idDisp")
pasosFull <- merge(pasosTDisp, sensores, by="idSensor")

#Write results
write_csv(pasosFull, "Datasets/pasosfull.csv")
