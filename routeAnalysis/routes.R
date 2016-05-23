library(plyr)
library(parallel)
require(lubridate)

#Load data
data <- read.csv(file="Datasets/pasosfull.csv")

#Remove time>2000 (noise)
data <- data[!data$tiempo>2000,]
pasosFull <- data

#Transform time to UNIX Time (It facilitates calculations)
time_unix<-function(time){
  time=as.character(time)
  a=unlist(strsplit(time, split="[T.]"))
  time_out=as.numeric(as.POSIXct(paste(a[1],a[2],sep=" ")))
  a[3]=gsub('[^0-9]+', '', a[3])
  time_out=as.numeric(time_out)+as.numeric(a[3])/1000.0
}

data$tinicio=unlist(mclapply(data$tinicio,time_unix))
data$tfin=unlist(mclapply(data$tfin,time_unix))

#Order data by time
data <- data[order(as.numeric(data$tinicio)),]

#Remove ids with frequency<2
idx<-data$idDisp
df<-data[data$idDisp ==idx, ]
agrupados <- subset(df, data$idDisp ==idx)
count <- count(agrupados$idDisp)
freq <- count[count$freq>1,]
more_than_one <- agrupados[agrupados$idDisp %in% freq$x,]

#Add hour, minute, second, day and month variables
pasosFull$horaPaso <- hour(pasosFull$tinicio)
pasosFull$minutoPaso <- minute(pasosFull$tinicio)
pasosFull$segundoPaso <- second(pasosFull$tinicio)
pasosFull$diaPaso <- day(pasosFull$tinicio)
pasosFull$mesPaso <- month(pasosFull$tinicio)

#Check for vehicles that have passed for at least two different sensors,
#the same day, with a difference of 3 hours max
i <- 1
finals <- NULL
for(id in freq[,1]){
  x <- pasosFull[which(pasosFull$idDisp %in% id),]
  y <- length(unique(x$idSensor))
  dia <- length(unique(x$diaPaso))
  if(y>1){
    if(dia == 1){
      horas <- abs(x$horaPaso[1] - x$horaPaso[2])
      if(horas<3){
        finals[i] <- as.character(x$idDisp[1])
        i <- i+1
      }
    }
  }
}

#Distance between sensors:
#1010-1060 = 65km
#1060-1050 = 33km
#1050-1040 = 25km
#1010-1020 = 56km
#1020-1030 = 53km

#Returns the distance between every sensor
distancia_tramos <- function(sensorActual, sensorAnterior){
  distancia <- 0
  
  if(sensorActual==1011 || sensorActual==1012){
    if(sensorAnterior==1021 || sensorAnterior==1022){
      distancia <- 56
    }
    if(sensorAnterior==1031 || sensorAnterior==1032){
      distancia <- 109
    }
    if(sensorAnterior==1041 || sensorAnterior==1042){
      distancia <- 123
    }
    if(sensorAnterior==1051 || sensorAnterior==1052){
      distancia <- 98
    }
    if(sensorAnterior==1061 || sensorAnterior==1062){
      distancia <- 65
    }
  }
  if(sensorActual==1021 || sensorActual==1022){
    if(sensorAnterior==1011 || sensorAnterior==1012){
      distancia <- 56
    }
    if(sensorAnterior==1031 || sensorAnterior==1032){
      distancia <- 53
    }
    if(sensorAnterior==1041 || sensorAnterior==1042){
      distancia <- 179
    }
    if(sensorAnterior==1051 || sensorAnterior==1052){
      distancia <- 154
    }
    if(sensorAnterior==1061 || sensorAnterior==1062){
      distancia <- 121
    }
  }
  if(sensorActual==1031 || sensorActual==1032){
    if(sensorAnterior==1021 || sensorAnterior==1022){
      distancia <- 53
    }
    if(sensorAnterior==1011 || sensorAnterior==1012){
      distancia <- 109
    }
    if(sensorAnterior==1041 || sensorAnterior==1042){
      distancia <- 232
    }
    if(sensorAnterior==1051 || sensorAnterior==1052){
      distancia <- 207
    }
    if(sensorAnterior==1061 || sensorAnterior==1062){
      distancia <- 174
    }
  }
  if(sensorActual==1041 || sensorActual==1042){
    if(sensorAnterior==1021 || sensorAnterior==1022){
      distancia <- 179
    }
    if(sensorAnterior==1031 || sensorAnterior==1032){
      distancia <- 232
    }
    if(sensorAnterior==1011 || sensorAnterior==1012){
      distancia <- 123
    }
    if(sensorAnterior==1051 || sensorAnterior==1052){
      distancia <- 25
    }
    if(sensorAnterior==1061 || sensorAnterior==1062){
      distancia <- 58
    }
  }
  if(sensorActual==1051 || sensorActual==1052){
    if(sensorAnterior==1021 || sensorAnterior==1022){
      distancia <- 154
    }
    if(sensorAnterior==1031 || sensorAnterior==1032){
      distancia <- 207
    }
    if(sensorAnterior==1041 || sensorAnterior==1042){
      distancia <- 25
    }
    if(sensorAnterior==1011 || sensorAnterior==1012){
      distancia <- 98
    }
    if(sensorAnterior==1061 || sensorAnterior==1062){
      distancia <- 33
    }
  }
  if(sensorActual==1061 || sensorActual==1062){
    if(sensorAnterior==1021 || sensorAnterior==1022){
      distancia <- 121
    }
    if(sensorAnterior==1031 || sensorAnterior==1032){
      distancia <- 174
    }
    if(sensorAnterior==1041 || sensorAnterior==1042){
      distancia <- 58
    }
    if(sensorAnterior==1051 || sensorAnterior==1052){
      distancia <- 33
    }
    if(sensorAnterior==1011 || sensorAnterior==1012){
      distancia <- 65
    }
  }
  distancia
}

tramo1020 <- NULL
tramo1030 <- NULL
tramo1040 <- NULL
tramo1050 <- NULL
tramo1060 <- NULL
tramo2030 <- NULL
tramo2040 <- NULL
tramo2050 <- NULL
tramo2060 <- NULL
tramo3040 <- NULL
tramo3050 <- NULL
tramo3060 <- NULL
tramo4050 <- NULL
tramo4060 <- NULL
tramo5060 <- NULL

#Updates the data.frames with the vehicle id, speed and detected time
actualizaMedias <- function(sensor1, sensor2, velmedia, tiempo, mac){
  newData <- NULL
  if(sensor1==1011 || sensor1==1012){
    if(sensor2==1021 || sensor2==1022){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo1020 <<- rbind(tramo1020, newData)
    }
    if(sensor2==1031 || sensor2==1032){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo1030 <<- rbind(tramo1030, newData)
    }
    if(sensor2==1041 || sensor2==1042){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo1040 <<- rbind(tramo1040, newData)
    }
    if(sensor2==1051 || sensor2==1052){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo1050 <<- rbind(tramo1050, newData)
    }
    if(sensor2==1061 || sensor2==1062){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo1060 <<- rbind(tramo1060, newData)
    }
  }
  if(sensor1==1021 || sensor1==1022){
    if(sensor2==1031 || sensor2==1032){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo2030 <<- rbind(tramo2030, newData)
    }
    if(sensor2==1041 || sensor2==1042){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo2040 <<- rbind(tramo2040, newData)
    }
    if(sensor2==1051 || sensor2==1052){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo2050 <<- rbind(tramo2050, newData)
    }
    if(sensor2==1061 || sensor2==1062){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo2060 <<- rbind(tramo2060, newData)
    }
  }
  if(sensor1==1031 || sensor1==1032){
    if(sensor2==1041 || sensor2==1042){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo3040 <<- rbind(tramo3040, newData)
    }
    if(sensor2==1051 || sensor2==1052){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo3050 <<- rbind(tramo3050, newData)
    }
    if(sensor2==1061 || sensor2==1062){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo3060 <<- rbind(tramo3060, newData)
    }
  }
  if(sensor1==1041 || sensor1==1042){
    if(sensor2==1051 || sensor2==1052){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo4050 <<- rbind(tramo4050, newData)
    }
    if(sensor2==1061 || sensor2==1062){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo4060 <<- rbind(tramo4060, newData)
    }
  }
  if(sensor1==1051 || sensor1==1052){
    if(sensor2==1061 || sensor2==1062){
      newData$idDisp <- mac
      newData$speed <- velmedia
      newData$at <- tiempo
      tramo5060 <<- rbind(tramo5060, newData)
    }
  }
}

#Main procedure:
#Checks for a given id if it has been detected previously by other sensor,
#if that's the case, it calculates the speed and updates the data.frames
dataTramos <- data[data$idDisp %in% finals,]
last2 <- NULL

for(i in 1:500){
  evento <- dataTramos[i,]
  sensor <- evento$idSensor
  mac <- as.character(evento$idDisp)
  time <- evento$tinicio
  
  if(!(mac %in% last2$idDisp)){
    last2 <- rbind(last2, evento)
  }else{
    x <- which(last2$idDisp %in% mac)
    anterior <- last2[x,]
    dist <- distancia_tramos(sensor, anterior$idSensor)
    tiempo <- abs(time - anterior$tfin)
    velMedia <- ((dist*1000)/tiempo)*3.6
    actualizaMedias(sensor, anterior$idSensor, as.numeric(velMedia), as.numeric(time), mac)
    last2 <- last2[-x,]
    last2 <- rbind(last2, evento)
  }
}

#Data.frames formatting
tramo1020 <- as.data.frame(tramo1020)
tramo1020$idDisp <- as.character(tramo1020$idDisp)
tramo1020$speed <- as.numeric(tramo1020$speed)
tramo1020$at <- as.numeric(tramo1020$at)
rownames(tramo1020) <- NULL

tramo1030 <- as.data.frame(tramo1030)
tramo1030$idDisp <- as.character(tramo1030$idDisp)
tramo1030$speed <- as.numeric(tramo1030$speed)
tramo1030$at <- as.numeric(tramo1030$at)
rownames(tramo1030) <- NULL

tramo1040 <- as.data.frame(tramo1040)
tramo1040$idDisp <- as.character(tramo1040$idDisp)
tramo1040$speed <- as.numeric(tramo1040$speed)
tramo1040$at <- as.numeric(tramo1040$at)
rownames(tramo1040) <- NULL

tramo1050 <- as.data.frame(tramo1050)
tramo1050$idDisp <- as.character(tramo1050$idDisp)
tramo1050$speed <- as.numeric(tramo1050$speed)
tramo1050$at <- as.numeric(tramo1050$at)
rownames(tramo1050) <- NULL

tramo1060 <- as.data.frame(tramo1060)
tramo1060$idDisp <- as.character(tramo1060$idDisp)
tramo1060$speed <- as.numeric(tramo1060$speed)
tramo1060$at <- as.numeric(tramo1060$at)
rownames(tramo1060) <- NULL

tramo2030 <- as.data.frame(tramo2030)
tramo2030$idDisp <- as.character(tramo2030$idDisp)
tramo2030$speed <- as.numeric(tramo2030$speed)
tramo2030$at <- as.numeric(tramo2030$at)
rownames(tramo2030) <- NULL

tramo2040 <- as.data.frame(tramo2040)
tramo2040$idDisp <- as.character(tramo2040$idDisp)
tramo2040$speed <- as.numeric(tramo2040$speed)
tramo2040$at <- as.numeric(tramo2040$at)
rownames(tramo2040) <- NULL

tramo2050 <- as.data.frame(tramo2050)
tramo2050$idDisp <- as.character(tramo2050$idDisp)
tramo2050$speed <- as.numeric(tramo2050$speed)
tramo2050$at <- as.numeric(tramo2050$at)
rownames(tramo2050) <- NULL

tramo2060 <- as.data.frame(tramo2060)
tramo2060$idDisp <- as.character(tramo2060$idDisp)
tramo2060$speed <- as.numeric(tramo2060$speed)
tramo2060$at <- as.numeric(tramo2060$at)
rownames(tramo2060) <- NULL

tramo3040 <- as.data.frame(tramo3040)
tramo3040$idDisp <- as.character(tramo3040$idDisp)
tramo3040$speed <- as.numeric(tramo3040$speed)
tramo3040$at <- as.numeric(tramo3040$at)
rownames(tramo3040) <- NULL

tramo3050 <- as.data.frame(tramo3050)
tramo3050$idDisp <- as.character(tramo3050$idDisp)
tramo3050$speed <- as.numeric(tramo3050$speed)
tramo3050$at <- as.numeric(tramo3050$at)
rownames(tramo3050) <- NULL

tramo3060 <- as.data.frame(tramo3060)
tramo3060$idDisp <- as.character(tramo3060$idDisp)
tramo3060$speed <- as.numeric(tramo3060$speed)
tramo3060$at <- as.numeric(tramo3060$at)
rownames(tramo3060) <- NULL

tramo4050 <- as.data.frame(tramo4050)
tramo4050$idDisp <- as.character(tramo4050$idDisp)
tramo4050$speed <- as.numeric(tramo4050$speed)
tramo4050$at <- as.numeric(tramo4050$at)
rownames(tramo4050) <- NULL

tramo4060 <- as.data.frame(tramo4060)
tramo4060$idDisp <- as.character(tramo4060$idDisp)
tramo4060$speed <- as.numeric(tramo4060$speed)
tramo4060$at <- as.numeric(tramo4060$at)
rownames(tramo4060) <- NULL

tramo5060 <- as.data.frame(tramo5060)
tramo5060$idDisp <- as.character(tramo5060$idDisp)
tramo5060$speed <- as.numeric(tramo5060$speed)
tramo5060$at <- as.numeric(tramo5060$at)
rownames(tramo5060) <- NULL

write.csv(tramo1020, "Datasets/route/tramo1020.csv")
write.csv(tramo1030, "Datasets/route/tramo1030.csv")
write.csv(tramo1040, "Datasets/route/tramo1040.csv")
write.csv(tramo1050, "Datasets/route/tramo1050.csv")
write.csv(tramo1060, "Datasets/route/tramo1060.csv")
write.csv(tramo2030, "Datasets/route/tramo2030.csv")
write.csv(tramo2040, "Datasets/route/tramo2040.csv")
write.csv(tramo2050, "Datasets/route/tramo2050.csv")
write.csv(tramo2060, "Datasets/route/tramo2060.csv")
write.csv(tramo3040, "Datasets/route/tramo3040.csv")
write.csv(tramo3050, "Datasets/route/tramo3050.csv")
write.csv(tramo3060, "Datasets/route/tramo3060.csv")
write.csv(tramo4050, "Datasets/route/tramo4050.csv")
write.csv(tramo4060, "Datasets/route/tramo4060.csv")
write.csv(tramo5060, "Datasets/route/tramo5060.csv")
