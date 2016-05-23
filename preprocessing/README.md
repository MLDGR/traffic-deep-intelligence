# Data Generation
Data loading:
```R
dispositivos <- read_csv("Datasets/dispositivos.csv")
pasos <- read_csv("Datasets/pasos.csv")
sensores <- read_csv("Datasets/sensores.csv")
```
Add detection time variable:
```R
pasosTfiltered <- transform(pasos, tiempo = pasos$tfin - pasos$tinicio)
```
Change variable name hex(idDispositivo) to idDisp:
```R
colnames(dispositivos)[1] <- "idDisp"
colnames(pasosTfiltered)[2] <- "idDisp"
```
Merge data by id:
```R
pasosTDisp <- merge(pasosTfiltered, dispositivos, by="idDisp")
pasosFull <- merge(pasosTDisp, sensores, by="idSensor")
```
Write the resulting dataset:
```R
write_csv(pasosFull, "Datasets/pasosfull.csv")
```
