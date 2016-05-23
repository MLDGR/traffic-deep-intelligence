#Transform time (Date) to UNIX Time (It facilitates calculations)
#@param time A date register of the original dataset
#@export Returns in UNIX time format the date entered in the time parameter.
time_unix<-function(time){
  time=as.character(time)
  a=unlist(strsplit(time, split="[T.]"))
  time_out=as.numeric(as.POSIXct(paste(a[1],a[2],sep=" ")))
  a[3]=gsub('[^0-9]+', '', a[3])
  time_out=as.numeric(time_out)+as.numeric(a[3])/1000.0
}

#Obtain and export the dataset with Unix Time conversion.
#@param dataset_name Name of the file containing the original dataset to be processed.
#@param colums Vector containing the position of the columns containing the time values of interest of the original dataset.
#@export The name of the exported file.
data_time_unix<-function(dataset_name,colums){
  library(parallel)
  data=read.csv(file=dataset_name)
  aux=mclapply(colums,function(x){
    data[,x]<<-unlist(mclapply(data[,x],time_unix))
  },mc.cores = getOption("mc.cores", 1))
  
  for(i in 1:length(colums)){
    data[,colums[i]]<-aux[i]
  }
  
  return (export_modified_csv(data,dataset_name,"UNIX"))

}
