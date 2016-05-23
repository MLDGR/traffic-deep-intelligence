#Transform original dataset of detections, in time series. (Measures in every seconds)
#@param dataset Dataset with UNIX time.
#@param dataset_vel_acum Dataset that will contain the acumulated values for every seconds. (It use their dimensions)
#@export Dataset that contain the acumulated values for every seconds.

# dataset=data
# dataset_vel_acum

conver_to_time_serie_in_secs<-function(dataset,dataset_vel_acum){
  t_min=dataset_vel_acum[1,1]
  col=which(dataset[i,1]==names(dataset_vel_acum))#Sensor colum
  
  dataset=dataset[c(3,7)]
  
  dataset_vel_acum=dataset_vel_acum[,2]
  
  for (i in 1:dim(dataset)[1]){
#     t=dataset[i,7]#Time to add
    t=dataset[i,2]#Time to add
#     t_mark=as.integer(dataset[i,3])
    t_mark=as.integer(dataset[i,1])#File in new dataset (UNIX TIME)
    
#     row=which(t_mark==dataset_vel_acum[,1])#Time
    
    row=(t_mark-t_min)+1
    
    t_num=as.integer(t)
    t_num_last=t-t_num
    
    if(t>1){
      dataset_vel_acum[row:(row+t_num)]=dataset_vel_acum[row:(row+t_num)]+1
      dataset_vel_acum[(row+t_num+1)]=dataset_vel_acum[(row+t_num+1)]+t_num_last
    }else{
      dataset_vel_acum[row]=dataset_vel_acum[row]+t
    }

  }
#   plot.ts(dataset_vel_acum)
  return(dataset_vel_acum)
}


#Deprecated for trans_sec_to_xx
#Acumulate data of detections (minute dataset)
#@param dataset_vel_acum Dataset that contain the acumulated values for every seconds.
#@export Dataset that contain the acumulated values for every minutes.
trans_sec_to_mins<-function(dataset_vel_acum){
  
  aux=seq(from=as.numeric(dataset_vel_acum[1,1]), to=as.numeric(dataset_vel_acum[length(dataset_vel_acum[,1]),1]), by=60)
  
  out=as.data.frame(matrix(data=0,nrow=length(aux), ncol = 2))
  out[,1]=aux
  
  out1=sapply(1:length(aux),function(x){
    sum(dataset_vel_acum[x:x+59,2])/60
  })
  
}


#Acumulate data of detections (1 minute dataset is usefull)
#@param dataset_vel_acum Dataset that contain the acumulated values for every seconds.
#@param interval Desired interval for accumulating output values. (60 for 1 minute intervals)
#@param time_axis Time axis desired time intervals for output. (Facilitates synchronization axes)
#@export Dataset that contain the acumulated values for every interval specified.

trans_sec_to_xx<-function(dataset_vel_acum,interval,time_axis){
  
#   aux=seq(from=as.numeric(dataset_vel_acum[1,1]), to=as.numeric(dataset_vel_acum[length(dataset_vel_acum[,1]),1]), by=interval)
  aux=time_axis
  
  out=as.data.frame(matrix(data=0,nrow=length(aux), ncol = 2))
  out[,1]=aux
  
  out1=sapply(0:(length(aux)-1),function(x){
    a=sum(dataset_vel_acum[(x*interval+1):(x*interval+interval),2])/interval
    })
  
}


#Obtain dataset of time series and export it.
#@param unix_dataset_name Name of the files that contains the dataset with UNIX time.
#@param time_to_process_in_secs Number of seconds to be reckoned from the time of the last record contained in the dataset for the processing performed. (60 * 60 * 24 * 7 * 8 equals 8 weeks)
#@param interval Desired interval for accumulating output values. (60 for 1 minute intervals)
#@export Name the exported file that contains the velocity values accumulated in the expecificado interval.
# 
# unix_dataset_name=dataset_UNIX_name
# time_to_process_in_secs=60*60*24*7*8
# interval=60

process_sensors<-function(unix_dataset_name, time_to_process_in_secs,interval){
  library(parallel)
  data=read.csv(file=unix_dataset_name)
  
  sensors=unique(data$idSensor)
  
  end_time=max(data$tfin)
  start_time=end_time-time_to_process_in_secs
  
  # Create unified_time_axis
  # time_axis=as.integer(seq(from=(as.integer(start_time)), to=(as.integer(end_time)),by=1))
  time_axis=(as.integer(start_time)-1):(as.integer(end_time)+1)
  
  time_axis_out=as.integer(seq(from=(as.integer(start_time)), to=(as.integer(end_time)),by=interval))
  
  sensors_mins_values=mclapply(sensors,function(sensor){
    #Sensor data
    print(sensor)
    data=data[which(data$idSensor==sensor),]
    
    #Selectioned data
    data=data[which(data$tinicio>=start_time),]
    
    if(dim(data)[1]==0){
      0
    }else{
      #Initialice output dataset
      dataset_vel_acum=as.data.frame(matrix(data=0,nrow=length(time_axis), ncol = 2))
      dataset_vel_acum[,1]=time_axis
      
      names(dataset_vel_acum)=c("UNIX_time",sensor)
      
      dataset_vel_acum_in_secs=conver_to_time_serie_in_secs(data,dataset_vel_acum)
      
      dataset_vel_acum_in_secs=as.data.frame(cbind(time_axis,dataset_vel_acum_in_secs))
      
      names(dataset_vel_acum_in_secs)=c("UNIX_time",sensor)
      
#       dataset_vel_acum_mins=trans_sec_to_min(dataset_vel_acum)
      
      dataset_vel_acum_interval=trans_sec_to_xx(dataset_vel_acum_in_secs,interval,time_axis_out)
      
      dataset_vel_acum_interval
    }
  },mc.cores = getOption("mc.cores", 1))

  #Convert to dataframe and export results
  dataset=as.data.frame(sensors_mins_values)

####

  time_axis_min=seq(from=as.integer(start_time),to=as.integer(end_time),by=interval)

  dataset=cbind(time_axis_min,dataset)
  names(dataset)=c("UNIX_time",sensors)
  
  #Add new string to classificate files
  export_modified_csv(dataset,unix_dataset_name,"TScomplete")
}