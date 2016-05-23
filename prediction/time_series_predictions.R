#Obtains predictions 1-step-ahead for different structs of data
#@param data_1min_filenameName the files obtained from the "process sensors ()" function that contain the processed data from all sensors sampled every second. 
#This function generates an output file for prediction with 3 columns, which contain the time axis in UNIX time, the actual data used in training the model prediction and the set of output data contains the actual data to the penultimate prediction point and the last point (test), respectively.
predict_test<-function(data_1min_filename){
  data_1min_r=read.csv(data_1min_filename)
  
  sensors=names(data_1min_r[,-1])
  
  mclapply(2:(length(sensors)+1),function(x){
    data_1min=data_1min_r[,x]
    time_1min=data_1min_r[,1]
    
    #Dataset modified / transformed / accumulated in intervals of 10, 15, 30, 45 and 60 minutes
    data_10min <- apply(matrix(data_1min[1:((length(data_1min)%/%10)*10)], ncol=10, byrow=TRUE),1,mean)
    data_15min <- apply(matrix(data_1min[1:((length(data_1min)%/%15)*15)], ncol=15, byrow=TRUE),1,mean)
    data_30min <- apply(matrix(data_1min[1:((length(data_1min)%/%30)*30)], ncol=30, byrow=TRUE),1,mean)
    data_45min <- apply(matrix(data_1min[1:((length(data_1min)%/%45)*45)], ncol=45, byrow=TRUE),1,mean)
    data_60min <- apply(matrix(data_1min[1:((length(data_1min)%/%60)*60)], ncol=60, byrow=TRUE),1,mean)
    
    time_10min <- apply(matrix(time_1min[1:((length(time_1min)%/%10)*10)], ncol=10, byrow=TRUE),1,mean)
    time_15min <- apply(matrix(time_1min[1:((length(time_1min)%/%15)*15)], ncol=15, byrow=TRUE),1,mean)
    time_30min <- apply(matrix(time_1min[1:((length(time_1min)%/%30)*30)], ncol=30, byrow=TRUE),1,mean)
    time_45min <- apply(matrix(time_1min[1:((length(time_1min)%/%45)*45)], ncol=45, byrow=TRUE),1,mean)
    time_60min <- apply(matrix(time_1min[1:((length(time_1min)%/%60)*60)], ncol=60, byrow=TRUE),1,mean)
    
    list_names=list("data_10min","data_15min","data_30min","data_45min","data_60min")
    list_datasets=list(data_10min,data_15min,data_30min,data_45min,data_60min)
    list_time=list(time_10min,time_15min,time_30min,time_45min,time_60min)
    
    #Predictions 1-step-ahead for all new datasets
    mclapply(1:length(list_datasets),function(y){
      
      data=list_datasets[[y]]
      #      train_n=1:as.integer(length(data)*0.8)
      #      test_n=(length(train_n)+1):(length(data))
      
      train=data[-length(data)]
      test=data[length(data)]
      
      fit=auto.arima(train)
      preds=forecast(fit,h=length(test))
      
      aux=list_datasets[[y]]
      aux[length(aux)]=as.numeric(preds[[4]])
      
      output_data=as.data.frame(cbind(list_time[[y]],list_datasets[[y]],aux))
      names(output_data)=c("Unix_Time","Real_Data","Predicted_data")
      
      export_modified_csv(output_data,data_1min_filename,paste(list_names[[y]],"predicted_1_step_ahead",sep="_"))
      })
    
    })
  NULL
  }