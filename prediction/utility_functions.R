#Add new string to classificate files
#@param data Object to export.
#@param last_name Name of the file containing the data on which it has worked.
#@param new_string characteristic name that summarizes the operations performed on the dataset to be exported. (The objective is to facilitate recognition of each dataset)
#@export The name of the exported file.

export_modified_csv<-function(data,last_name,new_string){
  split=unlist(strsplit(last_name, split="\\."))
  if(grep("Datasets/",last_name)==1){
    out_file_name=paste(paste(split[1],new_string,sep="_"),split[2],sep=".")
  }else{
    out_file_name=paste("Datasets/",paste(paste(split[1],new_string,sep="_"),split[2],sep="."),sep="")
  }
  
  write.csv(data, file=out_file_name,row.names=FALSE)
  return (out_file_name)
}