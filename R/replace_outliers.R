replace_outliers <-
function(cdata,replace.by.NA=FALSE,iqr.multiplier=1.5,replace.by.median=FALSE){
  data_sum<-summary(cdata,na.rm=TRUE)
  iqr_value<-data_sum[5]-data_sum[2]
  upper_limit<-data_sum[5]+iqr.multiplier*iqr_value
  lower_limit<-data_sum[2]-(iqr.multiplier*iqr_value)
  
  if(replace.by.median==TRUE){
    
 
      cdata[which(cdata<lower_limit)]<-NA
      cdata[which(cdata>upper_limit)]<-NA
    
      data_median<-median(cdata,na.rm=TRUE)
      
      cdata[which(is.na(cdata)==TRUE)]<-data_median
        
  }else{
    
        
        if(is.na(iqr_value)==FALSE){
          #if(iqr_value>0)
            {
            if(length(which(cdata<lower_limit)==TRUE)>0){
              if(replace.by.NA==TRUE){
                cdata[which(cdata<lower_limit)]<-NA
              }else{
                cdata[which(cdata<lower_limit)]<-lower_limit #min(cdata[which(cdata>lower_limit & cdata<upper_limit)],na.rm=TRUE)
                
              }
            }
            
            if(length(which(cdata>upper_limit)==TRUE)>0){
              
              if(replace.by.NA==TRUE){
                cdata[which(cdata>upper_limit)]<-NA
              }else{
                
                cdata[which(cdata>upper_limit)]<-upper_limit #max(cdata[which(cdata>lower_limit & cdata<upper_limit)],na.rm=TRUE)
              }
            }
          }
        }
    
  }
  
  return(cdata)
}
