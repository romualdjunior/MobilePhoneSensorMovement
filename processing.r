## METHODS FOR THE PROCESSING ##

require(stringr)
require(RcppRoll)

#Extract_feature_long function
extract_feature_long=function(data,threeshold_mean,threeshold_sd,threeshold_time){
  
  #1-compute the norm of the acceleration in three dimension with the gravity removed\GN=|gFx^2+gFy^2+gFz^2-O.8|)
  
  
  data$gN<-NaN
  data$gN<- abs(((data$gFx)^2 + (data$gFy)^2 + (data$gFz)^2 -0.8))
  
  #2 compute the left rolling average \(gNm\) of \(gN\) over 10 measurements (see the RcppRoll package)
  
  data$gNm<-NaN
  gNm<-NaN
  gNm<-roll_mean(data$gN,n=10,align="left",na.rm = TRUE)
  nrow(gNm)
  data$gNm<-gNm[1:nrow(data)]
  
  
  # #3 left rolling standard deviation gNsd
  data$gNsd<-NaN
  gNsd<-NaN
  gNsd<-roll_sd(data$gN,n=10,align = "left",na.rm=TRUE)
  data$gNsd<-gNsd[1:nrow(data)]
  
  
  #4 data \(gNm > threeshold\_mean\) and \(gNsd > threeshold\_sd\)
  data<-data %>% filter(gNm>threeshold_mean |  gNsd>threeshold_sd )
  
  #
  # # 
  # #5compute \(dt_i = time_i-time_{i-1}\)
  data$dt<-NaN
  dt<-NaN
  for (i in seq(2,nrow(data))){
    dt[i]=data$time[i] - data$time[i-1]
  }
  data$dt<-dt
  # 
  
  # #6 pid for all groups where \(dt_i<threeshold\_time\ , \forall\, i \in g\)
  # 
  data$pid<-0
  
  
  for (i in seq(2,nrow(data))){
    if(data$dt[i]<threeshold_time){
      if(data$gNm[i]>threeshold_mean)
      {data$pid[i]=1}
      else if(data$gNsd[i]> threeshold_sd)
      {data$pid[i]=2}
      else
      {data$pid[i]<-3}
      
      
    }
    else data$pid[i]<-0
    
  }
  
  # # 
  return (data)
  
}

#Extract_feature_large function
extract_feature_large=function(data,threeshold_mean,threeshold_sd,threeshold_time){
  
  #1-compute the norm of the acceleration in three dimension with the gravity removed\GN=|gFx^2+gFy^2+gFz^2-O.8|)
  
  data$gN<-NaN
  data$gN<- abs(((data$gFx)^2 + (data$gFy)^2 + (data$gFz)^2 -0.8))
  
  #2 compute the left rolling average \(gNm\) of \(gN\) over 10 measurements (see the RcppRoll package)
  
  data$gNm<-NaN
  gNm<-NaN
  gNm<-roll_mean(data$gN,n=10,align="left",na.rm = TRUE)
  nrow(gNm)
  data$gNm<-gNm[1:nrow(data)]
  
  
  #3 left rolling standard deviation gNsd
  data$gNsd<-NaN
  gNsd<-NaN
  gNsd<-roll_sd(data$gN,n=10,align = "left",na.rm=TRUE)
  data$gNsd<-gNsd[1:nrow(data)]
  
  
  #4 data \(gNm > threeshold\_mean\) and \(gNsd > threeshold\_sd\)
  data<-data %>% filter(gNm>threeshold_mean |  gNsd>threeshold_sd )
  
  
  #
  #5compute \(dt_i = time_i-time_{i-1}\)
  data$dt<-NaN
  dt<-NaN
  for (i in seq(2,nrow(data))){
    data$dt[i]=data$time[i] - data$time[i-1]
  }
  
  
  
  
  #6 pid for all groups where \(dt_i<threeshold\_time\ , \forall\, i \in g\)
  
  data$pid<-0
  
  
  for (i in seq(2,nrow(data))){
    if(data$dt[i]<threeshold_time){
      if(data$gNm[i]>threeshold_mean)
      {data$pid[i]=1}
      else if(data$gNsd[i]> threeshold_sd)
      {data$pid[i]=2}
      else
      {data$pid[i]<-3}
      
      
    }
    
  }
  
  
  #7 #8 find all the groups of measurements with a duration between 1s and 3s and removethe other measurements
  
  data<-data %>% filter(time>=1 & time<=3)
  
  
  #9 create a feature \(localtime_i = time_i-min_{i\in g}(time_i)\)
  data$localtime<-NaN
  
  for (i in seq(1,nrow(data))){
    data$localtime[i]<-data$time[i]-min(data$time)
    
  }
  
  
  #10 create a feature tbin_i=floor(localtime_i*2)
  data$tbin<-NaN
  
  for (i in seq(1,nrow(data))){
    data$tbin[i]<-floor(data$localtime[i]*2)
    
  }
  
  
  
  #11 compute the mean for each group and each tbin of \(gFx,gFy,gFz,wx,wy,wz\)
  pids<-NaN
  pids<-unique(data$pid)
  gFx_mean_0<-gFx_mean_1<-gFx_mean_2<-gFy_mean_0<-gFy_mean_1<-gFy_mean_2<-gFz_mean_0<-gFz_mean_1<-gFz_mean_2<-wx_mean_0<-wx_mean_1<-wx_mean_2<-wy_mean_0<-wy_mean_1<-wy_mean_2<-wz_mean_0<-wz_mean_1<-wz_mean_2<-NaN
  for (i in seq(1,length(pids))){
    gFx_mean_0[i]<- mean((data %>% filter(tbin==0 | pid==pids[i]) %>% select(gFx))$gFx)
    gFx_mean_1[i]<- mean((data %>% filter(tbin==1 | pid==pids[i]) %>% select(gFx))$gFx)
    gFx_mean_2[i]<- mean((data %>% filter(tbin==2 | pid==pids[i]) %>% select(gFx))$gFx)
    gFy_mean_0[i]<- mean((data %>% filter(tbin==0 | pid==pids[i]) %>% select(gFy))$gFy)
    gFy_mean_1[i]<- mean((data %>% filter(tbin==1 | pid==pids[i]) %>% select(gFy))$gFy)
    gFy_mean_2[i]<- mean((data %>% filter(tbin==2 | pid==pids[i]) %>% select(gFy))$gFy)
    gFz_mean_0[i]<- mean((data %>% filter(tbin==0 | pid==pids[i]) %>% select(gFz))$gFz)
    gFz_mean_1[i]<- mean((data %>% filter(tbin==1 | pid==pids[i]) %>% select(gFz))$gFz)
    gFz_mean_2[i]<- mean((data %>% filter(tbin==2 | pid==pids[i]) %>% select(gFz))$gFz)
    wx_mean_0[i]<- mean((data %>% filter(tbin==0  | pid==pids[i]) %>% select(wx))$wx)
    wx_mean_1[i]<- mean((data %>% filter(tbin==1 |  pid==pids[i]) %>% select(wx))$wx)
    wx_mean_2[i]<- mean((data %>% filter(tbin==2 |  pid==pids[i]) %>% select(wx))$wx)
    wy_mean_0[i]<- mean((data %>% filter(tbin==0 |  pid==pids[i]) %>% select(wy))$wy)
    wy_mean_1[i]<- mean((data %>% filter(tbin==1 |  pid==pids[i]) %>% select(wy))$wy)
    wy_mean_2[i]<- mean((data %>% filter(tbin==2 |  pid==pids[i]) %>% select(wy))$wy)
    wz_mean_0[i]<- mean((data %>% filter(tbin==0 |  pid==pids[i]) %>% select(wz))$wz)
    wz_mean_1[i]<- mean((data %>% filter(tbin==1 |  pid==pids[i]) %>% select(wz))$wz)
    wz_mean_2[i]<- mean((data %>% filter(tbin==2 |  pid==pids[i]) %>% select(wz))$wz)
  }
  
  # 
  # #12 transform the data to large format where each row is a group and each column a sensor mean over a time bin
  
  transform_data<-NaN
  pid<-rep(0,length(pids))
  for (i in seq(1,length(pids))){
    pid[i]=pids[i]
  }
  transform_data<-data.frame(pid,gFx_mean_0,gFx_mean_1,gFx_mean_2,gFy_mean_0,gFy_mean_1,gFy_mean_2,gFz_mean_0,
                             gFz_mean_1,gFz_mean_2,wx_mean_0,wx_mean_1,wx_mean_2,wy_mean_0,wy_mean_1,wy_mean_2,
                             wz_mean_0,wz_mean_1,wz_mean_2)
  
  
  transform_data
  
  
}

## OTHER METHOD ##


#function to convert character dataset with ',' to a numeric dataset
# always use it before calling the processing functions
clean_dataframe<-function(data){
  #the columns X should be  already dropped here
  drops<-c("X")
  df=data.frame()
  df<-data[,!(names(data) %in% drops )]
  
  
  #changing the decimal comma , in numbers into . and converting the characters into numeric
  df<-data.frame(sapply(df,function(x) sub(",",".",x)))
  df<-data.frame(sapply(df,function(x) as.numeric(x)))
  
  return (df)
}


#function for the filtering functionality of the datatable

datatable_condition=function (df,filter_text){
  condition=""
  if(str_detect(filter_text,">")) condition=">"
  if(str_detect(filter_text,">=")) condition=">="
  if(str_detect(filter_text,"<")) condition="<"
  if(str_detect(filter_text,"<="))condition="<="
  if(str_detect(filter_text,"=="))condition="=="
  variable=unlist(strsplit(filter_text, condition)[1])[1]
  value=unlist(strsplit(filter_text, condition)[1])[2]
  if (condition==">") return (df %>% filter(get(variable)>value))
  else if (condition==">=") return (df %>% filter(get(variable)>=value))
  else if (condition=="<") return (df %>% filter(get(variable)<value))
  else if (condition=="<=") return (df %>% filter(get(variable)<=value))
  else if (condition=="==") return (df %>% filter(get(variable)==value))
  
  
}


df=read.csv("FORCE-G_LINEAR_ACCELERATOR_GYROSCOPE_INPUT_N.csv",sep=";")
df=clean_dataframe(df)
df
df=extract_feature_large(df,0.1,0,0.5)
