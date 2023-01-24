### SHINY SERVER ###
source("processing.r")
require(RcppRoll)


server = function(input, output, session) {
  
  #variable for displaying the dataset
  output$contents <- renderTable({
    
   
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = ";",
                       quote = input$quote)
        df=clean_dataframe(df)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp=="head")
      head(df)
    else df
    
    
  }) 
  
  #display the summary of the dataset
  output$summary <- renderPrint({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = ";",
                          quote = input$quote)
    df=clean_dataframe(df)
    
    summary(df)
  })
  
  #display the str of the _long or _large features
  output$str <- renderPrint({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = ";",
                          quote = input$quote)
    df=clean_dataframe(df)
    
    
    
    if(input$threeshold_mean!="" & input$threeshold_sd!="" & input$threeshold_time!="")
    {
      if(input$feature=="Long")
      {
        return(str(extract_feature_long(df,as.numeric(input$threeshold_mean),as.numeric(input$threeshold_sd),as.numeric(input$threeshold_time))))
      }
      else if(input$feature=="Large")
      {
        return(str(extract_feature_large(df,as.numeric(input$threeshold_mean),as.numeric(input$threeshold_sd),as.numeric(input$threeshold_time))))
        
      }
      
    }
    
  })
  
  #render the ggplot of the long and large features
  output$ggplot<-renderPlot({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = ";",
                          quote = input$quote)
    df=clean_dataframe(df)
    
    if(input$threeshold_mean!=""& input$threeshold_sd!="" & input$threeshold_time!="")
    {
      if(input$feature=="Long")
      {
        X_long=extract_feature_long(df,as.numeric(input$threeshold_mean),as.numeric(input$threeshold_sd),as.numeric(input$threeshold_time))
        ggplot(X_long %>% filter(pid>0)) + geom_line(aes(x=time,y=gNsd,color=as.factor(pid),group=pid)) + geom_point(data=X_long %>% filter(pid==00),aes(x=time,y=gNsd,color=as.factor(pid),group=pid)) 
        
        
      }
      else if(input$feature=="Large")
      {
        X_large=extract_feature_large(df,as.numeric(input$threeshold_mean),as.numeric(input$threeshold_sd),as.numeric(input$threeshold_time))
        ggplot(X_large) + geom_point(data=X_large,aes(x=pid,y=gFx_mean_0,group=pid),color="yellow")+
          geom_point(data=X_large,aes(x=pid,y=gFx_mean_1,group=pid),color="yellow")  + geom_point(data=X_large,aes(x=pid,y=gFx_mean_2,group=pid),color="yellow")+
          geom_point(data=X_large,aes(x=pid,y=gFy_mean_0,group=pid),color="red") +geom_point(data=X_large,aes(x=pid,y=gFy_mean_1,group=pid),color="red")+
          geom_point(data=X_large,aes(x=pid,y=gFy_mean_2,group=pid),color="red") + geom_point(data=X_large,aes(x=pid,y=gFz_mean_0,group=pid),color="white")+
          geom_point(data=X_large,aes(x=pid,y=gFz_mean_1,group=pid),color="white") + geom_point(data=X_large,aes(x=pid,y=gFz_mean_2,group=pid),color="white")+
          geom_point(data=X_large,aes(x=pid,y=wx_mean_0,group=pid),color="blue") + geom_point(data=X_large,aes(x=pid,y=wx_mean_1,group=pid),color="blue")+
          geom_point(data=X_large,aes(x=pid,y=wx_mean_2,group=pid),color="blue")+geom_point(data=X_large,aes(x=pid,y=wy_mean_0,group=pid),color="orange")+
          geom_point(data=X_large,aes(x=pid,y=wy_mean_1,group=pid),color="orange")+ geom_point(data=X_large,aes(x=pid,y=wy_mean_2,group=pid),color="orange")+
          geom_point(data=X_large,aes(x=pid,y=wz_mean_0,group=pid),color="violet")+geom_point(data=X_large,aes(x=pid,y=wz_mean_1,group=pid),color="violet")+
          geom_point(data=X_large,aes(x=pid,y=wz_mean_2,group=pid),color="violet")+ xlab("Pid")+ylab("All Means")+
          ggtitle("ALL THE MEANS BY GROUP, ONE COLOR")+guides(colour = "legend", size = "legend")
        
      }
      
    }
    
    
  })
  
  #Description for each typoe of movement or dataset
  
  output$description <- renderText({
    
    
    if(input$type_dataset=="X_movement") paste("This dataset is made up of all the 3 sensors coordinates for the X Movement")
    
    else if(input$type_dataset=="O_movement") paste("This dataset is made up of all the 3 sensors coordinates for the O Movement")
    
    else paste("This dataset is made up of all the 3 sensors coordinates for the Noise")
    
    
    
  })
  
  #Display the datatable with its filtering option
  output$mytable1 <- DT::renderDataTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = ";",
                          quote = input$quote)
    df=clean_dataframe(df)
    
    if(input$filter_text!="" && input$column=="all") {
      print("ici")
      final_df=datatable_condition(df,input$filter_text)
      DT::datatable(final_df)
    }
    else if(input$filter_text!="" && input$column!="all") {
      print("ici ici")
      final_df=datatable_condition(df,input$filter_text)
      DT::datatable(final_df[input$column])
    }
    else if(input$filter_text=="" && input$column!="all") {
      print("ici ici ici")
      
      DT::datatable(df[input$column])
    }
    else DT::datatable(df)
  })
  
  #Display the visualisation in 3d of each sensor
  
  output$visualisation<-renderPlotly ({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = ";",
                          quote = input$quote)
    
    df=clean_dataframe(df)
    
    if(input$sensor=="G-ForceMeter")
      plot_ly(data = df,x=~gFx,y=~gFy,z=~gFz,color = I("blue"),height = 700)
    else if(input$sensor=="Accelerometer")
      plot_ly(data = df,x=~ax,y=~ay,z=~az,color = I("green"),height = 700)
    else if(input$sensor=="Gyroscope")
      plot_ly(data = df,x=~wx,y=~wy,z=~wz,color = I("red"),height = 700)
    
    
  }) 
  
  #display the visualisation of the processing 
  output$processing_visualisation<-renderPlotly ({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = ";",
                          quote = input$quote)
    
    df=clean_dataframe(df)
    
    if(input$threeshold_mean!=""& input$threeshold_sd!="" & input$threeshold_time!="")
    {
      if(input$feature=="Long"){
        x_long=extract_feature_long(df,as.numeric(input$threeshold_mean),as.numeric(input$threeshold_sd),as.numeric(input$threeshold_time))
        if(input$sensor_2==1)
          plot_ly(data = x_long,x=~gFx,y=~gFy,z=~gFz,color = I("blue"),height = 400)
        else if(input$sensor_2==2)
          plot_ly(data = x_long,x=~ax,y=~ay,z=~az,color = I("green"),height = 400)
        else if(input$sensor_2==3)
          plot_ly(data = x_long,x=~wx,y=~wy,z=~wz,color = I("red"),height = 400)
      }
      
    }
    
  })
  
  #allow us to download the output files with the group and labels as entries 
  output$download_data <- downloadHandler(
    
    filename = function() {
      name=""
      if(input$type_dataset=="X_movement")
      {
        if(input$feature=="Long")
          name="FORCE-G_LINEAR_ACCELERATOR_GYROSCOPE_OUTPUT_LONG_X"
        else if(input$feature=="Large")
          name="FORCE-G_LINEAR_ACCELERATOR_GYROSCOPE_OUTPUT_LARGE_X"
      }
        
      
      else if(input$type_dataset=="O_movement")
      {
        if(input$feature=="Long")
          name="FORCE-G_LINEAR_ACCELERATOR_GYROSCOPE_OUTPUT_LONG_O"
        else if(input$feature=="Large")
          name="FORCE-G_LINEAR_ACCELERATOR_GYROSCOPE_OUTPUT_LARGE_O"
      }
      
      
      else if(input$type_dataset=="Noise")
      {
        if(input$feature=="Long")
          name="FORCE-G_LINEAR_ACCELERATOR_GYROSCOPE_OUTPUT_LONG_N"
        else if(input$feature=="Large")
          name="FORCE-G_LINEAR_ACCELERATOR_GYROSCOPE_OUTPUT_LARGE_N"
      }
      
      paste(name, ".csv", sep = ";")
    },
    content = function(file) {
      
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = ";",
                     quote = input$quote)
      
      df=clean_dataframe(df)
      if(input$group_1!=""& input$group_2!=""){
        if(input$pid_1!=""& input$pid_2!="" & input$pid_3!=""){
          if(input$feature=="Long")
          {
            df=extract_feature_long(df,as.numeric(input$threeshold_mean),as.numeric(input$threeshold_sd),as.numeric(input$threeshold_time))
            df$group<-NaN
            df$label<-NaN
            for (i in seq(1,nrow(df))){
              if(df$pid[i]==1)
                df$label[i]=input$pid_1
              else if(df$pid[i]==2)
                df$label[i]=input$pid_2
              else if(df$pid[i]==3)
                df$label[i]=input$pid_3
              else 
                df$label[i]="N"
              
              
              if(df$gNm[i]>input$threeshold_mean)
                df$group[i]=input$group_1
              else if(df$gNsd[i]>input$threeshold_sd)
                df$group[i]=input$group_2
            }
            
          }
          
          else if(input$feature=="Large")
          {
            df=extract_feature_large(df,as.numeric(input$threeshold_mean),as.numeric(input$threeshold_sd),as.numeric(input$threeshold_time))
            df$group<-NaN
            df$label<-NaN
            gN<-NaN
            gN<- abs((((df$gFx_mean_0 + df$gFx_mean_1 +df$gFx_mean_2)/3 )^2 + ((df$gFy_mean_0 + df$gFy_mean_1 +df$gFy_mean_2)/3 )^2 + ((df$gFz_mean_0 + df$gFz_mean_1 +df$gFz_mean_2)/3 )^2 -0.8))
            
            gNm<-NaN
            gNm<-roll_mean(gN,align="left",na.rm = TRUE)
            
            gNsd<-NaN
            gNsd<-roll_sd(gN,align = "left",na.rm=TRUE)
            
            for (i in seq(1,nrow(df))){
              if(df$pid[i]==1)
                df$label[i]=input$pid_1
              else if(df$pid[i]==2)
                df$label[i]=input$pid_2
              else if(df$pid[i]==3)
                df$label[i]=input$pid_3
              else
                df$label[i]="N"
              
              if(gNm[i]>input$threeshold_mean)
                df$group[i]=input$group_1
              else if(gNsd[i]>input$threeshold_sd)
                df$group[i]=input$group_2
            }
          }
          
          write.csv(df, file, row.names = FALSE)
          
          
        }
      }
      
    }
  )
}
