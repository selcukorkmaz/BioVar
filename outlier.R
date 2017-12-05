
outlier <- function(data, analyte, subject, replicate, time, gender){
      
  
  data$replicate.subject = as.numeric(paste0(data[,replicate], data[,subject]))
  
  
  names = c(subject, gender, time, replicate, "replicate.subject", analyte)
  
  
  dataFull = data[,names]
  names(dataFull)[6] = "value"
  head(dataFull)
  
  dataFull = dataFull[complete.cases(dataFull),]
  table(dataFull$value <=0)
  dataFull[dataFull$value <=0,"value"] = NA
  
  dataFull = dataFull[complete.cases(dataFull),]
  head(dataFull)
  dim(dataFull)
  
  tbl = table(dataFull$subject)
  
  naSubject = tbl[!(tbl%%2==0)]
  naSubject2 = as.numeric(names(naSubject))
  
  if(length(naSubject2) >0){
    for(i in 1:length(naSubject2)){
      
      nas3 = dataFull[dataFull$subject == naSubject2[i],]
      
      tbl2 = table(nas3$time)
      
      nas4 = tbl2[!(tbl2%%2==0)]
      nas5 = as.numeric(names(nas4))
      
      for(j in 1:length(nas5)){
        
        dataFull[dataFull$subject == naSubject2[i] & dataFull$time == nas5[j],"value"] = NA
        
        
      }
      
    }
    
  }
  
  dataFull = dataFull[complete.cases(dataFull),]
  head(dataFull)
  dim(dataFull)
  
  dataFull = dataFull[order(dataFull[,gender], decreasing = FALSE), ]
  
  
  
  
  ### STEP 1 #####
  
  value = 1
  criticalValue = 0
  
  outlierDataList = list()
  
  j = 0
  outlierList = list()
  
  while(value > criticalValue){
    
    j=j+1
    dataFull = dataFull[complete.cases(dataFull),]
    
    dataList = list()
    
    for(i in 1:length(levels(as.factor(dataFull$subject)))){
      
      
      print(paste("Subject:", i))
      data = dataFull[dataFull$subject == i,]
      data = data[complete.cases(data),]
      
      rep1 = data[data$replicate.subject == unique(data$replicate.subject)[1],"value"]
      rep2 = data[data$replicate.subject == unique(data$replicate.subject)[2],"value"]
      
      repData = cbind.data.frame(rep1,rep2)
      
      repVar = apply(repData, 1, var)
      
      subject = rep(i, length(rep1))
      time = unique(data$time)
      subject_time = paste0(subject, time)
      
      reps = cbind.data.frame(subject, replicate.subject = data$replicate.subject, time = unique(data$time),subject_time, rep1, rep2, var = repVar)
      
      dataList[[i]] = reps
      
      
    }
    
    dataNew = do.call(rbind.data.frame, dataList)
    head(dataNew)
    subjectNumber = nrow(dataNew)
    df = 2
    
    vars = dataNew$var
    names(vars) = paste0(dataNew$subject, dataNew$time)
    value <- max(vars)/sum(vars)
    
    criticalValue = prospectr:::Cul(0.05, df, subjectNumber)
    
    if(value > criticalValue){
      
      outlierDataList[[dataNew[which.max(vars),"subject"]]] = dataNew[which.max(dataNew$var),]
      outlierSubject = dataNew[which.max(vars),"subject"]
      outlierTime = dataNew[which.max(vars),"time"]
      
      dataFull[dataFull$subject==outlierSubject & dataFull$time ==outlierTime,][,"value"] = c(NA, NA)
      
      outlierList[[j]] = dataNew[which.max(dataNew$var),]
      
      dataFull = dataFull[complete.cases(dataFull),]
      
    }
    
  }
  
  dataAfterStep1 = dataFull
  
  outlierStep1 = do.call(rbind.data.frame, outlierList)
  
  
  ### STEP 2 ####
  
  
   dataFull = dataFull[!(rownames(dataFull) %in% rownames(dataFull[dataFull$subject %in% outlierStep1$subject,])), ]
  
  dataFull2 = dataFull
  value = 1
  criticalValue = 0
  
  outlierDataList2 = list()
  
  while(value > criticalValue){
    
    # dataFull3 = dataFull2[,c("subject", "value")]
    
    vars =  tapply(dataFull2$value, as.factor(dataFull2$subject), var)
    #names(vars) = unique(dataFull3$subject)
    
    subjectNumber = length(vars)
    df = length(unique(dataFull2$time))-1
    
    value <- max(vars)/sum(vars)
    
    criticalValue = prospectr:::Cul(0.05, df, subjectNumber)
    
    
    if(value > criticalValue){
      
      outlierSubject = dataFull2[dataFull2$subject == names(which.max(vars)),]
      
      
      
      outlierDataList2[[outlierSubject$subject[1]]] = outlierSubject
      
      dataFull2 = dataFull2[!(rownames(dataFull2) %in% rownames(outlierSubject)), ]
      
      cat("\nOutlier Found", "\nValue", value, "\nCritical Value:", criticalValue, "\n")
      cat("subject",outlierSubject$subject[1], "is outlier\n")
    }else{
      
      cat("\nNo Outlier Found\n")
      
    }
    
  }
  
  # outlierStep2 = do.call(rbind.data.frame, outlierDataList2)
  
  outlierStep2 = dataFull[!(dataFull$subject %in% dataFull2$subject),]
  
  dataAfterStep2 = dataFull2
  
  ### STEP 3 ####
  
  dataReed = dataFull2
  analyteValue = dataReed$value
  subjects = as.factor(dataReed$subject)
  
  means <- tapply(analyteValue, subjects, mean)
  names(means) <- levels(subjects)
  
  
  maxDifference = 1
  absoluteDifference = 1

  while(maxDifference > absoluteDifference/3 && length(means) > 0){
    outlierListReed = list()
    
    
    ## High-value outliers
    order = sort(means, decreasing = TRUE)
    maxDifference = abs(diff(order[1:2]))
    absoluteDifference = abs((diff(range(order))))
    
    
    if(maxDifference > absoluteDifference/3){
      
      outlier = order[1]
      outSubject = names(outlier)
      
      outlierListReed[[as.numeric(outSubject)]] = data.frame(subject = outSubject, threshold = round(absoluteDifference/3, 3), maxDifference = round(maxDifference, 3), Outlier = "High-value outlier")
      
      cat("subject", outSubject, "is a high-value outlier \n-------------------------------------------- \nMoving on to check low-value outlier \n--------------------------------------------")
      
    }else{
      
      cat("\nNo high-value outlier\n")
    }
    
    
    order = sort(means, decreasing = FALSE)
    maxDifference = abs(diff(order[1:2]))
    absoluteDifference = abs((diff(range(order))))
    
    if(maxDifference > absoluteDifference/3){
      
      outlier = order[1]
      outSubject = names(outlier)
      outlierListReed[[as.numeric(outSubject)]] = data.frame(subject = outSubject, threshold = round(absoluteDifference/3, 3), maxDifference = round(maxDifference, 3), Outlier = "Low-value outlier")
      
      cat("subject", outSubject, "is a low-value outlier \n-------------------------------------------- \nOutlier detection process is successfully completed \n--------------------------------------------")
    }else{
      
      cat("\nNo low-value outlier\n" )
    }
    
    outlierListReed = do.call(rbind.data.frame, outlierListReed)
    
    if(nrow(outlierListReed) > 0){
      # outlierSubjectsReed = do.call(rbind.data.frame,outlierListReed)
      # names(outlierSubjectsReed) = "Subjects"
      
      dataReed = dataReed[!(dataReed$subject %in%  outlierListReed),]
      
      means = means[!(names(means) %in% outlierListReed$subject)]
      
    }else{
      
      dataReed = dataReed
      
    }
  }
  
  cat("\n------------------------------------","\nStep 3 is succesfully completed. All outliers found and removed from dataset.","\n------------------------------------")
  
  dataWithoutOutliers = dataReed
  
  
  if(nrow(outlierStep1) > 0){
    outlierStep1 = outlierStep1[,-c(2,4)]
    names(outlierStep1) = c("Subject", "Time", "1st replication", "2nd replication", "Variance")
  }else{
    
    outlierStep1 = cbind.data.frame(Note = "No outliers found.")
    
  }
  
  if(nrow(outlierStep2) > 0){
    outlierStep2 = outlierStep2[,-5]
    names(outlierStep2) = c("Subject", "Gender", "Time", "Replicate", "Value")
    rownames(outlierStep2) = NULL
  }else{
    
    outlierStep2 = cbind.data.frame(Note = "No outliers found.")
    
  }
  
  if(nrow(outlierListReed) > 0){
    
    outlierStep3 = outlierListReed
    names(outlierStep3) = c("Subject", "Threshold", "Maximum difference", "Outlier")
    
    
  }else{
    
    outlierStep3 = cbind.data.frame(Note = "No outliers found.")

  }
    
    
  
  outlierResult = list(step1 = outlierStep1, step2 = outlierStep2, step3 = outlierStep3, dataWithoutOutliers = dataWithoutOutliers, dataAfterStep1 = dataAfterStep1, dataAfterStep2 = dataAfterStep2)
  
  outlierResult
  
  
        
  }



        
        
        
