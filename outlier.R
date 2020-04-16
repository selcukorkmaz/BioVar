
outlier <- function(data, analyte, subject, replicate, time, gender, decimal, 
                    outlierS1 = "replicates", outlierS2 = TRUE, outlierS3 = TRUE, 
                    showResult){
      
  
  data$replicate.subject = as.numeric(paste0(data[,replicate], data[,subject]))

  names = c(subject, gender, time, replicate, "replicate.subject", analyte)

  dataFull = data[,names]
  names(dataFull)[6] = "value"
  head(dataFull)
  
  dataFull = dataFull[complete.cases(dataFull),]
  table(dataFull$value <=0)
  
   # if(!(showResult == "mom" || showResult == "lnmom")){
   #   dataFull[dataFull$value <=0,"value"] = NA
   # }
  
  dataFull = dataFull[complete.cases(dataFull),]
  head(dataFull)
  dim(dataFull)
  
  tbl = table(dataFull[,subject])
  
  naSubject = tbl[!(tbl%%2==0)]
  naSubject2 = as.numeric(names(naSubject))
  
  if(length(naSubject2) >0){
    for(i in 1:length(naSubject2)){
      
      nas3 = dataFull[dataFull[,subject] == naSubject2[i],]
      
      tbl2 = table(nas3[,time])
      
      nas4 = tbl2[!(tbl2%%2==0)]
      nas5 = as.numeric(names(nas4))
      
      for(j in 1:length(nas5)){
        
        dataFull[dataFull[,subject] == naSubject2[i] & dataFull[,time] == nas5[j],"value"] = NA
        
        
      }
      
    }
    
  }
  
  dataFull = dataFull[complete.cases(dataFull),]
  head(dataFull)
  dim(dataFull)
  
  dataFull = dataFull[order(dataFull[,gender], decreasing = FALSE), ]
  
  dataOrg = dataFull
  dataOrg$subject_time = paste0(dataOrg[,subject],dataOrg[,time])
  
  
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
    
    s = unique(dataFull[,subject])
    
    for(i in 1:length(levels(as.factor(dataFull[,subject])))){
      
      print(paste("Subject:", s[i]))
      data = dataFull[dataFull[,subject] == s[i],]
      data = data[complete.cases(data),]
      
      sData = split(data, data[,time])
      repList = list()
      
      for(d in 1:length(sData)){
        
        tmp = sData[[d]]
        r1=NA
        r2=NA
        
        if(nrow(tmp) == 2){
          r1 = tmp[1,"value"]
          r2 = tmp[2,"value"]
          
        }
        
        if(nrow(tmp) == 1){
          
          if(tmp[,time][1] == 1){
            
            r1 = tmp[1,"value"]
            r2=NA
          
          }
          
          if(tmp[,time][1] == 2){
            
            r2 = tmp[1,"value"]
            r1=NA
            }
          
          
        }
        
        repList[[d]] = cbind.data.frame(rep1 = r1, rep2 = r2)

      }
      
      repData = do.call(rbind.data.frame, repList)
      
      # rep1 = data[data$replicate.subject == unique(data$replicate.subject)[1],"value"]
      # rep2 = data[data$replicate.subject == unique(data$replicate.subject)[2],"value"]
      # 
      # repData = cbind.data.frame(rep1,rep2)
      
      repVar = apply(repData, 1, var)
      
      # subject = rep(i, length(rep1))
      subject_time = paste0(rep(s[i], nrow(repData)), unique(data[,time]))
      
      reps = cbind.data.frame(subject = rep(dataFull[dataFull[,subject] == s[i],subject][[1]], 
                                            length(subject_time)), time = unique(data[,time]),subject_time, 
                              repData$rep1, repData$rep2, var = repVar)
      
      dataList[[i]] = reps
      
      
    }
    
    dataNew = do.call(rbind.data.frame, dataList)
    head(dataNew)
    subjectNumber = nrow(dataNew)
    df = 2
    
    vars = dataNew$var
    names(vars) = paste0(dataNew$subject, dataNew$time)
    
    if(sum(vars, na.rm=TRUE) > 0){
      value <- max(vars, na.rm=TRUE)/sum(vars, na.rm=TRUE)
    }else{
      
      value = 0
    }
    criticalValue = prospectr:::Cul(0.05, df, subjectNumber)
    dataNew$value = value
    dataNew$cv = criticalValue
    
    
    if(value > criticalValue){
      
      outlierDataList[[dataNew[which.max(vars), "subject"]]] = dataNew[which.max(dataNew$var),]
      outlierSubject = dataNew[which.max(vars), "subject"]
      outlierTime = dataNew[which.max(vars), "time"]
      
      dataFull = dataFull[!(dataFull[,subject] %in% outlierSubject),]
      
      outlierList[[j]] = dataNew[which.max(dataNew$var),]
      
      dataFull = dataFull[complete.cases(dataFull),]
      
    }
    
  }
  
  
  dataAfterStep1 = dataFull
  
  outlierStep1 = do.call(rbind.data.frame, outlierList)
  
  if(nrow(outlierStep1) > 0){
    outlierStep1$var = round(outlierStep1$var, decimal)
    outlierStep1$value = round(outlierStep1$value, decimal)
    outlierStep1$cv = round(outlierStep1$cv, decimal)
    outlierStep1$var = round(outlierStep1$var, decimal)
  }
  
   if(outlierS1 == "replicates"){
    
    dataFull = dataOrg[!(rownames(dataOrg) %in% rownames(dataOrg[dataOrg[,"subject_time"] %in% outlierStep1[,c("subject_time")],])), ]
    
   }else{
    
     dataFull = dataOrg
    
  }
  

  
  # if(outlierS1 == "subjects"){
  #   
  #   dataFull = dataFull[!(rownames(dataFull) %in% rownames(dataFull[dataFull[,subject] %in% outlierStep1[,"subject"],])), ]
  #   
  # }

  
  ### STEP 2 ####
  dataOrg2 = dataFull
  dataFull2 = dataFull
  value = 1
  criticalValue = 0
  
  outlierDataList2 = list()
  
  while(value > criticalValue){
    
    vars =  tapply(dataFull2$value, as.factor(dataFull2[,subject]), var)
    vars <- vars[!is.na(vars) & !is.infinite(vars)]
    namesVars = names(vars)
    
    splitData = split(dataFull2, as.factor(dataFull2[,subject]))
    
    for(i in 1:length(splitData)){
    
      splitData[namesVars[i]][[1]]["Variance"] = vars[i]
    
    }
    
    dataFull2 = do.call(rbind.data.frame, splitData)
    
    subjectNumber = length(vars)
    df = length(unique(dataFull2[,time]))-1
    
    value <- max(vars, na.rm=TRUE)/sum(vars, na.rm=TRUE)
    
    criticalValue = prospectr:::Cul(0.05, df, subjectNumber)
    
    if(!is.numeric(criticalValue)){stop("criticalValue not numeric, not exist")}
    if(!is.numeric(value)){stop("value not numeric, not exist")}
   
    print(head(dataFull2))
    print(tail(dataFull2))
    print(nrow(dataFull2))
    print(ncol(dataFull2))
    
     print(paste0("value: ",value))
    print(paste0("criticalValue: ", criticalValue))
    print(paste0("df: ", df))
    print(paste0("subjectNumber: ", subjectNumber))
    print(dataFull2[time])
    
    if(value > criticalValue){
      
      outlierSubject = dataFull2[dataFull2[,subject] == names(which.max(vars)),]
      
      
      outlierSubject$CochranValue = value
      outlierSubject$cv = criticalValue
      
      outlierDataList2[[outlierSubject[,subject][1]]] = outlierSubject
      
      dataFull2 = dataFull2[!(rownames(dataFull2) %in% rownames(outlierSubject)), ]
      
      cat("\nOutlier Found", "\nValue", value, "\nCritical Value:", criticalValue, "\n")
      cat(subject,outlierSubject[,subject][1], "is outlier\n")
      
    }else{
      
      cat("\nNo Outlier Found\n")
      
    }
    
  }
  
  outlierStep2 = do.call(rbind.data.frame, outlierDataList2)
  
  if(nrow(outlierStep2) > 0){
  rownames(outlierStep2) = NULL
  outlierStep2$Variance = round(outlierStep2$Variance, decimal)
  outlierStep2$CochranValue = round(outlierStep2$CochranValue, decimal)
  outlierStep2$cv = round(outlierStep2$cv, decimal)
  }
  # outlierStep2 = dataFull[!(dataFull[,subject] %in% dataFull2[,subject]),]

  dataAfterStep2 = dataFull2
  
  if(!outlierS2){
    
    dataFull2 = dataOrg2
  }
  
  

  ### STEP 3 ####
  
  dataOrg3 = dataFull2
  dataReed = dataFull2
  analyteValue = dataReed$value
  subjects = as.factor(dataReed[,subject])
  
  means <- tapply(analyteValue, subjects, mean)
  names(means) <- levels(subjects)
  
  
  maxDifference = 1
  absoluteDifference = 1

  
    outlierListReed = list()
    
    
    ## High-value outliers
    order = sort(means, decreasing = TRUE)
    maxDifference = abs(diff(order[1:2]))
    absoluteDifference = abs((diff(range(order))))
    
    
    if(maxDifference > absoluteDifference/3){
      
      outlier = order[1]
      outSubject = names(outlier)
      
      outlierListReed[[as.numeric(outSubject)]] = data.frame(subject = outSubject, threshold = round(absoluteDifference/3, decimal), maxDifference = round(maxDifference, decimal), Outlier = "High-value outlier")
      
      cat(subject, outSubject, "is a high-value outlier \n-------------------------------------------- \nMoving on to check low-value outlier \n--------------------------------------------")
      
    }else{
      
      cat("\nNo high-value outlier\n")
    }
    
    
    order = sort(means, decreasing = FALSE)
    maxDifference = abs(diff(order[1:2]))
    absoluteDifference = abs((diff(range(order))))
    
    if(maxDifference > absoluteDifference/3){
      
      outlier = order[1]
      outSubject = names(outlier)
      outlierListReed[[as.numeric(outSubject)]] = data.frame(subject = outSubject, threshold = round(absoluteDifference/3, decimal), maxDifference = round(maxDifference, decimal), Outlier = "Low-value outlier")
      
      cat(subject, outSubject, "is a low-value outlier \n-------------------------------------------- \nOutlier detection process is successfully completed \n--------------------------------------------")
    }else{
      
      cat("\nNo low-value outlier\n" )
    }
    
    outlierListReed = do.call(rbind.data.frame, outlierListReed)
    
    if(nrow(outlierListReed) > 0){
      # outlierSubjectsReed = do.call(rbind.data.frame,outlierListReed)
      # names(outlierSubjectsReed) = "Subjects"
      
      dataReed = dataReed[!(dataReed[,subject] %in%  outlierListReed[,"subject"]),]
      
      means = means[!(names(means) %in% outlierListReed[,"subject"])]
      
    }else{
      
      dataReed = dataReed
      
    }
  
  
  cat("\n------------------------------------","\nStep 3 is succesfully completed. All outliers found and removed from dataset.","\n------------------------------------")
  
  if(!outlierS3){
    dataWithoutOutliers = dataOrg3
    
  }else{
  
    dataWithoutOutliers = dataReed
  
  }
  
  if(nrow(outlierStep1) > 0){
    outlierStep1 = outlierStep1[,-3]
    names(outlierStep1) = c("Subject", "Time", "1st replication", "2nd replication", "Variance", "Ratio of maximum replicate variance to the sum of replicate variances", "Critical value")
  }else{
    
    outlierStep1 = cbind.data.frame(Note = "No outliers found.")
    
  }
  
  if(nrow(outlierStep2) > 0){
    outlierStep2 = outlierStep2[,-5]
    names(outlierStep2) = c("Subject", "Gender", "Time", "Replicate", analyte, "Variance", "Ratio of maximum individual variance to the sum of individual variances", "Critical value")
    rownames(outlierStep2) = NULL
    
    outlierStep2 = outlierStep2[, c(1,2,7:8)]
    outlierStep2 = unique(outlierStep2)
    
  }else{
    
    outlierStep2 = cbind.data.frame(Note = "No outliers found.")
    
  }
  
  if(nrow(outlierListReed) > 0){
    
    outlierStep3 = outlierListReed
    outlierStep3 = outlierStep3[,c(1,3,2,4)]
    names(outlierStep3) = c("Subject", "Difference between extremes", "1/3 Range",  "Outlier")
    
    
  }else{
    
    outlierStep3 = cbind.data.frame(Note = "No outliers found.")

  }
    
    
  dataWithoutOutliers = dataWithoutOutliers[,c(subject, gender, time, replicate, "replicate.subject", "value")]
  names(dataWithoutOutliers) = c("subject", "gender", "time", "replicate", "replicate.subject", "value")
  
  outlierResult = list(step1 = outlierStep1, step2 = outlierStep2, step3 = outlierStep3, dataWithoutOutliers = dataWithoutOutliers, dataAfterStep1 = dataAfterStep1, dataAfterStep2 = dataAfterStep2)
  
  
  
  
        
  }



        
        
        
