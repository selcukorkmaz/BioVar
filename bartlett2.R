bartlettWithin <- function(data, subject, time, replicate, alpha = 0.05, decimal = 3, correction = TRUE ){
  
  data$replicate.subject = as.numeric(paste0(data[,replicate], data[,subject]))
  
  dataFull = data
  
  dataFull = dataFull[complete.cases(dataFull),]
  
  dataList = list()
  
  s = unique(dataFull[,subject])
  # nWithin = list()
  
  for(i in 1:length(s)){
    
    data = dataFull[dataFull[,subject] == s[i],]
    # data = data[complete.cases(data),]
    
    rep1 = data[data$replicate.subject == unique(data$replicate.subject)[1],"value"]
    rep2 = data[data$replicate.subject == unique(data$replicate.subject)[2],"value"]
    
    repData = cbind.data.frame(rep1,rep2)
     nWithin = nrow(repData)-1
    
    # repData = repData[complete.cases(repData),]
    
    s2 = sd(apply(repData, 1, mean, na.rm=TRUE))^2
    lns2 = log(s2)
    
    
    s2New = s2*( nrow(repData)-1)
    lns2New = lns2*( nrow(repData)-1)
    
    # subject = rep(i, length(rep1))
    # subject_time = paste0(rep(s[i], length(rep1)), unique(data[,time]))
    
    reps = cbind.data.frame(s2New, lns2New,nWithin)
    
    dataList[[i]] = reps
    
    
  }
  
  dataNew = do.call(rbind.data.frame, dataList)
  
  n = sum(dataNew$nWithin)
  wslvar = sum(dataNew$lns2New)
  wavar = log(sum(dataNew$s2New)/n)
  chisqr = n*wavar-wslvar
  correctionFactor =1+(1/(3*(nrow(dataNew)-1)))*(sum(1/dataNew$nWithin)-(1/sum(dataNew$nWithin)))
  correctedChisqr = chisqr/correctionFactor
  stat = ifelse(correction, correctedChisqr, chisqr)
  criticalValue = qchisq(1-alpha,nrow(dataNew)-1)
  pValue = 1-pchisq(stat,nrow(dataNew)-1,lower.tail = TRUE)
  pValue2 = ifelse(pValue<0.001, "<0.001", formatC(pValue, digits = decimal, format = "f"))
  res = ifelse(pValue<0.05, "Heterogeneous", "Homogeneous")
  stat = formatC(stat, digits = decimal, format = "f")
  
  bartlettWithin = cbind.data.frame(Method = "Bartlett", "Chi-square" = stat, "p value" = pValue2, Result = res)
  
  colnames(bartlettWithin)[2] = ifelse(correction, "Corrected Chi-Square", "Chi-square")
  
  return(bartlettWithin)
  
}


bartlettWithin(data, subject, time, replicate, alpha = 0.05, decimal = 5, correction = TRUE)

