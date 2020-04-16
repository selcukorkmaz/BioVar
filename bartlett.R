# # 
# data = read.table("~/Documents/Studies/BiologicalVariation/Rcodes/data.txt", header = T, sep = "\t")
# head(data)
# 
# subject = "Subject"; time="Time"; replicate="Replicate"; alpha = 0.05; decimal = 3; correction = TRUE; test = "bartlett"
# colnames(data)[5] = "value"

homogeneityAnalytical <- function(data, subject, time, replicate, alpha = 0.05, decimal = 3, correction = TRUE, test = "bartlett"){

  data$replicate.subject = as.numeric(paste0(data[,replicate], data[,subject]))

  dataFull = data

  dataFull = dataFull[complete.cases(dataFull),]
  
  dataList = list()
  
  s = unique(dataFull[,subject])
  
  for(i in 1:length(s)){
    
    data = dataFull[dataFull[,subject] == s[i],]
    data = data[complete.cases(data),]
    
    rep1 = data[data$replicate.subject == unique(data$replicate.subject)[1],"value"]
    rep2 = data[data$replicate.subject == unique(data$replicate.subject)[2],"value"]
    
    repData = cbind.data.frame(rep1,rep2)
    
    repVar = apply(repData, 1, var)
    
    # subject = rep(i, length(rep1))
    subject_time = paste0(rep(s[i], length(rep1)), unique(data[,time]))
    
    reps = cbind.data.frame(subject = rep(dataFull[dataFull[,subject] == s[i],subject][[1]], length(subject_time)), time = unique(data[,time]),subject_time, rep1, rep2, var = repVar)
    
    dataList[[i]] = reps
    
    
  }
  
  dataNew = do.call(rbind.data.frame, dataList)
  
  if(test == "bartlett"){
    
  dataNew = dataNew[dataNew$var > 0,]
    
  dataNewVar = dataNew$var[!is.infinite(dataNew$var)]
  
  n = nrow(dataNew)
  wavar = log(sum(dataNewVar, na.rm = T)/n)
  wslvar = sum(log(dataNewVar), na.rm = T)
  chisqr = n*wavar-wslvar
  correctionFactor = 1+(1/(3*(n-1)))*(n-1/n)
  correctedChisqr = chisqr/correctionFactor
  stat = ifelse(correction, correctedChisqr, chisqr)
  criticalValue = qchisq(1-as.numeric(alpha),n-1)
  pValue = 1-pchisq(stat,n-1,lower.tail = TRUE)
  pValue2 = ifelse(pValue<0.001, "<0.001", formatC(pValue, digits = decimal, format = "f"))
  res = ifelse(pValue<0.05, "Heterogeneous", "Homogeneous")
  stat = formatC(stat, digits = decimal, format = "f")
  
  analyticalResult = cbind.data.frame(Method = "Bartlett", "Chi-square" = stat, "p value" = pValue2, Result = res)

  colnames(analyticalResult)[2] = ifelse(correction, "Corrected Chi-Square", "Chi-square")
  }else{
    
    
    cohchranValue = formatC(max(dataNew$var)/sum(dataNew$var), digits = decimal, format = "f")
    criticalValue = formatC(prospectr:::Cul(0.05, 2, nrow(dataNew)), digits = decimal, format = "f")
    res = ifelse(max(dataNew$var)/sum(dataNew$var) > prospectr:::Cul(0.05, 2, nrow(dataNew)), "Heterogeneous", "Homogeneous")
    
    analyticalResult = cbind.data.frame(Method = "Cochran", Value = cohchranValue, "Critical Value" = criticalValue,
                                         Result = res)
  }
  
  
  return(analyticalResult)
  
  }



homogeneityWithin <- function(data, subject, time, replicate, alpha = 0.05, decimal = 3, correction = TRUE, test = "bartlett" ){
  
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
  
  if(test == "bartlett"){
  
  n = sum(dataNew$nWithin)
  wslvar = sum(dataNew$lns2New)
  wavar = log(sum(dataNew$s2New)/n)
  chisqr = n*wavar-wslvar
  correctionFactor =1+(1/(3*(nrow(dataNew)-1)))*(sum(1/dataNew$nWithin)-(1/sum(dataNew$nWithin)))
  correctedChisqr = chisqr/correctionFactor
  stat = ifelse(correction, correctedChisqr, chisqr)
  criticalValue = qchisq(1-as.numeric(alpha),nrow(dataNew)-1)
  pValue = 1-pchisq(stat,nrow(dataNew)-1,lower.tail = TRUE)
  pValue2 = ifelse(pValue<0.001, "<0.001", formatC(pValue, digits = decimal, format = "f"))
  res = ifelse(pValue<0.05, "Heterogeneous", "Homogeneous")
  stat = formatC(stat, digits = decimal, format = "f")
  
  withinResult = cbind.data.frame(Method = "Bartlett", "Chi-square" = stat, "p value" = pValue2, Result = res)
  
  colnames(withinResult)[2] = ifelse(correction, "Corrected Chi-Square", "Chi-square")
  
  }else{
    
    s2 = dataNew$s2New/dataNew$nWithin
    
    cohchranValue = formatC(max(s2)/sum(s2), digits = decimal, format = "f")
    criticalValue = formatC(prospectr:::Cul(0.05, mean(dataNew$nWithin)+1, nrow(dataNew)), digits = decimal, format = "f")
    res = ifelse(max(s2)/sum(s2) > prospectr:::Cul(0.05, mean(dataNew$nWithin), nrow(dataNew)), "Heterogeneous", "Homogeneous")
    
    withinResult = cbind.data.frame(Method = "Cochran", Value = cohchranValue, "Critical Value" = criticalValue,
                                         Result = res)
    
    
  }
  return(withinResult)
  
}

  