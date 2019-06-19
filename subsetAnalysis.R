# data = dataWithoutOutliers; subject = "subject"; gender = "Gender"; analyte = "value"; CVresult = "original"
# 
# subset(data, subject, gender, analyte, CVresult = "original")

subsetAnalysis <- function(data, subject = "subject", gender = "gender", analyte, CVresult = "original", decimal){      
  
  if(CVresult == "lnTransformed"){
    
    analyteValue = data$value
    loganalytevalue = log(analyteValue)
    # logdata = data
    data$value = loganalytevalue      
    
  }
  
  genderLevels = levels(data[,"gender"])
  
  dataHomogenity = data
  analyteValue = data$value
  if(is.null(analyteValue)){
    
    analyteValue = data[,analyte]
  }
  
  subjects = as.factor(data[,"subject"])
  
  means <- tapply(analyteValue, subjects, mean)
  names(means) = levels(subjects)
  
  genderU = cbind.data.frame(unique(data[c("subject", "gender")]))
  
  gender2 = genderU[with(genderU, order(genderU[,"subject"])), ]
  
  dataTtest = cbind.data.frame(gender2, means)
  
  
  bartlet = bartlett.test(dataTtest$means, dataTtest[, "gender"])
  
  
  HomogenityGenderResult =  data.frame(matrix(NA,1,5))
  names(HomogenityGenderResult) = c("Test", "Statistic", "df", "p-value", "Result")
  
  HomogenityGenderResult[1,1] = "Bartlett"
  HomogenityGenderResult[1,2] = round(bartlet$statistic[[1]],decimal)
  HomogenityGenderResult[1,3] = round(bartlet$parameter[[1]],decimal)
  HomogenityGenderResult[1,4] = ifelse(bartlet$p.value[[1]] < 0.001, "<0.001", round(bartlet$p.value[[1]],3))
  HomogenityGenderResult[1,5] = ifelse(bartlet$p.value > 0.05, "Homogeneous", "Heterogeneous")
  
  if(bartlet$p.value > 0.05){ varEqual = TRUE}else{ varEqual = FALSE}
  
  # if(HomogenityGenderResult[1,4] == 0){HomogenityGenderResult[1,4] = "<0.05"} 
  
  ttest = t.test(as.formula(paste0("means~","gender")), data = dataTtest, var.equal = varEqual)
  
  sd = tapply(dataTtest[,"means"], dataTtest[,"gender"], sd)
  
  ttestResult =  data.frame(matrix(NA,1,7))
  names(ttestResult) = c("Analyte", paste0("Mean (", genderLevels[1],")"), paste0("SD (", genderLevels[1],")"), paste0("Mean (", genderLevels[2],")"), paste0("SD (", genderLevels[2],")"), "p-value", "Result")
  ttestResult[1,1] = analyte
  ttestResult[1,2] = round(ttest$estimate[[1]],decimal)
  ttestResult[1,3] = round(sd[[1]],decimal)
  ttestResult[1,4] = round(ttest$estimate[[2]],decimal)
  ttestResult[1,5] = round(sd[[2]],decimal)
  ttestResult[1,6] = ifelse(ttest$p.value < 0.001, "<0.001",  round(ttest$p.value,3))
  ttestResult[1,7] = ifelse(ttest$p.value > 0.05, "No difference", "Different")
  
  # if(ttestResult[1,6] == 0){ttestResult[1,6] = "<0.05"} 
  
  
  mwtest = wilcox.test(as.formula(paste0("means~","gender")), data = dataTtest)
  median = tapply(dataTtest[,"means"], dataTtest[,"gender"], median)
  q = tapply(dataTtest[,"means"], dataTtest[,"gender"], quantile, type = 6)
  iqr1 = round(q[[genderLevels[1]]][4] - q[[genderLevels[1]]][2],decimal)
  iqr2 = round(q[[genderLevels[2]]][4] - q[[genderLevels[2]]][2],decimal)
  
  
  
  mwtestResult =  data.frame(matrix(NA,1,7))
  names(mwtestResult) = c("Analyte", paste0("Median (", genderLevels[1],")"), paste0("IQR (", genderLevels[1],")"), paste0("Median (", genderLevels[2],")"), paste0("IQR (", genderLevels[2],")"), "p-value", "Result")
  mwtestResult[1,1] = analyte
  mwtestResult[1,2] = round(median[genderLevels[1]],decimal)
  mwtestResult[1,3] = round(iqr1,decimal)
  mwtestResult[1,4] = round(median[genderLevels[2]],decimal)
  mwtestResult[1,5] = round(iqr2,decimal)
  mwtestResult[1,6] = ifelse(mwtest$p.value < 0.001, "<0.001",  round(mwtest$p.value,3))
  mwtestResult[1,7] = ifelse(mwtest$p.value > 0.05,"No difference","Different")
  
  # if(mwtestResult[1,6] == 0){mwtestResult[1,6] = "<0.05"} 
  
  
  if(is.null(data$value)){
    dataHomogenity2 = data[,c("subject", analyte, "gender")]
    vars =  tapply(dataHomogenity2[,analyte], as.factor(dataHomogenity2[,"subject"]), var)
    
  }else{
    
    dataHomogenity2 = data[,c("subject", "value", "gender")]
    vars =  tapply(dataHomogenity2$value, as.factor(dataHomogenity2[,"subject"]), var)
    
  }
  
  names(vars) = levels(as.factor(dataHomogenity2[,"subject"]))
  
  genderU = cbind.data.frame(unique(dataHomogenity2[-2]))
  
  gender2 = genderU[with(genderU, order(genderU[,"subject"])), ]
  
  dataHomogenity3 = cbind.data.frame(vars, gender2)
  names(dataHomogenity3) = c("vars", "subject", "gender")
  
  fTest = var.test(vars ~ gender, data = dataHomogenity3, alternative = "two.sided")  
  
  bartlet = bartlett.test(dataHomogenity3$vars, dataHomogenity3[,"gender"])
  
  
  HomogenitySIAResult =  data.frame(matrix(NA,1,5))
  names(HomogenitySIAResult) = c("Test", "Statistic", "df", "p-value", "Result")
  
  HomogenitySIAResult[1,1] = "Bartlett"
  HomogenitySIAResult[1,2] = round(bartlet$statistic[[1]],decimal)
  HomogenitySIAResult[1,3] = round(bartlet$parameter[[1]],decimal)
  HomogenitySIAResult[1,4] = ifelse(bartlet$p.value[[1]] < 0.001, "<0.001",  round(bartlet$p.value[[1]],3))
  HomogenitySIAResult[1,5] = ifelse(bartlet$p.value > 0.05, "Homogeneous","Heterogeneous")
  
  
  if(bartlet$p.value > 0.05){ varEqual = TRUE}else{ varEqual = FALSE}
  
  # if(HomogenitySIAResult[1,4] == 0){HomogenitySIAResult[1,4] = "<0.05"} 
  
  
  # formula = as.formula(paste0("vars ~ ", gender))
  
  ttest = t.test(vars~gender, data = dataHomogenity3, var.equal = varEqual)
  
  sd = tapply(dataHomogenity3$vars, dataHomogenity3[,"gender"], sd)
  
  fTestResultSIA =  data.frame(matrix(NA,1,4))
  names(fTestResultSIA) = c("Analyte", "Ratio of variances", "p value","Result")
  fTestResultSIA[1,1] = analyte
  fTestResultSIA[1,2] = round(fTest$statistic,decimal)
  fTestResultSIA[1,3] = ifelse(fTest$p.value < 0.001, "<0.001", round(fTest$p.value,3))
  fTestResultSIA[1,4] = ifelse(fTest$p.value > 0.05, "No difference", "Different")
  
  # if(fTestResultSIA[1,3] == 0){fTestResultSIA[1,3] = "<0.05"} 
  
  
  
  subsetResult = list(homogenity = HomogenityGenderResult, ttest = ttestResult, homogenitySIA = HomogenitySIAResult, fTestSIA = fTestResultSIA, mw = mwtestResult)
  
  return(subsetResult)
}
      
      
      
      
