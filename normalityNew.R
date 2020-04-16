

# data = read.table("~/Documents/Studies/BiologicalVariation/Rcodes/data.txt",header = T, sep = "\t")
# 
# data$Measurement = log(data$Measurement)
# dim(data)
# 
# subject="Subject"
# analyte = "Measurement"
# replicate = "Replicate"
# time = "Time"
# gender="Gender"
# 
# dataNew = outlier(data = data, analyte=analyte, subject=subject, replicate=replicate,
#                   time=time, gender=gender, decimal =3, outlierS1 = "subjetcs", outlierS2 = TRUE, outlierS3 = TRUE,
#                   showResult = showResult)
# 
# dataNew$step1
# dataNew$step2
# dataNew$step3
# 
# data = dataNew$dataWithoutOutliers
# dim(data2)
# head(data)
# subject="subject"
# analyte = "value"
# decimal=3
# test = "sw"
# transformation = TRUE
# 
# 
# showResult = "lnTransformed"



normalityNew <- function(data, subject, analyte, decimal, test = "sw", transformation = FALSE, showResult = "original"){        
 
  if(nrow(data) > 0){   
   library(nortest)
    resultStep1 = NULL
    resultStep2 = NULL    
    
          data <-data[order(data[, subject]),]
          # analyteValue = data$value
          # loganalytevalue = log(analyteValue)
          # logdata = data
          # logdata$value = loganalytevalue
          
          if(showResult == "original"){
            
            dat = "Original"
          
          }
          
          if(showResult == "lnTransformed"){
            
            dat = "Log-transformed"
            
          }
          
          
          if(showResult == "transformBack"){
            
            dat = "Transform back to original"
            
          }
          
          if(showResult == "cv"){
            
            dat = "CV"
            
          }
          
          if(showResult == "mom"){
            
            dat = "MoM"
            
          }
          
          if(showResult == "lnmom"){
            
            dat = "lnMoM"
            
          }
          
          replicate = as.factor(data[,subject])
          nreplicate = length(levels(replicate))
          nameanalyte = analyte
          
          # sw1 = tapply(analyteValue, replicate, shapiro.test)
          # 
          # 
          
          splitData = split(data, data[,subject])
          swList = list()
          
          for(i in 1:length(splitData)){
            
            
            tmp = splitData[[i]]
            
            if(test == "sw"){
            sw = shapiro.test(tmp[,analyte])
            method = "Shapiro-Wilk"
            }else{
             
              sw = ad.test(tmp[,analyte])
              method = "Anderson-Darling" 
              
            }
            
            subj = paste0("Subject:",tmp[,subject][[1]])
            stat = formatC(sw$statistic, digits = decimal, format = "f")
            pValue = ifelse(sw$p.value<0.001, "<0.001", formatC(sw$p.value, digits = decimal, format = "f"))
            res = ifelse(sw$p.value<0.05, "Non-normal", "Normal")
            
            swList[[i]] = cbind.data.frame(subj, method, dat, stat, pValue, res)
            
          }
          
          swTestResult = do.call(rbind.data.frame, swList)
          colnames(swTestResult) = c("Subject", "Method", "Data Type", "Statistic", "p value", "Result")
          rownames(swTestResult) = NULL
          
          meanval = tapply(data$value, replicate, mean)
          
          if(test == "sw"){
            sw2 = shapiro.test(meanval)
            method = "Shapiro-Wilk"
          }else{
            sw2 = ad.test(meanval)
            method = "Anderson-Darling"
  
          }
          
          stat = formatC(sw2$statistic, digits = decimal, format = "f")
          pValue = ifelse(sw2$p.value<0.001, "<0.001", formatC(sw2$p.value, digits = decimal, format = "f"))
          res = ifelse(sw2$p.value<0.05, "Non-normal", "Normal")
          sw2TestResult = cbind.data.frame( method, dat, stat, pValue, res)
          colnames(sw2TestResult) = c("Method", "Data Type", "Statistic", "p value", "Result")
          rownames(sw2TestResult) = NULL
          
          normalityResult = list(Step1 = swTestResult, Step2 = sw2TestResult)
          
          return(normalityResult)
          
      }
  
  }
        
        
