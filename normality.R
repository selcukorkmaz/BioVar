
normality <- function(data, subject, analyte){        
 
if(nrow(data) > 0){   
 library(nortest)
  resultStep1 = NULL
  resultStep2 = NULL      
        analyteValue = data$value
        loganalytevalue = log(analyteValue)
        logdata = data
        logdata$value = loganalytevalue
        replicate = as.factor(data[,"subject"])
        nreplicate = length(levels(replicate))
        nameanalyte = analyte
        
        sw1 = tapply(analyteValue, replicate, shapiro.test)
        unlist1 = data.frame(matrix(unlist(sw1), nrow=nreplicate, ncol=4, byrow=T))
        unlist1$X2 = as.numeric(as.character(unlist1$X2))
        pval1 = unlist1[,2]
        
        nreject1 = table(pval1>0.05)[[1]]
        
        if(nreject1 == length(pval1)){
          
          naccept1 = 0
        }else{
          
          naccept1 = table(pval1>0.05)[[2]]
          
        }
        normalityRate = naccept1/nrow(unlist1)
        
        
        if(normalityRate > 0.50){ # if data are normally distributed, then it uses original data, if not it applies a logarithmic transformation and uses this transformed-data for the rest of the calculations
          
          test = "Shapiro-Wilk"
          naccept = naccept1
          nreject = nreject1
          normalityRate = round(normalityRate*100,2)
          
          resultStep1 = cbind.data.frame(Step1 = "On set of results from each individual",Test = test, "# of acceptance of normality" = naccept, "# of rejection normality" = nreject, "Normality rate" = paste0("%",normalityRate), Comment = paste0("The data is assumed to be normal on set of results from each individual for ", nameanalyte) )
          rownames(resultStep1) = NULL
          # cat("\n----------------------------------------------------------------------------------\nSTEP 1. NORMALITY TEST:-ON SET OF RESULTS FROM EACH INDIVIDUAL")
          # cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied")
          # cat("\n----------------------------------------------------------------------------------\nThe data is assumed to be normal on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
          # cat("\nNumber of normally distributed subjectervations", naccept1,"\n----------------------------------------------------------------------------------")
          # cat("\nNumber of non-normally distributed subjectervations", nreject1,"\n----------------------------------------------------------------------------------")
          # 
          meanval = tapply(analyteValue, replicate, mean)
          sw2 = shapiro.test(meanval)
          pval2 = sw2$p.value
          
          if (pval2 >= 0.05){
            
            resultStep2 = cbind.data.frame(Step2 = "On mean values of subjects", Test = "Shapiro-Wilk", "Test statistic" = round(sw2$statistic,2), 
                                            "p value" = round(pval2,2), Comment = paste0("The data is assumed to be normal on mean values of subjects for ", nameanalyte),  
                                            "Final result" = "Analyses can be continued by Linear Mixed Effect Models or ANOVA using the original data.")
            rownames(resultStep2) = NULL
            # cat("\n----------------------------------------------------------------------------------\nSTEP 2. NORMALITY TEST:-ON MEAN VALUES OF SUBJECTS")
            # cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
            # cat("\nThe data is assumed to be normal on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
            # cat("\nAnalyses can be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
          } else {
            ks1 = nortest::lillie.test(meanval)
            pval3 = ks1$p.value
            
            if (pval3 >= 0.05){
              
              resultStep2 = cbind.data.frame(Step2 = paste0("On mean values of subjects. Normality assumption is violated on mean values of subjects for ", nameanalyte, ". Kolmogorov-Smirnov's normality test is applied. The data is assumed to be normal on mean values of subjects for ", nameanalyte), Test = "Kolmogorov-Smirnov", "Test statistic" = round(ks1$statistic,2), 
                                              "p value" = round(pval3,2), Comment = paste0("The data is assumed to be normal on mean values of subjects for ", nameanalyte),  
                                              "Final result" = "Analyses can be continued by Linear Mixed Effect Models or ANOVA using the original data.")
               
              # cat("\n----------------------------------------------------------------------------------\nSTEP 2. NORMALITY TEST:-ON MEAN VALUES OF SUBJECTS")
              # cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
              # cat("\nNormality assumption is violated on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
              # cat("\nKolmogorov-Smirnov's normality test is applied", "\n----------------------------------------------------------------------------------")
              # cat("\nThe data is assumed to be normal on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
              # cat("\nAnalyses can be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
            } else {
              logmeanval = log(meanval)
              sw3 = shapiro.test(logmeanval)
              pval4 = sw3$p.value
              
              if (pval4 >= 0.05){
                
                resultStep2 = cbind.data.frame(Step2 = paste0("On mean values of subjects (normality assumption is violated on mean values of subjects for ",
                                                               nameanalyte, ". Kolmogorov-Smirnov's normality test is applied. Normality assumption is violated again on mean values of subjects for ", nameanalyte, ". Logarithmic transformation is applied. The data is assumed to be normal on mean values of subjects for ", nameanalyte), 
                                                Test = "Kolmogorov-Smirnov", "Test statistic" = round(sw3$statistic,2), 
                                                "p value" = round(pval4,2), Comment = paste0("The data is assumed to be normal on mean values of subjects for ", nameanalyte),  
                                                "Final result" = "Analyses can be continued by Linear Mixed Effect Models or ANOVA using the log-transformed data.")
                
                # 
                # cat("\n----------------------------------------------------------------------------------\nSTEP 2. NORMALITY TEST:-ON MEAN VALUES OF SUBJECTS")
                # cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
                # cat("\nNormality assumption is violated on mean values of subjects for ", nameanalyte,"\n----------------------------------------------------------------------------------")
                # cat("\nKolmogorov-Smirnov's normality test is applied", "\n----------------------------------------------------------------------------------")
                # cat("\nNormality assumption is violated again on mean values of subjects for ", nameanalyte,"\n----------------------------------------------------------------------------------")
                # cat("\nLogarithmic transformation is applied", "\n----------------------------------------------------------------------------------")
                # cat("\nThe data is assumed to be normal on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
                # cat("\nAnalyses can be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
              } else{
                
                resultStep2 = cbind.data.frame(Step2 ="On mean values of subjects", "Final comment" = paste0("Shapiro-Wilk's normality test is applied. Normality assumption is violated on mean values of subjects for ", nameanalyte, 
                       ". Kolmogorov-Smirnov's normality test is applied. Normality assumption is violated again on mean values of subjects for ", nameanalyte,
                       ". Logarithmic transformation is applied. Normality assumption is violated again on mean values of subjects for ", nameanalyte, ". The data is not normally distributed for ", nameanalyte, 
                       ". Analyses should not be continued by Linear Mixed Effect Models or ANOVA.")
              )  
          
            }
            }
          }
          
        }else {
          sw6 = tapply(loganalytevalue, replicate, shapiro.test)
          unlist2 = data.frame(matrix(unlist(sw6), nrow=nreplicate, ncol=4, byrow=T))
          unlist2$X2 = as.numeric(as.character(unlist2$X2))
          pval6 = unlist2[,2]
          
          nreject2 = table(pval6>0.05)[[1]]
          
          if(length(pval6) == nreject2){
            
            naccept2 = 0
          }else{
            
            naccept2 = table(pval6>0.05)[2]
          }
          
          names(naccept2) = names(nreject2) = NULL
          
          normalityRate = naccept2/nrow(unlist2)
          
          
          if(naccept2 > nreject2){
            meanval = tapply(analyteValue, replicate, mean)
            logmeanval = log(meanval)
            sw7 = shapiro.test(logmeanval)
            pval7 = sw7$p.value
            
            if (pval7 >= 0.05){
              resultStep1 = cbind.data.frame(Step1 = "On set of results from each individual", Warning = "Logarithmic transformation is applied",
                                             Test = test, "# of acceptance of normality" = naccept2, 
                                             "# of rejection normality" = nreject2, "Normality rate" = paste0("%",normalityRate), Comment = paste0("The data is assumed to be normal on set of results from each individual for ", nameanalyte) )
              rownames(resultStep1) = NULL
              
              
              # cat("\n----------------------------------------------------------------------------------\nSTEP 1. NORMALITY TEST:-ON SET OF RESULTS FROM EACH INDIVIDUAL")
              # cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied")
              # cat("\n----------------------------------------------------------------------------------\nNormality assumption is violated on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
              # cat("\nNumber of normally distributed subjectervations", naccept1,"\n----------------------------------------------------------------------------------")
              # cat("\nNumber of non-normally distributed subjectervations", nreject1,"\n----------------------------------------------------------------------------------")
              # cat("\nLogarithmic transformation is applied", "\n----------------------------------------------------------------------------------")
              # cat("\nNumber of normally distributed subjectervations", naccept2,"\n----------------------------------------------------------------------------------")
              # cat("\nNumber of non-normally distributed subjectervations", nreject2,"\n----------------------------------------------------------------------------------")
              # cat("\nThe data is assumed to be normal on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
              
              
              resultStep2 = cbind.data.frame(Step2 = "On mean values of subjects", Warning = "Logarithmic transformation is applied", Test = "Shapiro-Wilk", "Test statistic" = round(sw7$statistic,2), 
                                             "p value" = round(pval7,2), Comment = paste0("The data is assumed to be normal on mean values of subjects for ", nameanalyte),  
                                             "Final result" = "Analyses can be continued by Linear Mixed Effect Models or ANOVA using the log-transformed data.")
              rownames(resultStep2) = NULL
              
              # cat("\n----------------------------------------------------------------------------------\nSTEP 2. NORMALITY TEST:-ON MEAN VALUES OF SUBJECTS")
              # cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
              # cat("\nThe data is assumed to be normal on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
              # cat("\nAnalyses can be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
              
              
              
            } else {
              
              
              resultStep1 = cbind.data.frame(Step1 = "On set of results from each individual", Warning = "Logarithmic transformation is applied", Test = "Shapiro-Wilk", 
                                             "# of acceptance of normality" = naccept1, "# of rejection normality" = nreject1, "Normality rate" = naccept1/(naccept1+nreject1), Comment = paste0("The data is assumed to be normal on set of results from each individual for ", nameanalyte) )
              rownames(resultStep1) = NULL
              
        
              resultStep2 = cbind.data.frame(Step2 ="On mean values of subjects", "Final comment" = paste0("Shapiro-Wilk's normality test is applied to the trasnformed data. Normality assumption is violated again on mean values of subjects for ", nameanalyte,
                                                                                                            ". The data is not normally distributed for ", nameanalyte, 
                                                                                                           ". Analyses should not be continued by Linear Mixed Effect Models or ANOVA."))
                                             
                                             
              
            }
          } else {
            
            resultStep1 = cbind.data.frame(Step1 = "On set of results from each individual",Test = "Shapiro-Wilk", "# of acceptance of normality" = naccept1, 
                                           "# of rejection normality" = nreject1, "Normality rate" = naccept1/(naccept1+nreject1), 
                                            Comment = paste0("The data is not normally distributed for ", nameanalyte), "Final result" =  "Analyses should not be continued by Linear Mixed Effect Models or ANOVA")
            rownames(resultStep1) = NULL
          }
        
        }
        
}else{
  
  resultStep1 = cbind.data.frame(Note = "There is no observation left after removing outliers. Therefore, the analysis is terminated.")
  rownames(resultStep1) = NULL
  
  resultStep2 = cbind.data.frame(Note = "There is no observation left after removing outliers. Therefore, the analysis is terminated.")
  rownames(resultStep2) = NULL
  
}
        
        normalityResult = list(Step1 = resultStep1, Step2 = resultStep2)
        
        return(normalityResult)
        
        }
        
        
