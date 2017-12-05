## Outlier detection procedure for biological variation data
## [1] Braga F, Panteghini M. Generation of data on within-subject biological variation in
#laboratory medicine. Critical Reviews in Clinical Laboratory Sciences. 2016

rm(list=ls())
library(WriteXLS)
library(ggplot2)

#library(outliers)


setwd("~/Dropbox/Abdurrahman BV/Veriler/")

tests = read.table("tests.txt", header = F, stringsAsFactors = F)


      testName = "RBC"
      
      print(testName)
      
      data = read.table(paste0(testName,"_data.txt"), header = T, sep = "\t")
      
      splitSubject = split(data, data$subject)
      
      splitList = lapply(splitSubject, FUN = function(x){
      
        x[,colSums(is.na(x))==0]
      
      })
      
      
      data = plyr::rbind.fill(lapply(splitList, as.data.frame))
      
      
      dataFull = reshape(data, varying = names(data)[4:13], direction = "long", sep = "")
      dataFull$'replicate.subject' = as.numeric(paste0(dataFull$replicate, dataFull$subject))
      names(dataFull)[6] = "value"
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
      cat("\n------------------------------------\nStarting outlier analysis for", toupper(as.character(levels(dataFull$analyte))),"\n------------------------------------")
      cat("\n------------------------------------","\nNormality Assumption","\n------------------------------------")
      
      library(dplyr)
      
      data_grp <- group_by(dataFull, subject)
      plotData <- dplyr::summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
      
      plotData$subject <- as.factor(plotData$subject)
      
      
      ggplot(plotData, aes(y = Mean, x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('RBC'~(x10^6 / mu~'L'))) +
        xlab("Subjects")
      
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/log/rbc.jpg")
      ggsave(path, width = 12, height = 9)
      
      #####################################################################################
      # 1. Normality Assumption #
      #####################################################################################
      
      
      ## Outlier detection procedure for biological variation data
      ## [2] Braga F, Panteghini M. Generation of data on within-subject biological variation in
      #laboratory medicine. Critical Reviews in Clinical Laboratory Sciences. 2016
      
      library(nortest)
      
      #data = dataWithoutWithinSubjectOutliers
      #data = read.csv("Veriler_R_format.csv",header = T)
      data = dataFull
      
      #####################################################################################
      #       Step 1. NORMALITY and HOMOGENITY TESTS:-ON SET OF RESULTS FROM EACH INDIVIDUAL  [2]         #
      #####################################################################################
      #       Step 2. NORMALITY and HOMOGENITY TESTS:-ON MEAN VALUES OF SUBJECTS [2]                      #
      #####################################################################################
      
      analyteValue = data$value
      loganalytevalue = log(analyteValue)
      logdata = data
      logdata$value = loganalytevalue
      replicate = as.factor(data$subject)
      nreplicate = length(levels(replicate))
      nameanalyte = toupper(as.character(levels(data$analyte)))
      
      sw1 = tapply(analyteValue, replicate, shapiro.test)
      unlist1 = data.frame(matrix(unlist(sw1), nrow=nreplicate, ncol=4, byrow=T))
      unlist1$X2 = as.numeric(as.character(unlist1$X2))
      pval1 = unlist1[,2]
      naccept1 = table(pval1>0.05)[[2]]
      nreject1 = table(pval1>0.05)[[1]]
      names(naccept1) = names(nreject1) = NULL
      
      if(nreject1 == 0){ # if data are normally distributed, then it uses original data, if not it applies a logarithmic transformation and uses this transformed-data for the rest of the calculations
      
        cat("\n----------------------------------------------------------------------------------\nSTEP 1. NORMALITY TEST:-ON SET OF RESULTS FROM EACH INDIVIDUAL")
        cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied")
        cat("\n----------------------------------------------------------------------------------\nThe data is assumed to be normal on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
        cat("\nNumber of normally distributed subjectervations", naccept1,"\n----------------------------------------------------------------------------------")
        cat("\nNumber of non-normally distributed subjectervations", nreject1,"\n----------------------------------------------------------------------------------")
      
        meanval = tapply(analyteValue, replicate, mean)
        sw2 = shapiro.test(meanval)
        pval2 = sw2$p.value
      
        if (pval2 >= 0.05){
          cat("\n----------------------------------------------------------------------------------\nSTEP 2. NORMALITY TEST:-ON MEAN VALUES OF SUBJECTS")
          cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
          cat("\nThe data is assumed to be normal on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
          cat("\nAnalyses can be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
        } else {
          ks1 = lillie.test(meanval)
          pval3 = ks1$p.value
      
          if (pval3 >= 0.05){
            cat("\n----------------------------------------------------------------------------------\nSTEP 2. NORMALITY TEST:-ON MEAN VALUES OF SUBJECTS")
            cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
            cat("\nNormality assumption is violated on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
            cat("\nKolmogorov-Smirnov's normality test is applied", "\n----------------------------------------------------------------------------------")
            cat("\nThe data is assumed to be normal on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
            cat("\nAnalyses can be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
          } else {
            logmeanval = log(meanval)
            sw3 = shapiro.test(logmeanval)
            pval4 = sw3$p.value
      
            if (pval4 >= 0.05){
              cat("\n----------------------------------------------------------------------------------\nSTEP 2. NORMALITY TEST:-ON MEAN VALUES OF SUBJECTS")
              cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
              cat("\nNormality assumption is violated on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
              cat("\nKolmogorov-Smirnov's normality test is applied", "\n----------------------------------------------------------------------------------")
              cat("\nNormality assumption is violated again on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
              cat("\nLogarithmic transformation is applied", "\n----------------------------------------------------------------------------------")
              cat("\nThe data is assumed to be normal on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
              cat("\nAnalyses can be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
            } else{
              cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
              cat("\nNormality assumption is violated on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
              cat("\nKolmogorov-Smirnov's normality test is applied", "\n----------------------------------------------------------------------------------")
              cat("\nNormality assumption is violated again on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
              cat("\nLogarithmic transformation is applied", "\n----------------------------------------------------------------------------------")
              cat("\nNormality assumption is violated again on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
              cat("\nThe data is not normally distributed for", nameanalyte,"\n----------------------------------------------------------------------------------")
              cat("\nAnalyses should not be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
            }
          }
      
        }
      
      }else {
        sw6 = tapply(loganalytevalue, replicate, shapiro.test)
        unlist2 = data.frame(matrix(unlist(sw6), nrow=nreplicate, ncol=4, byrow=T))
        unlist2$X2 = as.numeric(as.character(unlist2$X2))
        pval6 = unlist2[,2]
        naccept2 = table(pval6>0.05)[2]
        nreject2 = table(pval6>0.05)[1]
        names(naccept2) = names(nreject2) = NULL
      
        if(naccept2 > nreject2){
          meanval = tapply(analyteValue, replicate, mean)
          logmeanval = log(meanval)
          sw7 = shapiro.test(logmeanval)
          pval7 = sw7$p.value
      
          if (pval7 >= 0.05){
            cat("\n----------------------------------------------------------------------------------\nSTEP 1. NORMALITY TEST:-ON SET OF RESULTS FROM EACH INDIVIDUAL")
            cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied")
            cat("\n----------------------------------------------------------------------------------\nNormality assumption is violated on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
            cat("\nNumber of normally distributed subjectervations", naccept1,"\n----------------------------------------------------------------------------------")
            cat("\nNumber of non-normally distributed subjectervations", nreject1,"\n----------------------------------------------------------------------------------")
            cat("\nLogarithmic transformation is applied", "\n----------------------------------------------------------------------------------")
            cat("\nNumber of normally distributed subjectervations", naccept2,"\n----------------------------------------------------------------------------------")
            cat("\nNumber of non-normally distributed subjectervations", nreject2,"\n----------------------------------------------------------------------------------")
            cat("\nThe data is assumed to be normal on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
      
            cat("\n----------------------------------------------------------------------------------\nSTEP 2. NORMALITY TEST:-ON MEAN VALUES OF SUBJECTS")
            cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
            cat("\nThe data is assumed to be normal on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
            cat("\nAnalyses can be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
          } else {
            cat("\n----------------------------------------------------------------------------------\nSTEP 1. NORMALITY TEST:-ON SET OF RESULTS FROM EACH INDIVIDUAL")
            cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied")
            cat("\n----------------------------------------------------------------------------------\nNormality assumption is violated on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
            cat("\nNumber of normally distributed subjectervations", naccept1,"\n----------------------------------------------------------------------------------")
            cat("\nNumber of non-normally distributed subjectervations", nreject1,"\n----------------------------------------------------------------------------------")
            cat("\nLogarithmic transformation is applied", "\n----------------------------------------------------------------------------------")
            cat("\nThe data is assumed to be normal on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
      
            cat("\n----------------------------------------------------------------------------------\nSTEP 2. NORMALITY TEST:-ON MEAN VALUES OF SUBJECTS")
            cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied","\n----------------------------------------------------------------------------------")
            cat("\nNormality assumption is violated again on mean values of subjects for", nameanalyte,"\n----------------------------------------------------------------------------------")
            cat("\nThe data is not normally distributed for", nameanalyte,"\n----------------------------------------------------------------------------------")
            cat("\nAnalyses should not be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
          }
        } else {
          cat("\n----------------------------------------------------------------------------------\nSTEP 1. NORMALITY TEST:-ON SET OF RESULTS FROM EACH INDIVIDUAL")
          cat("\n----------------------------------------------------------------------------------\nShapiro-Wilk's normality test is applied")
          cat("\n----------------------------------------------------------------------------------\nNormality assumption is violated on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
          cat("\nNumber of normally distributed subjectervations", naccept1,"\n----------------------------------------------------------------------------------")
          cat("\nNumber of non-normally distributed subjectervations", nreject1,"\n----------------------------------------------------------------------------------")
          cat("\nLogarithmic transformation is applied", "\n----------------------------------------------------------------------------------")
          cat("\nNormality assumption is violated again on set of results from each individual for", nameanalyte,"\n----------------------------------------------------------------------------------")
          cat("\nThe data is not normally distributed for", nameanalyte,"\n----------------------------------------------------------------------------------")
          cat("\nAnalyses should not be continued by Linear Mixed Effect Models or ANOVA","\n----------------------------------------------------------------------------------")
        }
      
        # #Between-subject variance homogeneity
        # tapply(analyteValue, factor(replicate), var)
        # betwSampBartRes = bartlett.test(analyteValue, factor(replicate))
        #
        # tapply(loganalytevalue, factor(replicate), var)
        # betwSampBartLogRes = bartlett.test(loganalytevalue, factor(replicate))
        #
        # #Between-duplicate variance homogeneity
        # bartletres = bartletlogres = matrix(NA,length(levels(replicate)),1)
        # rownames(bartletres) = rownames(bartletlogres) = levels(replicate)
        # colnames(bartletres) = colnames(bartletlogres) = "p.val"
      }
      
      # for (i in 1:length(levels(replicate))){
      #   x = data[data$subject == as.numeric(levels(replicate))[i],]
      #   logx = logdata[logdata$subject == as.numeric(levels(replicate))[i],]
      #   bart = bartlett.test(x$value, factor(x$replicate.subject))
      #   bart.log = bartlett.test(logx$value, factor(logx$replicate.subject))
      #   bartletres[i,1] = bart$p.value
      #   bartletlogres[i,1] = bart.log$p.value
      # }
      # bartletres
      # bartletlogres = data.frame(row = 1:nrow(bartletlogres), bartletlogres)
      #
      # withinsubjectHomogenity =  data.frame(matrix(NA,1,2))
      # names(withinsubjectHomogenity) = c("Within subject homogenity", "Subjects")
      # tbl = table(bartletlogres$p.val<0.05)
      #
      # if(TRUE %in% names(tbl)){
      #
      #     withinsubjectHomogenity[1,1] = "Some subjects have heterogeneous variances"
      #
      #     subjects = bartletlogres[bartletlogres$p.val<0.05,][,1]
      #
      #     if(length(subjects) == 1){
      #
      #       withinsubjectHomogenity[1,2] = subjects[[1]]
      #
      #     }else{
      #
      #       withinsubjectHomogenity[1,2] = paste(subjects, collapse = ",")
      #
      #
      #     }
      #
      # }else{
      #   withinsubjectHomogenity[1,1] = "homogeneous"
      #   withinsubjectHomogenity[1,2] = "No subject"
      #   }
      #
      # path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/ttest/withinsubjectHomogenity.xls")
      # WriteXLS(withinsubjectHomogenity, path)
      #
      #
      # bartletTest =  data.frame(matrix(NA,1,5))
      # names(bartletTest) = c("Test", "Statistic", "df", "p-value", "result")
      #
      # bartletTest[1,1] = "Bartlett"
      # bartletTest[1,2] = betwSampBartLogRes$statistic[[1]]
      # bartletTest[1,3] = betwSampBartLogRes$parameter[[1]]
      # bartletTest[1,4] = betwSampBartLogRes$p.value[[1]]
      # bartletTest[1,5] = if(bartletTest[1,4] > 0.05){"homogeneous"}else{"heterogeneous"}
      #
      # path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/ttest/betweenSubjectHomogenity.xls")
      # WriteXLS(bartletTest, path)
      
      
      #####################################################################################
      # 2. Outlier Detection #
      #####################################################################################
      
      
      library(dplyr)
      
      dataFull = dataFull
      
      data_grp <- group_by(dataFull, subject)
      plotData <- summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
      
      plotData$subject <- as.factor(plotData$subject)
      
      plotPath = paste0(toupper(as.character(levels(dataFull$analyte))), " Values")
      
      ggplot(plotData, aes(y = Mean, x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/log/Before outlier control.jpg")
      ggsave(path, width = 12, height = 9)
      
      ggplot(plotData, aes(y = exp(Mean), x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = exp(Max), ymin = exp(Min)), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/org/Before outlier control.jpg")
      ggsave(path, width = 12, height = 9)
      
      #####################################################################################
      # Step 1. Look for outliers in the sets of duplicate results using the Cochran test #
      # that examines the ratio of the maximum variance to the sum of the variances and   #
      # compares this to the appropriate critical values in statistical tables [1].       #
      #####################################################################################
      
      cat("\n------------------------------------","\nStep 1. Look for outliers in the sets of replicate results using the Cochran test","\n------------------------------------")
      
      
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
      
      outlierData = do.call(rbind.data.frame, outlierList)
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/outliers/Step1_outliers.xls")
      
      WriteXLS(outlierData, path)
      
      cat("\n------------------------------------","\nCheck for the outliers in the 'outlierData' table above","\n------------------------------------")
      
      cat("\n------------------------------------","\nStep 1 is succesfully completed. Moving on Step 2","\n------------------------------------")
      
      
      ## Plot
      data_grp <- group_by(dataFull, subject)
      plotData <- summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
      
      plotData$subject <- as.factor(plotData$subject)
      plotPath = paste0(toupper(as.character(levels(dataFull$analyte))), " Values")
      
      ggplot(plotData, aes(y = Mean, x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/log/After step1.jpg")
      ggsave(path, width = 12, height = 9)
      
      ggplot(plotData, aes(y = exp(Mean), x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = exp(Max), ymin = exp(Min)), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/org/After step1.jpg")
      ggsave(path, width = 12, height = 9)
      
      
      ##########################################################################################
      # Step 2. Use Cochran test to look for outliers in the variances of the results from     #
      # each subject to see if any individual's dispersion of results is larger or smaller     #
      # than that of the group as a whole (i.e. examining the heterogeneity of  within-subject #
      # biological variation  [1].                                                             #
      ##########################################################################################
      
      
      cat("\n------------------------------------","\nStep 2. Use Cochran test to look for outliers in the variances of the results from each subject","\n------------------------------------")
      
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
      
      outlierData2 = do.call(rbind.data.frame, outlierDataList2)
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/outliers/Step2_outliers.xls")
      WriteXLS(outlierData2, path)
      
      cat("\n------------------------------------","\nCheck for the outliers in the 'outlierData2' table above","\n------------------------------------")
      cat("\n------------------------------------","\nStep 2 is succesfully completed. Moving on Step 3","\n------------------------------------")
      
      ## Plot
      
      data_grp <- group_by(dataFull2, subject)
      plotData <- summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
      
      plotData$subject <- as.factor(plotData$subject)
      
      ggplot(plotData, aes(y = Mean, x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/log/After step2.jpg")
      ggsave(path, width = 12, height = 9)
      
      ggplot(plotData, aes(y = exp(Mean), x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = exp(Max), ymin = exp(Min)), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/org/After step2.jpg")
      ggsave(path, width = 12, height = 9)
      
      
      ###############################################################################################
      # Step 3. Use Reed's criterion to see if any individual has a mean value that differs greatly #
      # from the other subjects [1].                                                                #
      ###############################################################################################
      
      cat("\n------------------------------------","\nStep 3. Use Reed's criterion to see if any individual has a mean value that differs greatly from the other subjects","\n------------------------------------")
      
      dataReed = dataFull2
      analyteValue = dataReed$value
      subjects = as.factor(dataReed$subject)
      
      means <- tapply(analyteValue, subjects, mean)
      names(means) <- levels(subjects)
      
      
      maxDifference = 1
      absoluteDifference = 1
      
      while(maxDifference > absoluteDifference/3){
        outlierListReed = list()
      
      
        ## High-value outliers
        order = sort(means, decreasing = TRUE)
        maxDifference = abs(diff(order[1:2]))
        absoluteDifference = abs((diff(range(order))))
      
      
        if(maxDifference > absoluteDifference/3){
      
          outlier = order[1]
          outSubject = names(outlier)
      
          outlierListReed[[as.numeric(outSubject)]] = as.numeric(outSubject)
      
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
          outlierListReed[[as.numeric(outSubject)]] = as.numeric(outSubject)
      
          cat("subject", outSubject, "is a low-value outlier \n-------------------------------------------- \nOutlier detection process is successfully completed \n--------------------------------------------")
        }else{
      
          cat("\nNo low-value outlier\n" )
        }
      
      
        if(length(outlierListReed) != 0){
          outlierSubjectsReed = do.call(rbind.data.frame,outlierListReed)
          names(outlierSubjectsReed) = "Subjects"
      
          dataReed = dataReed[!(dataReed$subject %in%  outlierSubjectsReed[,1]),]
      
          means = means[!(names(means) %in% outlierSubjectsReed[,1])]
      
        }else{
      
          dataReed = dataReed
      
        }
      }
      
      cat("\n------------------------------------","\nStep 3 is succesfully completed. All outliers found and removed from dataset.","\n------------------------------------")
      
      dataWithoutOutliers = dataReed
      
      dim(dataFull)
      dim(dataWithoutOutliers)
      
      write.table(dataWithoutOutliers, paste0(testName,"_withoutOutlier.txt"), quote=F, sep="\t", row.names = F)
      
      
      ## Plot
      data_grp <- group_by(dataWithoutOutliers, subject)
      plotData <- summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
      
      plotData$subject <- as.factor(plotData$subject)
      
      ggplot(plotData, aes(y = Mean, x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/log/After step3.jpg")
      ggsave(path, width = 12, height = 9)
      
      ggplot(plotData, aes(y = exp(Mean), x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = exp(Max), ymin = exp(Min)), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/org/After step3.jpg")
      ggsave(path, width = 12, height = 9)
      
      
      
      ##########################################################################
      # Between and Within Subject Homogenity
      #########################################################################
       data = dataWithoutOutliers
       analyteValue = data$value
       replicate = as.factor(data$subject)
       nreplicate = length(levels(replicate))
       nameanalyte = toupper(as.character(levels(data$analyte)))
      
      # #Between-subject variance homogeneity
      # betweenVar = tapply(analyteValue, factor(replicate), var)
      # names(betweenVar) = levels(unique( factor(replicate)))
      # betweenVar  = cbind.data.frame(subject = names(betweenVar), variance = betweenVar)
      #
      # path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/ttest/betweenSubjectVariance.xls")
      # WriteXLS(betweenVar, path, row.names = F)
      #
      #
      # betwSampBartRes = bartlett.test(analyteValue, factor(replicate))
      
      #Between-duplicate variance homogeneity
      bartletres = matrix(NA,length(levels(replicate)),1)
      rownames(bartletres) =levels(replicate)
      colnames(bartletres) = "p.val"
      
      
      for (i in 1:length(levels(replicate))){
        x = data[data$subject == as.numeric(levels(replicate))[i],]
        bart = bartlett.test(x$value, factor(x$replicate.subject))
        bartletres[i,1] = bart$p.value
      }
      
      bartletlogres = data.frame(row = rownames(bartletres), bartletres)
      bartletlogres = bartletlogres[complete.cases(bartletlogres),]
      
      withinsubjectHomogenity =  data.frame(matrix(NA,1,2))
      names(withinsubjectHomogenity) = c("Within subject homogenity", "Subjects")
      tbl = table(bartletlogres$p.val<0.05)
      
      if(TRUE %in% names(tbl)){
      
        withinsubjectHomogenity[1,1] = "Some subjects have heterogeneous variances"
      
        subjects = rownames(bartletlogres[bartletlogres$p.val<0.05,])
      
        if(length(subjects) == 1){
      
          withinsubjectHomogenity[1,2] = subjects[[1]]
      
        }else{
      
          withinsubjectHomogenity[1,2] = paste(subjects, collapse = ",")
      
      
        }
      
      }else{
        withinsubjectHomogenity[1,1] = "homogeneous"
        withinsubjectHomogenity[1,2] = "No subject"
      }
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/ttest/withinsubjectHomogenity.xls")
      WriteXLS(withinsubjectHomogenity, path)
      
      head(data)
      dim(data)
      
      if(withinsubjectHomogenity$Subjects !=  "No subject"){
        nonHomogenSubjects = as.numeric(withinsubjectHomogenity$Subjects)
        data = data[!(data$subject %in% nonHomogenSubjects),]
      }
      
      #############################################################################################
      analyteValue = data$value
      replicate = as.factor(data$subject)
      nreplicate = length(levels(replicate))
      nameanalyte = toupper(as.character(levels(data$analyte)))
      
      #Between-subject variance homogeneity
      betweenVar = tapply(analyteValue, factor(replicate), var)
      names(betweenVar) = levels(unique( factor(replicate)))
      betweenVar  = cbind.data.frame(subject = names(betweenVar), variance = betweenVar)
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/ttest/betweenSubjectVariance.xls")
      WriteXLS(betweenVar, path, row.names = F)
      
      
      betwSampBartRes = bartlett.test(analyteValue, factor(replicate))
      
      bartletTest =  data.frame(matrix(NA,1,5))
      names(bartletTest) = c("Test", "Statistic", "df", "p-value", "result")
      
      bartletTest[1,1] = "Bartlett"
      bartletTest[1,2] = betwSampBartRes$statistic[[1]]
      bartletTest[1,3] = betwSampBartRes$parameter[[1]]
      bartletTest[1,4] = betwSampBartRes$p.value[[1]]
      bartletTest[1,5] = if(bartletTest[1,4] > 0.05){"homogeneous"}else{"heterogeneous"}
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/ttest/betweenSubjectHomogenity.xls")
      WriteXLS(bartletTest, path)
      
      
      ## Plot
      data_grp <- group_by(data, subject)
      plotData <- summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
      
      plotData$subject <- as.factor(plotData$subject)
      
      ggplot(plotData, aes(y = Mean, x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/log/After within variance homogenity.jpg")
      ggsave(path, width = 12, height = 9)
      
      ggplot(plotData, aes(y = exp(Mean), x = subject)) +
        geom_point() +
        geom_errorbar(aes(ymax = exp(Max), ymin = exp(Min)), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(bquote('PLT-I'~(x10^3 / mu~'mol'))) +
        xlab("Subjects")
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/plots/org/After within variance homogenity.jpg")
      ggsave(path, width = 12, height = 9)
      
      
      
      #####################################################################################
      # 1. Homogenity for Gender #
      #####################################################################################
      
      # data = dataWithoutOutliers
      dataHomogenity = data
      analyteValue = data$value
      subjects = as.factor(data$subject)
      gender = unique(data[-c(1,3,5:8)])[,"gender"]
      means <- tapply(analyteValue, subjects, mean)
      names(means) <- levels(subjects)
      
      bartlet = bartlett.test(means, gender)
      dataTtest = cbind.data.frame(means, gender)
      
      
      HomogenityGenderResult =  data.frame(matrix(NA,1,5))
      names(HomogenityGenderResult) = c("Test", "Statistic", "df", "p-value", "result")
      
      HomogenityGenderResult[1,1] = "Bartlett"
      HomogenityGenderResult[1,2] = bartlet$statistic[[1]]
      HomogenityGenderResult[1,3] = bartlet$parameter[[1]]
      HomogenityGenderResult[1,4] = bartlet$p.value[[1]]
      HomogenityGenderResult[1,5] = if(bartlet$p.value > 0.05){"homogeneous"}else{"heterogeneous"}
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/ttest/HomogenityGenderResult.xls")
      WriteXLS(HomogenityGenderResult, path)
      
      
      dataHomogenity2 = data[,c("subject", "value", "gender")]
      vars =  tapply(dataHomogenity2$value, as.factor(dataHomogenity2$subject), var)
      names(vars) = unique(dataHomogenity2$subject)
      
      dataHomogenity3 = cbind.data.frame(vars, names(vars),gender)
      bartletGender = bartlett.test(dataHomogenity3$vars, dataHomogenity3$gender)
      
      
      if(bartlet$p.value > 0.05){ varEqual = TRUE}else{ varEqual = FALSE}
      
      ttest = t.test(means~gender, data = dataTtest, var.equal = varEqual)
      
      sd = tapply(means, gender, sd)
      
      ttestResult =  data.frame(matrix(NA,1,6))
      names(ttestResult) = c("Analyte", "Mean_female", "SD_female", "Mean_male", "SD_male", "pvalue")
      ttestResult[1,1] = toupper(as.character(levels(dataFull$analyte)))
      ttestResult[1,2] = ttest$estimate[[1]]
      ttestResult[1,3] = sd[[1]]
      ttestResult[1,4] = ttest$estimate[[2]]
      ttestResult[1,5] = sd[[2]]
      ttestResult[1,6] = ttest$p.value
      
      path = paste0("Results/Analyte/", toupper(as.character(levels(dataFull$analyte))),"/ttest/ttest.xls")
      #write.table(ttestResult, path, quote=F, sep="\t", row.names = F)
      WriteXLS(ttestResult, path)
      
      
      
      
      #####################################################################################
      # 1. Linear Mixed Effects Models #
      #####################################################################################
      
      #####################################################################################
      # a. All Group #
      #####################################################################################
      
      
      library(lme4)
      library(multcomp)
      library(nlme)
      
      dataFull = data
      
      cat("\n------------------------------------\nStarting LME analysis for", toupper(as.character(levels(dataFull$analyte))),"\n------------------------------------")
      
      dataFull$subject = as.factor(dataFull$subject)
      dataFull$replicate = as.factor(dataFull$replicate)
      dataFull$time = as.factor(dataFull$time)
      
      N <- nrow(dataFull)
      r <- length(unique(dataFull$subject))
      ni <- as.numeric(table(dataFull[ ,"subject"]))
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      ## lme model
      bv <- lme(value ~1, random=~1|subject/time,data=dataFull)
      # bv <- lmer(data=dataFull, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
      
      
      summary = summary(bv)
      
      grandMean = summary$coefficients$fixed[[1]]
      
      seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
      
      llMean = grandMean-1.96*seGrandMean
      ulMean = grandMean+1.96*seGrandMean
      
      table= nlme::VarCorr(bv)
      
      
      sigma_between = as.numeric(table[2,2])
      sigma_within = as.numeric(table[4,2])
      sigma_analytical = as.numeric(table[5,2])
      sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      w1 = 2
      w2 = sgConstant
      w3 = 2
      w1u = 2
      w2u = sgConstant
      w3u = 2
      c2u = (w2u-w1u)/(w3u)
      c3u = w2u-1-c2u
      c2 = 1
      c3 = 0
      alpha = 0.025
      inf = 999999999
      
      ll_analytical = sqrt((sigma_analytical^2)/qf(alpha, bv$dims$ngrps[[1]], inf, lower.tail = F))
      ul_analytical = sqrt((sigma_analytical^2)/qf(alpha, bv$dims$ngrps[[1]], inf, lower.tail = T))
      
      lower_analytical = ll_analytical/grandMean
      upper_analytical = ul_analytical/grandMean
      
      sigma2_between = sigma_between^2
      sigma2_within = sigma_within^2
      sigma2_analytical = sigma_analytical^2
      
      msu_within = 2*sigma2_within+sigma2_analytical
      msu_between = sigma2_between*sgConstant + sigma2_analytical +  sigma2_within*2
      
      dfbetween = (bv$dims$ngrps[[2]]-1)
      dfwithin = (bv$dims$ngrps[[1]]-bv$dims$ngrps[[2]])
      dfanalytical = (bv$dims$ngrps[[1]])
      
      g1 = 1-1/(qf(alpha, dfbetween, inf, lower.tail = F))
      g2 = 1-1/(qf(alpha, dfwithin, inf, lower.tail = F))
      g3 = 1-1/(qf(alpha, dfanalytical, inf, lower.tail = F))
      
      h1 =1/(qf(1-alpha, dfbetween, inf, lower.tail = F))-1
      h2 = 1/(qf(1-alpha, dfwithin, inf, lower.tail = F))-1
      h3 = 1/(qf(1-alpha, dfanalytical, inf, lower.tail = F))-1
      
      g12 = (((qf(alpha, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha, dfbetween, dfwithin, lower.tail = F))
      g13 = (((qf(alpha, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha, dfbetween, dfanalytical, lower.tail = F))
      g23 = (((qf(alpha, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha, dfwithin, dfanalytical, lower.tail = F))
      g32 = (((qf(alpha, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha, dfanalytical, dfwithin, lower.tail = F))
      
      h12 = (((1-qf(1-alpha,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha,  dfbetween, dfwithin, lower.tail = F))
      h13 = (((1-qf(1-alpha,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha,  dfbetween, dfanalytical, lower.tail = F))
      h23 = (((1-qf(1-alpha, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha, dfwithin, dfanalytical, lower.tail = F))
      h32 = (((1-qf(1-alpha, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha, dfanalytical, dfwithin, lower.tail = F))
      
      VLba = (g2^2)*(msu_within^2)+(h3^2)*(sigma2_analytical^2)+g23*msu_within*sigma2_analytical
      VUba = (h2^2)*(msu_within^2)+(g3^2)*(sigma2_analytical^2)+h23*msu_within*sigma2_analytical
      
      VLa = g1^2*msu_between^2+h2^2*c2^2*msu_within^2+h3^2*c3^2*sigma2_analytical^2+g12*c2*msu_between*msu_within+g13*abs(c3)*msu_between*sigma2_analytical
      VLa2 = h1^2*msu_between^2+g2^2*c2^2*msu_within^2+g3^2*c3^2*sigma2_analytical^2+h12*c2*msu_between*msu_within+h13*abs(c3)*msu_between*sigma2_analytical
      
      ll_within = sqrt((msu_within-sigma2_analytical-sqrt(VLba))/2)
      ul_within = sqrt((msu_within-sigma2_analytical+sqrt(VUba))/2)
      
      lower_within = ll_within/grandMean
      upper_within = ul_within/grandMean
      
      ll_between = sqrt((msu_between - msu_within - sqrt(VLa))/sgConstant)
      ul_between = sqrt((msu_between - msu_within + sqrt(VLa2))/sgConstant)
      
      lower_between = ll_between/grandMean
      upper_between = ul_between/grandMean
      
      c34 = (sgConstant-2)/2
      c35 = sgConstant-1-c34
      
      ll_total = sqrt((msu_between + c34*msu_within + c35*sigma2_analytical- sqrt(VLa))/sgConstant)
      ul_total = sqrt((msu_between + c34*msu_within + c35*sigma2_analytical+ sqrt(VLa2))/sgConstant)
      
      lower_total = ll_total/grandMean
      upper_total = ul_total/grandMean
      
      ## CV table (log transfomed)
      ########################################################################################
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma (log)", "CV (log)", "Lower (log)", "Upper (log)", "CV% (log)")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "sigma (log)"] = sigma_between
      CVTable[2, "sigma (log)"] = sigma_within
      CVTable[3, "sigma (log)"] = sigma_analytical
      CVTable[4, "sigma (log)"] = sigma_total
      
      CVTable[1, "CV (log)"] = CV_between
      CVTable[2, "CV (log)"] = CV_within
      CVTable[3, "CV (log)"] = CV_analytical
      CVTable[4, "CV (log)"] = CV_total
      
      CVTable[1, "Lower (log)"] = lower_between
      CVTable[2, "Lower (log)"] = lower_within
      CVTable[3, "Lower (log)"] = lower_analytical
      CVTable[4, "Lower (log)"] = lower_total
      
      CVTable[1, "Upper (log)"] = upper_between
      CVTable[2, "Upper (log)"] = upper_within
      CVTable[3, "Upper (log)"] = upper_analytical
      CVTable[4, "Upper (log)"] = upper_total
      
      CVTable[1, "CV% (log)"] = CV_between*100
      CVTable[2, "CV% (log)"] = CV_within*100
      CVTable[3, "CV% (log)"] = CV_analytical*100
      CVTable[4, "CV% (log)"] = CV_total*100
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = 1.96 #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean (log)", "Lower Mean (log)", "Upper Mean (log)", "CV_A% (log)", "CV_I% (log)", "CV_G%v", "II (log)", "RCV% (log)", "n")
      CVresults[1,1] = "All Subjects"
      CVresults[1,2] = grandMean
      CVresults[1,3] = llMean
      CVresults[1,4] = ulMean
      CVresults[1,5] = CVa
      CVresults[1,6] = CVi
      CVresults[1,7] = CVg
      CVresults[1,8] = II
      CVresults[1,9] = RCV
      CVresults[1,10] = nrow(dataFull)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal (log)","Desirable (log)","Minimal (log)")
      colnames(errorTable) = c("Imprecision,% (log)", "Bias,% (log)","TEa,% (log)")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,2))
      errorTable[2,1] = paste("<",round(0.50*CVi,2))
      errorTable[3,1] = paste("<",round(0.75*CVi,2))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      
      CVTable
      CVresults
      errorTable
      
      
      path = paste0("Results/Analyte/",toupper(as.character(levels(dataFull$analyte))),"/log/all")
      
      # write.table(CVTable, paste0(path,"/CVTable_All_log.txt"), quote=F, sep="\t", row.names = F)
      # write.table(CVresults, paste0(path,"/CVresults_All_log.txt"), quote=F, sep="\t", row.names = F)
      # write.table(errorTable, paste0(path,"/errorTable_All_log.txt"), quote=F, sep="\t", row.names = T)
      WriteXLS(CVTable, paste0(path,"/CVTable_All_log.xls"))
      WriteXLS(CVresults, paste0(path,"/CVresults_All_log.xls"))
      #WriteXLS(errorTable, paste0(path,"/errorTable_All_log.xls"))
      
      
      
      ## CV table (back transformed)
      ########################################################################################
      
      
      ## CV table (back to original data)
      
      transformedMean = exp(grandMean+0.5*sigma_total^2)
      
      llTransformedMean = exp(llMean+0.5*sigma_total^2)
      ulTransformedMean = exp(ulMean+0.5*sigma_total^2)
      
      
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma (org)", "CV (org)", "Lower (org)", "Upper (org)", "CV% (org)")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "CV (org)"] = sqrt(exp(sigma_between^2)-1)
      CVTable[2, "CV (org)"] = sqrt(exp(sigma_within^2)-1)
      CVTable[3, "CV (org)"] = sqrt(exp(sigma_analytical^2)-1)
      CVTable[4, "CV (org)"] = sqrt(exp(sigma_total^2)-1)
      
      CVTable[1, "sigma (org)"] = CVTable[1, "CV (org)"]*transformedMean/100
      CVTable[2, "sigma (org)"] = CVTable[2, "CV (org)"]*transformedMean/100
      CVTable[3, "sigma (org)"] = CVTable[3, "CV (org)"]*transformedMean/100
      CVTable[4, "sigma (org)"] = CVTable[4, "CV (org)"]*transformedMean/100
      
      CVTable[1, "Lower (org)"] = sqrt(exp(ll_between^2)-1)
      CVTable[2, "Lower (org)"] = sqrt(exp(ll_within^2)-1)
      CVTable[3, "Lower (org)"] = sqrt(exp(ll_analytical^2)-1)
      CVTable[4, "Lower (org)"] = sqrt(exp(ll_total^2)-1)
      
      CVTable[1, "Upper (org)"] = sqrt(exp(ul_between^2)-1)
      CVTable[2, "Upper (org)"] = sqrt(exp(ul_within^2)-1)
      CVTable[3, "Upper (org)"] = sqrt(exp(ul_analytical^2)-1)
      CVTable[4, "Upper (org)"] = sqrt(exp(ul_total^2)-1)
      
      CVTable[1, "CV% (org)"] = CVTable[1, "CV (org)"]*100
      CVTable[2, "CV% (org)"] = CVTable[2, "CV (org)"]*100
      CVTable[3, "CV% (org)"] = CVTable[3, "CV (org)"]*100
      CVTable[4, "CV% (org)"] = CVTable[4, "CV (org)"]*100
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = 1.96 #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean (org)", "Lower Mean (org)", "Upper Mean (org)", "CV_A% (org)", "CV_I% (org)", "CV_G% (org)", "II (org)", "RCV% (org)", "n")
      CVresults[1,1] = "All Subjects"
      CVresults[1,2] = transformedMean
      CVresults[1,3] = llTransformedMean
      CVresults[1,4] = ulTransformedMean
      CVresults[1,5] = CVa
      CVresults[1,6] = CVi
      CVresults[1,7] = CVg
      CVresults[1,8] = II
      CVresults[1,9] = RCV
      CVresults[1,10] = nrow(dataFull)
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal (org)","Desirable (org)","Minimal (org)")
      colnames(errorTable) = c("Imprecision,% (org)", "Bias,% (org)","TEa,% (org)")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,2))
      errorTable[2,1] = paste("<",round(0.50*CVi,2))
      errorTable[3,1] = paste("<",round(0.75*CVi,2))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2))
                                        ,2))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      
      CVTable
      CVresults
      errorTable
      
      path = paste0("Results/Analyte/",toupper(as.character(levels(dataFull$analyte))),"/org/all")
      
      # write.table(CVTable, paste0(path,"/CVTable_All_org.txt"), quote=F, sep="\t", row.names = F)
      # write.table(CVresults, paste0(path,"/CVresults_All_org.txt"), quote=F, sep="\t", row.names = F)
      # write.table(errorTable, paste0(path,"/errorTable_All_org.txt"), quote=F, sep="\t", row.names = T)
      
      WriteXLS(CVTable, paste0(path,"/CVTable_All_org.xls"))
      WriteXLS(CVresults, paste0(path,"/CVresults_All_org.xls"))
      #WriteXLS(errorTable, paste0(path,"/errorTable_All_org.xls"))
      
      ################################################################################
      
      dataFull = data
      
      dataGender = split(dataFull, dataFull$gender)
      
      #####################################################################################
      # b. Gender = Male #
      #####################################################################################
      
      dataMale = dataGender$Male
      
      cat("\n------------------------------------\nStarting LME analysis for", toupper(as.character(levels(dataMale$analyte))),"\n------------------------------------")
      
      dataMale$subject = as.factor(dataMale$subject)
      dataMale$replicate = as.factor(dataMale$replicate)
      dataMale$time = as.factor(dataMale$time)
      
      N <- nrow(dataMale)
      r <- length(unique(dataMale$subject))
      ni <- as.numeric(table(dataMale[ ,"subject"]))
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      
      ## lme model
      bv <- lme(value ~1, random=~1|subject/time,data=dataMale)
      summary = summary(bv)
      
      grandMean = summary$coefficients$fixed[[1]]
      
      seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
      
      llMean = grandMean-1.96*seGrandMean
      ulMean = grandMean+1.96*seGrandMean
      
      table= nlme::VarCorr(bv)
      
      table= nlme::VarCorr(bv)
      
      sigma_between = as.numeric(table[2,2])
      sigma_within = as.numeric(table[4,2])
      sigma_analytical = as.numeric(table[5,2])
      sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      w1 = 2
      w2 = sgConstant
      w3 = 2
      w1u = 2
      w2u = sgConstant
      w3u = 2
      c2u = (w2u-w1u)/(w3u)
      c3u = w2u-1-c2u
      c2 = 1
      c3 = 0
      alpha = 0.025
      inf = 999999999
      
      ll_analytical = sqrt((sigma_analytical^2)/qf(alpha, bv$dims$ngrps[[1]], inf, lower.tail = F))
      ul_analytical = sqrt((sigma_analytical^2)/qf(alpha, bv$dims$ngrps[[1]], inf, lower.tail = T))
      
      lower_analytical = ll_analytical/grandMean
      upper_analytical = ul_analytical/grandMean
      
      sigma2_between = sigma_between^2
      sigma2_within = sigma_within^2
      sigma2_analytical = sigma_analytical^2
      
      msu_within = 2*sigma2_within+sigma2_analytical
      msu_between = sigma2_between*sgConstant + sigma2_analytical +  sigma2_within*2
      
      
      dfbetween = (bv$dims$ngrps[[2]]-1)
      dfwithin = (bv$dims$ngrps[[1]]-bv$dims$ngrps[[2]])
      dfanalytical = (bv$dims$ngrps[[1]])
      
      g1 = 1-1/(qf(alpha, dfbetween, inf, lower.tail = F))
      g2 = 1-1/(qf(alpha, dfwithin, inf, lower.tail = F))
      g3 = 1-1/(qf(alpha, dfanalytical, inf, lower.tail = F))
      
      h1 =1/(qf(1-alpha, dfbetween, inf, lower.tail = F))-1
      h2 = 1/(qf(1-alpha, dfwithin, inf, lower.tail = F))-1
      h3 = 1/(qf(1-alpha, dfanalytical, inf, lower.tail = F))-1
      
      g12 = (((qf(alpha, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha, dfbetween, dfwithin, lower.tail = F))
      g13 = (((qf(alpha, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha, dfbetween, dfanalytical, lower.tail = F))
      g23 = (((qf(alpha, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha, dfwithin, dfanalytical, lower.tail = F))
      g32 = (((qf(alpha, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha, dfanalytical, dfwithin, lower.tail = F))
      
      h12 = (((1-qf(1-alpha,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha,  dfbetween, dfwithin, lower.tail = F))
      h13 = (((1-qf(1-alpha,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha,  dfbetween, dfanalytical, lower.tail = F))
      h23 = (((1-qf(1-alpha, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha, dfwithin, dfanalytical, lower.tail = F))
      h32 = (((1-qf(1-alpha, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha, dfanalytical, dfwithin, lower.tail = F))
      
      VLba = (g2^2)*(msu_within^2)+(h3^2)*(sigma2_analytical^2)+g23*msu_within*sigma2_analytical
      VUba = (h2^2)*(msu_within^2)+(g3^2)*(sigma2_analytical^2)+h23*msu_within*sigma2_analytical
      
      VLa = g1^2*msu_between^2+h2^2*c2^2*msu_within^2+h3^2*c3^2*sigma2_analytical^2+g12*c2*msu_between*msu_within+g13*abs(c3)*msu_between*sigma2_analytical
      VLa2 = h1^2*msu_between^2+g2^2*c2^2*msu_within^2+g3^2*c3^2*sigma2_analytical^2+h12*c2*msu_between*msu_within+h13*abs(c3)*msu_between*sigma2_analytical
      
      ll_within = sqrt((msu_within-sigma2_analytical-sqrt(VLba))/2)
      ul_within = sqrt((msu_within-sigma2_analytical+sqrt(VUba))/2)
      
      lower_within = ll_within/grandMean
      upper_within = ul_within/grandMean
      
      ll_between = sqrt((msu_between - msu_within - sqrt(VLa))/sgConstant)
      ul_between = sqrt((msu_between - msu_within + sqrt(VLa2))/sgConstant)
      
      lower_between = ll_between/grandMean
      upper_between = ul_between/grandMean
      
      c34 = (sgConstant-2)/2
      c35 = sgConstant-1-c34
      
      ll_total = sqrt((msu_between + c34*msu_within + c35*sigma2_analytical- sqrt(VLa))/sgConstant)
      ul_total = sqrt((msu_between + c34*msu_within + c35*sigma2_analytical+ sqrt(VLa2))/sgConstant)
      
      lower_total = ll_total/grandMean
      upper_total = ul_total/grandMean
      
      ## CV table (log transformed)
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma (log)", "CV (log)", "Lower (log)", "Upper (log)", "CV% (log)")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "sigma (log)"] = sigma_between
      CVTable[2, "sigma (log)"] = sigma_within
      CVTable[3, "sigma (log)"] = sigma_analytical
      CVTable[4, "sigma (log)"] = sigma_total
      
      CVTable[1, "CV (log)"] = CV_between
      CVTable[2, "CV (log)"] = CV_within
      CVTable[3, "CV (log)"] = CV_analytical
      CVTable[4, "CV (log)"] = CV_total
      
      CVTable[1, "Lower (log)"] = lower_between
      CVTable[2, "Lower (log)"] = lower_within
      CVTable[3, "Lower (log)"] = lower_analytical
      CVTable[4, "Lower (log)"] = lower_total
      
      CVTable[1, "Upper (log)"] = upper_between
      CVTable[2, "Upper (log)"] = upper_within
      CVTable[3, "Upper (log)"] = upper_analytical
      CVTable[4, "Upper (log)"] = upper_total
      
      CVTable[1, "CV% (log)"] = CV_between*100
      CVTable[2, "CV% (log)"] = CV_within*100
      CVTable[3, "CV% (log)"] = CV_analytical*100
      CVTable[4, "CV% (log)"] = CV_total*100
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = 1.96 #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean (log)", "Lower Mean (log)", "Upper Mean (log)", "CV_A% (log)", "CV_I% (log)", "CV_G% (log)", "II (log)", "RCV% (log)", "n")
      CVresults[1,1] = "All Subjects"
      CVresults[1,2] = grandMean
      CVresults[1,3] = llMean
      CVresults[1,4] = ulMean
      CVresults[1,5] = CVa
      CVresults[1,6] = CVi
      CVresults[1,7] = CVg
      CVresults[1,8] = II
      CVresults[1,9] = RCV
      CVresults[1,10] = nrow(dataMale)
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal (log)","Desirable (log)","Minimal (log)")
      colnames(errorTable) = c("Imprecision,% (log)", "Bias,%","TEa,% (log)")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,2))
      errorTable[2,1] = paste("<",round(0.50*CVi,2))
      errorTable[3,1] = paste("<",round(0.75*CVi,2))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      
      CVTable
      CVresults
      errorTable
      
      path = paste0("Results/Analyte/",toupper(as.character(levels(dataFull$analyte))),"/log/male")
      
      # write.table(CVTable, paste0(path,"/CVTable_Male_log.txt"), quote=F, sep="\t", row.names = F)
      # write.table(CVresults, paste0(path,"/CVresults_Male_log.txt"), quote=F, sep="\t", row.names = F)
      # write.table(errorTable, paste0(path,"/errorTable_Male_log.txt"), quote=F, sep="\t", row.names = T)
      
      WriteXLS(CVTable, paste0(path,"/CVTable_Male_log.xls"))
      WriteXLS(CVresults, paste0(path,"/CVresults_Male_log.xls"))
      #WriteXLS(errorTable, paste0(path,"/errorTable_Male_log.xls"))
      
      
      #############################################################################################
      
      ## CV table (back to original data)
      
      transformedMean = exp(grandMean+0.5*sigma_total^2)
      
      llTransformedMean = exp(llMean+0.5*sigma_total^2)
      ulTransformedMean = exp(ulMean+0.5*sigma_total^2)
      
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma (org)", "CV (org)", "Lower (org)", "Upper (org)", "CV% (org)")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "CV (org)"] = sqrt(exp(sigma_between^2)-1)
      CVTable[2, "CV (org)"] = sqrt(exp(sigma_within^2)-1)
      CVTable[3, "CV (org)"] = sqrt(exp(sigma_analytical^2)-1)
      CVTable[4, "CV (org)"] = sqrt(exp(sigma_total^2)-1)
      
      CVTable[1, "sigma (org)"] = CVTable[1, "CV (org)"]*transformedMean/100
      CVTable[2, "sigma (org)"] = CVTable[2, "CV (org)"]*transformedMean/100
      CVTable[3, "sigma (org)"] = CVTable[3, "CV (org)"]*transformedMean/100
      CVTable[4, "sigma (org)"] = CVTable[4, "CV (org)"]*transformedMean/100
      
      CVTable[1, "Lower (org)"] = sqrt(exp(ll_between^2)-1)
      CVTable[2, "Lower (org)"] = sqrt(exp(ll_within^2)-1)
      CVTable[3, "Lower (org)"] = sqrt(exp(ll_analytical^2)-1)
      CVTable[4, "Lower (org)"] = sqrt(exp(ll_total^2)-1)
      
      CVTable[1, "Upper (org)"] = sqrt(exp(ul_between^2)-1)
      CVTable[2, "Upper (org)"] = sqrt(exp(ul_within^2)-1)
      CVTable[3, "Upper (org)"] = sqrt(exp(ul_analytical^2)-1)
      CVTable[4, "Upper (org)"] = sqrt(exp(ul_total^2)-1)
      
      CVTable[1, "CV% (org)"] = CVTable[1, "CV (org)"]*100
      CVTable[2, "CV% (org)"] = CVTable[2, "CV (org)"]*100
      CVTable[3, "CV% (org)"] = CVTable[3, "CV (org)"]*100
      CVTable[4, "CV% (org)"] = CVTable[4, "CV (org)"]*100
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = 1.96 #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean (org)", "Lower Mean (org)", "Upper Mean (org)", "CV_A% (org)", "CV_I% (org)", "CV_G% (org)", "II (org)", "RCV% (org)", "n")
      CVresults[1,1] = "All Subjects"
      CVresults[1,2] = transformedMean
      CVresults[1,3] = llTransformedMean
      CVresults[1,4] = ulTransformedMean
      CVresults[1,5] = CVa
      CVresults[1,6] = CVi
      CVresults[1,7] = CVg
      CVresults[1,8] = II
      CVresults[1,9] = RCV
      CVresults[1,10] = nrow(dataMale)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal (org)","Desirable (org)","Minimal (org)")
      colnames(errorTable) = c("Imprecision,% (org)", "Bias,%","TEa,% (org)")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,2))
      errorTable[2,1] = paste("<",round(0.50*CVi,2))
      errorTable[3,1] = paste("<",round(0.75*CVi,2))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      
      CVTable
      CVresults
      errorTable
      
      path = paste0("Results/Analyte/",toupper(as.character(levels(dataMale$analyte))),"/org/male")
      
      # write.table(CVTable, paste0(path,"/CVTable_Male_org.txt"), quote=F, sep="\t", row.names = F)
      # write.table(CVresults, paste0(path,"/CVresults_Male_org.txt"), quote=F, sep="\t", row.names = F)
      # write.table(errorTable, paste0(path,"/errorTable_Male_org.txt"), quote=F, sep="\t", row.names = T)
      
      WriteXLS(CVTable, paste0(path,"/CVTable_Male_org.xls"))
      WriteXLS(CVresults, paste0(path,"/CVresults_Male_org.xls"))
      # WriteXLS(errorTable, paste0(path,"/errorTable_Male_org.xls"))
      
      
      
      #####################################################################################
      # c. Gender = Female #
      #####################################################################################
      
      dataFemale = dataGender$Female
      
      cat("\n------------------------------------\nStarting LME analysis for", toupper(as.character(levels(dataFemale$analyte))),"\n------------------------------------")
      
      dataFemale$subject = as.factor(dataFemale$subject)
      dataFemale$replicate = as.factor(dataFemale$replicate)
      dataFemale$time = as.factor(dataFemale$time)
      
      N <- nrow(dataFemale)
      r <- length(unique(dataFemale$subject))
      ni <- as.numeric(table(dataFemale[ ,"subject"]))
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      
      ## lme model
      bv <- lme(value ~1, random=~1|subject/time,data=dataFemale)
      summary = summary(bv)
      
      grandMean = summary$coefficients$fixed[[1]]
      
      seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
      
      llMean = grandMean-1.96*seGrandMean
      ulMean = grandMean+1.96*seGrandMean
      
      table= nlme::VarCorr(bv)
      
      table= nlme::VarCorr(bv)
      
      sigma_between = as.numeric(table[2,2])
      sigma_within = as.numeric(table[4,2])
      sigma_analytical = as.numeric(table[5,2])
      sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      w1 = 2
      w2 = sgConstant
      w3 = 2
      w1u = 2
      w2u = sgConstant
      w3u = 2
      c2u = (w2u-w1u)/(w3u)
      c3u = w2u-1-c2u
      c2 = 1
      c3 = 0
      alpha = 0.025
      inf = 999999999
      
      ll_analytical = sqrt((sigma_analytical^2)/qf(alpha, bv$dims$ngrps[[1]], inf, lower.tail = F))
      ul_analytical = sqrt((sigma_analytical^2)/qf(alpha, bv$dims$ngrps[[1]], inf, lower.tail = T))
      
      lower_analytical = ll_analytical/grandMean
      upper_analytical = ul_analytical/grandMean
      
      sigma2_between = sigma_between^2
      sigma2_within = sigma_within^2
      sigma2_analytical = sigma_analytical^2
      
      msu_within = 2*sigma2_within+sigma2_analytical
      msu_between = sigma2_between*sgConstant + sigma2_analytical +  sigma2_within*2
      
      
      dfbetween = (bv$dims$ngrps[[2]]-1)
      dfwithin = (bv$dims$ngrps[[1]]-bv$dims$ngrps[[2]])
      dfanalytical = (bv$dims$ngrps[[1]])
      
      g1 = 1-1/(qf(alpha, dfbetween, inf, lower.tail = F))
      g2 = 1-1/(qf(alpha, dfwithin, inf, lower.tail = F))
      g3 = 1-1/(qf(alpha, dfanalytical, inf, lower.tail = F))
      
      h1 =1/(qf(1-alpha, dfbetween, inf, lower.tail = F))-1
      h2 = 1/(qf(1-alpha, dfwithin, inf, lower.tail = F))-1
      h3 = 1/(qf(1-alpha, dfanalytical, inf, lower.tail = F))-1
      
      g12 = (((qf(alpha, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha, dfbetween, dfwithin, lower.tail = F))
      g13 = (((qf(alpha, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha, dfbetween, dfanalytical, lower.tail = F))
      g23 = (((qf(alpha, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha, dfwithin, dfanalytical, lower.tail = F))
      g32 = (((qf(alpha, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha, dfanalytical, dfwithin, lower.tail = F))
      
      h12 = (((1-qf(1-alpha,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha,  dfbetween, dfwithin, lower.tail = F))
      h13 = (((1-qf(1-alpha,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha,  dfbetween, dfanalytical, lower.tail = F))
      h23 = (((1-qf(1-alpha, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha, dfwithin, dfanalytical, lower.tail = F))
      h32 = (((1-qf(1-alpha, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha, dfanalytical, dfwithin, lower.tail = F))
      
      VLba = (g2^2)*(msu_within^2)+(h3^2)*(sigma2_analytical^2)+g23*msu_within*sigma2_analytical
      VUba = (h2^2)*(msu_within^2)+(g3^2)*(sigma2_analytical^2)+h23*msu_within*sigma2_analytical
      
      VLa = g1^2*msu_between^2+h2^2*c2^2*msu_within^2+h3^2*c3^2*sigma2_analytical^2+g12*c2*msu_between*msu_within+g13*abs(c3)*msu_between*sigma2_analytical
      VLa2 = h1^2*msu_between^2+g2^2*c2^2*msu_within^2+g3^2*c3^2*sigma2_analytical^2+h12*c2*msu_between*msu_within+h13*abs(c3)*msu_between*sigma2_analytical
      
      ll_within = sqrt((msu_within-sigma2_analytical-sqrt(VLba))/2)
      ul_within = sqrt((msu_within-sigma2_analytical+sqrt(VUba))/2)
      
      lower_within = ll_within/grandMean
      upper_within = ul_within/grandMean
      
      ll_between = sqrt((msu_between - msu_within - sqrt(VLa))/sgConstant)
      ul_between = sqrt((msu_between - msu_within + sqrt(VLa2))/sgConstant)
      
      lower_between = ll_between/grandMean
      upper_between = ul_between/grandMean
      
      c34 = (sgConstant-2)/2
      c35 = sgConstant-1-c34
      
      ll_total = sqrt((msu_between + c34*msu_within + c35*sigma2_analytical- sqrt(VLa))/sgConstant)
      ul_total = sqrt((msu_between + c34*msu_within + c35*sigma2_analytical+ sqrt(VLa2))/sgConstant)
      
      lower_total = ll_total/grandMean
      upper_total = ul_total/grandMean
      
      ## CV table
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma (log)", "CV (log)", "Lower (log)", "Upper (log)", "CV% (log)")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "sigma (log)"] = sigma_between
      CVTable[2, "sigma (log)"] = sigma_within
      CVTable[3, "sigma (log)"] = sigma_analytical
      CVTable[4, "sigma (log)"] = sigma_total
      
      CVTable[1, "CV (log)"] = CV_between
      CVTable[2, "CV (log)"] = CV_within
      CVTable[3, "CV (log)"] = CV_analytical
      CVTable[4, "CV (log)"] = CV_total
      
      CVTable[1, "Lower (log)"] = lower_between
      CVTable[2, "Lower (log)"] = lower_within
      CVTable[3, "Lower (log)"] = lower_analytical
      CVTable[4, "Lower (log)"] = lower_total
      
      CVTable[1, "Upper (log)"] = upper_between
      CVTable[2, "Upper (log)"] = upper_within
      CVTable[3, "Upper (log)"] = upper_analytical
      CVTable[4, "Upper (log)"] = upper_total
      
      CVTable[1, "CV% (log)"] = CV_between*100
      CVTable[2, "CV% (log)"] = CV_within*100
      CVTable[3, "CV% (log)"] = CV_analytical*100
      CVTable[4, "CV% (log)"] = CV_total*100
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = 1.96 #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean (log)", "Lower Mean (log)", "Upper Mean (log)",  "CV_A% (log)", "CV_I% (log)", "CV_G% (log)", "II (log)", "RCV% (log)", "n")
      CVresults[1,1] = "All Subjects"
      CVresults[1,2] = grandMean
      CVresults[1,3] = llMean
      CVresults[1,4] = ulMean
      CVresults[1,5] = CVa
      CVresults[1,6] = CVi
      CVresults[1,7] = CVg
      CVresults[1,8] = II
      CVresults[1,9] = RCV
      CVresults[1,10] = nrow(dataFemale)
      
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal (log)","Desirable (log)","Minimal (log)")
      colnames(errorTable) = c("Imprecision,% (log)", "Bias,% (log)","TEa,% (log)")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,2))
      errorTable[2,1] = paste("<",round(0.50*CVi,2))
      errorTable[3,1] = paste("<",round(0.75*CVi,2))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      
      CVTable
      CVresults
      errorTable
      
      path = paste0("Results/Analyte/",toupper(as.character(levels(dataFull$analyte))),"/log/female")
      
      # write.table(CVTable, paste0(path,"/CVTable_Female_log.txt"), quote=F, sep="\t", row.names = F)
      # write.table(CVresults, paste0(path,"/CVresults_Female_log.txt"), quote=F, sep="\t", row.names = F)
      # write.table(errorTable, paste0(path,"/errorTable_Female_log.txt"), quote=F, sep="\t", row.names = T)
      
      WriteXLS(CVTable, paste0(path,"/CVTable_Female_log.xls"))
      WriteXLS(CVresults, paste0(path,"/CVresults_Female_log.xls"))
      # WriteXLS(errorTable, paste0(path,"/errorTable_Female_log.xls"))
      
      
      # CV table (back-transformed)
      ######################################################################################
      
      
      ## CV table (back to original data)
      
      transformedMean = exp(grandMean+0.5*sigma_total^2)
      
      llTransformedMean = exp(llMean+0.5*sigma_total^2)
      ulTransformedMean = exp(ulMean+0.5*sigma_total^2)
      
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma (org)", "CV (org)", "Lower (org)", "Upper (org)", "CV% (org)")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "CV (org)"] = sqrt(exp(sigma_between^2)-1)
      CVTable[2, "CV (org)"] = sqrt(exp(sigma_within^2)-1)
      CVTable[3, "CV (org)"] = sqrt(exp(sigma_analytical^2)-1)
      CVTable[4, "CV (org)"] = sqrt(exp(sigma_total^2)-1)
      
      CVTable[1, "sigma (org)"] = CVTable[1, "CV (org)"]*transformedMean/100
      CVTable[2, "sigma (org)"] = CVTable[2, "CV (org)"]*transformedMean/100
      CVTable[3, "sigma (org)"] = CVTable[3, "CV (org)"]*transformedMean/100
      CVTable[4, "sigma (org)"] = CVTable[4, "CV (org)"]*transformedMean/100
      
      CVTable[1, "Lower (org)"] = sqrt(exp(ll_between^2)-1)
      CVTable[2, "Lower (org)"] = sqrt(exp(ll_within^2)-1)
      CVTable[3, "Lower (org)"] = sqrt(exp(ll_analytical^2)-1)
      CVTable[4, "Lower (org)"] = sqrt(exp(ll_total^2)-1)
      
      CVTable[1, "Upper (org)"] = sqrt(exp(ul_between^2)-1)
      CVTable[2, "Upper (org)"] = sqrt(exp(ul_within^2)-1)
      CVTable[3, "Upper (org)"] = sqrt(exp(ul_analytical^2)-1)
      CVTable[4, "Upper (org)"] = sqrt(exp(ul_total^2)-1)
      
      CVTable[1, "CV% (org)"] = CVTable[1, "CV (org)"]*100
      CVTable[2, "CV% (org)"] = CVTable[2, "CV (org)"]*100
      CVTable[3, "CV% (org)"] = CVTable[3, "CV (org)"]*100
      CVTable[4, "CV% (org)"] = CVTable[4, "CV (org)"]*100
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = 1.96 #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean (org)", "Lower Mean (org)", "Upper Mean (org)", "CV_A% (org)", "CV_I% (org)", "CV_G% (org)", "II (org)", "RCV% (org)", "n")
      CVresults[1,1] = "All Subjects"
      CVresults[1,2] = transformedMean
      CVresults[1,3] = llTransformedMean
      CVresults[1,4] = ulTransformedMean
      CVresults[1,5] = CVa
      CVresults[1,6] = CVi
      CVresults[1,7] = CVg
      CVresults[1,8] = II
      CVresults[1,9] = RCV
      CVresults[1,10] = nrow(dataFemale)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal (org)","Desirable (org)","Minimal (org)")
      colnames(errorTable) = c("Imprecision,% (org)", "Bias,%","TEa,% (org)")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,2))
      errorTable[2,1] = paste("<",round(0.50*CVi,2))
      errorTable[3,1] = paste("<",round(0.75*CVi,2))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),2))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),2))
      
      
      CVTable
      CVresults
      errorTable
      
      path = paste0("Results/Analyte/",toupper(as.character(levels(dataFemale$analyte))),"/org/female")
      
      # write.table(CVTable, paste0(path,"/CVTable_Female_org.txt"), quote=F, sep="\t", row.names = F)
      # write.table(CVresults, paste0(path,"/CVresults_Female_org.txt"), quote=F, sep="\t", row.names = F)
      # write.table(errorTable, paste0(path,"/errorTable_Female_org.txt"), quote=F, sep="\t", row.names = T)
      
      WriteXLS(CVTable, paste0(path,"/CVTable_Female_org.xls"))
      WriteXLS(CVresults, paste0(path,"/CVresults_Female_org.xls"))
      # WriteXLS(errorTable, paste0(path,"/errorTable_Female_org.xls"))
      cat("\n------------------------------------\nEnd of analysis for", toupper(as.character(levels(dataFull$analyte))),"\n------------------------------------")
      
      bartletlogres[bartletlogres$p.val<0.05,]

