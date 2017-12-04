# 
# data = dataFull2
# gender = "Gender"
# subject = "subject"
# replicate = "replicate"
# time = "time"
# 
# subset = TRUE; CVresult = "transformed"
# 
# res = analysisOfVariance(data, gender, subject, replicate, time, subset = TRUE, CVresult = "transformed")

analysisOfVariance <- function(data, gender, subject, replicate, time, subset = TRUE, CVresult, method = "anova"){
  
  if(CVresult == "transformed" || CVresult == "transformBack"){
    
    data$value = log(data$value)
    
  }
  
  if(subset){
    
    dataGender = split(data, data[,gender])
    
    
  }
  
  if(CVresult == "original"){
    
    
    
    ######################### Original ###################################
    
          data[,subject] = as.factor(data[,subject])
          data[,replicate] = as.factor(data[,replicate])
          data[,time] = as.factor(data[,time])
          
          N <- nrow(data)
          r <- length(unique(data[,subject]))
          ni <- as.numeric(table(data[ ,subject]))
          sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
          
          
          
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
          
          
       
          
          
          ## lme model
          if(method == "lme"){
            bv <- lme(value ~1, random=~1|subject/time, data=data)
          # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
          # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
            summary = summary(bv)
            
            grandMean = summary$coefficients$fixed[[1]]
            
            seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
            
            llMean = grandMean-1.96*seGrandMean
            ulMean = grandMean+1.96*seGrandMean            
            
            table= nlme::VarCorr(bv)
            
            
            sigma_between = as.numeric(table[2,2])
            sigma_within = as.numeric(table[4,2])
            sigma_analytical = as.numeric(table[5,2])
            # sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
            
            sigma2_between = sigma_between^2
            sigma2_within = sigma_within^2
            sigma2_analytical = sigma_analytical^2
          
          }else{
            
            anova = summary(aov(value ~ subject/time, data))
            bv <- lme(value ~1, random=~1|subject/time, data=data)
            
            sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
            sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
            sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
            
            grandMean = mean(data$value)
            seGrandMean =  sd(data$value)/sqrt(N)
          }
          
          
          
          llMean = grandMean-qnorm(0.975)*seGrandMean
          ulMean = grandMean+qnorm(0.975)*seGrandMean
          
    
          
          sigma_analytical = sqrt(sigma2_analytical)
          sigma_within = sqrt(sigma2_within)
          sigma_between = sqrt(sigma2_between)
          sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
          
          
          CV_analytical = abs(sigma_analytical/grandMean)
          CV_within = abs(sigma_within/grandMean)
          CV_between = abs(sigma_between/grandMean)
          CV_total = abs(sigma_total/grandMean)
          
          
          
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
          
            
          ############# ALL subjects
          
          CVTable = data.frame(matrix(NA,4,6))
          colnames(CVTable) = c("Source", "sigma", "CV", "Lower", "Upper", "CV%")
          
          CVTable[1,"Source"] = "Between"
          CVTable[2,"Source"] = "Within"
          CVTable[3,"Source"] = "Analytical"
          CVTable[4,"Source"] = "Total"
          
          CVTable[1, "sigma"] = round(sigma_between,4)
          CVTable[2, "sigma"] = round(sigma_within,4)
          CVTable[3, "sigma"] = round(sigma_analytical,4)
          CVTable[4, "sigma"] = round(sigma_total,4)
          
          CVTable[1, "CV"] = round(CV_between,4)
          CVTable[2, "CV"] = round(CV_within,4)
          CVTable[3, "CV"] = round(CV_analytical,4)
          CVTable[4, "CV"] = round(CV_total,4)
          
          CVTable[1, "Lower"] = round(lower_between,4)
          CVTable[2, "Lower"] = round(lower_within,4)
          CVTable[3, "Lower"] = round(lower_analytical,4)
          CVTable[4, "Lower"] = round(lower_total,4)
          
          CVTable[1, "Upper"] = round(upper_between,4)
          CVTable[2, "Upper"] = round(upper_within,4)
          CVTable[3, "Upper"] = round(upper_analytical,4)
          CVTable[4, "Upper"] = round(upper_total,4)
          
          CVTable[1, "CV%"] = round(CV_between*100,4)
          CVTable[2, "CV%"] = round(CV_within*100,4)
          CVTable[3, "CV%"] = round(CV_analytical*100,4)
          CVTable[4, "CV%"] = round(CV_total*100,4)
          
          CVa = CVTable[3,6] #in percentage
          CVi = CVTable[2,6]#in percentage
          CVg = CVTable[1,6] #in percentage
          
          z = qnorm(0.975) #for alpha = 0.05
          #z = 2.58 #for alpha = 0.01
          
          ### Calculation of Index of Individuality (II) ###
          #II indicates the utility of population-based reference values
          II = CVi/CVg
          
          ### Calculation of Reference Change Value (RCV) ###
          #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
          #two consecutive measurements from the same individual is significant or not.
          RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
          
          CVresults = data.frame(matrix(NA,1,10))
          colnames(CVresults) = c("Group", "Mean", "Lower Mean", "Upper Mean", "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
          CVresults[1,1] = "All Subjects"
          CVresults[1,2] = round(grandMean,4)
          CVresults[1,3] = round(llMean,4)
          CVresults[1,4] = round(ulMean,4)
          CVresults[1,5] = round(CVa,4)
          CVresults[1,6] = round(CVi,4)
          CVresults[1,7] = round(CVg,4)
          CVresults[1,8] = round(II,4)
          CVresults[1,9] = round(RCV,4)
          CVresults[1,10] = nrow(data)
          
          
          errorTable = matrix(NA,3,3)
          rownames(errorTable) = c("Optimal","Desirable","Minimal")
          colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
          
          errorTable[1,1] = paste("<",round(0.25*CVi,4))
          errorTable[2,1] = paste("<",round(0.50*CVi,4))
          errorTable[3,1] = paste("<",round(0.75*CVi,4))
          
          errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),4))
          errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),4))
          errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),4))
          
          errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),4))
          errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),4))
          errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),4))
          
          
          errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
          
          resultsAllOriginal = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
          
          
          ################### Subset  Original ###############
          
          if(subset){
            
            
            gender1 = names(dataGender)[1]
            
            ############################# Gender = Gender 1 #######################################
            
            dataGender1 = dataGender[[gender1]]
            dataGender1[,subject] = as.factor(dataGender1[,subject])
            dataGender1[,replicate] = as.factor(dataGender1[,replicate])
            dataGender1[,time] = as.factor(dataGender1[,time])
            
            
            N <- nrow(dataGender1)
            r <- length(unique(dataGender1[,subject]))
            ni <- as.numeric(table(dataGender1[ ,subject]))
            ni = ni[ni>0]
            sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
            
            
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
            
            
            
            
            
            ## lme model
            if(method == "lme"){
              bv <- lme(value ~1, random=~1|subject/time, data=dataGender1)
              # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
              # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
              summary = summary(bv)
              
              grandMean = summary$coefficients$fixed[[1]]
              
              seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
              
              llMean = grandMean-1.96*seGrandMean
              ulMean = grandMean+1.96*seGrandMean            
              
              table= nlme::VarCorr(bv)
              
              
              sigma_between = as.numeric(table[2,2])
              sigma_within = as.numeric(table[4,2])
              sigma_analytical = as.numeric(table[5,2])
              # sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
              
              sigma2_between = sigma_between^2
              sigma2_within = sigma_within^2
              sigma2_analytical = sigma_analytical^2
              
            }else{
              
              anova = summary(aov(value ~ subject/time, dataGender1))
              bv <- lme(value ~1, random=~1|subject/time, data=dataGender1)
              
              sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
              sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
              sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
              
              grandMean = mean(dataGender1$value)
              seGrandMean =  sd(dataGender1$value)/sqrt(N)
            }
            
            
            
            llMean = grandMean-qnorm(0.975)*seGrandMean
            ulMean = grandMean+qnorm(0.975)*seGrandMean
            
            
            
            sigma_analytical = sqrt(sigma2_analytical)
            sigma_within = sqrt(sigma2_within)
            sigma_between = sqrt(sigma2_between)
            sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
            
            
            CV_analytical = abs(sigma_analytical/grandMean)
            CV_within = abs(sigma_within/grandMean)
            CV_between = abs(sigma_between/grandMean)
            CV_total = abs(sigma_total/grandMean)
            
            
            
            
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
            
            CVTable = data.frame(matrix(NA,4,6))
            colnames(CVTable) = c("Source", "sigma", "CV", "Lower", "Upper", "CV%")
            
            CVTable[1,"Source"] = "Between"
            CVTable[2,"Source"] = "Within"
            CVTable[3,"Source"] = "Analytical"
            CVTable[4,"Source"] = "Total"
            
            CVTable[1, "sigma"] = round(sigma_between,4)
            CVTable[2, "sigma"] = round(sigma_within,4)
            CVTable[3, "sigma"] = round(sigma_analytical,4)
            CVTable[4, "sigma"] = round(sigma_total,4)
            
            CVTable[1, "CV"] = round(CV_between,4)
            CVTable[2, "CV"] = round(CV_within,4)
            CVTable[3, "CV"] = round(CV_analytical,4)
            CVTable[4, "CV"] = round(CV_total,4)
            
            CVTable[1, "Lower"] = round(lower_between,4)
            CVTable[2, "Lower"] = round(lower_within,4)
            CVTable[3, "Lower"] = round(lower_analytical,4)
            CVTable[4, "Lower"] = round(lower_total,4)
            
            CVTable[1, "Upper"] = round(upper_between,4)
            CVTable[2, "Upper"] = round(upper_within,4)
            CVTable[3, "Upper"] = round(upper_analytical,4)
            CVTable[4, "Upper"] = round(upper_total,4)
            
            CVTable[1, "CV%"] = round(CV_between*100,4)
            CVTable[2, "CV%"] = round(CV_within*100,4)
            CVTable[3, "CV%"] = round(CV_analytical*100,4)
            CVTable[4, "CV%"] = round(CV_total*100,4)
            
            CVa = CVTable[3,6] #in percentage
            CVi = CVTable[2,6]#in percentage
            CVg = CVTable[1,6] #in percentage
            
            z = qnorm(0.975) #for alpha = 0.05
            #z = 2.58 #for alpha = 0.01
            
            ### Calculation of Index of Individuality (II) ###
            #II indicates the utility of population-based reference values
            II = CVi/CVg
            
            ### Calculation of Reference Change Value (RCV) ###
            #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
            #two consecutive measurements from the same individual is significant or not.
            RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
            
            CVresults = data.frame(matrix(NA,1,10))
            colnames(CVresults) = c("Group", "Mean", "Lower Mean", "Upper Mean", "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
            CVresults[1,1] = gender1
            CVresults[1,2] = round(grandMean,4)
            CVresults[1,3] = round(llMean,4)
            CVresults[1,4] = round(ulMean,4)
            CVresults[1,5] = round(CVa,4)
            CVresults[1,6] = round(CVi,4)
            CVresults[1,7] = round(CVg,4)
            CVresults[1,8] = round(II,4)
            CVresults[1,9] = round(RCV,4)
            CVresults[1,10] = nrow(dataGender1)
            
            
            errorTable = matrix(NA,3,3)
            rownames(errorTable) = c("Optimal","Desirable","Minimal")
            colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
            
            errorTable[1,1] = paste("<",round(0.25*CVi,4))
            errorTable[2,1] = paste("<",round(0.50*CVi,4))
            errorTable[3,1] = paste("<",round(0.75*CVi,4))
            
            errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),4))
            errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),4))
            errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),4))
            
            errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),4))
            errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),4))
            errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),4))
            
            
            errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
            
            
            resultsOriginalGender1 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
            
            
#################################### Gender = Gender 2 ############################
            gender2 = names(dataGender)[2]
            

            datagender2 = dataGender[[gender2]]
            datagender2[,subject] = as.factor(datagender2[,subject])
            datagender2[,replicate] = as.factor(datagender2[,replicate])
            datagender2[,time] = as.factor(datagender2[,time])
            
            
            N <- nrow(datagender2)
            r <- length(unique(datagender2[,subject]))
            ni <- as.numeric(table(datagender2[ ,subject]))
            ni = ni[ni>0]
            sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
            
            
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
            
            
            
            
            
            ## lme model
            if(method == "lme"){
              bv <- lme(value ~1, random=~1|subject/time, data=datagender2)
              # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
              # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
              summary = summary(bv)
              
              grandMean = summary$coefficients$fixed[[1]]
              
              seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
              
              llMean = grandMean-1.96*seGrandMean
              ulMean = grandMean+1.96*seGrandMean            
              
              table= nlme::VarCorr(bv)
              
              
              sigma_between = as.numeric(table[2,2])
              sigma_within = as.numeric(table[4,2])
              sigma_analytical = as.numeric(table[5,2])
              # sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
              
              sigma2_between = sigma_between^2
              sigma2_within = sigma_within^2
              sigma2_analytical = sigma_analytical^2
              
            }else{
              
              anova = summary(aov(value ~ subject/time, datagender2))
              bv <- lme(value ~1, random=~1|subject/time, data=datagender2)
              
              sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
              sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
              sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
              
              grandMean = mean(datagender2$value)
              seGrandMean =  sd(datagender2$value)/sqrt(N)
            }
            
            
            
            llMean = grandMean-qnorm(0.975)*seGrandMean
            ulMean = grandMean+qnorm(0.975)*seGrandMean
            
            
            
            sigma_analytical = sqrt(sigma2_analytical)
            sigma_within = sqrt(sigma2_within)
            sigma_between = sqrt(sigma2_between)
            sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
            
            
            CV_analytical = abs(sigma_analytical/grandMean)
            CV_within = abs(sigma_within/grandMean)
            CV_between = abs(sigma_between/grandMean)
            CV_total = abs(sigma_total/grandMean)
            
            
            
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
            
            CVTable = data.frame(matrix(NA,4,6))
            colnames(CVTable) = c("Source", "sigma", "CV", "Lower", "Upper", "CV%")
            
            CVTable[1,"Source"] = "Between"
            CVTable[2,"Source"] = "Within"
            CVTable[3,"Source"] = "Analytical"
            CVTable[4,"Source"] = "Total"
            
            CVTable[1, "sigma"] = round(sigma_between,4)
            CVTable[2, "sigma"] = round(sigma_within,4)
            CVTable[3, "sigma"] = round(sigma_analytical,4)
            CVTable[4, "sigma"] = round(sigma_total,4)
            
            CVTable[1, "CV"] = round(CV_between,4)
            CVTable[2, "CV"] = round(CV_within,4)
            CVTable[3, "CV"] = round(CV_analytical,4)
            CVTable[4, "CV"] = round(CV_total,4)
            
            CVTable[1, "Lower"] = round(lower_between,4)
            CVTable[2, "Lower"] = round(lower_within,4)
            CVTable[3, "Lower"] = round(lower_analytical,4)
            CVTable[4, "Lower"] = round(lower_total,4)
            
            CVTable[1, "Upper"] = round(upper_between,4)
            CVTable[2, "Upper"] = round(upper_within,4)
            CVTable[3, "Upper"] = round(upper_analytical,4)
            CVTable[4, "Upper"] = round(upper_total,4)
            
            CVTable[1, "CV%"] = round(CV_between*100,4)
            CVTable[2, "CV%"] = round(CV_within*100,4)
            CVTable[3, "CV%"] = round(CV_analytical*100,4)
            CVTable[4, "CV%"] = round(CV_total*100,4)
            
            CVa = CVTable[3,6] #in percentage
            CVi = CVTable[2,6]#in percentage
            CVg = CVTable[1,6] #in percentage
            
            z = qnorm(0.975) #for alpha = 0.05
            #z = 2.58 #for alpha = 0.01
            
            ### Calculation of Index of Individuality (II) ###
            #II indicates the utility of population-based reference values
            II = CVi/CVg
            
            ### Calculation of Reference Change Value (RCV) ###
            #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
            #two consecutive measurements from the same individual is significant or not.
            RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
            
            CVresults = data.frame(matrix(NA,1,10))
            colnames(CVresults) = c("Group", "Mean", "Lower Mean", "Upper Mean", "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
            CVresults[1,1] = gender2
            CVresults[1,2] = round(grandMean,4)
            CVresults[1,3] = round(llMean,4)
            CVresults[1,4] = round(ulMean,4)
            CVresults[1,5] = round(CVa,4)
            CVresults[1,6] = round(CVi,4)
            CVresults[1,7] = round(CVg,4)
            CVresults[1,8] = round(II,4)
            CVresults[1,9] = round(RCV,4)
            CVresults[1,10] = nrow(datagender2)
            
            
            errorTable = matrix(NA,3,3)
            rownames(errorTable) = c("Optimal","Desirable","Minimal")
            colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
            
            errorTable[1,1] = paste("<",round(0.25*CVi,4))
            errorTable[2,1] = paste("<",round(0.50*CVi,4))
            errorTable[3,1] = paste("<",round(0.75*CVi,4))
            
            errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),4))
            errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),4))
            errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),4))
            
            errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),4))
            errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),4))
            errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),4))
            
            
            errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
            
            
            resultsOriginalGender2 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
            
            
            
          }
          
          
          
          
  }else{
    resultsAllOriginal = NULL
    resultsOriginalGender1 = NULL
    resultsOriginalGender2 = NULL
    
  }    
  
  if(CVresult == "transformed"){
    
    ########### Transformed ############################
    
    data[,subject] = as.factor(data[,subject])
    data[,replicate] = as.factor(data[,replicate])
    data[,time] = as.factor(data[,time])
    
    N <- nrow(data)
    r <- length(unique(data[,subject]))
    ni <- as.numeric(table(data[ ,subject]))
    sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
    
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
    
    
    
    
    
    ## lme model
    if(method == "lme"){
      bv <- lme(value ~1, random=~1|subject/time, data=data)
      # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
      # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
      summary = summary(bv)
      
      grandMean = summary$coefficients$fixed[[1]]
      
      seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
      
      llMean = grandMean-1.96*seGrandMean
      ulMean = grandMean+1.96*seGrandMean            
      
      table= nlme::VarCorr(bv)
      
      
      sigma_between = as.numeric(table[2,2])
      sigma_within = as.numeric(table[4,2])
      sigma_analytical = as.numeric(table[5,2])
      # sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
      
      sigma2_between = sigma_between^2
      sigma2_within = sigma_within^2
      sigma2_analytical = sigma_analytical^2
      
    }else{
      
      anova = summary(aov(value ~ subject/time, data))
      bv <- lme(value ~1, random=~1|subject/time, data=data)
      
      sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
      sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
      sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
      
      grandMean = mean(data$value)
      seGrandMean =  sd(data$value)/sqrt(N)
    }
    
    
    
    llMean = grandMean-qnorm(0.975)*seGrandMean
    ulMean = grandMean+qnorm(0.975)*seGrandMean
    
    
    
    sigma_analytical = sqrt(sigma2_analytical)
    sigma_within = sqrt(sigma2_within)
    sigma_between = sqrt(sigma2_between)
    sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
    
    
    CV_analytical = abs(sigma_analytical/grandMean)
    CV_within = abs(sigma_within/grandMean)
    CV_between = abs(sigma_between/grandMean)
    CV_total = abs(sigma_total/grandMean)
    
    
    
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
    
    ############# ALL subjects
    
    CVTable = data.frame(matrix(NA,4,6))
    colnames(CVTable) = c("Source", "sigma", "CV", "Lower", "Upper", "CV%")
    
    CVTable[1,"Source"] = "Between"
    CVTable[2,"Source"] = "Within"
    CVTable[3,"Source"] = "Analytical"
    CVTable[4,"Source"] = "Total"
    
    CVTable[1, "sigma"] = round(sigma_between,4)
    CVTable[2, "sigma"] = round(sigma_within,4)
    CVTable[3, "sigma"] = round(sigma_analytical,4)
    CVTable[4, "sigma"] = round(sigma_total,4)
    
    CVTable[1, "CV"] = round(CV_between,4)
    CVTable[2, "CV"] = round(CV_within,4)
    CVTable[3, "CV"] = round(CV_analytical,4)
    CVTable[4, "CV"] = round(CV_total,4)
    
    CVTable[1, "Lower"] = round(lower_between,4)
    CVTable[2, "Lower"] = round(lower_within,4)
    CVTable[3, "Lower"] = round(lower_analytical,4)
    CVTable[4, "Lower"] = round(lower_total,4)
    
    CVTable[1, "Upper"] = round(upper_between,4)
    CVTable[2, "Upper"] = round(upper_within,4)
    CVTable[3, "Upper"] = round(upper_analytical,4)
    CVTable[4, "Upper"] = round(upper_total,4)
    
    CVTable[1, "CV%"] = round(CV_between*100,4)
    CVTable[2, "CV%"] = round(CV_within*100,4)
    CVTable[3, "CV%"] = round(CV_analytical*100,4)
    CVTable[4, "CV%"] = round(CV_total*100,4)
    
    CVa = CVTable[3,6] #in percentage
    CVi = CVTable[2,6]#in percentage
    CVg = CVTable[1,6] #in percentage
    
    z = qnorm(0.975) #for alpha = 0.05
    #z = 2.58 #for alpha = 0.01
    
    ### Calculation of Index of Individuality (II) ###
    #II indicates the utility of population-based reference values
    II = CVi/CVg
    
    ### Calculation of Reference Change Value (RCV) ###
    #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
    #two consecutive measurements from the same individual is significant or not.
    RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
    
    CVresults = data.frame(matrix(NA,1,10))
    colnames(CVresults) = c("Group", "Mean", "Lower Mean", "Upper Mean", "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
    CVresults[1,1] = "All Subjects"
    CVresults[1,2] = round(grandMean,4)
    CVresults[1,3] = round(llMean,4)
    CVresults[1,4] = round(ulMean,4)
    CVresults[1,5] = round(CVa,4)
    CVresults[1,6] = round(CVi,4)
    CVresults[1,7] = round(CVg,4)
    CVresults[1,8] = round(II,4)
    CVresults[1,9] = round(RCV,4)
    CVresults[1,10] = nrow(data)
    
    
    errorTable = matrix(NA,3,3)
    rownames(errorTable) = c("Optimal","Desirable","Minimal")
    colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
    
    errorTable[1,1] = paste("<",round(0.25*CVi,4))
    errorTable[2,1] = paste("<",round(0.50*CVi,4))
    errorTable[3,1] = paste("<",round(0.75*CVi,4))
    
    errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),4))
    errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),4))
    errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),4))
    
    errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),4))
    errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),4))
    errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),4))
    
    
    errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
    
    resultsAllTransformed = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
    
    
    if(subset){
      
      
      gender1 = names(dataGender)[1]
      
      ############################# Gender = Gender 1 #######################################
      
      dataGender1 = dataGender[[gender1]]
      dataGender1[,subject] = as.factor(dataGender1[,subject])
      dataGender1[,replicate] = as.factor(dataGender1[,replicate])
      dataGender1[,time] = as.factor(dataGender1[,time])
      
      
      N <- nrow(dataGender1)
      r <- length(unique(dataGender1[,subject]))
      ni <- as.numeric(table(dataGender1[ ,subject]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      
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
      
      
      
      
      
      ## lme model
      if(method == "lme"){
        bv <- lme(value ~1, random=~1|subject/time, data=dataGender1)
        # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
        # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
        summary = summary(bv)
        
        grandMean = summary$coefficients$fixed[[1]]
        
        seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
        
        llMean = grandMean-1.96*seGrandMean
        ulMean = grandMean+1.96*seGrandMean            
        
        table= nlme::VarCorr(bv)
        
        
        sigma_between = as.numeric(table[2,2])
        sigma_within = as.numeric(table[4,2])
        sigma_analytical = as.numeric(table[5,2])
        # sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
        
        sigma2_between = sigma_between^2
        sigma2_within = sigma_within^2
        sigma2_analytical = sigma_analytical^2
        
      }else{
        
        anova = summary(aov(value ~ subject/time, dataGender1))
        bv <- lme(value ~1, random=~1|subject/time, data=dataGender1)
        
        sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
        sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
        sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
        
        grandMean = mean(dataGender1$value)
        seGrandMean =  sd(dataGender1$value)/sqrt(N)
      }
      
      
      
      llMean = grandMean-qnorm(0.975)*seGrandMean
      ulMean = grandMean+qnorm(0.975)*seGrandMean
      
      
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
      sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
      
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      
      
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
      
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma", "CV", "Lower", "Upper", "CV%")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "sigma"] = round(sigma_between,4)
      CVTable[2, "sigma"] = round(sigma_within,4)
      CVTable[3, "sigma"] = round(sigma_analytical,4)
      CVTable[4, "sigma"] = round(sigma_total,4)
      
      CVTable[1, "CV"] = round(CV_between,4)
      CVTable[2, "CV"] = round(CV_within,4)
      CVTable[3, "CV"] = round(CV_analytical,4)
      CVTable[4, "CV"] = round(CV_total,4)
      
      CVTable[1, "Lower"] = round(lower_between,4)
      CVTable[2, "Lower"] = round(lower_within,4)
      CVTable[3, "Lower"] = round(lower_analytical,4)
      CVTable[4, "Lower"] = round(lower_total,4)
      
      CVTable[1, "Upper"] = round(upper_between,4)
      CVTable[2, "Upper"] = round(upper_within,4)
      CVTable[3, "Upper"] = round(upper_analytical,4)
      CVTable[4, "Upper"] = round(upper_total,4)
      
      CVTable[1, "CV%"] = round(CV_between*100,4)
      CVTable[2, "CV%"] = round(CV_within*100,4)
      CVTable[3, "CV%"] = round(CV_analytical*100,4)
      CVTable[4, "CV%"] = round(CV_total*100,4)
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = qnorm(0.975) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", "Lower Mean", "Upper Mean", "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender1
      CVresults[1,2] = round(grandMean,4)
      CVresults[1,3] = round(llMean,4)
      CVresults[1,4] = round(ulMean,4)
      CVresults[1,5] = round(CVa,4)
      CVresults[1,6] = round(CVi,4)
      CVresults[1,7] = round(CVg,4)
      CVresults[1,8] =round(II,4)
      CVresults[1,9] = round(RCV,4)
      CVresults[1,10] = nrow(dataGender1)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,4))
      errorTable[2,1] = paste("<",round(0.50*CVi,4))
      errorTable[3,1] = paste("<",round(0.75*CVi,4))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),4))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),4))
      
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsTransformedGender1 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      
      ################################################# Gender= Gender 2 ##############################################
      
      
      gender2 = names(dataGender)[2]
      

      datagender2 = dataGender[[gender2]]
      datagender2[,subject] = as.factor(datagender2[,subject])
      datagender2[,replicate] = as.factor(datagender2[,replicate])
      datagender2[,time] = as.factor(datagender2[,time])
      
      
      N <- nrow(datagender2)
      r <- length(unique(datagender2[,subject]))
      ni <- as.numeric(table(datagender2[ ,subject]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      
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
      
      
      
      
      
      ## lme model
      if(method == "lme"){
        bv <- lme(value ~1, random=~1|subject/time, data=datagender2)
        # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
        # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
        summary = summary(bv)
        
        grandMean = summary$coefficients$fixed[[1]]
        
        seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
        
        llMean = grandMean-1.96*seGrandMean
        ulMean = grandMean+1.96*seGrandMean            
        
        table= nlme::VarCorr(bv)
        
        
        sigma_between = as.numeric(table[2,2])
        sigma_within = as.numeric(table[4,2])
        sigma_analytical = as.numeric(table[5,2])
        # sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
        
        sigma2_between = sigma_between^2
        sigma2_within = sigma_within^2
        sigma2_analytical = sigma_analytical^2
        
      }else{
        
        anova = summary(aov(value ~ subject/time, datagender2))
        bv <- lme(value ~1, random=~1|subject/time, data=datagender2)
        
        sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
        sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
        sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
        
        grandMean = mean(datagender2$value)
        seGrandMean =  sd(datagender2$value)/sqrt(N)
      }
      
      
      
      llMean = grandMean-qnorm(0.975)*seGrandMean
      ulMean = grandMean+qnorm(0.975)*seGrandMean
      
      
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
      sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
      
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      
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
      
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma", "CV", "Lower", "Upper", "CV%")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "sigma"] = round(sigma_between,4)
      CVTable[2, "sigma"] = round(sigma_within,4)
      CVTable[3, "sigma"] = round(sigma_analytical,4)
      CVTable[4, "sigma"] = round(sigma_total,4)
      
      CVTable[1, "CV"] = round(CV_between,4)
      CVTable[2, "CV"] = round(CV_within,4)
      CVTable[3, "CV"] = round(CV_analytical,4)
      CVTable[4, "CV"] = round(CV_total,4)
      
      CVTable[1, "Lower"] = round(lower_between,4)
      CVTable[2, "Lower"] = round(lower_within,4)
      CVTable[3, "Lower"] = round(lower_analytical,4)
      CVTable[4, "Lower"] = round(lower_total,4)
      
      CVTable[1, "Upper"] = round(upper_between,4)
      CVTable[2, "Upper"] = round(upper_within,4)
      CVTable[3, "Upper"] = round(upper_analytical,4)
      CVTable[4, "Upper"] = round(upper_total,4)
      
      CVTable[1, "CV%"] = round(CV_between*100,4)
      CVTable[2, "CV%"] = round(CV_within*100,4)
      CVTable[3, "CV%"] = round(CV_analytical*100,4)
      CVTable[4, "CV%"] = round(CV_total*100,4)
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = qnorm(0.975) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", "Lower Mean", "Upper Mean", "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender2
      CVresults[1,2] = round(grandMean,4)
      CVresults[1,3] = round(llMean,4)
      CVresults[1,4] = round(ulMean,4)
      CVresults[1,5] = round(CVa,4)
      CVresults[1,6] = round(CVi,4)
      CVresults[1,7] = round(CVg,4)
      CVresults[1,8] = round(II,4)
      CVresults[1,9] = round(RCV,4)
      CVresults[1,10] = nrow(datagender2)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,4))
      errorTable[2,1] = paste("<",round(0.50*CVi,4))
      errorTable[3,1] = paste("<",round(0.75*CVi,4))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),4))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),4))
      
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsTransformedGender2 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      
    }
    
    
    
  }else{
    resultsAllTransformed = NULL
    resultsTransformedGender1 = NULL
    resultsTransformedGender2 = NULL
    
  } 
  
  if(CVresult == "transformBack") {
    
    
    ####################### Transform Back ######################
    
    
    
    data[,subject] = as.factor(data[,subject])
    data[,replicate] = as.factor(data[,replicate])
    data[,time] = as.factor(data[,time])
    
    N <- nrow(data)
    r <- length(unique(data[,subject]))
    ni <- as.numeric(table(data[ ,subject]))
    sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
    
    
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
    
    
    
    
    
    ## lme model
    if(method == "lme"){
      bv <- lme(value ~1, random=~1|subject/time, data=data)
      # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
      # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
      summary = summary(bv)
      
      grandMean = summary$coefficients$fixed[[1]]
      
      seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
      
      llMean = grandMean-1.96*seGrandMean
      ulMean = grandMean+1.96*seGrandMean            
      
      table= nlme::VarCorr(bv)
      
      
      sigma_between = as.numeric(table[2,2])
      sigma_within = as.numeric(table[4,2])
      sigma_analytical = as.numeric(table[5,2])
      # sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
      
      sigma2_between = sigma_between^2
      sigma2_within = sigma_within^2
      sigma2_analytical = sigma_analytical^2
      
    }else{
      
      anova = summary(aov(value ~ subject/time, data))
      bv <- lme(value ~1, random=~1|subject/time, data=data)
      
      sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
      sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
      sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
      
      grandMean = mean(data$value)
      seGrandMean =  sd(data$value)/sqrt(N)
    }
    
    
    
    llMean = grandMean-qnorm(0.975)*seGrandMean
    ulMean = grandMean+qnorm(0.975)*seGrandMean
    
    
    
    sigma_analytical = sqrt(sigma2_analytical)
    sigma_within = sqrt(sigma2_within)
    sigma_between = sqrt(sigma2_between)
    sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
    
    
    CV_analytical = abs(sigma_analytical/grandMean)
    CV_within = abs(sigma_within/grandMean)
    CV_between = abs(sigma_between/grandMean)
    CV_total = abs(sigma_total/grandMean)
    
    
    
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
    
    transformedMean = exp(grandMean+0.5*sigma_total^2)
    
    llTransformedMean = exp(llMean+0.5*sigma_total^2)
    ulTransformedMean = exp(ulMean+0.5*sigma_total^2)
    
    CVTable = data.frame(matrix(NA,4,6))
    colnames(CVTable) = c("Source", "sigma", "CV", "Lower", "Upper", "CV%")
    
    CVTable[1,"Source"] = "Between"
    CVTable[2,"Source"] = "Within"
    CVTable[3,"Source"] = "Analytical"
    CVTable[4,"Source"] = "Total"
    
    CVTable[1, "CV"] = round(sqrt(exp(sigma_between^2)-1),4)
    CVTable[2, "CV"] = round(sqrt(exp(sigma_within^2)-1),4)
    CVTable[3, "CV"] = round(sqrt(exp(sigma_analytical^2)-1),4)
    CVTable[4, "CV"] = round(sqrt(exp(sigma_total^2)-1),4)
    
    CVTable[1, "sigma"] = round(CVTable[1, "CV"]*transformedMean/100,4)
    CVTable[2, "sigma"] = round(CVTable[2, "CV"]*transformedMean/100,4)
    CVTable[3, "sigma"] = round(CVTable[3, "CV"]*transformedMean/100,4)
    CVTable[4, "sigma"] = round(CVTable[4, "CV"]*transformedMean/100,4)
    
    CVTable[1, "Lower"] = round(sqrt(exp(ll_between^2)-1),4)
    CVTable[2, "Lower"] = round(sqrt(exp(ll_within^2)-1),4)
    CVTable[3, "Lower"] = round(sqrt(exp(ll_analytical^2)-1),4)
    CVTable[4, "Lower"] = round(sqrt(exp(ll_total^2)-1),4)
    
    CVTable[1, "Upper"] = round(sqrt(exp(ul_between^2)-1),4)
    CVTable[2, "Upper"] = round(sqrt(exp(ul_within^2)-1),4)
    CVTable[3, "Upper"] = round(sqrt(exp(ul_analytical^2)-1),4)
    CVTable[4, "Upper"] = round(sqrt(exp(ul_total^2)-1),4)
    
    CVTable[1, "CV%"] = round(CVTable[1, "CV"]*100,4)
    CVTable[2, "CV%"] = round(CVTable[2, "CV"]*100,4)
    CVTable[3, "CV%"] = round(CVTable[3, "CV"]*100,4)
    CVTable[4, "CV%"] = round(CVTable[4, "CV"]*100,4)
    
    CVa = CVTable[3,6] #in percentage
    CVi = CVTable[2,6]#in percentage
    CVg = CVTable[1,6] #in percentage
    
    z = qnorm(0.975) #for alpha = 0.05
    #z = 2.58 #for alpha = 0.01
    
    ### Calculation of Index of Individuality (II) ###
    #II indicates the utility of population-based reference values
    II = CVi/CVg
    
    ### Calculation of Reference Change Value (RCV) ###
    #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
    #two consecutive measurements from the same individual is significant or not.
    RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
    
    CVresults = data.frame(matrix(NA,1,10))
    colnames(CVresults) = c("Group", "Mean", "Lower Mean", "Upper Mean", "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
    CVresults[1,1] = "All Subjects"
    CVresults[1,2] = round(transformedMean,4)
    CVresults[1,3] = round(llTransformedMean,4)
    CVresults[1,4] = round(ulTransformedMean,4)
    CVresults[1,5] = round(CVa,4)
    CVresults[1,6] = round(CVi,4)
    CVresults[1,7] = round(CVg,4)
    CVresults[1,8] = round(II,4)
    CVresults[1,9] = round(RCV,4)
    CVresults[1,10] = nrow(data)
    
    
    errorTable = matrix(NA,3,3)
    rownames(errorTable) = c("Optimal","Desirable","Minimal")
    colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
    
    errorTable[1,1] = paste("<",round(0.25*CVi,4))
    errorTable[2,1] = paste("<",round(0.50*CVi,4))
    errorTable[3,1] = paste("<",round(0.75*CVi,4))
    
    errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),4))
    errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),4))
    errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),4))
    
    errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),4))
    errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),4))
    errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),4))
    
    errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
    
    resultsAllTransformBack = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
    
    if(subset){
      
      
      gender1 = names(dataGender)[1]
      
      ############################# Gender = Gender 1 #######################################
      
      dataGender1 = dataGender[[gender1]]
      dataGender1[,subject] = as.factor(dataGender1[,subject])
      dataGender1[,replicate] = as.factor(dataGender1[,replicate])
      dataGender1[,time] = as.factor(dataGender1[,time])
      
      
      N <- nrow(dataGender1)
      r <- length(unique(dataGender1[,subject]))
      ni <- as.numeric(table(dataGender1[ ,subject]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      
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
      
      
      
      
      
      ## lme model
      if(method == "lme"){
        bv <- lme(value ~1, random=~1|subject/time, data=dataGender1)
        # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
        # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
        summary = summary(bv)
        
        grandMean = summary$coefficients$fixed[[1]]
        
        seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
        
        llMean = grandMean-1.96*seGrandMean
        ulMean = grandMean+1.96*seGrandMean            
        
        table= nlme::VarCorr(bv)
        
        
        sigma_between = as.numeric(table[2,2])
        sigma_within = as.numeric(table[4,2])
        sigma_analytical = as.numeric(table[5,2])
        # sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
        
        sigma2_between = sigma_between^2
        sigma2_within = sigma_within^2
        sigma2_analytical = sigma_analytical^2
        
      }else{
        
        anova = summary(aov(value ~ subject/time, dataGender1))
        bv <- lme(value ~1, random=~1|subject/time, data=dataGender1)
        
        sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
        sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
        sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
        
        grandMean = mean(dataGender1$value)
        seGrandMean =  sd(dataGender1$value)/sqrt(N)
      }
      
      
      
      llMean = grandMean-qnorm(0.975)*seGrandMean
      ulMean = grandMean+qnorm(0.975)*seGrandMean
      
      
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
      sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
      
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      
      
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
      
      transformedMean = exp(grandMean+0.5*sigma_total^2)
      
      llTransformedMean = exp(llMean+0.5*sigma_total^2)
      ulTransformedMean = exp(ulMean+0.5*sigma_total^2)
      
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma", "CV", "Lower", "Upper", "CV%")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "CV"] = round(sqrt(exp(sigma_between^2)-1),4)
      CVTable[2, "CV"] = round(sqrt(exp(sigma_within^2)-1),4)
      CVTable[3, "CV"] = round(sqrt(exp(sigma_analytical^2)-1),4)
      CVTable[4, "CV"] = round(sqrt(exp(sigma_total^2)-1),4)
      
      CVTable[1, "sigma"] = round(CVTable[1, "CV"]*transformedMean/100,4)
      CVTable[2, "sigma"] = round(CVTable[2, "CV"]*transformedMean/100,4)
      CVTable[3, "sigma"] = round(CVTable[3, "CV"]*transformedMean/100,4)
      CVTable[4, "sigma"] = round(CVTable[4, "CV"]*transformedMean/100,4)
      
      CVTable[1, "Lower"] = round(sqrt(exp(ll_between^2)-1),4)
      CVTable[2, "Lower"] = round(sqrt(exp(ll_within^2)-1),4)
      CVTable[3, "Lower"] = round(sqrt(exp(ll_analytical^2)-1),4)
      CVTable[4, "Lower"] = round(sqrt(exp(ll_total^2)-1),4)
      
      CVTable[1, "Upper"] = round(sqrt(exp(ul_between^2)-1),4)
      CVTable[2, "Upper"] = round(sqrt(exp(ul_within^2)-1),4)
      CVTable[3, "Upper"] = round(sqrt(exp(ul_analytical^2)-1),4)
      CVTable[4, "Upper"] = round(sqrt(exp(ul_total^2)-1),4)
      
      CVTable[1, "CV%"] = round(CVTable[1, "CV"]*100,4)
      CVTable[2, "CV%"] = round(CVTable[2, "CV"]*100,4)
      CVTable[3, "CV%"] = round(CVTable[3, "CV"]*100,4)
      CVTable[4, "CV%"] = round(CVTable[4, "CV"]*100,4)
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = qnorm(0.975) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", "Lower Mean", "Upper Mean", "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender1
      CVresults[1,2] = round(transformedMean,4)
      CVresults[1,3] = round(llTransformedMean,4)
      CVresults[1,4] = round(ulTransformedMean,4)
      CVresults[1,5] = round(CVa,4)
      CVresults[1,6] = round(CVi,4)
      CVresults[1,7] = round(CVg,4)
      CVresults[1,8] = round(II,4)
      CVresults[1,9] = round(RCV,4)
      CVresults[1,10] = nrow(dataGender1)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,4))
      errorTable[2,1] = paste("<",round(0.50*CVi,4))
      errorTable[3,1] = paste("<",round(0.75*CVi,4))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),4))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),4))
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsTransformBackGender1 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      
      ################################################# Gender= Gender 2 ##############################################
      
      
      gender2 = names(dataGender)[2]
      
      
      datagender2 = dataGender[[gender2]]
      datagender2[,subject] = as.factor(datagender2[,subject])
      datagender2[,replicate] = as.factor(datagender2[,replicate])
      datagender2[,time] = as.factor(datagender2[,time])
      
      
      N <- nrow(datagender2)
      r <- length(unique(datagender2[,subject]))
      ni <- as.numeric(table(datagender2[ ,subject]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
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
      
      
      
      
      
      ## lme model
      if(method == "lme"){
        bv <- lme(value ~1, random=~1|subject/time, data=datagender2)
        # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
        # bv <- lmer(data=data, REML = TRUE, value ~ 1 + (1|subject) + (1|subject:time))
        summary = summary(bv)
        
        grandMean = summary$coefficients$fixed[[1]]
        
        seGrandMean =  sqrt(summary$varFix)[[1]]/sqrt(N)
        
        llMean = grandMean-1.96*seGrandMean
        ulMean = grandMean+1.96*seGrandMean            
        
        table= nlme::VarCorr(bv)
        
        
        sigma_between = as.numeric(table[2,2])
        sigma_within = as.numeric(table[4,2])
        sigma_analytical = as.numeric(table[5,2])
        # sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
        
        sigma2_between = sigma_between^2
        sigma2_within = sigma_within^2
        sigma2_analytical = sigma_analytical^2
        
      }else{
        
        anova = summary(aov(value ~ subject/time, datagender2))
        bv <- lme(value ~1, random=~1|subject/time, data=datagender2)
        
        sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
        sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
        sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
        
        grandMean = mean(datagender2$value)
        seGrandMean =  sd(datagender2$value)/sqrt(N)
      }
      
      
      
      llMean = grandMean-qnorm(0.975)*seGrandMean
      ulMean = grandMean+qnorm(0.975)*seGrandMean
      
      
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
      sigma_total = sqrt(sigma_analytical^2 + sigma_within^2 + sigma_between^2)
      
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      
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
      
      transformedMean = exp(grandMean+0.5*sigma_total^2)
      
      llTransformedMean = exp(llMean+0.5*sigma_total^2)
      ulTransformedMean = exp(ulMean+0.5*sigma_total^2)
      
      CVTable = data.frame(matrix(NA,4,6))
      colnames(CVTable) = c("Source", "sigma", "CV", "Lower", "Upper", "CV%")
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "CV"] = round(sqrt(exp(sigma_between^2)-1),4)
      CVTable[2, "CV"] = round(sqrt(exp(sigma_within^2)-1),4)
      CVTable[3, "CV"] = round(sqrt(exp(sigma_analytical^2)-1),4)
      CVTable[4, "CV"] = round(sqrt(exp(sigma_total^2)-1),4)
      
      CVTable[1, "sigma"] = round(CVTable[1, "CV"]*transformedMean/100,4)
      CVTable[2, "sigma"] = round(CVTable[2, "CV"]*transformedMean/100,4)
      CVTable[3, "sigma"] = round(CVTable[3, "CV"]*transformedMean/100,4)
      CVTable[4, "sigma"] = round(CVTable[4, "CV"]*transformedMean/100,4)
      
      CVTable[1, "Lower"] = round(sqrt(exp(ll_between^2)-1),4)
      CVTable[2, "Lower"] = round(sqrt(exp(ll_within^2)-1),4)
      CVTable[3, "Lower"] = round(sqrt(exp(ll_analytical^2)-1),4)
      CVTable[4, "Lower"] = round(sqrt(exp(ll_total^2)-1),4)
      
      CVTable[1, "Upper"] = round(sqrt(exp(ul_between^2)-1),4)
      CVTable[2, "Upper"] = round(sqrt(exp(ul_within^2)-1),4)
      CVTable[3, "Upper"] = round(sqrt(exp(ul_analytical^2)-1),4)
      CVTable[4, "Upper"] = round(sqrt(exp(ul_total^2)-1),4)
      
      CVTable[1, "CV%"] = round(CVTable[1, "CV"]*100,4)
      CVTable[2, "CV%"] = round(CVTable[2, "CV"]*100,4)
      CVTable[3, "CV%"] = round(CVTable[3, "CV"]*100,4)
      CVTable[4, "CV%"] = round(CVTable[4, "CV"]*100,4)
      
      CVa = CVTable[3,6] #in percentage
      CVi = CVTable[2,6]#in percentage
      CVg = CVTable[1,6] #in percentage
      
      z = qnorm(0.975) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", "Lower Mean", "Upper Mean", "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender2
      CVresults[1,2] = round(transformedMean,4)
      CVresults[1,3] = round(llTransformedMean,4)
      CVresults[1,4] = round(ulTransformedMean,4)
      CVresults[1,5] = round(CVa,4)
      CVresults[1,6] = round(CVi,4)
      CVresults[1,7] = round(CVg,4)
      CVresults[1,8] = round(II,4)
      CVresults[1,9] = round(RCV,4)
      CVresults[1,10] = nrow(datagender2)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,4))
      errorTable[2,1] = paste("<",round(0.50*CVi,4))
      errorTable[3,1] = paste("<",round(0.75*CVi,4))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),4))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),4))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),4))
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsTransformBackGender2 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      
    }
    
    
  
    
  }else{
    resultsAllTransformBack = NULL
    resultsTransformBackGender1 = NULL
    resultsTransformBackGender2 = NULL
    
  } 
  
  
  allResults = list(resultsAllOriginal = resultsAllOriginal, resultsOriginalGender1 = resultsOriginalGender1, resultsOriginalGender2 = resultsOriginalGender2,
                    resultsAllTransformed = resultsAllTransformed, resultsTransformedGender1 = resultsTransformedGender1, resultsTransformedGender2 = resultsTransformedGender2,
                    resultsAllTransformBack = resultsAllTransformBack,  resultsTransformBackGender1 = resultsTransformBackGender1,
                    resultsTransformBackGender2 = resultsTransformBackGender2)
  
  return(allResults)
}