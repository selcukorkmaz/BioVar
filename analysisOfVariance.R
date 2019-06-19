# 
# data = dataFull2
# gender = "Gender"
# subject = "subject"
# replicate = "replicate"
# time = "time"
# 
# subset = TRUE; CVresult = "transformed"
# 
# res = analysisOfVariance(data, gender, subject, replicate, time, method = "anova", CVresult = "transformBack", decimal = 3)

analysisOfVariance <- function(data, gender, subject, replicate, time, CVresult, decimal, alpha = alpha){
  
  library(nlme)
  
  alpha = as.numeric(alpha)
  
  lower_title = paste0("Lower Limit (",(1-alpha)*100,"%)")
  upper_title = paste0("Upper Limit (",(1-alpha)*100,"%)")
  
  lowerMean_title = paste0("Lower Limit (",(1-alpha)*100,"%)")
  upperMean_title = paste0("Upper Limit (",(1-alpha)*100,"%)")
  
  
  if(CVresult == "lnTransformed" || CVresult == "transformBack"){

    data$value = log(data$value)

  }
  
  data = data[,c("subject", "gender", "time", "replicate", "value")]
  
  dataGender = split(data, data[,"gender"])
    
  
######################### CV-ANOVA ###################################
         
  if(CVresult == "cv"){
    
    means = tapply(data$value, data$subject, mean)
    
    subjects = split(data, data$subject)
    
    
    for(i in 1:length(means)){
      
      subjects[[i]][,"value"] = subjects[[i]][,"value"] / means[i] 
      
    }
    
    
    data = do.call(rbind.data.frame, subjects)
    
    
    data[,"subject"] = as.factor(data[,"subject"])
    data[,"replicate"] = as.factor(data[,"replicate"])
    data[,"time"] = as.factor(data[,"time"])
    
    bv <- lme(value ~1, random=~1|subject/time, data=data)
    
    
    N <- nrow(data)
    r <- length(unique(data[,"subject"]))
    ni <- as.numeric(table(data[ ,"subject"]))
    sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))

    ti = ni/2
    replicateNumber = 2
    
    w1List = list()
    
    for(i in 1:length(ti)){
      
      w1List[[i]] = rep(((replicateNumber^2)*((1/ni[i])-(1/sum(ni))))/(length(unique(data[,"time"])) - 1),ti[i])
      
    }
    
    w3List = list()
    
    for(i in 1:length(ti)){
      
      w3List[[i]] = rep((replicateNumber^2)*((1/sqrt(replicateNumber^2)) - (1/ni[i]))/(bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]),ti[i])
      
    }
    
    w1uList1 = list()
    
    for(i in 1:length(ti)){
      w1uList1[[i]] = sum(rep(1/sqrt(replicateNumber^2), ti[i]))
    }
    
    
    w3uList1 = list()
    
    for(i in 1:length(ti)){
      w3uList1[[i]] = ti[i]/sum(rep(1/sqrt(replicateNumber^2), ti[i]))
    }
    
    
    
    
    w1 = do.call(sum, w1List)
    w2 = sgConstant
    w3 = do.call(sum, w3List)
    w1u = sum((1/ti))/sum((1/(ti*(ti/unlist(w1uList1)))))
    w2u = ti[1]/sum(1/(ti*(ti/(0.5*(ti)))))
    w3u = (bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]) / sum((ti-1)/unlist(w3uList1))
    c2u = (w2u-w1u)/(w3u)
    c3u = w2u-1-c2u
    c2 = w1u/w3u
    c3 = 0
    alpha = alpha
    inf = 999999999
    
    
    ## lme model
    
    anova = summary(aov(value ~ subject/time, data))
    
    sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
    sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
    sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
    
    grandMean = mean(data$value)
    seGrandMean =  sd(data$value)/sqrt(N)
    
    
    
    llMean = grandMean-qnorm(1-alpha/2)*seGrandMean
    ulMean = grandMean+qnorm(1-alpha/2)*seGrandMean
    
    
    
    sigma_analytical = sqrt(sigma2_analytical)
    sigma_within = sqrt(sigma2_within)
    sigma_between = ifelse(sigma2_between > 0, sqrt(sigma2_between),0)
    sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
    
    
    CV_analytical = abs(sigma_analytical/grandMean)
    CV_within = abs(sigma_within/grandMean)
    CV_between = abs(sigma_between/grandMean)
    CV_total = abs(sigma_total/grandMean)
    
    
    
    ll_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = F))
    ul_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = T))
    
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
    
    g1 = 1-1/(qf(alpha/2, dfbetween, inf, lower.tail = F))
    g2 = 1-1/(qf(alpha/2, dfwithin, inf, lower.tail = F))
    g3 = 1-1/(qf(alpha/2, dfanalytical, inf, lower.tail = F))
    
    h1 =1/(qf(1-alpha/2, dfbetween, inf, lower.tail = F))-1
    h2 = 1/(qf(1-alpha/2, dfwithin, inf, lower.tail = F))-1
    h3 = 1/(qf(1-alpha/2, dfanalytical, inf, lower.tail = F))-1
    
    g12 = (((qf(alpha/2, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha/2, dfbetween, dfwithin, lower.tail = F))
    g13 = (((qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F))
    g23 = (((qf(alpha/2, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))
    g32 = (((qf(alpha/2, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))
    
    h12 = (((1-qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))
    h13 = (((1-qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))
    h23 = (((1-qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))
    h32 = (((1-qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))
    
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
    
    CVTable = data.frame(matrix(NA,4,5))
    colnames(CVTable) = c("Source", "sigma", "CV%", lower_title, upper_title)
    
    CVTable[1,"Source"] = "Between"
    CVTable[2,"Source"] = "Within"
    CVTable[3,"Source"] = "Analytical"
    CVTable[4,"Source"] = "Total"
    
    CVTable[1, "sigma"] = round(sigma_between,decimal)
    CVTable[2, "sigma"] = round(sigma_within,decimal)
    CVTable[3, "sigma"] = round(sigma_analytical,decimal)
    CVTable[4, "sigma"] = round(sigma_total,decimal)

    CVTable[1, lower_title] = ifelse(sigma_between > 0, round(lower_between*100,decimal),0)
    CVTable[2, lower_title] = round(lower_within*100,decimal)
    CVTable[3, lower_title] = round(lower_analytical*100,decimal)
    CVTable[4, lower_title] = round(lower_total*100,decimal)
    
    CVTable[1, upper_title] = ifelse(sigma_between > 0, round(upper_between*100,decimal),0)
    CVTable[2, upper_title] = round(upper_within*100,decimal)
    CVTable[3, upper_title] = round(upper_analytical*100,decimal)
    CVTable[4, upper_title] = round(upper_total*100,decimal)
    
    CVTable[1, "CV%"] = round(CV_between*100,decimal)
    CVTable[2, "CV%"] = round(CV_within*100,decimal)
    CVTable[3, "CV%"] = round(CV_analytical*100,decimal)
    CVTable[4, "CV%"] = round(CV_total*100,decimal)
    
    CVa = CVTable[3,3] #in percentage
    CVi = CVTable[2,3]#in percentage
    CVg = CVTable[1,3] #in percentage
    
    z = qnorm(1-alpha/2) #for alpha = 0.05
    #z = 2.58 #for alpha = 0.01
    
    ### Calculation of Index of Individuality (II) ###
    #II indicates the utility of population-based reference values
    II = CVi/CVg
    
    ### Calculation of Reference Change Value (RCV) ###
    #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
    #two consecutive measurements from the same individual is significant or not.
    RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
    
    CVresults = data.frame(matrix(NA,1,10))
    colnames(CVresults) = c("Group", "Mean", lowerMean_title, upperMean_title, "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
    CVresults[1,1] = "All Subjects"
    CVresults[1,2] = round(grandMean,decimal)
    CVresults[1,3] = round(llMean,decimal)
    CVresults[1,4] = round(ulMean,decimal)
    CVresults[1,5] = round(CVa,decimal)
    CVresults[1,6] = round(CVi,decimal)
    CVresults[1,7] = round(CVg,decimal)
    CVresults[1,8] = round(II,decimal)
    CVresults[1,9] = round(RCV,decimal)
    CVresults[1,10] = nrow(data)
    
    
    errorTable = matrix(NA,3,3)
    rownames(errorTable) = c("Optimal","Desirable","Minimal")
    colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
    
    errorTable[1,1] = paste("<",round(0.25*CVi,decimal))
    errorTable[2,1] = paste("<",round(0.50*CVi,decimal))
    errorTable[3,1] = paste("<",round(0.75*CVi,decimal))
    
    errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),decimal))
    
    errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),decimal))
    
    
    errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
    
    resultsAllCvAnova = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
    
    
    ################### Subset  Original ###############
    
      gender1 = names(dataGender)[1]
      
      ############################# Gender = Gender 1 #######################################
      
      dataGender1 = dataGender[[gender1]]
      dataGender1[,"subject"] = as.factor(dataGender1[,"subject"])
      dataGender1[,"replicate"] = as.factor(dataGender1[,"replicate"])
      dataGender1[,"time"] = as.factor(dataGender1[,"time"])
      
      bv <- lme(value ~1, random=~1|subject/time, data=dataGender1)
      
      
      N <- nrow(dataGender1)
      r <- length(unique(dataGender1[,"subject"]))
      ni <- as.numeric(table(dataGender1[ ,"subject"]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      
      ti = ni/2
      replicateNumber = 2
      
      w1List = list()
      
      for(i in 1:length(ti)){
        
        w1List[[i]] = rep(((replicateNumber^2)*((1/ni[i])-(1/sum(ni))))/(length(unique(data[,"time"])) - 1),ti[i])
        
      }
      
      w3List = list()
      
      for(i in 1:length(ti)){
        
        w3List[[i]] = rep((replicateNumber^2)*((1/sqrt(replicateNumber^2)) - (1/ni[i]))/(bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]),ti[i])
        
      }
      
      w1uList1 = list()
      
      for(i in 1:length(ti)){
        w1uList1[[i]] = sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      w3uList1 = list()
      
      for(i in 1:length(ti)){
        w3uList1[[i]] = ti[i]/sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      
      w1 = do.call(sum, w1List)
      w2 = sgConstant
      w3 = do.call(sum, w3List)
      w1u = sum((1/ti))/sum((1/(ti*(ti/unlist(w1uList1)))))
      w2u = ti[1]/sum(1/(ti*(ti/(0.5*(ti)))))
      w3u = (bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]) / sum((ti-1)/unlist(w3uList1))
      c2u = (w2u-w1u)/(w3u)
      c3u = w2u-1-c2u
      c2 = w1u/w3u
      c3 = 0
      alpha = alpha
      inf = 999999999
      
      
      anova = summary(aov(value ~ subject/time, dataGender1))
      
      sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
      sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
      sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
      
      grandMean = mean(dataGender1$value)
      seGrandMean =  sd(dataGender1$value)/sqrt(N)
      
      
      llMean = grandMean-qnorm(1-alpha/2)*seGrandMean
      ulMean = grandMean+qnorm(1-alpha/2)*seGrandMean
      
      
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
      sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
      
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      
      
      ll_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = F))
      ul_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = T))
      
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
      
      g1 = 1-1/(qf(alpha/2, dfbetween, inf, lower.tail = F))
      g2 = 1-1/(qf(alpha/2, dfwithin, inf, lower.tail = F))
      g3 = 1-1/(qf(alpha/2, dfanalytical, inf, lower.tail = F))
      
      h1 =1/(qf(1-alpha/2, dfbetween, inf, lower.tail = F))-1
      h2 = 1/(qf(1-alpha/2, dfwithin, inf, lower.tail = F))-1
      h3 = 1/(qf(1-alpha/2, dfanalytical, inf, lower.tail = F))-1
      
      g12 = (((qf(alpha/2, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha/2, dfbetween, dfwithin, lower.tail = F))
      g13 = (((qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F))
      g23 = (((qf(alpha/2, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))
      g32 = (((qf(alpha/2, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
      h12 = (((1-qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))
      h13 = (((1-qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))
      h23 = (((1-qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))
      h32 = (((1-qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
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
      
      CVTable = data.frame(matrix(NA,4,5))
      colnames(CVTable) = c("Source", "sigma", "CV%", lower_title, upper_title)
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "sigma"] = round(sigma_between,decimal)
      CVTable[2, "sigma"] = round(sigma_within,decimal)
      CVTable[3, "sigma"] = round(sigma_analytical,decimal)
      CVTable[4, "sigma"] = round(sigma_total,decimal)
      
      CVTable[1, lower_title] = round(lower_between*100,decimal)
      CVTable[2, lower_title] = round(lower_within*100,decimal)
      CVTable[3, lower_title] = round(lower_analytical*100,decimal)
      CVTable[4, lower_title] = round(lower_total*100,decimal)
      
      CVTable[1, upper_title] = round(upper_between*100,decimal)
      CVTable[2, upper_title] = round(upper_within*100,decimal)
      CVTable[3, upper_title] = round(upper_analytical*100,decimal)
      CVTable[4, upper_title] = round(upper_total*100,decimal)
      
      CVTable[1, "CV%"] = round(CV_between*100,decimal)
      CVTable[2, "CV%"] = round(CV_within*100,decimal)
      CVTable[3, "CV%"] = round(CV_analytical*100,decimal)
      CVTable[4, "CV%"] = round(CV_total*100,decimal)
      
      CVa = CVTable[3,3] #in percentage
      CVi = CVTable[2,3]#in percentage
      CVg = CVTable[1,3] #in percentage
      
      z = qnorm(1-alpha/2) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", lowerMean_title, upperMean_title, "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender1
      CVresults[1,2] = round(grandMean,decimal)
      CVresults[1,3] = round(llMean,decimal)
      CVresults[1,4] = round(ulMean,decimal)
      CVresults[1,5] = round(CVa,decimal)
      CVresults[1,6] = round(CVi,decimal)
      CVresults[1,7] = round(CVg,decimal)
      CVresults[1,8] = round(II,decimal)
      CVresults[1,9] = round(RCV,decimal)
      CVresults[1,10] = nrow(dataGender1)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,decimal))
      errorTable[2,1] = paste("<",round(0.50*CVi,decimal))
      errorTable[3,1] = paste("<",round(0.75*CVi,decimal))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsCvAnovaGender1 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      #################################### Gender = Gender 2 ############################
      gender2 = names(dataGender)[2]
      
      
      datagender2 = dataGender[[gender2]]
      datagender2[,"subject"] = as.factor(datagender2[,"subject"])
      datagender2[,"replicate"] = as.factor(datagender2[,"replicate"])
      datagender2[,"time"] = as.factor(datagender2[,"time"])
      
      bv <- lme(value ~1, random=~1|subject/time, data=datagender2)
      
      
      N <- nrow(datagender2)
      r <- length(unique(datagender2[,"subject"]))
      ni <- as.numeric(table(datagender2[ ,"subject"]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      
      ti = ni/2
      replicateNumber = 2
      
      w1List = list()
      
      for(i in 1:length(ti)){
        
        w1List[[i]] = rep(((replicateNumber^2)*((1/ni[i])-(1/sum(ni))))/(length(unique(data[,"time"])) - 1),ti[i])
        
      }
      
      w3List = list()
      
      for(i in 1:length(ti)){
        
        w3List[[i]] = rep((replicateNumber^2)*((1/sqrt(replicateNumber^2)) - (1/ni[i]))/(bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]),ti[i])
        
      }
      
      w1uList1 = list()
      
      for(i in 1:length(ti)){
        w1uList1[[i]] = sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      w3uList1 = list()
      
      for(i in 1:length(ti)){
        w3uList1[[i]] = ti[i]/sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      
      w1 = do.call(sum, w1List)
      w2 = sgConstant
      w3 = do.call(sum, w3List)
      w1u = sum((1/ti))/sum((1/(ti*(ti/unlist(w1uList1)))))
      w2u = ti[1]/sum(1/(ti*(ti/(0.5*(ti)))))
      w3u = (bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]) / sum((ti-1)/unlist(w3uList1))
      c2u = (w2u-w1u)/(w3u)
      c3u = w2u-1-c2u
      c2 = w1u/w3u
      c3 = 0
      alpha = alpha
      inf = 999999999
      
      
      
      anova = summary(aov(value ~ subject/time, datagender2))
      
      sigma2_analytical = anova[[1]][[2]][3]/anova[[1]][[1]][3]
      sigma2_within = (anova[[1]][[2]][2]/anova[[1]][[1]][2]-anova[[1]][[2]][3]/anova[[1]][[1]][3])/w3
      sigma2_between = (anova[[1]][[2]][1]/anova[[1]][[1]][1] - sigma2_analytical-w1*sigma2_within)/w2
      
      grandMean = mean(datagender2$value)
      seGrandMean =  sd(datagender2$value)/sqrt(N)
      
      llMean = grandMean-qnorm(1-alpha/2)*seGrandMean
      ulMean = grandMean+qnorm(1-alpha/2)*seGrandMean
      
      
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
      sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
      
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      
      ll_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = F))
      ul_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = T))
      
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
      
      g1 = 1-1/(qf(alpha/2, dfbetween, inf, lower.tail = F))
      g2 = 1-1/(qf(alpha/2, dfwithin, inf, lower.tail = F))
      g3 = 1-1/(qf(alpha/2, dfanalytical, inf, lower.tail = F))
      
      h1 =1/(qf(1-alpha/2, dfbetween, inf, lower.tail = F))-1
      h2 = 1/(qf(1-alpha/2, dfwithin, inf, lower.tail = F))-1
      h3 = 1/(qf(1-alpha/2, dfanalytical, inf, lower.tail = F))-1
      
      g12 = (((qf(alpha/2, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha/2, dfbetween, dfwithin, lower.tail = F))
      g13 = (((qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F))
      g23 = (((qf(alpha/2, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))
      g32 = (((qf(alpha/2, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
      h12 = (((1-qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))
      h13 = (((1-qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))
      h23 = (((1-qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))
      h32 = (((1-qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
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
      
      CVTable = data.frame(matrix(NA,4,5))
      colnames(CVTable) = c("Source", "sigma", "CV%", lower_title, upper_title)
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "sigma"] = round(sigma_between,decimal)
      CVTable[2, "sigma"] = round(sigma_within,decimal)
      CVTable[3, "sigma"] = round(sigma_analytical,decimal)
      CVTable[4, "sigma"] = round(sigma_total,decimal)
      
      CVTable[1, lower_title] = round(lower_between*100,decimal)
      CVTable[2, lower_title] = round(lower_within*100,decimal)
      CVTable[3, lower_title] = round(lower_analytical*100,decimal)
      CVTable[4, lower_title] = round(lower_total*100,decimal)
      
      CVTable[1, upper_title] = round(upper_between*100,decimal)
      CVTable[2, upper_title] = round(upper_within*100,decimal)
      CVTable[3, upper_title] = round(upper_analytical*100,decimal)
      CVTable[4, upper_title] = round(upper_total*100,decimal)
      
      CVTable[1, "CV%"] = round(CV_between*100,decimal)
      CVTable[2, "CV%"] = round(CV_within*100,decimal)
      CVTable[3, "CV%"] = round(CV_analytical*100,decimal)
      CVTable[4, "CV%"] = round(CV_total*100,decimal)
      
      CVa = CVTable[3,3] #in percentage
      CVi = CVTable[2,3]#in percentage
      CVg = CVTable[1,3] #in percentage
      
      z = qnorm(1-alpha/2) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", lowerMean_title, upperMean_title, "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender2
      CVresults[1,2] = round(grandMean,decimal)
      CVresults[1,3] = round(llMean,decimal)
      CVresults[1,4] = round(ulMean,decimal)
      CVresults[1,5] = round(CVa,decimal)
      CVresults[1,6] = round(CVi,decimal)
      CVresults[1,7] = round(CVg,decimal)
      CVresults[1,8] = round(II,decimal)
      CVresults[1,9] = round(RCV,decimal)
      CVresults[1,10] = nrow(datagender2)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,decimal))
      errorTable[2,1] = paste("<",round(0.50*CVi,decimal))
      errorTable[3,1] = paste("<",round(0.75*CVi,decimal))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsCvAnovaGender2 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      
    
    
    
    
    
  }else{
    resultsAllCvAnova = NULL
    resultsCvAnovaGender1 = NULL
    resultsCvAnovaGender2 = NULL
    
  }  

######################### Original ###################################
         
  if(CVresult != "transformBack" && CVresult != "cv"){
    
    data[,"subject"] = as.factor(data[,"subject"])
    data[,"replicate"] = as.factor(data[,"replicate"])
    data[,"time"] = as.factor(data[,"time"])
    
    ## lme model
    
    bv <- lme(value ~1, random=~1|subject/time, data=data, method = "REML")
    
    N <- nrow(data)
    r <- length(unique(data[,"subject"]))
    ni <- as.numeric(table(data[ ,"subject"]))
    sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
    ti = ni/2
    replicateNumber = 2
    
    w1List = list()
    
    for(i in 1:length(ti)){
      
      w1List[[i]] = rep(((replicateNumber^2)*((1/ni[i])-(1/sum(ni))))/(length(unique(data[,"time"])) - 1),ti[i])
      
    }
    
    w3List = list()
    
    for(i in 1:length(ti)){
    
      w3List[[i]] = rep((replicateNumber^2)*((1/sqrt(replicateNumber^2)) - (1/ni[i]))/(bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]),ti[i])
    
    }
    
    w1uList1 = list()
   
     for(i in 1:length(ti)){
       w1uList1[[i]] = sum(rep(1/sqrt(replicateNumber^2), ti[i]))
     }
    
    
    w3uList1 = list()
    
    for(i in 1:length(ti)){
      w3uList1[[i]] = ti[i]/sum(rep(1/sqrt(replicateNumber^2), ti[i]))
    }

    # CI
    w1 = do.call(sum, w1List)
    w2 = sgConstant
    w3 = do.call(sum, w3List)
    w1u = sum((1/ti))/sum((1/(ti*(ti/unlist(w1uList1)))))
    w2u = ti[1]/sum(1/(ti*(ti/(0.5*(ti)))))
    w3u = (bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]) / sum((ti-1)/unlist(w3uList1))
    c2u = (w2u-w1u)/(w3u)
    c3u = w2u-1-c2u
    c2 = w1u/w3u
    c3 = 0
    alpha = alpha
    inf = 999999999
    
    
    
    
    

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
    #   sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
    
    sigma2_between = sigma_between^2
    sigma2_within = sigma_within^2
    sigma2_analytical = sigma_analytical^2
    
    llMean = grandMean-qnorm(1-alpha/2)*seGrandMean
    ulMean = grandMean+qnorm(1-alpha/2)*seGrandMean
    
    
    
    sigma_analytical = sqrt(sigma2_analytical)
    sigma_within = sqrt(sigma2_within)
    sigma_between = sqrt(sigma2_between)
    sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
    
    
    CV_analytical = abs(sigma_analytical/grandMean)
    CV_within = abs(sigma_within/grandMean)
    CV_between = abs(sigma_between/grandMean)
    CV_total = abs(sigma_total/grandMean)
    
    
    
    ll_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = F))
    ul_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = T))
    
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
    
    g1 = 1-1/(qf(alpha/2, dfbetween, inf, lower.tail = F))
    g2 = 1-1/(qf(alpha/2, dfwithin, inf, lower.tail = F))
    g3 = 1-1/(qf(alpha/2, dfanalytical, inf, lower.tail = F))
    
    h1 =1/(qf(1-alpha/2, dfbetween, inf, lower.tail = F))-1
    h2 = 1/(qf(1-alpha/2, dfwithin, inf, lower.tail = F))-1
    h3 = 1/(qf(1-alpha/2, dfanalytical, inf, lower.tail = F))-1
    
    g12 = (((qf(alpha/2, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha/2, dfbetween, dfwithin, lower.tail = F))
    g13 = (((qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F))
    g23 = (((qf(alpha/2, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))
    g32 = (((qf(alpha/2, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))
    
    h12 = (((1-qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))
    h13 = (((1-qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))
    h23 = (((1-qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))
    h32 = (((1-qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))
    
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
    
    CVTable = data.frame(matrix(NA,4,5))
    colnames(CVTable) = c("Source", "sigma", "CV%", lower_title, upper_title)
    
    CVTable[1,"Source"] = "Between"
    CVTable[2,"Source"] = "Within"
    CVTable[3,"Source"] = "Analytical"
    CVTable[4,"Source"] = "Total"
    
    CVTable[1, "sigma"] = round(sigma_between,decimal)
    CVTable[2, "sigma"] = round(sigma_within,decimal)
    CVTable[3, "sigma"] = round(sigma_analytical,decimal)
    CVTable[4, "sigma"] = round(sigma_total,decimal)
    
    CVTable[1, lower_title] = round(lower_between*100,decimal)
    CVTable[2, lower_title] = round(lower_within*100,decimal)
    CVTable[3, lower_title] = round(lower_analytical*100,decimal)
    CVTable[4, lower_title] = round(lower_total*100,decimal)
    
    CVTable[1, upper_title] = round(upper_between*100,decimal)
    CVTable[2, upper_title] = round(upper_within*100,decimal)
    CVTable[3, upper_title] = round(upper_analytical*100,decimal)
    CVTable[4, upper_title] = round(upper_total*100,decimal)
    
    CVTable[1, "CV%"] = round(CV_between*100,decimal)
    CVTable[2, "CV%"] = round(CV_within*100,decimal)
    CVTable[3, "CV%"] = round(CV_analytical*100,decimal)
    CVTable[4, "CV%"] = round(CV_total*100,decimal)
    
    CVa = CVTable[3,3] #in percentage
    CVi = CVTable[2,3]#in percentage
    CVg = CVTable[1,3] #in percentage
    
    z = qnorm(1-alpha/2) #for alpha = 0.05
    #z = 2.58 #for alpha = 0.01
    
    ### Calculation of Index of Individuality (II) ###
    #II indicates the utility of population-based reference values
    II = CVi/CVg
    
    ### Calculation of Reference Change Value (RCV) ###
    #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
    #two consecutive measurements from the same individual is significant or not.
    RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
    
    CVresults = data.frame(matrix(NA,1,10))
    colnames(CVresults) = c("Group", "Mean", lowerMean_title, upperMean_title, "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
    CVresults[1,1] = "All Subjects"
    CVresults[1,2] = round(grandMean,decimal)
    CVresults[1,3] = round(llMean,decimal)
    CVresults[1,4] = round(ulMean,decimal)
    CVresults[1,5] = round(CVa,decimal)
    CVresults[1,6] = round(CVi,decimal)
    CVresults[1,7] = round(CVg,decimal)
    CVresults[1,8] = round(II,decimal)
    CVresults[1,9] = round(RCV,decimal)
    CVresults[1,10] = nrow(data)
    
    
    errorTable = matrix(NA,3,3)
    rownames(errorTable) = c("Optimal","Desirable","Minimal")
    colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
    
    errorTable[1,1] = paste("<",round(0.25*CVi,decimal))
    errorTable[2,1] = paste("<",round(0.50*CVi,decimal))
    errorTable[3,1] = paste("<",round(0.75*CVi,decimal))
    
    errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),decimal))
    
    errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),decimal))
    
    
    errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
    
    resultsAllOriginalLme = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
    
    
    ################### Subset  Original ###############
    
    
      
      
      gender1 = names(dataGender)[1]
      
      ############################# Gender = Gender 1 #######################################
      
      dataGender1 = dataGender[[gender1]]
      dataGender1[,"subject"] = as.factor(dataGender1[,"subject"])
      dataGender1[,"replicate"] = as.factor(dataGender1[,"replicate"])
      dataGender1[,"time"] = as.factor(dataGender1[,"time"])
      
      bv <- lme(value ~1, random=~1|subject/time, data=dataGender1)
      
      
      N <- nrow(dataGender1)
      r <- length(unique(dataGender1[,"subject"]))
      ni <- as.numeric(table(dataGender1[ ,"subject"]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      ti = ni/2
      replicateNumber = 2
      
      w1List = list()
      
      for(i in 1:length(ti)){
        
        w1List[[i]] = rep(((replicateNumber^2)*((1/ni[i])-(1/sum(ni))))/(length(unique(data[,"time"])) - 1),ti[i])
        
      }
      
      w3List = list()
      
      for(i in 1:length(ti)){
        
        w3List[[i]] = rep((replicateNumber^2)*((1/sqrt(replicateNumber^2)) - (1/ni[i]))/(bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]),ti[i])
        
      }
      
      w1uList1 = list()
      
      for(i in 1:length(ti)){
        w1uList1[[i]] = sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      w3uList1 = list()
      
      for(i in 1:length(ti)){
        w3uList1[[i]] = ti[i]/sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      
      w1 = do.call(sum, w1List)
      w2 = sgConstant
      w3 = do.call(sum, w3List)
      w1u = sum((1/ti))/sum((1/(ti*(ti/unlist(w1uList1)))))
      w2u = ti[1]/sum(1/(ti*(ti/(0.5*(ti)))))
      w3u = (bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]) / sum((ti-1)/unlist(w3uList1))
      c2u = (w2u-w1u)/(w3u)
      c3u = w2u-1-c2u
      c2 = w1u/w3u
      c3 = 0
      alpha = alpha
      inf = 999999999
      
      
      
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
        #   sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
        
        sigma2_between = sigma_between^2
        sigma2_within = sigma_within^2
        sigma2_analytical = sigma_analytical^2
        
    

      llMean = grandMean-qnorm(1-alpha/2)*seGrandMean
      ulMean = grandMean+qnorm(1-alpha/2)*seGrandMean
      
      
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
      sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
      
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      
      
      ll_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = F))
      ul_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = T))
      
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
      
      g1 = 1-1/(qf(alpha/2, dfbetween, inf, lower.tail = F))
      g2 = 1-1/(qf(alpha/2, dfwithin, inf, lower.tail = F))
      g3 = 1-1/(qf(alpha/2, dfanalytical, inf, lower.tail = F))
      
      h1 =1/(qf(1-alpha/2, dfbetween, inf, lower.tail = F))-1
      h2 = 1/(qf(1-alpha/2, dfwithin, inf, lower.tail = F))-1
      h3 = 1/(qf(1-alpha/2, dfanalytical, inf, lower.tail = F))-1
      
      g12 = (((qf(alpha/2, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha/2, dfbetween, dfwithin, lower.tail = F))
      g13 = (((qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F))
      g23 = (((qf(alpha/2, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))
      g32 = (((qf(alpha/2, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
      h12 = (((1-qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))
      h13 = (((1-qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))
      h23 = (((1-qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))
      h32 = (((1-qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
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
      
      CVTable = data.frame(matrix(NA,4,5))
      colnames(CVTable) = c("Source", "sigma", "CV%", lower_title, upper_title)
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "sigma"] = round(sigma_between,decimal)
      CVTable[2, "sigma"] = round(sigma_within,decimal)
      CVTable[3, "sigma"] = round(sigma_analytical,decimal)
      CVTable[4, "sigma"] = round(sigma_total,decimal)
      
      CVTable[1, lower_title] = round(lower_between*100,decimal)
      CVTable[2, lower_title] = round(lower_within*100,decimal)
      CVTable[3, lower_title] = round(lower_analytical*100,decimal)
      CVTable[4, lower_title] = round(lower_total*100,decimal)
      
      CVTable[1, upper_title] = round(upper_between*100,decimal)
      CVTable[2, upper_title] = round(upper_within*100,decimal)
      CVTable[3, upper_title] = round(upper_analytical*100,decimal)
      CVTable[4, upper_title] = round(upper_total*100,decimal)
      
      CVTable[1, "CV%"] = round(CV_between*100,decimal)
      CVTable[2, "CV%"] = round(CV_within*100,decimal)
      CVTable[3, "CV%"] = round(CV_analytical*100,decimal)
      CVTable[4, "CV%"] = round(CV_total*100,decimal)
      
      CVa = CVTable[3,3] #in percentage
      CVi = CVTable[2,3]#in percentage
      CVg = CVTable[1,3] #in percentage
      
      z = qnorm(1-alpha/2) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", lowerMean_title, upperMean_title, "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender1
      CVresults[1,2] = round(grandMean,decimal)
      CVresults[1,3] = round(llMean,decimal)
      CVresults[1,4] = round(ulMean,decimal)
      CVresults[1,5] = round(CVa,decimal)
      CVresults[1,6] = round(CVi,decimal)
      CVresults[1,7] = round(CVg,decimal)
      CVresults[1,8] = round(II,decimal)
      CVresults[1,9] = round(RCV,decimal)
      CVresults[1,10] = nrow(dataGender1)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,decimal))
      errorTable[2,1] = paste("<",round(0.50*CVi,decimal))
      errorTable[3,1] = paste("<",round(0.75*CVi,decimal))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsOriginalGenderLme1 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      #################################### Gender = Gender 2 ############################
      gender2 = names(dataGender)[2]
      
      
      datagender2 = dataGender[[gender2]]
      datagender2[,"subject"] = as.factor(datagender2[,"subject"])
      datagender2[,"replicate"] = as.factor(datagender2[,"replicate"])
      datagender2[,"time"] = as.factor(datagender2[,"time"])
      
      bv <- lme(value ~1, random=~1|subject/time, data=datagender2)
      
      
      N <- nrow(datagender2)
      r <- length(unique(datagender2[,"subject"]))
      ni <- as.numeric(table(datagender2[ ,"subject"]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      ti = ni/2
      replicateNumber = 2
      
      w1List = list()
      
      for(i in 1:length(ti)){
        
        w1List[[i]] = rep(((replicateNumber^2)*((1/ni[i])-(1/sum(ni))))/(length(unique(data[,"time"])) - 1),ti[i])
        
      }
      
      w3List = list()
      
      for(i in 1:length(ti)){
        
        w3List[[i]] = rep((replicateNumber^2)*((1/sqrt(replicateNumber^2)) - (1/ni[i]))/(bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]),ti[i])
        
      }
      
      w1uList1 = list()
      
      for(i in 1:length(ti)){
        w1uList1[[i]] = sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      w3uList1 = list()
      
      for(i in 1:length(ti)){
        w3uList1[[i]] = ti[i]/sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      
      w1 = do.call(sum, w1List)
      w2 = sgConstant
      w3 = do.call(sum, w3List)
      w1u = sum((1/ti))/sum((1/(ti*(ti/unlist(w1uList1)))))
      w2u = ti[1]/sum(1/(ti*(ti/(0.5*(ti)))))
      w3u = (bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]) / sum((ti-1)/unlist(w3uList1))
      c2u = (w2u-w1u)/(w3u)
      c3u = w2u-1-c2u
      c2 = w1u/w3u
      c3 = 0
      alpha = alpha
      inf = 999999999
      
      
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
        #   sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
        
        sigma2_between = sigma_between^2
        sigma2_within = sigma_within^2
        sigma2_analytical = sigma_analytical^2
        
      
      
  
      llMean = grandMean-qnorm(1-alpha/2)*seGrandMean
      ulMean = grandMean+qnorm(1-alpha/2)*seGrandMean
      
      
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
      sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
      
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      
      ll_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = F))
      ul_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = T))
      
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
      
      g1 = 1-1/(qf(alpha/2, dfbetween, inf, lower.tail = F))
      g2 = 1-1/(qf(alpha/2, dfwithin, inf, lower.tail = F))
      g3 = 1-1/(qf(alpha/2, dfanalytical, inf, lower.tail = F))
      
      h1 =1/(qf(1-alpha/2, dfbetween, inf, lower.tail = F))-1
      h2 = 1/(qf(1-alpha/2, dfwithin, inf, lower.tail = F))-1
      h3 = 1/(qf(1-alpha/2, dfanalytical, inf, lower.tail = F))-1
      
      g12 = (((qf(alpha/2, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha/2, dfbetween, dfwithin, lower.tail = F))
      g13 = (((qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F))
      g23 = (((qf(alpha/2, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))
      g32 = (((qf(alpha/2, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
      h12 = (((1-qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))
      h13 = (((1-qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))
      h23 = (((1-qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))
      h32 = (((1-qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
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
      
      CVTable = data.frame(matrix(NA,4,5))
      colnames(CVTable) = c("Source", "sigma", "CV%", lower_title, upper_title)
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
      
      CVTable[1, "sigma"] = round(sigma_between,decimal)
      CVTable[2, "sigma"] = round(sigma_within,decimal)
      CVTable[3, "sigma"] = round(sigma_analytical,decimal)
      CVTable[4, "sigma"] = round(sigma_total,decimal)
    
      CVTable[1, lower_title] = round(lower_between*100,decimal)
      CVTable[2, lower_title] = round(lower_within*100,decimal)
      CVTable[3, lower_title] = round(lower_analytical*100,decimal)
      CVTable[4, lower_title] = round(lower_total*100,decimal)
      
      CVTable[1, upper_title] = round(upper_between*100,decimal)
      CVTable[2, upper_title] = round(upper_within*100,decimal)
      CVTable[3, upper_title] = round(upper_analytical*100,decimal)
      CVTable[4, upper_title] = round(upper_total*100,decimal)
      
      CVTable[1, "CV%"] = round(CV_between*100,decimal)
      CVTable[2, "CV%"] = round(CV_within*100,decimal)
      CVTable[3, "CV%"] = round(CV_analytical*100,decimal)
      CVTable[4, "CV%"] = round(CV_total*100,decimal)
      
      CVa = CVTable[3,3] #in percentage
      CVi = CVTable[2,3]#in percentage
      CVg = CVTable[1,3] #in percentage
      
      z = qnorm(1-alpha/2) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", lowerMean_title, upperMean_title, "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender2
      CVresults[1,2] = round(grandMean,decimal)
      CVresults[1,3] = round(llMean,decimal)
      CVresults[1,4] = round(ulMean,decimal)
      CVresults[1,5] = round(CVa,decimal)
      CVresults[1,6] = round(CVi,decimal)
      CVresults[1,7] = round(CVg,decimal)
      CVresults[1,8] = round(II,decimal)
      CVresults[1,9] = round(RCV,decimal)
      CVresults[1,10] = nrow(datagender2)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,decimal))
      errorTable[2,1] = paste("<",round(0.50*CVi,decimal))
      errorTable[3,1] = paste("<",round(0.75*CVi,decimal))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsOriginalGenderLme2 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      
    
    
    
    
    
  }else{
    resultsAllOriginalLme = NULL
    resultsOriginalGenderLme1 = NULL
    resultsOriginalGenderLme2 = NULL
    
  }  
  
####################### Transform Back ######################
         
  if(CVresult == "transformBack") {
    data[,"subject"] = as.factor(data[,"subject"])
    data[,"replicate"] = as.factor(data[,"replicate"])
    data[,"time"] = as.factor(data[,"time"])
    
    bv <- lme(value ~1, random=~1|subject/time, data=data)
    
    N <- nrow(data)
    r <- length(unique(data[,"subject"]))
    ni <- as.numeric(table(data[ ,"subject"]))
    sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
    
    ti = ni/2
    replicateNumber = 2
    
    w1List = list()
    
    for(i in 1:length(ti)){
      
      w1List[[i]] = rep(((replicateNumber^2)*((1/ni[i])-(1/sum(ni))))/(length(unique(data[,"time"])) - 1),ti[i])
      
    }
    
    w3List = list()
    
    for(i in 1:length(ti)){
      
      w3List[[i]] = rep((replicateNumber^2)*((1/sqrt(replicateNumber^2)) - (1/ni[i]))/(bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]),ti[i])
      
    }
    
    w1uList1 = list()
    
    for(i in 1:length(ti)){
      w1uList1[[i]] = sum(rep(1/sqrt(replicateNumber^2), ti[i]))
    }
    
    
    w3uList1 = list()
    
    for(i in 1:length(ti)){
      w3uList1[[i]] = ti[i]/sum(rep(1/sqrt(replicateNumber^2), ti[i]))
    }
    
    
    
    w1 = do.call(sum, w1List)
    w2 = sgConstant
    w3 = do.call(sum, w3List)
    w1u = sum((1/ti))/sum((1/(ti*(ti/unlist(w1uList1)))))
    w2u = ti[1]/sum(1/(ti*(ti/(0.5*(ti)))))
    w3u = (bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]) / sum((ti-1)/unlist(w3uList1))
    c2u = (w2u-w1u)/(w3u)
    c3u = w2u-1-c2u
    c2 = w1u/w3u
    c3 = 0
    alpha = alpha
    inf = 999999999
    
    
    
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
      #   sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
      
      sigma2_between = sigma_between^2
      sigma2_within = sigma_within^2
      sigma2_analytical = sigma_analytical^2
      
    
    
    llMean = grandMean-qnorm(1-alpha/2)*seGrandMean
    ulMean = grandMean+qnorm(1-alpha/2)*seGrandMean
    
    
    
    sigma_analytical = sqrt(sigma2_analytical)
    sigma_within = sqrt(sigma2_within)
    sigma_between = sqrt(sigma2_between)
    sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
    
    
    CV_analytical = abs(sigma_analytical/grandMean)
    CV_within = abs(sigma_within/grandMean)
    CV_between = abs(sigma_between/grandMean)
    CV_total = abs(sigma_total/grandMean)
    
    
    
    ll_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = F))
    ul_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = T))
    
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
    
    g1 = 1-1/(qf(alpha/2, dfbetween, inf, lower.tail = F))
    g2 = 1-1/(qf(alpha/2, dfwithin, inf, lower.tail = F))
    g3 = 1-1/(qf(alpha/2, dfanalytical, inf, lower.tail = F))
    
    h1 =1/(qf(1-alpha/2, dfbetween, inf, lower.tail = F))-1
    h2 = 1/(qf(1-alpha/2, dfwithin, inf, lower.tail = F))-1
    h3 = 1/(qf(1-alpha/2, dfanalytical, inf, lower.tail = F))-1
    
    g12 = (((qf(alpha/2, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha/2, dfbetween, dfwithin, lower.tail = F))
    g13 = (((qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F))
    g23 = (((qf(alpha/2, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))
    g32 = (((qf(alpha/2, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))
    
    h12 = (((1-qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))
    h13 = (((1-qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))
    h23 = (((1-qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))
    h32 = (((1-qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))
    
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
    
    CVTable = data.frame(matrix(NA,4,5))
    colnames(CVTable) = c("Source", "sigma", "CV%", lower_title, upper_title)
    
    CVTable[1,"Source"] = "Between"
    CVTable[2,"Source"] = "Within"
    CVTable[3,"Source"] = "Analytical"
    CVTable[4,"Source"] = "Total"

    CVTable[1, "sigma"] = round(sqrt(exp(sigma_between^2)-1)*transformedMean/100,decimal)
    CVTable[2, "sigma"] = round(sqrt(exp(sigma_within^2)-1)*transformedMean/100,decimal)
    CVTable[3, "sigma"] = round(sqrt(exp(sigma_analytical^2)-1)*transformedMean/100,decimal)
    CVTable[4, "sigma"] = round(sqrt(exp(sigma_total^2)-1)*transformedMean/100,decimal)
    
    CVTable[1, lower_title] = round(sqrt(exp(ll_between^2)-1)*100,decimal)
    CVTable[2, lower_title] = round(sqrt(exp(ll_within^2)-1)*100,decimal)
    CVTable[3, lower_title] = round(sqrt(exp(ll_analytical^2)-1)*100,decimal)
    CVTable[4, lower_title] = round(sqrt(exp(ll_total^2)-1)*100,decimal)
    
    CVTable[1, upper_title] = round(sqrt(exp(ul_between^2)-1)*100,decimal)
    CVTable[2, upper_title] = round(sqrt(exp(ul_within^2)-1)*100,decimal)
    CVTable[3, upper_title] = round(sqrt(exp(ul_analytical^2)-1)*100,decimal)
    CVTable[4, upper_title] = round(sqrt(exp(ul_total^2)-1)*100,decimal)
    
    CVTable[1, "CV%"] = round(sqrt(exp(sigma_between^2)-1)*100,decimal)
    CVTable[2, "CV%"] = round(sqrt(exp(sigma_within^2)-1)*100,decimal)
    CVTable[3, "CV%"] = round(sqrt(exp(sigma_analytical^2)-1)*100,decimal)
    CVTable[4, "CV%"] = round(sqrt(exp(sigma_total^2)-1)*100,decimal)
    
    CVa = CVTable[3,3] #in percentage
    CVi = CVTable[2,3]#in percentage
    CVg = CVTable[1,3] #in percentage
    
    z = qnorm(1-alpha/2) #for alpha = 0.05
    #z = 2.58 #for alpha = 0.01
    
    ### Calculation of Index of Individuality (II) ###
    #II indicates the utility of population-based reference values
    II = CVi/CVg
    
    ### Calculation of Reference Change Value (RCV) ###
    #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
    #two consecutive measurements from the same individual is significant or not.
    RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
    
    CVresults = data.frame(matrix(NA,1,10))
    colnames(CVresults) = c("Group", "Mean", lowerMean_title, upperMean_title, "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
    CVresults[1,1] = "All Subjects"
    CVresults[1,2] = round(transformedMean,decimal)
    CVresults[1,3] = round(llTransformedMean,decimal)
    CVresults[1,4] = round(ulTransformedMean,decimal)
    CVresults[1,5] = round(CVa,decimal)
    CVresults[1,6] = round(CVi,decimal)
    CVresults[1,7] = round(CVg,decimal)
    CVresults[1,8] = round(II,decimal)
    CVresults[1,9] = round(RCV,decimal)
    CVresults[1,10] = nrow(data)
    
    
    errorTable = matrix(NA,3,3)
    rownames(errorTable) = c("Optimal","Desirable","Minimal")
    colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
    
    errorTable[1,1] = paste("<",round(0.25*CVi,decimal))
    errorTable[2,1] = paste("<",round(0.50*CVi,decimal))
    errorTable[3,1] = paste("<",round(0.75*CVi,decimal))
    
    errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),decimal))
    
    errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),decimal))
    errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),decimal))
    
    errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
    
    resultsAllTransformBackLnLme = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
    
    
      
      
      gender1 = names(dataGender)[1]
      
      ############################# Gender = Gender 1 #######################################
      
      dataGender1 = dataGender[[gender1]]
      dataGender1[,"subject"] = as.factor(dataGender1[,"subject"])
      dataGender1[,"replicate"] = as.factor(dataGender1[,"replicate"])
      dataGender1[,"time"] = as.factor(dataGender1[,"time"])
      
      bv <- lme(value ~1, random=~1|subject/time, data=dataGender1)
      
      N <- nrow(dataGender1)
      r <- length(unique(dataGender1[,"subject"]))
      ni <- as.numeric(table(dataGender1[ ,"subject"]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      ti = ni/2
      replicateNumber = 2
      
      w1List = list()
      
      for(i in 1:length(ti)){
        
        w1List[[i]] = rep(((replicateNumber^2)*((1/ni[i])-(1/sum(ni))))/(length(unique(data[,"time"])) - 1),ti[i])
        
      }
      
      w3List = list()
      
      for(i in 1:length(ti)){
        
        w3List[[i]] = rep((replicateNumber^2)*((1/sqrt(replicateNumber^2)) - (1/ni[i]))/(bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]),ti[i])
        
      }
      
      w1uList1 = list()
      
      for(i in 1:length(ti)){
        w1uList1[[i]] = sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      w3uList1 = list()
      
      for(i in 1:length(ti)){
        w3uList1[[i]] = ti[i]/sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      
      w1 = do.call(sum, w1List)
      w2 = sgConstant
      w3 = do.call(sum, w3List)
      w1u = sum((1/ti))/sum((1/(ti*(ti/unlist(w1uList1)))))
      w2u = ti[1]/sum(1/(ti*(ti/(0.5*(ti)))))
      w3u = (bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]) / sum((ti-1)/unlist(w3uList1))
      c2u = (w2u-w1u)/(w3u)
      c3u = w2u-1-c2u
      c2 = w1u/w3u
      c3 = 0
      alpha = alpha
      inf = 999999999
      
      
      
      
     
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
        #   sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
        
        sigma2_between = sigma_between^2
        sigma2_within = sigma_within^2
        sigma2_analytical = sigma_analytical^2
        
      
      
      
      llMean = grandMean-qnorm(1-alpha/2)*seGrandMean
      ulMean = grandMean+qnorm(1-alpha/2)*seGrandMean
      
      
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
      sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
      
      
      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      
      
      ll_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = F))
      ul_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = T))
      
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
      
      g1 = 1-1/(qf(alpha/2, dfbetween, inf, lower.tail = F))
      g2 = 1-1/(qf(alpha/2, dfwithin, inf, lower.tail = F))
      g3 = 1-1/(qf(alpha/2, dfanalytical, inf, lower.tail = F))
      
      h1 =1/(qf(1-alpha/2, dfbetween, inf, lower.tail = F))-1
      h2 = 1/(qf(1-alpha/2, dfwithin, inf, lower.tail = F))-1
      h3 = 1/(qf(1-alpha/2, dfanalytical, inf, lower.tail = F))-1
      
      g12 = (((qf(alpha/2, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha/2, dfbetween, dfwithin, lower.tail = F))
      g13 = (((qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F))
      g23 = (((qf(alpha/2, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))
      g32 = (((qf(alpha/2, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
      h12 = (((1-qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))
      h13 = (((1-qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))
      h23 = (((1-qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))
      h32 = (((1-qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
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
      
      CVTable = data.frame(matrix(NA,4,5))
      colnames(CVTable) = c("Source", "sigma", "CV%", lower_title, upper_title)
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"
 
      CVTable[1, "sigma"] = round(sqrt(exp(sigma_between^2)-1)*transformedMean/100,decimal)
      CVTable[2, "sigma"] = round(sqrt(exp(sigma_within^2)-1)*transformedMean/100,decimal)
      CVTable[3, "sigma"] = round(sqrt(exp(sigma_analytical^2)-1)*transformedMean/100,decimal)
      CVTable[4, "sigma"] = round(sqrt(exp(sigma_total^2)-1)*transformedMean/100,decimal)
      
      CVTable[1, lower_title] = round(sqrt(exp(ll_between^2)-1)*100,decimal)
      CVTable[2, lower_title] = round(sqrt(exp(ll_within^2)-1)*100,decimal)
      CVTable[3, lower_title] = round(sqrt(exp(ll_analytical^2)-1)*100,decimal)
      CVTable[4, lower_title] = round(sqrt(exp(ll_total^2)-1)*100,decimal)
      
      CVTable[1, upper_title] = round(sqrt(exp(ul_between^2)-1)*100,decimal)
      CVTable[2, upper_title] = round(sqrt(exp(ul_within^2)-1)*100,decimal)
      CVTable[3, upper_title] = round(sqrt(exp(ul_analytical^2)-1)*100,decimal)
      CVTable[4, upper_title] = round(sqrt(exp(ul_total^2)-1)*100,decimal)
      
      CVTable[1, "CV%"] = round(sqrt(exp(sigma_between^2)-1)*100,decimal)
      CVTable[2, "CV%"] = round(sqrt(exp(sigma_within^2)-1)*100,decimal)
      CVTable[3, "CV%"] = round(sqrt(exp(sigma_analytical^2)-1)*100,decimal)
      CVTable[4, "CV%"] = round(sqrt(exp(sigma_total^2)-1)*100,decimal)
      
      CVa = CVTable[3,3] #in percentage
      CVi = CVTable[2,3]#in percentage
      CVg = CVTable[1,3] #in percentage
      
      z = qnorm(1-alpha/2) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", lowerMean_title, upperMean_title, "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender1
      CVresults[1,2] = round(transformedMean,decimal)
      CVresults[1,3] = round(llTransformedMean,decimal)
      CVresults[1,4] = round(ulTransformedMean,decimal)
      CVresults[1,5] = round(CVa,decimal)
      CVresults[1,6] = round(CVi,decimal)
      CVresults[1,7] = round(CVg,decimal)
      CVresults[1,8] = round(II,decimal)
      CVresults[1,9] = round(RCV,decimal)
      CVresults[1,10] = nrow(dataGender1)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,decimal))
      errorTable[2,1] = paste("<",round(0.50*CVi,decimal))
      errorTable[3,1] = paste("<",round(0.75*CVi,decimal))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsTransformBackGenderLnLme1 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      
      ################################################# Gender= Gender 2 ##############################################
      
      
      gender2 = names(dataGender)[2]
      
      
      datagender2 = dataGender[[gender2]]
      datagender2[,"subject"] = as.factor(datagender2[,"subject"])
      datagender2[,"replicate"] = as.factor(datagender2[,"replicate"])
      datagender2[,"time"] = as.factor(datagender2[,"time"])
      
      bv <- lme(value ~1, random=~1|subject/time, data=datagender2)
      
      N <- nrow(datagender2)
      r <- length(unique(datagender2[,"subject"]))
      ni <- as.numeric(table(datagender2[ ,"subject"]))
      ni = ni[ni>0]
      sgConstant <- (1/(r - 1))*sum((ni^2)*(1/ni - 1/N))
      
      
      ti = ni/2
      replicateNumber = 2
      
      w1List = list()
      
      for(i in 1:length(ti)){
        
        w1List[[i]] = rep(((replicateNumber^2)*((1/ni[i])-(1/sum(ni))))/(length(unique(data[,"time"])) - 1),ti[i])
        
      }
      
      w3List = list()
      
      for(i in 1:length(ti)){
        
        w3List[[i]] = rep((replicateNumber^2)*((1/sqrt(replicateNumber^2)) - (1/ni[i]))/(bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]),ti[i])
        
      }
      
      w1uList1 = list()
      
      for(i in 1:length(ti)){
        w1uList1[[i]] = sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      w3uList1 = list()
      
      for(i in 1:length(ti)){
        w3uList1[[i]] = ti[i]/sum(rep(1/sqrt(replicateNumber^2), ti[i]))
      }
      
      
      
      w1 = do.call(sum, w1List)
      w2 = sgConstant
      w3 = do.call(sum, w3List)
      w1u = sum((1/ti))/sum((1/(ti*(ti/unlist(w1uList1)))))
      w2u = ti[1]/sum(1/(ti*(ti/(0.5*(ti)))))
      w3u = (bv$dims$ngrps[["time"]]-bv$dims$ngrps[["subject"]]) / sum((ti-1)/unlist(w3uList1))
      c2u = (w2u-w1u)/(w3u)
      c3u = w2u-1-c2u
      c2 = w1u/w3u
      c3 = 0
      alpha = alpha
      inf = 999999999
      
      
      
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
        #   sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)
        
        sigma2_between = sigma_between^2
        sigma2_within = sigma_within^2
        sigma2_analytical = sigma_analytical^2
        
      
      llMean = grandMean-qnorm(1-alpha/2)*seGrandMean
      ulMean = grandMean+qnorm(1-alpha/2)*seGrandMean
      
      sigma_analytical = sqrt(sigma2_analytical)
      sigma_within = sqrt(sigma2_within)
      sigma_between = sqrt(sigma2_between)
        sigma_total = sqrt(sigma2_analytical + sigma2_within + sigma2_between)

      CV_analytical = abs(sigma_analytical/grandMean)
      CV_within = abs(sigma_within/grandMean)
      CV_between = abs(sigma_between/grandMean)
      CV_total = abs(sigma_total/grandMean)
      
      
      ll_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = F))
      ul_analytical = sqrt((sigma_analytical^2)/qf(alpha/2, bv$dims$ngrps[[1]], inf, lower.tail = T))
      
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
      
      g1 = 1-1/(qf(alpha/2, dfbetween, inf, lower.tail = F))
      g2 = 1-1/(qf(alpha/2, dfwithin, inf, lower.tail = F))
      g3 = 1-1/(qf(alpha/2, dfanalytical, inf, lower.tail = F))
      
      h1 =1/(qf(1-alpha/2, dfbetween, inf, lower.tail = F))-1
      h2 = 1/(qf(1-alpha/2, dfwithin, inf, lower.tail = F))-1
      h3 = 1/(qf(1-alpha/2, dfanalytical, inf, lower.tail = F))-1
      
      g12 = (((qf(alpha/2, dfbetween, dfwithin, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfwithin, lower.tail = F)^2)-h2^2)/(qf(alpha/2, dfbetween, dfwithin, lower.tail = F))
      g13 = (((qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)-1)^2)-g1^2*(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F)^2)-h3^2)/(qf(alpha/2, dfbetween, dfanalytical, lower.tail = F))
      g23 = (((qf(alpha/2, dfwithin, dfanalytical, lower.tail = F)-1)^2)-g2^2*(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-h3^2)/(qf(alpha/2, dfwithin, dfanalytical, lower.tail = F))
      g32 = (((qf(alpha/2, dfanalytical, dfwithin, lower.tail = F)-1)^2)-g3^2*(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-h2^2)/(qf(alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
      h12 = (((1-qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2,  dfbetween, dfwithin, lower.tail = F))
      h13 = (((1-qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2)-h1^2*(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2,  dfbetween, dfanalytical, lower.tail = F))
      h23 = (((1-qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2)-h2^2*(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))^2-g3^2)/(qf(1-alpha/2, dfwithin, dfanalytical, lower.tail = F))
      h32 = (((1-qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2)-h3^2*(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))^2-g2^2)/(qf(1-alpha/2, dfanalytical, dfwithin, lower.tail = F))
      
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
      
      CVTable = data.frame(matrix(NA,4,5))
      colnames(CVTable) = c("Source", "sigma", "CV%", lower_title, upper_title)
      
      CVTable[1,"Source"] = "Between"
      CVTable[2,"Source"] = "Within"
      CVTable[3,"Source"] = "Analytical"
      CVTable[4,"Source"] = "Total"

      CVTable[1, "sigma"] = round(sqrt(exp(sigma_between^2)-1)*transformedMean/100,decimal)
      CVTable[2, "sigma"] = round(sqrt(exp(sigma_within^2)-1)*transformedMean/100,decimal)
      CVTable[3, "sigma"] = round(sqrt(exp(sigma_analytical^2)-1)*transformedMean/100,decimal)
      CVTable[4, "sigma"] = round(sqrt(exp(sigma_total^2)-1)*transformedMean/100,decimal)
      
      CVTable[1, lower_title] = round(sqrt(exp(ll_between^2)-1)*100,decimal)
      CVTable[2, lower_title] = round(sqrt(exp(ll_within^2)-1)*100,decimal)
      CVTable[3, lower_title] = round(sqrt(exp(ll_analytical^2)-1)*100,decimal)
      CVTable[4, lower_title] = round(sqrt(exp(ll_total^2)-1)*100,decimal)
      
      CVTable[1, upper_title] = round(sqrt(exp(ul_between^2)-1)*100,decimal)
      CVTable[2, upper_title] = round(sqrt(exp(ul_within^2)-1)*100,decimal)
      CVTable[3, upper_title] = round(sqrt(exp(ul_analytical^2)-1)*100,decimal)
      CVTable[4, upper_title] = round(sqrt(exp(ul_total^2)-1)*100,decimal)
      
      CVTable[1, "CV%"] = round(sqrt(exp(sigma_between^2)-1)*100,decimal)
      CVTable[2, "CV%"] = round(sqrt(exp(sigma_within^2)-1)*100,decimal)
      CVTable[3, "CV%"] = round(sqrt(exp(sigma_analytical^2)-1)*100,decimal)
      CVTable[4, "CV%"] = round(sqrt(exp(sigma_total^2)-1)*100,decimal)
      
      CVa = CVTable[3,3] #in percentage
      CVi = CVTable[2,3]#in percentage
      CVg = CVTable[1,3] #in percentage
      
      z = qnorm(1-alpha/2) #for alpha = 0.05
      #z = 2.58 #for alpha = 0.01
      
      ### Calculation of Index of Individuality (II) ###
      #II indicates the utility of population-based reference values
      II = CVi/CVg
      
      ### Calculation of Reference Change Value (RCV) ###
      #RCV is also called as ‘critical difference (CD)’, and indicates whether the difference between
      #two consecutive measurements from the same individual is significant or not.
      RCV = sqrt(2)*z*sqrt((CVa^2)+(CVi^2))
      
      CVresults = data.frame(matrix(NA,1,10))
      colnames(CVresults) = c("Group", "Mean", lowerMean_title, upperMean_title, "CV_A%", "CV_I%", "CV_G%", "II", "RCV%", "n")
      CVresults[1,1] = gender2
      CVresults[1,2] = round(transformedMean,decimal)
      CVresults[1,3] = round(llTransformedMean,decimal)
      CVresults[1,4] = round(ulTransformedMean,decimal)
      CVresults[1,5] = round(CVa,decimal)
      CVresults[1,6] = round(CVi,decimal)
      CVresults[1,7] = round(CVg,decimal)
      CVresults[1,8] = round(II,decimal)
      CVresults[1,9] = round(RCV,decimal)
      CVresults[1,10] = nrow(datagender2)
      
      
      errorTable = matrix(NA,3,3)
      rownames(errorTable) = c("Optimal","Desirable","Minimal")
      colnames(errorTable) = c("Imprecision,%", "Bias,%","TEa,%")
      
      errorTable[1,1] = paste("<",round(0.25*CVi,decimal))
      errorTable[2,1] = paste("<",round(0.50*CVi,decimal))
      errorTable[3,1] = paste("<",round(0.75*CVi,decimal))
      
      errorTable[1,2] = paste("<",round(0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,2] = paste("<",round(0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,2] = paste("<",round(0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      errorTable[1,3] = paste("<",round(1.65*(0.25*CVi)+0.125*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[2,3] = paste("<",round(1.65*(0.50*CVi)+0.250*sqrt((CVi^2)*(CVg^2)),decimal))
      errorTable[3,3] = paste("<",round(1.65*(0.75*CVi)+0.375*sqrt((CVi^2)*(CVg^2)),decimal))
      
      errorTable = cbind.data.frame("Quality Levels" = row.names(errorTable), errorTable)
      
      
      resultsTransformBackGenderLnLme2 = list(CVTable = CVTable, CVresults = CVresults, errorTable = errorTable)
      
      
      
    
    
    
  }else{
    resultsAllTransformBackLnLme = NULL
    resultsTransformBackGenderLnLme1 = NULL
    resultsTransformBackGenderLnLme2 = NULL
    
  } 
  
      resultsAllTransformBack = NULL
      resultsTransformBackGender1 = NULL
      resultsTransformBackGender2 = NULL
      
      resultsAllOriginal = NULL
      resultsOriginalGender1 = NULL
      resultsOriginalGender2 = NULL
      
  allResults = list(resultsAllOriginal = resultsAllOriginal, resultsOriginalGender1 = resultsOriginalGender1, resultsOriginalGender2 = resultsOriginalGender2,
                    resultsAllOriginalLme = resultsAllOriginalLme, resultsOriginalGenderLme1 = resultsOriginalGenderLme1, resultsOriginalGenderLme2 = resultsOriginalGenderLme2,
                    resultsAllTransformBack = resultsAllTransformBack,  resultsTransformBackGender1 = resultsTransformBackGender1,
                    resultsTransformBackGender2 = resultsTransformBackGender2, resultsAllTransformBackLnLme = resultsAllTransformBackLnLme,  resultsTransformBackGenderLnLme1 = resultsTransformBackGenderLnLme1,
                    resultsTransformBackGenderLnLme2 = resultsTransformBackGenderLnLme2, resultsAllCvAnova = resultsAllCvAnova,
                    resultsCvAnovaGender1 = resultsCvAnovaGender1, resultsCvAnovaGender2 = resultsCvAnovaGender2)
  
  return(allResults)
}

