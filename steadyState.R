# # 
# data = read.table("~/Documents/GitHub/BV/data/example_data.txt",header = T)
# measure = "Measurement"; time = "Time"
# central = "mean"; decimal = 3; alpha = 0.05

steadyState <- function(data, measure, time, central = "median", decimal = 3, alpha = 0.05){

    alpha = as.numeric(alpha)
  
    if(central == "median"){

      averageTime = aggregate(data[,measure], list(data[,time]), median, na.rm=TRUE)
      colnames(averageTime) = c("time", "value")

  }

if(central == "mean"){
  
  averageTime = aggregate(data[,measure], list(data[,time]), mean, na.rm=TRUE)
  colnames(averageTime) = c("time", "value")
  
}

if(central == "median"){
  
  ylabel = "Median Value"
}else{
  ylabel = "Mean Value"
  
}

lr = lm(value ~ time, data = averageTime)
plot(averageTime$time, averageTime$value, xlab = "Time", ylab = ylabel,
     pch=19, cex = 2)

abline(lr[[1]][[1]], lr[[1]][[2]], col = "red", lwd = 3)

summary = as.data.frame(summary(lr)[[4]])

summary2= as.data.frame(apply(summary, 2, formatC, decimal, format="f"))
summary2$`Pr(>|t|)` = as.character(summary2$`Pr(>|t|)`)


if(summary$`Pr(>|t|)`[1] < 0.001){
  
  summary2$`Pr(>|t|)`[1] <-"<0.001"
}

if(summary$`Pr(>|t|)`[2] < 0.001){
  
  summary2$`Pr(>|t|)`[2] <- "<0.001"
}

ci = confint(lr, level=1-alpha)

res = ifelse(ci[2,1] < 0 && ci[2,2] > 0, "Steady State", "No Steady State")
res2 = c(NA, res)

ci2= as.data.frame(apply(ci, 2, formatC, decimal, format="f"))

colnames(ci2) = c(paste0("Lower Limit (",(1-alpha)*100,"%)"), paste0("Upper Limit (",(1-alpha)*100,"%)"))

anovaTable = cbind.data.frame(Variable = rownames(summary), summary2, ci2)
anovaTable$Result = res2

colnames(anovaTable)[5] = "p value"
rownames(anovaTable) = NULL
anovaTable

}

