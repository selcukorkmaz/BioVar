setwd("~/Documents/GitHub/BV/")

library(DT)
library(shiny)
library(lme4)
library(multcomp)
library(nlme)
library(ggplot2)
library(dplyr)
library(prospectr)
library(nortest)
library(knitr)
source("outlier.R")
source("normality.R")
source("subsetAnalysis.R")
source("analysisOfVariance.R")
source("wideToLong.R")
source("momTransformation.R")

data = read.table("data/example_data.txt", header = T, sep = "\t")
head(data)

measure = "Measurement"
time = "Time"

steadyState <- function(data, measure, time, central = "median", decimal = 3, alpha = 0.05){

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
plot(averageTime$time, averageTime$value, xlab = "Time", ylab = ylabel)

abline(lr[[1]][[1]], lr[[1]][[2]], col = "red")

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
ci2= as.data.frame(apply(ci, 2, formatC, decimal, format="f"))

colnames(ci)

anovaTable = cbind.data.frame(Variable = rownames(anovaTable), summary2, ci)



}