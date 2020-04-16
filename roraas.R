library(boot)
library(lme4)
library(arm)
library(msm)
library(nlme)
library(plyr) #plyr needs to be loaded before dplyr to avoid conflicts
library(dplyr)

set.seed(seed = 42)

P = 48; S = 10; R = 2; meanMale = 1; meanFemale = 1.5; 
CVgFemale = 30; CVi = 10; CVa = 5; CVgMale = 30

Subject <- rep(1:P, each = S * R)
nGender = P*S*R/2
# nMale = P-nFemale
Gender <- c(rep("Female", nGender), rep("Male", nGender))
Time <- rep(1:S, each = R, times = P)
Replicate <- rep(1:R, each = 1, times = P * S)

femaleValues = rep(meanFemale*rtnorm(P/2,1,CVgFemale/100, 1-3*CVgFemale/100, 1+3*CVgFemale/100), each = S*R)
maleValues = rep(meanMale*rtnorm(P/2,1,CVgMale/100, 1-3*CVgMale/100, 1+3*CVgMale/100), each = S*R)

mean(femaleValues)
mean(maleValues)


value1 <- c(femaleValues,maleValues)
value2 <- rep(rtnorm(S*P,1,CVi/100, 1-3*CVi/100, 1+3*CVi/100), each = R)
value3 <- rep(rtnorm(P*S*R,1,CVa/100, 1-3*CVa/100, 1+3*CVa/100), each = 1)

Measurement <- value1 * value2 * value3

output <- (data.frame(Subject, Gender, Time, Replicate, Measurement))
output$Subject <- as.factor(output$Subject)
output$Time <- as.factor(output$Time)
output$Replicate <- as.factor(output$Replicate)
output$Measurement = as.numeric(formatC(output$Measurement, format = "f"))

head(output)

boxplot(output$Measurement ~ output$Gender)
mean = aggregate(output[, "Measurement"], list(output$Gender), mean)
sd = aggregate(output[, "Measurement"], list(output$Gender), sd)

mean
sd
round(((sd$x/mean$x)*100),2)

write.table(output, "~/Documents/Studies/BiologicalVariation/Rcodes/example_data.txt", quote = F, row.names = F, sep = "\t")





test.data<-create.nested.data(P = 24, S = 10, R = 2, pop.mean = 100, CVg = 30, CVi = 10, CVa = 5)
head(test.data)

write.table(output, "~/Documents/Studies/BiologicalVariation/Rcodes//example_data.txt", quote = F, row.names = F, sep = "\t")

inputdata = test.data

normalise.nested.data <- function(inputdata){
  tmp <- inputdata %>%
    group_by(Subject, Time) %>%
    mutate(replicate.count = n(), sample.sum = sum(Measurement, na.rm = TRUE))
  
  output <- tmp %>%
    group_by(Subject) %>%
    mutate(subject_mean = sum(sample.sum, na.rm =
                                TRUE)/sum(replicate.count, na.rm = TRUE),
           normalised_measurment = Measurement / subject_mean)
  return(output)
}


normalise.nested.data(test.data)


test.data$Subject <- as.factor(test.data$Subject)
test.data$Sample <- as.factor(test.data$Sample)
test.data$Replicate <- as.factor(test.data$Replicate)
test.data <- test.data[with(test.data, order(Subject, Sample,Replicate)), ]
head(test.data)
test.data <- na.omit(test.data)

testdatafile<-normalise.nested.data(test.data)
head(testdatafile)

cv.formula <- normalised_measurment ~ (1|Subject) + (1|Subject:Sample)
sd.formula <- measurment ~ (1|Subject) + (1|Subject:Sample)

mod1<-lmer(data=testdatafile, REML = TRUE, cv.formula)
display(mod1)
ci.model <- confint(mod1)


create.new.dataset.indices<-function(inputdata, indices){
  output <- {}
  for(i in 1:length(indices)){
    tmp<-subset(inputdata, inputdata[["Subject"]] == indices[i])
    tmp$Subject<-i
    output<-rbind(output,tmp)
  }
  return(output)
}


bootstrap.nested.data <- function(inputdata){
  #these nested samples are used to find the variance, and re-sampled
  # from the indices from the bootstrap samples
  
  anova_bootstrap_nested <- function(formula, data, indices) {
    d <- create.new.dataset.indices(inputdata, indices)
    mod<-lmer(data = d, formula)
    return(sigma.hat(mod)$sigma$'Subject:Sample')
  }
  
  
  anova_bootstrap <- function(formula, data, indices) {
    d <- create.new.dataset.indices(inputdata, indices)
    mod<-lmer(data = d, formula)
    #we resample from the indices for each resampled dataset for the
    # nested sampling important to use the correct subject number throught
    nested <- boot(indices, statistic=anova_bootstrap_nested, R=N,
                   formula = formula)
    return(c(sigma.hat(mod)$sigma$'Subject:Sample', var(nested$t)))
  }
  
  n.cores <- 32; B<- 1999; N<-999;
  
  results <- boot(unique(inputdata$Subject), statistic=anova_bootstrap,
                  R=B,
                  formula = cv.formula, parallel = "multicore", ncpus =
                    n.cores)
  return(results)
}