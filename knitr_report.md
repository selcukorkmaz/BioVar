# Biological Variation Report





This report was created using the **BV** tool for analyzing biological variation data. **BV** is developed by **Turcosa Analytics** and can be accessed online at http://opensoft.turcosa.com.tr/BV 

Additional documentation for the **BV**, including instructions on how to download the application for offline use, can be found at https://github.com/selcukorkmaz/BV


# Introduction

In this report, we consider the scenario where we have prior evidence that the treatment might work better in a one subpopulation than another. We use the term "adaptive design" to refer to a group sequential design that starts by enrolling from both subpopulations, and then decides whether or not to continue enrolling from each subpopulation based on interim analyses.  We use the term "standard designs" to refer to group sequential designs where the enrollment criteria are fixed.


*******
# 1. Outlier Detection

## 1.1. Step 1: Outliers in the sets of duplicate results using the Cochran test

![plot of chunk unnamed-chunk-1](tempFiguresForKnitrReport/unnamed-chunk-1-1.png)
*******


## 1.2. Step 2: Outliers in the variances of the results from each subject using the Cochran test

![plot of chunk unnamed-chunk-2](tempFiguresForKnitrReport/unnamed-chunk-2-1.png)
*******


## 1.3 Step 3: Outliers in the variances of the results from each subject using the Cochran test

![plot of chunk unnamed-chunk-3](tempFiguresForKnitrReport/unnamed-chunk-3-1.png)
*******

# Normality Test
## 2.1. Step 1: On set of results from each individual
![plot of chunk unnamed-chunk-4](tempFiguresForKnitrReport/unnamed-chunk-4-1.png)

## 2.2. Step 2: On mean values of subjects
![plot of chunk unnamed-chunk-5](tempFiguresForKnitrReport/unnamed-chunk-5-1.png)

# 3. Subset Analysis
## 3.1. Step 1: Student's t test for mean differences of gender
![plot of chunk unnamed-chunk-6](tempFiguresForKnitrReport/unnamed-chunk-6-1.png)

## 3.2. Step 2: Student's t test for average within-subject total variance
![plot of chunk unnamed-chunk-7](tempFiguresForKnitrReport/unnamed-chunk-7-1.png)

# 4. Analysis of Variance
## 4.1. All subjects



### 4.1.1. Step 1: Coefficient of variation results

![plot of chunk unnamed-chunk-8](tempFiguresForKnitrReport/unnamed-chunk-8-1.png)

### 4.1.2. Step 2: Analysis of variance table

![plot of chunk unnamed-chunk-9](tempFiguresForKnitrReport/unnamed-chunk-9-1.png)


### 4.1.3. Step 3: Quality measures

![plot of chunk unnamed-chunk-10](tempFiguresForKnitrReport/unnamed-chunk-10-1.png)

## 4.2. Gender 1

### 4.2.1. Step 1: Coefficient of variation results
![plot of chunk unnamed-chunk-11](tempFiguresForKnitrReport/unnamed-chunk-11-1.png)


### 4.2.2. Step 2: Analysis of variance table
![plot of chunk unnamed-chunk-12](tempFiguresForKnitrReport/unnamed-chunk-12-1.png)


### 4.2.3. Step 3: Quality measures
![plot of chunk unnamed-chunk-13](tempFiguresForKnitrReport/unnamed-chunk-13-1.png)


## 4.3. Gender 2


### 4.3.1. Step 1: Coefficient of variation results
![plot of chunk unnamed-chunk-14](tempFiguresForKnitrReport/unnamed-chunk-14-1.png)


### 4.3.2. Step 2: Analysis of variance table
![plot of chunk unnamed-chunk-15](tempFiguresForKnitrReport/unnamed-chunk-15-1.png)


### 4.3.3. Step 3: Quality measures
![plot of chunk unnamed-chunk-16](tempFiguresForKnitrReport/unnamed-chunk-16-1.png)


