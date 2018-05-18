# Biological Variation Report for (CD3)





This report was created using the **BV** tool for analyzing biological variation data. **BV** is developed by **Turcosa Analytics** and can be accessed online at http://opensoft.turcosa.com.tr/BV 

Additional documentation for the **BV**, including instructions on how to download the application for offline use, can be found at https://github.com/selcukorkmaz/BV


# Introduction 

In this report, we analyzed the biological analyte by following the steps provided by Braga and Panteghini (2017). First, we applied 3 different outlier detection steps: (i) outliers in the sets of duplicate results, (ii) outliers in the variances of the results from each subject and  (iii) outliers in the variances of the results from each subject. We used Cochran test for the first two and Reed's criterion for the last one. Then we applied normality tests using Shapiro-Wilk and Kolmogorov-Smirnov tests in two steps: (i) on set of results from each individual, (ii) on mean values of subjects. Next, we applied a subset analysis to compare (i) means and (i) average within-subject total variances of gender groups. Finally, we performed analysis of variance and provided coefficient of variation results, ANOVA table and quality measures for all subjects, males and females separately. Fo further details please see Braga and Panteghini (2017).

Here are the abbreviations we used in this report:

**CV:** coefficient of variation
**CV_A:** analytical CV
**CV_I:** within CV
**CV_G:** between CV
**II:** index of individuality
**RCV:** reference change value


*******
# 1. Outlier Detection

For outlier identification, we used Cochran’s test to detect outliers among duplicate measurements) and average within-subject total variance values, and Reed’s criterion to detect outliers among mean concentration values of subjects.

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

For normality checking, first, we applied Shapiro–Wilk test to the set of results from each individual to check data distribution and to validate the normality hypothesis. If the normal distribution assumption is rejected for most of the analyzed subjects (>50%), we applied the natural logarithmic scale transformation of all data. Then we applied the Shapiro-Wilk test to the transformed data. We also used the Shapiro-Wilk test to evaluate the distribution of mean concentration values of all subjects. If the test rejects the hypothesis of normality, we applied Kolmogorov-Smirnov test. If even this test confirms a skewed distribution, we applied a natural logarithmic scale transformation. We repeat the Shapiro–Wilk test on the log-transformed values to experimentally confirm the normality of the transformed data distribution. If the data transformation does not resolve the skewed distribution issue, the authors are forced to stop the calculations. On the other hand, if the normal distribution is in fact confirmed, it is possible to derive the variance components from the transformed data. These data must, however, be converted back before calculating the CVs to make these latter applicable to laboratory practice.

## 2.1. Step 1: On set of results from each individual
![plot of chunk unnamed-chunk-4](tempFiguresForKnitrReport/unnamed-chunk-4-1.png)

## 2.2. Step 2: On mean values of subjects
![plot of chunk unnamed-chunk-5](tempFiguresForKnitrReport/unnamed-chunk-5-1.png)

# 3. Subset Analysis

To compare the mean and average within-subject total variance values of gender groups, we applied independent Student’s t-tests. If t-test shows a statistically significant difference, all the ANOVA results must be evaluated separately for each gender group.

## 3.1. Step 1: Student's t test for mean differences of gender
![plot of chunk unnamed-chunk-6](tempFiguresForKnitrReport/unnamed-chunk-6-1.png)

## 3.2. Step 2: F test for average within-subject total variance
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


