---
title: "STAT 716 Homework 4"
author: "Fahim Hussain"
date: "October 6, 2018"
output:
   html_document:
    keep_md: true
---



```r
median(Boston$crim)
Boston %>% select(crim) %>% nrow
crim01 <- 1:506
crim01[Boston$crim > median(Boston$crim)] <-1
crim01[Boston$crim <= median(Boston$crim)] <- 0
Nboston <- Boston %>% mutate(crim01)
```


```r
glimpse(Nboston)
```

```
## Observations: 506
## Variables: 15
## $ crim    <dbl> 0.00632, 0.02731, 0.02729, 0.03237, 0.06905, 0.02985, ...
## $ zn      <dbl> 18.0, 0.0, 0.0, 0.0, 0.0, 0.0, 12.5, 12.5, 12.5, 12.5,...
## $ indus   <dbl> 2.31, 7.07, 7.07, 2.18, 2.18, 2.18, 7.87, 7.87, 7.87, ...
## $ chas    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ nox     <dbl> 0.538, 0.469, 0.469, 0.458, 0.458, 0.458, 0.524, 0.524...
## $ rm      <dbl> 6.575, 6.421, 7.185, 6.998, 7.147, 6.430, 6.012, 6.172...
## $ age     <dbl> 65.2, 78.9, 61.1, 45.8, 54.2, 58.7, 66.6, 96.1, 100.0,...
## $ dis     <dbl> 4.0900, 4.9671, 4.9671, 6.0622, 6.0622, 6.0622, 5.5605...
## $ rad     <int> 1, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, ...
## $ tax     <dbl> 296, 242, 242, 222, 222, 222, 311, 311, 311, 311, 311,...
## $ ptratio <dbl> 15.3, 17.8, 17.8, 18.7, 18.7, 18.7, 15.2, 15.2, 15.2, ...
## $ black   <dbl> 396.90, 396.90, 392.83, 394.63, 396.90, 394.12, 395.60...
## $ lstat   <dbl> 4.98, 9.14, 4.03, 2.94, 5.33, 5.21, 12.43, 19.15, 29.9...
## $ medv    <dbl> 24.0, 21.6, 34.7, 33.4, 36.2, 28.7, 22.9, 27.1, 16.5, ...
## $ crim01  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, ...
```


```r
pairs(Nboston)
```

![](716Homework4_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

####This graph is to help me see some relationship between the variables.


```r
set.seed(13)
sample <- sample.split(Nboston$crim, SplitRatio = 0.5)
train <- subset(Nboston, sample==TRUE)
test <- subset(Nboston, sample==FALSE)
crim01.test <- crim01[sample]
```

####I decided to split my dataset into a training set and test set with 50% of the data assigned to each set because my dataset is not too large. I decided to pursue a training set and test set because I would like to see how well the models I create will do in the training test and apply in the test set. If I don't create a training set and test set, then I will not be able to tell if my model overfitted my data or if my models are significant. My only concern is that my training set will only contain half of the data set, which is derived from an already small dataset. This might have a major impact on the QDA.

#LDA


```r
ldafit <- lda(crim01 ~.-crim01-crim, data=train)
ldapred <- predict(ldafit, data=test)
table(ldapred$class, crim01.test)
```

```
##    crim01.test
##       0   1
##   0 132  30
##   1   3  88
```

```r
mean(ldapred$class != crim01.test)
```

```
## [1] 0.1304348
```

####I fitted my LDA by doing the analysis with my dependent variable being the crime variable, where 0 signifies that crime is less than the median of the crime in the original dataset and 1 representing crime being more than the median of the crime in the original dataset. My independent variables exclude my dependent variable and the orignal crime data because it wil cause my error rate of the LDA to rise. The test error of my model is 13.04%. This is coming from doing the model in the training set and fitting the model in the test set. What my output is saying is that it correctly identified 132 areas where the crime rate is less than the median crime rate and 88 areas where the crime rate is more than the median crime rate. However, 30 areas have been incorrectly assigned as having a lower median crime rate, when in actuality, it has a higher than median crime rate,  which is a huge error.


#QDA

```r
qdafit <- qda(crim01 ~.-crim01-crim, data=train)
qdapred <- predict(qdafit, data=test)
table(qdapred$class, crim01.test)
```

```
##    crim01.test
##       0   1
##   0 133  25
##   1   2  93
```

```r
mean(qdapred$class != crim01.test)
```

```
## [1] 0.1067194
```

####QDA provides us with the lowest rate at 10.67%. This is a great rate and it fits the model well. We can see that it correctly predicted crime rates that are below the median crime rate extremely well. It also got 93 rates that are above the median correctly. This performed better than the LDA and will most likely perform better than the Logistic Regression.

#Logistic Regression


```r
glmfit <- glm(crim01 ~.-crim-crim01, family=binomial, data=train)
summary(glmfit)
```

```
## 
## Call:
## glm(formula = crim01 ~ . - crim - crim01, family = binomial, 
##     data = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7154  -0.1760  -0.0016   0.0006   3.4469  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -21.469044  11.219693  -1.914  0.05568 .  
## zn           -0.077274   0.049011  -1.577  0.11487    
## indus        -0.074489   0.066503  -1.120  0.26268    
## chas          1.640620   1.167769   1.405  0.16005    
## nox          54.005834  11.765524   4.590 4.43e-06 ***
## rm           -0.319612   1.085694  -0.294  0.76846    
## age           0.023162   0.019152   1.209  0.22652    
## dis           0.839807   0.328116   2.559  0.01048 *  
## rad           0.739692   0.219442   3.371  0.00075 ***
## tax          -0.009087   0.004025  -2.258  0.02396 *  
## ptratio       0.310820   0.207311   1.499  0.13380    
## black        -0.049329   0.023310  -2.116  0.03433 *  
## lstat         0.022378   0.086807   0.258  0.79657    
## medv          0.124167   0.096709   1.284  0.19917    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 349.589  on 252  degrees of freedom
## Residual deviance:  97.211  on 239  degrees of freedom
## AIC: 125.21
## 
## Number of Fisher Scoring iterations: 9
```

####Seems that the most significant predictors are nox, dis, rad, tax, and black.


```r
glmprob <- predict(glmfit, test, type="response")
glm.pred <- rep(0,253)
glm.pred[glmprob>0.5]<-1
table(glm.pred, crim01.test)
```

```
##         crim01.test
## glm.pred   0   1
##        0  98  15
##        1  37 103
```

```r
mean(glm.pred != crim01.test)
```

```
## [1] 0.2055336
```

####I get an error rate of 20.55%. For the logistic regression, I am correctly identifying 98 areas where crime is less than the median and 103 areas where crime is more than the median. However, my errors are far higher than when I used the LDA model. Because of this error rate, the LDA is a better choice for this dataset. 

#KNN


```r
train.x <- cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[sample, ]
test.x <- cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[!sample, ]
train.crim01 <- crim01[sample]
set.seed(14)
predknn <- knn(train.x,test.x,train.crim01, k=1)
table(predknn, crim01.test)
```

```
##        crim01.test
## predknn   0   1
##       0  96  18
##       1  39 100
```

```r
mean(predknn != crim01.test)
```

```
## [1] 0.2252964
```

####With k=1, we have a 22.53% error. This is a high error and increasing k will most likely reduce the error.


```r
set.seed(14)
predknn <- knn(train.x,test.x,train.crim01, k=10)
table(predknn, crim01.test)
```

```
##        crim01.test
## predknn  0  1
##       0 99 23
##       1 36 95
```

```r
mean(predknn != crim01.test)
```

```
## [1] 0.2332016
```

####Seems as though increasing k does not lower the error rate. The KNN method performed worst among all the other methods. This could be because of the split I did on my dataset, which was already small in the first place. KNN will probably have done better if I had a larger dataset, or if I had done a smaller ratio split between train and test set.
