---
title: "716 Homework 3"
author: "Fahim Hussain"
date: "September 20, 2018"
output:
  html_document:
    keep_md: true
---


###Question 13

#####a)Using the rnorm() function, create a vector, x, containing 100 observations drawn from a N(0, 1) distribution. This represents a feature, X.


```r
set.seed(1)
x <- rnorm(100)
x
```

```
##   [1] -0.626453811  0.183643324 -0.835628612  1.595280802  0.329507772
##   [6] -0.820468384  0.487429052  0.738324705  0.575781352 -0.305388387
##  [11]  1.511781168  0.389843236 -0.621240581 -2.214699887  1.124930918
##  [16] -0.044933609 -0.016190263  0.943836211  0.821221195  0.593901321
##  [21]  0.918977372  0.782136301  0.074564983 -1.989351696  0.619825748
##  [26] -0.056128740 -0.155795507 -1.470752384 -0.478150055  0.417941560
##  [31]  1.358679552 -0.102787727  0.387671612 -0.053805041 -1.377059557
##  [36] -0.414994563 -0.394289954 -0.059313397  1.100025372  0.763175748
##  [41] -0.164523596 -0.253361680  0.696963375  0.556663199 -0.688755695
##  [46] -0.707495157  0.364581962  0.768532925 -0.112346212  0.881107726
##  [51]  0.398105880 -0.612026393  0.341119691 -1.129363096  1.433023702
##  [56]  1.980399899 -0.367221476 -1.044134626  0.569719627 -0.135054604
##  [61]  2.401617761 -0.039240003  0.689739362  0.028002159 -0.743273209
##  [66]  0.188792300 -1.804958629  1.465554862  0.153253338  2.172611670
##  [71]  0.475509529 -0.709946431  0.610726353 -0.934097632 -1.253633400
##  [76]  0.291446236 -0.443291873  0.001105352  0.074341324 -0.589520946
##  [81] -0.568668733 -0.135178615  1.178086997 -1.523566800  0.593946188
##  [86]  0.332950371  1.063099837 -0.304183924  0.370018810  0.267098791
##  [91] -0.542520031  1.207867806  1.160402616  0.700213650  1.586833455
##  [96]  0.558486426 -1.276592208 -0.573265414 -1.224612615 -0.473400636
```

#####b) Using the rnorm() function, create a vector, eps, containing 100 observations drawn from a N(0, 0.25) distribution i.e. a normal distribution with mean zero and variance 0.25.


```r
eps <- rnorm(100, sd=0.25)
eps
```

```
##   [1] -0.155091669  0.010528968 -0.227730412  0.039507193 -0.163646161
##   [6]  0.441821817  0.179176869  0.227543557  0.096046339  0.420544020
##  [11] -0.158934113 -0.115411183  0.358070560 -0.162674088 -0.051845186
##  [16] -0.098201982 -0.079998217 -0.069778326  0.123547083 -0.044332621
##  [21] -0.126489366  0.335759706 -0.053644852 -0.044889133 -0.025047685
##  [26]  0.178166577 -0.018391101 -0.009408543 -0.170415120 -0.081067568
##  [31]  0.015040110 -0.147223622  0.132874048 -0.379598520  0.076639465
##  [36] -0.384112456 -0.075244032 -0.132069976 -0.163023695 -0.014224194
##  [41] -0.478589856  0.294145828 -0.416243109 -0.115882600 -0.278980026
##  [46] -0.187704750  0.521791636  0.004348905 -0.321575133 -0.410151384
##  [51]  0.112546775 -0.004639958 -0.079517094 -0.232340537 -0.371865078
##  [56] -0.268798074  0.250007201 -0.155316674 -0.346106712  0.467322656
##  [61]  0.106275094 -0.059661775  0.264620762  0.221605663 -0.154810762
##  [66]  0.551525616 -0.063756758 -0.356123663 -0.036099900  0.051884585
##  [71]  0.576994600  0.026450592  0.114249701 -0.019288234 -0.083500211
##  [76] -0.008681507  0.196909901  0.518811252  0.256848110  0.301977100
##  [81] -0.307830855  0.245973893  0.054981201 -0.366812507  0.130255686
##  [86] -0.039688651  0.366146828 -0.191520500 -0.107552938 -0.231527374
##  [91] -0.044275990  0.100502945 -0.182937043  0.207593292 -0.302020697
##  [96] -0.261996103  0.360289427 -0.253961866  0.102993678 -0.095269013
```

#####c) ) Using x and eps, generate a vector y according to the model Y = -1 +0.5X + \(\epsilon\). What is the length of the vector y? What are the values of 	$\beta_{0}$ and $\beta_{1}$ in this linear model?


```r
y <- -1 + 0.5*x + eps
length(y)
```

```
## [1] 100
```

#####The length of y is 100.	$\beta_{0}$ is -1 and 	$\beta_{1}$ is 0.5

#####Create a scatterplot displaying the relationship between x and y. Comment on what you observe.


```r
reg <- data.frame(x,y)
ggplot(reg, aes(x,y))+geom_point()
```

![](STAT716Homework3_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#####We can see that the relationship between x and y seems to be linear in an upward trend. There seems to be some scatter and deviation from the true line, which could be due to the error term.

#####e)Fit a least squares linear model to predict y using x. Comment on the model obtained. How do  $\widehat{\beta_{0}}$ and $\widehat{\beta_{1}}$ compare to $\beta_{0}$ and $\beta_{1}$?


```r
lm_fit <- lm(y~x)
summary(lm_fit)
```

```
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.46921 -0.15344 -0.03487  0.13485  0.58654 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.00942    0.02425  -41.63   <2e-16 ***
## x            0.49973    0.02693   18.56   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2407 on 98 degrees of freedom
## Multiple R-squared:  0.7784,	Adjusted R-squared:  0.7762 
## F-statistic: 344.3 on 1 and 98 DF,  p-value: < 2.2e-16
```

##### The values of  $\widehat{\beta_{0}}$ and $\widehat{\beta_{1}}$ are similar to the values of $\beta_{0}$ and $\beta_{1}$. I get -1.00942 and 0.49973 for each betas, respectively. This is extremely close to the -1 and 0.5 from the betas of the original equation. The p values are also close to 0, which means that I can reject the null and conclude that my betas are not 0. The F-statistic is also a high value, which shows that I have significant coefficients.

#####f)Display the least squares line on the scatterplot obtained in (d). Draw the population regression line on the plot, in a different color. Use the legend() command to create an appropriate legend.


```r
plot(x,y, pch=21, bg="black")
abline(lm_fit, col="red")
abline(-1,0.5, col="blue")
legend("bottomright", c("Least Squares Line", "Pop Regression Line"), col=c("red", "blue"), lty = c(1,1))
```

![](STAT716Homework3_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#####g) Now fit a polynomial regression model that predicts y using x and x2. Is there evidence that the quadratic term improves the model fit? Explain your answer.


```r
lm_fit2 <- lm(y~x+I(x^2))
summary(lm_fit2)
```

```
## 
## Call:
## lm(formula = y ~ x + I(x^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.4913 -0.1563 -0.0322  0.1451  0.5675 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.98582    0.02941 -33.516   <2e-16 ***
## x            0.50429    0.02700  18.680   <2e-16 ***
## I(x^2)      -0.02973    0.02119  -1.403    0.164    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2395 on 97 degrees of freedom
## Multiple R-squared:  0.7828,	Adjusted R-squared:  0.7784 
## F-statistic: 174.8 on 2 and 97 DF,  p-value: < 2.2e-16
```

#####The p value suggest that the quadratic term is not useful. The R-squared value increased but only by a small amount and the F-statistic value dropped. This shows that the extra quadratic term does not improve the model fit.

#####h)Repeat (a)-(f) after modifying the data generation process in such a way that there is less noise in the data. The initial model should remain the same. Describe your results.

#####In order to reduce the noise, I will lower the value of the variance of the error term.


```r
set.seed(2)
x<-rnorm(100)
eps <- rnorm(100,sd=0.1)
y<- -1 + 0.5*x + eps
reg <- data.frame(x,y)
ggplot(reg, aes(x,y))+geom_point()
```

![](STAT716Homework3_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
lm_fit3 <- lm(y~x)
summary(lm_fit3)
```

```
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.21496 -0.08196  0.01370  0.07195  0.20560 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.997223   0.009892 -100.81   <2e-16 ***
## x            0.495290   0.008566   57.82   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.09888 on 98 degrees of freedom
## Multiple R-squared:  0.9715,	Adjusted R-squared:  0.9712 
## F-statistic:  3343 on 1 and 98 DF,  p-value: < 2.2e-16
```

##### Because there is not much variance in the error term, the dispersion is minimized. This shows us a linear model that is almost perfectly correlated. The betas for the model is almost identical to the population model, with an incredibly high R-squared and F-statistic value.

```r
plot(x,y, pch=21, bg="black")
abline(lm_fit3, col="red")
abline(-1,0.5, col="blue")
legend("bottomright", c("Least Squares Line", "Pop Regression Line"), col=c("red", "blue"), lty = c(1,1))
```

![](STAT716Homework3_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

#####We see that the lines almost sit on top of each other, as expected.

#####i) Repeat (a)-(f) after modifying the data generation process in such a way that there is more noise in the data. The model (3.39) should remain the same. You can do this by increasing the variance of the normal distribution used to generate the error term  in (b). Describe your results

#####Instead of reducing the variance, I will increase the volume to increase the noise in the data.


```r
set.seed(3)
x<-rnorm(100)
eps <- rnorm(100,sd=0.6)
y<- -1 + 0.5*x + eps
reg <- data.frame(x,y)
ggplot(reg, aes(x,y))+geom_point()
```

![](STAT716Homework3_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
lm_fit4 <- lm(y~x)
summary(lm_fit3)
```

```
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.21496 -0.08196  0.01370  0.07195  0.20560 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.997223   0.009892 -100.81   <2e-16 ***
## x            0.495290   0.008566   57.82   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.09888 on 98 degrees of freedom
## Multiple R-squared:  0.9715,	Adjusted R-squared:  0.9712 
## F-statistic:  3343 on 1 and 98 DF,  p-value: < 2.2e-16
```


#####We see that the points are more dispersed out than the first plot I created. The R-squared has reduced and the residual standard error has increased. Our beta coefficients are this close to the true value, although a little more off than the other models.


```r
plot(x,y, pch=21, bg="black")
abline(lm_fit4, col="red")
abline(-1,0.5, col="blue")
legend("bottomright", c("Least Squares Line", "Pop Regression Line"), col=c("red", "blue"), lty = c(1,1))
```

![](STAT716Homework3_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

#####We can see a clear difference in this graph than from the other graphs. The fit has a noticeable smaller slope than the actual regression line but it has a higher intercept.


#####j)What are the confidence intervals for $\beta_{0}$ and $\beta_{1}$ based on the original data set, the noisier data set, and the less noisy data set? Comment on your results.


```r
confint(lm_fit)
```

```
##                  2.5 %     97.5 %
## (Intercept) -1.0575402 -0.9613061
## x            0.4462897  0.5531801
```

```r
confint(lm_fit3)
```

```
##                  2.5 %     97.5 %
## (Intercept) -1.0168532 -0.9775931
## x            0.4782907  0.5122886
```

```r
confint(lm_fit4)
```

```
##                  2.5 %     97.5 %
## (Intercept) -1.1192003 -0.8568834
## x            0.2919331  0.5998684
```

#####It seems as though the confidence intervals' width increases as the noise increases. However, all the intervals seem to center around the true slope and true intercept.
