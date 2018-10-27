General Linear Models Homework 1
================
Fahim Hussain

My Plots
--------

``` r
plot(jitter(y,0.1) ~ jitter(age), kyphosis,
     xlab = "Age",ylab = "kyphosis")
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-2-1.png)

**Seems like most kyphosis fall in the middle of the ages.**

``` r
plot(jitter(y,0.1) ~ jitter(number), kyphosis,
     xlab = "Number",ylab = "kyphosis")
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-3-1.png)

**We have a small outlier, but most fall at around 5.**

``` r
plot(jitter(y,0.1) ~ jitter(start), kyphosis,
     xlab = "Start",ylab = "kyphosis")
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-4-1.png)

**This is a little bit more spread out compared to number but we have some favor in the center. **

Residuals
---------

``` r
lmod <- glm(y ~ age+number+start,family = binomial, kyphosis)
summary(lmod)
```

    ## 
    ## Call:
    ## glm(formula = y ~ age + number + start, family = binomial, data = kyphosis)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9221  -0.6438  -0.4944  -0.2920   2.0815  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.418567   1.163424  -3.798 0.000146 ***
    ## age          0.007089   0.005493   1.290 0.196914    
    ## number       0.564492   0.198487   2.844 0.004455 ** 
    ## start              NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 83.234  on 80  degrees of freedom
    ## Residual deviance: 71.627  on 78  degrees of freedom
    ## AIC: 77.627
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
linpred <- predict(lmod)
predprob <- predict(lmod, type="response")
head(linpred)
```

    ##         1         2         3         4         5         6 
    ## -2.221793 -1.605076 -1.253245 -1.581929 -2.153509 -3.282494

``` r
head(predprob)
```

    ##          1          2          3          4          5          6 
    ## 0.09781050 0.16727338 0.22213897 0.17052252 0.10400373 0.03617666

``` r
rawres <- y - predprob
plot(rawres ~ linpred, xlab = "linear predictor", ylab = "residuals")
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-5-1.png)

**Seems like age is not significant because the p value is greater than 0.05 butnumber is significant because the p value is less than 0.05.Since my null deviance and my residual deviance is really high, my data is a bad fit. **

Binned Residuals
----------------

``` r
kyphosis2 <- mutate(kyphosis, residuals=residuals(lmod),
                    linpred = predict(lmod))
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.3

``` r
gdf <- group_by(kyphosis2, cut(linpred, breaks=unique(quantile(
  linpred, (1:100)/101))))
diagdf <- summarise(gdf, residuals = mean(residuals), linpred=mean(
  linpred))  
plot(residuals ~ linpred,diagdf, xlab = "linear predictor")
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-7-1.png)

**My binned residual plot seems to have a point in the middle, showing some sort of negative trend towards the center of the grand. **

``` r
gdf <- group_by(kyphosis2, Age)
diagdf <- summarize(gdf, residuals=mean(residuals))
ggplot(diagdf, aes(x=Age, y=residuals))+geom_point()
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-8-1.png)

**This scatter plot with Age vs residuals show a direct line in the center, showing a low correlation between the two. **

Residual with Start Predictor
-----------------------------

``` r
gdf2 <- group_by(kyphosis2, Start)
diagdf2 <- summarize(gdf2, residuals=mean(residuals))
ggplot(diagdf2, aes(x=Start, y=residuals))+geom_point()
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-9-1.png)

QQ Plot
-------

``` r
qqnorm(residuals(lmod))
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-10-1.png)

**The QQ plot shows that our data is not normally distributed, and has a high outlier among the group**

    ## Warning: package 'faraway' was built under R version 3.4.3

``` r
halfnorm(hatvalues(lmod))
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-12-1.png)

**THe half normal plot shows a similar graph to the QQ plot and shows a high deviance**

Goodness of Fit
---------------

``` r
kyphosism <- na.omit(kyphosis)
kyphosism <- mutate(kyphosism, predprob=predict(
  lmod, type="response"))
gdf <- group_by(kyphosism, cut(linpred, breaks=unique(
  quantile(linpred, (1:100)/101))))
hldf <- summarize(gdf, y=sum(y), ppred=mean(predprob), count=n())
```

``` r
hldf <- mutate(hldf, se.fit=sqrt(ppred*(1-ppred)/count))
ggplot(hldf, aes(x=ppred, y=y/count, ymin=y/count-2*se.fit,
                 ymax=y/count+2*se.fit))+geom_point()+
  geom_linerange(color=gray(0.75))+geom_abline(intercept=0,
                                               slope=1)+
  xlab("Predicted Probability") + ylab("Observed Proportion")
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-14-1.png)

**My fitted data does not fit the data well because of the constant values and the small outlier, which gives the line trend**

Hosmer-Lemeshow and p-value
---------------------------

``` r
hlstat <- with(hldf, sum( (y-count*ppred)^2/(count*ppred*(1-ppred))))
c(hlstat,nrow(hldf))
```

    ## [1] 71.91462 78.00000

``` r
1-pchisq(63.212, 56-1)
```

    ## [1] 0.2089823

**Since the p-value is moderate, we detect no lack of fit**

Predicted and Actual Outcomes
-----------------------------

``` r
thresh <- seq(0.01,0.5,0.01)
Sensitivity <- numeric(length(thresh))
Specificity <- numeric(length(thresh))
for(j in seq(along=thresh)){
  pp <- ifelse(kyphosism$predprob < thresh[j],"no","yes")
  xx <- xtabs( ~ age + number, kyphosism)
  Specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
  Sensitivity[j] <- xx[2,2]/(xx[2,1]+xx[2,2])
}
matplot(thresh,cbind(Sensitivity, Specificity), type="l",
         xlab="Threshold", ylab="Proportion",lty=1:2)
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
plot(1-Specificity, Sensitivity, type="l")
abline(0,1,lty=2)
```

![](GLMhomework1_files/figure-markdown_github/unnamed-chunk-16-2.png)

**My graphs did not come out as expected, and I was unable to calculate the probability that the model would prect a present outcome.**
