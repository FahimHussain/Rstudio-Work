GLM Homework 4
================
Fahim Hussain
April 21, 2018

a)Plot of Data
--------------

``` r
modelth <- lm(longevity~thorax,fruitfly)
modelth
```

    ## 
    ## Call:
    ## lm(formula = longevity ~ thorax, data = fruitfly)
    ## 
    ## Coefficients:
    ## (Intercept)       thorax  
    ##      -61.86       145.28

``` r
ggplot(fruitfly,aes(y=longevity,x=thorax))+geom_point()+geom_line(aes(y=fitted(modelth)))
```

![](glmhomework4_files/figure-markdown_github/unnamed-chunk-2-1.png)

**We can see that as the length of the thorax increases, the lifetime of fruitflies also increases.**

``` r
lm(longevity~activity,fruitfly)
```

    ## 
    ## Call:
    ## lm(formula = longevity ~ activity, data = fruitfly)
    ## 
    ## Coefficients:
    ##  (Intercept)   activityone   activitylow  activitymany  activityhigh  
    ##      63.5600        1.2400       -6.8000        0.9817      -24.8400

``` r
ggplot(fruitfly,aes(y=longevity,x=activity))+geom_point()
```

![](glmhomework4_files/figure-markdown_github/unnamed-chunk-3-1.png)

**We can see that the fruitflies with an isolated or one activity have longest likelihood of living longer. Those with a high activity(kept with 8 virgin fruitflies) have the lowest longevity.Therefore, it is better to have fruitflies isolated or kept with a few female fruitfly.**

b) Standard linear model
------------------------

``` r
lm1 <- lm(longevity~thorax+activity+thorax*activity,fruitfly)
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = longevity ~ thorax + activity + thorax * activity, 
    ##     data = fruitfly)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -25.9509  -6.7296  -0.9103   6.1854  30.3071 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -50.2420    21.8012  -2.305    0.023 *  
    ## thorax              136.1268    25.9517   5.245 7.27e-07 ***
    ## activityone           6.5172    33.8708   0.192    0.848    
    ## activitylow          -7.7501    33.9690  -0.228    0.820    
    ## activitymany         -1.1394    32.5298  -0.035    0.972    
    ## activityhigh        -11.0380    31.2866  -0.353    0.725    
    ## thorax:activityone   -4.6771    40.6518  -0.115    0.909    
    ## thorax:activitylow    0.8743    40.4253   0.022    0.983    
    ## thorax:activitymany   6.5478    39.3600   0.166    0.868    
    ## thorax:activityhigh -11.1268    38.1200  -0.292    0.771    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.71 on 114 degrees of freedom
    ## Multiple R-squared:  0.6534, Adjusted R-squared:  0.626 
    ## F-statistic: 23.88 on 9 and 114 DF,  p-value: < 2.2e-16

``` r
step(lm1)
```

    ## Start:  AIC=597.69
    ## longevity ~ thorax + activity + thorax * activity
    ## 
    ##                   Df Sum of Sq   RSS    AIC
    ## - thorax:activity  4    24.314 13107 589.92
    ## <none>                         13083 597.69
    ## 
    ## Step:  AIC=589.92
    ## longevity ~ thorax + activity
    ## 
    ##            Df Sum of Sq   RSS    AIC
    ## <none>                  13107 589.92
    ## - activity  4    9634.6 22742 650.25
    ## - thorax    1   12368.4 25476 670.32

    ## 
    ## Call:
    ## lm(formula = longevity ~ thorax + activity, data = fruitfly)
    ## 
    ## Coefficients:
    ##  (Intercept)        thorax   activityone   activitylow  activitymany  
    ##      -48.749       134.341         2.637        -7.015         4.139  
    ## activityhigh  
    ##      -20.004

**We see from the linear model and the step function that the interaction term does not help the model, so we remove it and only consider the thorax and activity terms.**

c) Residuals and Fitted
-----------------------

``` r
lm2 <- step(lm1)
```

    ## Start:  AIC=597.69
    ## longevity ~ thorax + activity + thorax * activity
    ## 
    ##                   Df Sum of Sq   RSS    AIC
    ## - thorax:activity  4    24.314 13107 589.92
    ## <none>                         13083 597.69
    ## 
    ## Step:  AIC=589.92
    ## longevity ~ thorax + activity
    ## 
    ##            Df Sum of Sq   RSS    AIC
    ## <none>                  13107 589.92
    ## - activity  4    9634.6 22742 650.25
    ## - thorax    1   12368.4 25476 670.32

``` r
plot(lm2)
```

![](glmhomework4_files/figure-markdown_github/unnamed-chunk-5-1.png)![](glmhomework4_files/figure-markdown_github/unnamed-chunk-5-2.png)![](glmhomework4_files/figure-markdown_github/unnamed-chunk-5-3.png)![](glmhomework4_files/figure-markdown_github/unnamed-chunk-5-4.png)

**We see from the residuals vs fitted graph that the residuals and the fitted values are uncorrelated, which shows a good graph between the two. We can also see from the QQ plot that the distribution is approximately normal**

d) Transformation of the Response
---------------------------------

``` r
plot(lm(log(longevity)~thorax+activity,fruitfly))
```

![](glmhomework4_files/figure-markdown_github/unnamed-chunk-6-1.png)![](glmhomework4_files/figure-markdown_github/unnamed-chunk-6-2.png)![](glmhomework4_files/figure-markdown_github/unnamed-chunk-6-3.png)![](glmhomework4_files/figure-markdown_github/unnamed-chunk-6-4.png)

**After transforming the response variable with a log transformation, we see from the residuals vs fitted graph that the interaction is nonlinear. Wee see that the residuals is mostly negative when the fitted value is large and small and it is mostly positive when the fitted values is in the middle.**

Gamma GLM
---------

``` r
gamodel <- glm(longevity~thorax+activity, family=Gamma(link = "log"),fruitfly)
plot(gamodel)
```

![](glmhomework4_files/figure-markdown_github/unnamed-chunk-7-1.png)![](glmhomework4_files/figure-markdown_github/unnamed-chunk-7-2.png)![](glmhomework4_files/figure-markdown_github/unnamed-chunk-7-3.png)![](glmhomework4_files/figure-markdown_github/unnamed-chunk-7-4.png)

**We see that the residuals vs fitted graph looks similar to the graph when we transformed the response variable to the log transformation. We see that the residuals are mostly negative when the fitted values are small and large, but positive or close to 0 when the fitted values are in the middle. For all three graphs from the previous models and this model, we see a nice normal QQ plot, showing a nice fit for the normal distribution.**

f) Gamma Density
----------------

``` r
fit <- fitted(gamodel)
avg <- mean(fit[25:49])
range <- seq(0,100,0.01)
gamma.shape(gamodel)
```

    ##                
    ## Alpha: 28.90202
    ## SE:     3.64958

``` r
alpha = 28.90202
beta <- mean(fruitfly[25:49,2])/ alpha
plot(range,dgamma(range,alpha,rate=1/beta),type="l")
```

![](glmhomework4_files/figure-markdown_github/unnamed-chunk-8-1.png)

**We can see from the density for the lifetime of a fluitfly, with a mean thorax length, is most likely to survive around 60 days. This makes sense because from our data, those with around average thorax length tend to survive around 60 days as well.**
