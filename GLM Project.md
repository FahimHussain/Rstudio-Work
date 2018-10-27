GLM Project
================
Fahim Hussain
May 19, 2018

``` r
Workinghours$nonwhite <- as.factor(Workinghours$nonwhite)
Workinghours$owned<-as.factor(Workinghours$owned)
Workinghours$mortgage<-as.factor(Workinghours$mortgage)
summary(Workinghours)
```

    ##      hours          income            age          education    
    ##  Min.   :   0   Min.   :-139.0   Min.   :18.00   Min.   : 0.00  
    ##  1st Qu.:   0   1st Qu.: 146.0   1st Qu.:28.00   1st Qu.:12.00  
    ##  Median :1304   Median : 247.0   Median :34.00   Median :12.00  
    ##  Mean   :1135   Mean   : 296.9   Mean   :36.81   Mean   :12.55  
    ##  3rd Qu.:1944   3rd Qu.: 368.8   3rd Qu.:44.00   3rd Qu.:14.00  
    ##  Max.   :5840   Max.   :7220.0   Max.   :64.00   Max.   :17.00  
    ##      child5          child13          child17      nonwhite owned   
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.000   0:2382   0:1079  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000   1:1000   1:2303  
    ##  Median :0.0000   Median :0.0000   Median :0.000                    
    ##  Mean   :0.5074   Mean   :0.5618   Mean   :0.215                    
    ##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.000                    
    ##  Max.   :4.0000   Max.   :5.0000   Max.   :6.000                    
    ##  mortgage occupation       unemp       
    ##  0:1597   other:1314   Min.   : 1.000  
    ##  1:1785   mp   : 962   1st Qu.: 4.000  
    ##           swcc :1021   Median : 5.000  
    ##           fr   :  85   Mean   : 5.641  
    ##                        3rd Qu.: 7.000  
    ##                        Max.   :30.000

**I changed the variables nonwhite, owned, and mortgage into factors because they are binary variables. 0 represents "no" and 1 represents "yes". This will help me fit the data better and not have an effect on the linear model, because 0 is not necessarily worst than 1. They just have two different meanings in terms of binary values. From the summary table, we can see that the mean number of hours worked by wives is less than the median amount, indicating a possible left skewed distribution. For income, the mean and median are close in value. Also, income is represented in hundreds of dollars. Meaning, that a value of 300 is actually $30,000. Since I changed my integer values into factors, I was able to receive information on how many wives were in certain categories. For example, I know that 1000 wives are nonwhite and 2382 are white. This shows that more than 2/3 of the data contain white women.**

``` r
ggplot(data=Workinghours,aes(x=hours)) +
  geom_histogram(breaks=seq(1, 4001, by=100),col="black",fill="blue") +
  labs(x="Working Hours",y="Amount of Wives")+ggtitle("Histogram of Working Hours of Wives")+theme(plot.title = element_text(hjust = 0.5))
```

![](project_files/figure-markdown_github/unnamed-chunk-3-1.png)

**I wanted to show the distribution of working hours for wives, which is best represented by a histogram. As we can see, most of the wives work around 2000 hours. This is represented in years, which is about 38 hours per week.**

``` r
amod <- gam(hours~s(income)+s(age)+s(education)+child5+child13+child17+nonwhite+
              owned+mortgage+occupation+s(unemp),data=Workinghours)
summary(amod)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## hours ~ s(income) + s(age) + s(education) + child5 + child13 + 
    ##     child17 + nonwhite + owned + mortgage + occupation + s(unemp)
    ## 
    ## Parametric coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1258.806     39.298  32.032  < 2e-16 ***
    ## child5         -375.590     20.387 -18.423  < 2e-16 ***
    ## child13        -158.138     18.166  -8.705  < 2e-16 ***
    ## child17          -4.314     29.738  -0.145 0.884662    
    ## nonwhite1       115.321     32.552   3.543 0.000402 ***
    ## owned1           19.830     48.703   0.407 0.683914    
    ## mortgage1       186.943     44.030   4.246 2.24e-05 ***
    ## occupationmp      3.627     40.109   0.090 0.927944    
    ## occupationswcc   50.784     34.704   1.463 0.143467    
    ## occupationfr   -223.572     90.080  -2.482 0.013116 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                edf Ref.df     F  p-value    
    ## s(income)    3.147  3.940 24.47  < 2e-16 ***
    ## s(age)       3.729  4.673 36.77  < 2e-16 ***
    ## s(education) 6.767  7.642 14.78  < 2e-16 ***
    ## s(unemp)     5.948  6.874  9.71 8.94e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.209   Deviance explained = 21.6%
    ## GCV = 6.3585e+05  Scale est. = 6.3028e+05  n = 3382

**I chose to pursue an additive model because a lot of my data points are clustered in certain regions, making a linear model approach difficult. I decided to spline my income variable, age, education, and unemp because they are high numeric variables that can be smoothed out.We can see that their estimated degrees of freedom are 3.1,3.7,6.7, and 5.9 respectively. My R-squared is low, which makes sense given that most of my data is clustered. We can see that child17, owned, occupationmp, and occupationswcc are insignificant to the model, as shown by the high p values. From the summary statistics, we see that having children under 5 have a huge negative effect on hours worked, which can be attributed with the fact that babies are considered to take a lot of time to take care of. child13 also have a large effect on hours worked, given that this variable is in the age range of 6-13, this makes sense because the kids are still not independent and need heavy care. child17 does not have nearly as much of an effect on hours worked compared to the other child variables, and it is nearly 0. This makes sense because children around 14-17 are a bit more independent and can take care of themselves.We can see that being nonwhite increases the number of hours worked, which can be because white family tend to make more money and don't need the wives to work as well. We can also see that owning a house and having a mortgage increases the number of hours worked for wives, which is probably because wives need to make money to help pay for the house. **

``` r
train <- Workinghours[1:2700,]
tamod <- gam(hours~s(income)+s(age)+s(education)+child5+child13+child17+nonwhite+
              owned+mortgage+occupation+s(unemp),data=train)
summary(tamod)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## hours ~ s(income) + s(age) + s(education) + child5 + child13 + 
    ##     child17 + nonwhite + owned + mortgage + occupation + s(unemp)
    ## 
    ## Parametric coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1252.948     45.146  27.753  < 2e-16 ***
    ## child5         -389.706     23.524 -16.566  < 2e-16 ***
    ## child13        -170.439     20.553  -8.293  < 2e-16 ***
    ## child17         -12.076     33.720  -0.358   0.7203    
    ## nonwhite1        79.730     40.253   1.981   0.0477 *  
    ## owned1            8.278     55.735   0.149   0.8819    
    ## mortgage1       220.251     49.273   4.470 8.15e-06 ***
    ## occupationmp     12.309     44.055   0.279   0.7800    
    ## occupationswcc   84.000     39.644   2.119   0.0342 *  
    ## occupationfr   -246.934     96.961  -2.547   0.0109 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                edf Ref.df      F  p-value    
    ## s(income)    3.321  4.142 20.993  < 2e-16 ***
    ## s(age)       3.149  3.977 35.235  < 2e-16 ***
    ## s(education) 1.040  1.079 61.233 2.76e-15 ***
    ## s(unemp)     5.040  6.008  9.586 1.84e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.207   Deviance explained = 21.3%
    ## GCV = 6.3906e+05  Scale est. = 6.3372e+05  n = 2700

**We can see that the model for my training set is not much different from my model of the original data. The biggest difference is the s(education) estimated degree of freedom severely dropped compared to the original model.**

``` r
fit1 <- gam(hours~age, data=train)
x1 <- summary(fit1)
ggplot(data=train,aes(x=age,y=hours)) +
  geom_point(color = "blue", size = 0.1) +
  xlim(18, 62) +
  ylim(0, 3000) +
  geom_smooth(method="gam", size=0.3) +
  labs(title="Age vs. Hours",x="age",y="hours")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_label(aes(x = 18, y = 2800), hjust = 0, 
             label = paste("Adj R2 = ",signif(x1$r.sq, 5),
                           "\nIntercept =",signif(x1$p.coeff[[1]],5 ),
                           " \nSlope =",signif(x1$p.coeff[[2]],5)))
```

    ## Warning: Removed 70 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 70 rows containing missing values (geom_point).

![](project_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
fit2 <- gam(hours~age, data=Workinghours)
x2 <- summary(fit2)
ggplot(data=train,aes(x=age,y=hours)) +
  geom_point(color = "red", size = 0.1) +
  xlim(18, 62) +
  ylim(0, 3000) +
  geom_smooth(method="gam", size=0.3) +
  labs(title="Age vs. Hours",x="age",y="hours")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_label(aes(x = 18, y = 2800), hjust = 0, 
             label = paste("Adj R2 = ",signif(x2$r.sq, 5),
                           "\nIntercept =",signif(x2$p.coeff[[1]],5 ),
                           " \nSlope =",signif(x2$p.coeff[[2]],5)))
```

    ## Warning: Removed 70 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 70 rows containing missing values (geom_point).

![](project_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
fit3 <- gam(hours~income, data=train)
x3<-summary(fit3)
ggplot(data=train,aes(x=income,y=hours)) +
  geom_point(color = "blue", size = 0.1) +
  xlim(0, 2000) +
  ylim(0, 3000) +
  geom_smooth(method="gam", size=0.3) +
  labs(title="Working Hours & Income",x="Income",y="Hours")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_label(aes(x = 0, y = 2800), hjust = 0, 
             label = paste("Adj R2 = ",signif(x3$r.sq, 5),
                           "\nIntercept =",signif(x3$p.coeff[[1]],5 ),
                           " \nSlope =",signif(x3$p.coeff[[2]],5)))
```

    ## Warning: Removed 32 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 32 rows containing missing values (geom_point).

![](project_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
fit4 <- gam(hours~income, data=Workinghours)
x4 <- summary(fit4)
ggplot(data=Workinghours,aes(x=income,y=hours)) +
  geom_point(color = "red", size = 0.1) +
  xlim(0, 2000) +
  ylim(0, 3000) +
  geom_smooth(method="gam", size=0.3) +
  labs(title="Working Hours & Income",x="Income",y="Hours")+
theme(plot.title = element_text(hjust = 0.5))+
  geom_label(aes(x = 0, y = 2800), hjust = 0, 
             label = paste("Adj R2 = ",signif(x4$r.sq, 5),
                           "\nIntercept =",signif(x4$p.coeff[[1]],5 ),
                           " \nSlope =",signif(x4$p.coeff[[2]],5)))
```

    ## Warning: Removed 35 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 35 rows containing missing values (geom_point).

![](project_files/figure-markdown_github/unnamed-chunk-9-1.png)
