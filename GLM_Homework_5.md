GLM Homework 5
================
Fahim Hussain
May 13, 2018

A) Building GAM Model
---------------------

``` r
gamdoctor <- gam(doctorco ~ sex +s(age)+s(agesq)+s(income)+
                   levyplus+freepoor+freerepa+illness+
                   s(actdays)+s(hscore)+chcond1+chcond2,
                   family=poisson,data = dvisits)
summary(gamdoctor)
```

    ## 
    ## Family: poisson 
    ## Link function: log 
    ## 
    ## Formula:
    ## doctorco ~ sex + s(age) + s(agesq) + s(income) + levyplus + freepoor + 
    ##     freerepa + illness + s(actdays) + s(hscore) + chcond1 + chcond2
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.94633    0.07436 -26.176  < 2e-16 ***
    ## sex          0.14373    0.05646   2.546  0.01090 *  
    ## levyplus     0.11606    0.07209   1.610  0.10740    
    ## freepoor    -0.48507    0.18121  -2.677  0.00743 ** 
    ## freerepa     0.13347    0.09349   1.428  0.15339    
    ## illness      0.13740    0.01919   7.160 8.09e-13 ***
    ## chcond1      0.11568    0.06654   1.738  0.08213 .  
    ## chcond2      0.14622    0.08238   1.775  0.07592 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##              edf Ref.df  Chi.sq p-value    
    ## s(age)     1.000  1.000   0.423 0.51556    
    ## s(agesq)   1.000  1.000   0.042 0.83681    
    ## s(income)  2.105  2.642   8.780 0.02716 *  
    ## s(actdays) 5.594  6.636 816.457 < 2e-16 ***
    ## s(hscore)  2.933  3.632  15.542 0.00402 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.186   Deviance explained = 26.2%
    ## UBRE = -0.19048  Scale est. = 1         n = 5190

**The variables that are not statistically significant are age, agesq, income, levyplus, freerepa, chcond1, and chcond2.**

B) Preferred Model
------------------

``` r
gamdoctor2 <- gam(doctorco ~ s(income)+s(actdays)+s(hscore)+sex+freepoor+illness, family=poisson, data=dvisits)
summary(gamdoctor2)
```

    ## 
    ## Family: poisson 
    ## Link function: log 
    ## 
    ## Formula:
    ## doctorco ~ s(income) + s(actdays) + s(hscore) + sex + freepoor + 
    ##     illness
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.85738    0.05403 -34.376  < 2e-16 ***
    ## sex          0.20940    0.05475   3.824 0.000131 ***
    ## freepoor    -0.68989    0.17384  -3.969 7.23e-05 ***
    ## illness      0.16439    0.01822   9.022  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##              edf Ref.df Chi.sq  p-value    
    ## s(income)  2.367  2.972  24.27 2.08e-05 ***
    ## s(actdays) 5.631  6.674 873.95  < 2e-16 ***
    ## s(hscore)  3.328  4.109  15.60  0.00382 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.189   Deviance explained = 25.7%
    ## UBRE = -0.18744  Scale est. = 1         n = 5190

**This model looks like the preferred model because our p value drops significantly further.**

C) GLM Model
------------

``` r
glmdoctor <- glm(doctorco ~ income+actdays+hscore+sex+freepoor+illness, family=poisson, data=dvisits)
summary(glmdoctor)
```

    ## 
    ## Call:
    ## glm(formula = doctorco ~ income + actdays + hscore + sex + freepoor + 
    ##     illness, family = poisson, data = dvisits)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8674  -0.6757  -0.5805  -0.4889   5.6263  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.856158   0.076572 -24.241  < 2e-16 ***
    ## income      -0.243983   0.078924  -3.091 0.001992 ** 
    ## actdays      0.130270   0.004834  26.946  < 2e-16 ***
    ## hscore       0.030290   0.009898   3.060 0.002213 ** 
    ## sex          0.203423   0.054596   3.726 0.000195 ***
    ## freepoor    -0.586256   0.172844  -3.392 0.000694 ***
    ## illness      0.206199   0.017331  11.898  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 5634.8  on 5189  degrees of freedom
    ## Residual deviance: 4398.2  on 5183  degrees of freedom
    ## AIC: 6743.7
    ## 
    ## Number of Fisher Scoring iterations: 6

**The GAM model is superior to the GLM model because it is adjusting the curved data to fit the model much better.We can see from the GLM model that there is a large possibility of having a negative value for the response, which does not make sense for the given dataset.**

D) Predicted to Visit Doctor
----------------------------

**From our summary table in the GLM model, we see that those that are female, do not receive free government care due to low income, have a higher amount of illnesses,and are in the lower spectrum of income are the most likely to visit the doctor.**

F) Prediction for Last Person
-----------------------------

``` r
x <- predict(gamdoctor2,dvisits[5190,], type="response")
prob=rep(0,10)
prob[1]=exp(-x)
for(i in 1:5){
  prob[i+1]=exp(-x)*(x^i)/prod(1:i)}
round(prob,6)
```

    ##  [1] 0.869864 0.121275 0.008454 0.000393 0.000014 0.000000 0.000000
    ##  [8] 0.000000 0.000000 0.000000

**Looks like that the probability that the last person does not visit a doctor is about 87%, visits once is 12%, and visits twice is 0.8%. After that, it is incredibly unlikely that she will visit more than twice.**
