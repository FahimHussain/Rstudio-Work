General Linear Models Homework 2
================
Fahim Hussain
March 17, 2018

``` r
library(faraway)
```

    ## Warning: package 'faraway' was built under R version 3.4.3

``` r
data(africa)
summary(africa)
```

    ##     miltcoup       oligarchy          pollib         parties     
    ##  Min.   :0.000   Min.   : 0.000   Min.   :0.000   Min.   : 0.00  
    ##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:1.000   1st Qu.:10.00  
    ##  Median :1.000   Median : 1.000   Median :2.000   Median :13.00  
    ##  Mean   :1.404   Mean   : 4.447   Mean   :1.667   Mean   :15.96  
    ##  3rd Qu.:2.000   3rd Qu.: 9.000   3rd Qu.:2.000   3rd Qu.:19.00  
    ##  Max.   :6.000   Max.   :18.000   Max.   :2.000   Max.   :62.00  
    ##                                   NA's   :5                      
    ##     pctvote           popn              size           numelec      
    ##  Min.   : 0.00   Min.   :  0.067   Min.   :   0.5   Min.   : 0.000  
    ##  1st Qu.:18.90   1st Qu.:  1.450   1st Qu.:  33.0   1st Qu.: 4.000  
    ##  Median :28.95   Median :  5.600   Median : 274.0   Median : 6.000  
    ##  Mean   :31.88   Mean   : 10.953   Mean   : 516.7   Mean   : 6.191  
    ##  3rd Qu.:43.04   3rd Qu.: 11.450   3rd Qu.: 813.0   3rd Qu.: 8.500  
    ##  Max.   :77.40   Max.   :113.800   Max.   :2506.0   Max.   :14.000  
    ##  NA's   :6                                                          
    ##     numregim    
    ##  Min.   :1.000  
    ##  1st Qu.:2.000  
    ##  Median :3.000  
    ##  Mean   :2.511  
    ##  3rd Qu.:3.000  
    ##  Max.   :4.000  
    ## 

``` r
coup <- africa$miltcoup
oli <- africa$oligarchy
pol <- africa$pollib
parties <- africa$parties
pctvote <- africa$pctvote
pop <- africa$popn
size <- africa$size
numelec <- africa$numelec
numregim <- africa$numregim
plot(coup~oli)
```

![](glmhomework2.1_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
plot(coup~pol)
```

![](glmhomework2.1_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
plot(coup~parties)
```

![](glmhomework2.1_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
plot(coup~pctvote)
```

![](glmhomework2.1_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
plot(coup~pop)
```

![](glmhomework2.1_files/figure-markdown_github/unnamed-chunk-1-5.png)

``` r
plot(coup~size)
```

![](glmhomework2.1_files/figure-markdown_github/unnamed-chunk-1-6.png)

``` r
plot(coup~numelec)
```

![](glmhomework2.1_files/figure-markdown_github/unnamed-chunk-1-7.png)

``` r
plot(coup~numregim)
```

![](glmhomework2.1_files/figure-markdown_github/unnamed-chunk-1-8.png)
