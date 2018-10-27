---
title: "Homework 2"
author: "Fahim Hussain"
date: "September 8, 2018"
output:
   html_document:
    keep_md: true
---

```r
library(MASS)
library(ggplot2)
library(dplyr)
library(cowplot)
```


```r
head(Boston, 5)
```

```
##      crim zn indus chas   nox    rm  age    dis rad tax ptratio  black
## 1 0.00632 18  2.31    0 0.538 6.575 65.2 4.0900   1 296    15.3 396.90
## 2 0.02731  0  7.07    0 0.469 6.421 78.9 4.9671   2 242    17.8 396.90
## 3 0.02729  0  7.07    0 0.469 7.185 61.1 4.9671   2 242    17.8 392.83
## 4 0.03237  0  2.18    0 0.458 6.998 45.8 6.0622   3 222    18.7 394.63
## 5 0.06905  0  2.18    0 0.458 7.147 54.2 6.0622   3 222    18.7 396.90
##   lstat medv
## 1  4.98 24.0
## 2  9.14 21.6
## 3  4.03 34.7
## 4  2.94 33.4
## 5  5.33 36.2
```

```r
dim(Boston)
```

```
## [1] 506  14
```

####How many rows are in this data set? How many columns? What do the rows and columns represent?
####There are 506 rows and 14 columns. The rows represent the observations and the columns represents the variables in a data set. For example, row 1 represents all the observed values from different variables(columns).

#b) Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.

```r
Boston %>% select(crim, dis, black,lstat,medv) %>% pairs()
```

![](716Homework2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

####I chose these 5 variables, crim, dis, black,lstat, and medv because I found them to be most interesting. I believed they would have some sort of relationship with each other. Some observations we can make is that, 1) crime rates are higher when the mean distance to the employment centers are smaller. This is probably because the area is most likely an urban area, which should have a higher crime rate than the rural areas. We can also see that the 2) lower status of the population decreases as the distance to the employment centers increase. This is probably due to the fact that lower status population are offered more work near employment centers. Consequently, 3) As the median value of occupied homes increases, crime rates decrease, distance to employment centers increase, and lower status of population decreases. This can most likely be attributed to the fact that wealthier individuals will tend to purchase more expensive homes in a more rural area, where crime rates is not significant. 4) The relationship between black population and the other variables are not apparent, there does not seem to be much significant relationship.


```r
Boston %>% select(nox, age, ptratio, indus, zn) %>% pairs()
```

![](716Homework2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

####I chose to examine these 5 variables because I would like a general idea of the area in general. Immediately, 1) we can see that as the proportion of owner-occupied units built prior to 1940(age) increases, the concentration of nitrogen oxides(nox) increases. It isn't a surprise that older homes will contain a lot of bacteria and harmful substances that can have a big effect on the air. Surprisingly, 2) we see that as the ptratio increases, the proportion of non-retail business acres per town(indus) increases. This could be possibly be explained by a larger population due to a higher ratio of pupils to students. 3) There does not seem to be any other significant analysis from the other graphs provided. 

#c)

```r
cor(Boston)[,1]
```

```
##        crim          zn       indus        chas         nox          rm 
##  1.00000000 -0.20046922  0.40658341 -0.05589158  0.42097171 -0.21924670 
##         age         dis         rad         tax     ptratio       black 
##  0.35273425 -0.37967009  0.62550515  0.58276431  0.28994558 -0.38506394 
##       lstat        medv 
##  0.45562148 -0.38830461
```



####Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
####Yes, as we have seen, there are many predictors associated with per capita crime rate. We can see that crimes are more prevalent near employment centers. Also, houses with a lower mean average value tend to be surrounded by a higher crime rate area. We can see some of these relationship from the above table. Obviously, crime will have a one-to-one relationship with itself. Crime has the biggest relationship with tax and rad. This is probably because high property tax rate will mean an expensive house and accebility to radial highways will allow criminals an easy escape.

#d)

```r
Boston %>% ggplot(aes(x=crim))+geom_histogram(fill="Red")
```

![](716Homework2_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
summary(Boston$crim)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##  0.00632  0.08204  0.25651  3.61352  3.67708 88.97620
```

#### Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor. 

#### Although most of the crime rates are near 0 or close to it, we see some neighborhoods with an abnormally high crime rate. From the summary statistics, the median crime rate is about 0.26% but the mean is 3.61%, which means we have a positive skew in the distribution.This is seen by the graph, and the skew heavily influence the mean. We can inference that there are certain suburbs with an abnormally high crime rate. Crime rate is bounded below by the minimum of 0.00632 and bounded above by the maximum of 88.9762. The high differential is a clear indicator that we have an abnormally high amount of crime rate in certain suburbs.


```r
plot_grid(Boston %>% ggplot(aes(x=tax))+geom_histogram(fill="Blue")+labs(title="Property Tax rate"),
          Boston %>% ggplot(aes(x=ptratio))+geom_histogram(fill="Green")+
            labs(title="Pupil-Teacher Ratio"),labels="AUTO")
```

![](716Homework2_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
summary(Boston$tax)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   187.0   279.0   330.0   408.2   666.0   711.0
```

```r
summary(Boston$ptratio)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   12.60   17.40   19.05   18.46   20.20   22.00
```

#### The median and mean are nearly identical for the ptratio and the tax is almost similar. However, we do see some suburbs have a high tax rate than normal and a high ptratio than normal. The tax range is from (187,711), which is a huge range. This is most likely due to the income difference among the individuals and the property values that they contain. The ptratio ranges from (12.6,22), which is not much of a difference. A difference of 10 is a fair value for the ratio.

#e)


```r
Boston %>% filter(chas==1) %>% nrow()
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.3
```

```
## [1] 35
```
####  How many of the suburbs in this data set bound the Charles river? 
#### There are 35 suburbs that bound the Charles River

#f)

```r
median(Boston$ptratio)
```

```
## [1] 19.05
```

#### What is the median pupil-teacher ratio among the towns in this data set?
#### The median is 19.05

#g)


```r
min(Boston$medv)
```

```
## [1] 5
```

```r
Boston[Boston$medv==5,]
```

```
##        crim zn indus chas   nox    rm age    dis rad tax ptratio  black
## 399 38.3518  0  18.1    0 0.693 5.453 100 1.4896  24 666    20.2 396.90
## 406 67.9208  0  18.1    0 0.693 5.683 100 1.4254  24 666    20.2 384.97
##     lstat medv
## 399 30.59    5
## 406 22.98    5
```

```r
summary(Boston)
```

```
##       crim                zn             indus            chas        
##  Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000  
##  1st Qu.: 0.08204   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000  
##  Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000  
##  Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917  
##  3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000  
##  Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000  
##       nox               rm             age              dis        
##  Min.   :0.3850   Min.   :3.561   Min.   :  2.90   Min.   : 1.130  
##  1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100  
##  Median :0.5380   Median :6.208   Median : 77.50   Median : 3.207  
##  Mean   :0.5547   Mean   :6.285   Mean   : 68.57   Mean   : 3.795  
##  3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188  
##  Max.   :0.8710   Max.   :8.780   Max.   :100.00   Max.   :12.127  
##       rad              tax           ptratio          black       
##  Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32  
##  1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38  
##  Median : 5.000   Median :330.0   Median :19.05   Median :391.44  
##  Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67  
##  3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23  
##  Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90  
##      lstat            medv      
##  Min.   : 1.73   Min.   : 5.00  
##  1st Qu.: 6.95   1st Qu.:17.02  
##  Median :11.36   Median :21.20  
##  Mean   :12.65   Mean   :22.53  
##  3rd Qu.:16.95   3rd Qu.:25.00  
##  Max.   :37.97   Max.   :50.00
```
####Which suburb of Boston has lowest median value of owneroccupied homes? What are the values of the other predictors for that suburb, and how do those values compare to the overall ranges for those predictors? Comment on your findings.

####399 and 406 are the suburbs with the lowest median value of owner occupied homes. I first looked for the minimum median value and then used that value to isolate the suburbs that contain that minimum. From the summary of Boston, we can see that 399 and 406 have an abnormally high amount of crime rate compared to average. The proportion of non-retail businesses are also significantly higher than the mean and median of the overall data. The age variable is also at its max for both 399 and 406, which means the houses are all built prior to 1940. This explains why both the suburbs have a higher nox value compared to average. However, the distance to an employment center is less than average. Despite their old, noxious homes, 399 and 406 have a significantly high tax rate compared to average. The ptratio and black values align to the average of the overall dataset. However, the lower status of the population greatly exceeds the average and median.

#h)


```r
Boston %>% filter(rm >7) %>% nrow()
```

```
## [1] 64
```

```r
Boston %>% filter(rm >8) 
```

```
##       crim zn indus chas    nox    rm  age    dis rad tax ptratio  black
## 1  0.12083  0  2.89    0 0.4450 8.069 76.0 3.4952   2 276    18.0 396.90
## 2  1.51902  0 19.58    1 0.6050 8.375 93.9 2.1620   5 403    14.7 388.45
## 3  0.02009 95  2.68    0 0.4161 8.034 31.9 5.1180   4 224    14.7 390.55
## 4  0.31533  0  6.20    0 0.5040 8.266 78.3 2.8944   8 307    17.4 385.05
## 5  0.52693  0  6.20    0 0.5040 8.725 83.0 2.8944   8 307    17.4 382.00
## 6  0.38214  0  6.20    0 0.5040 8.040 86.5 3.2157   8 307    17.4 387.38
## 7  0.57529  0  6.20    0 0.5070 8.337 73.3 3.8384   8 307    17.4 385.91
## 8  0.33147  0  6.20    0 0.5070 8.247 70.4 3.6519   8 307    17.4 378.95
## 9  0.36894 22  5.86    0 0.4310 8.259  8.4 8.9067   7 330    19.1 396.90
## 10 0.61154 20  3.97    0 0.6470 8.704 86.9 1.8010   5 264    13.0 389.70
## 11 0.52014 20  3.97    0 0.6470 8.398 91.5 2.2885   5 264    13.0 386.86
## 12 0.57834 20  3.97    0 0.5750 8.297 67.0 2.4216   5 264    13.0 384.54
## 13 3.47428  0 18.10    1 0.7180 8.780 82.9 1.9047  24 666    20.2 354.55
##    lstat medv
## 1   4.21 38.7
## 2   3.32 50.0
## 3   2.88 50.0
## 4   4.14 44.8
## 5   4.63 50.0
## 6   3.13 37.6
## 7   2.47 41.7
## 8   3.95 48.3
## 9   3.54 42.8
## 10  5.12 50.0
## 11  5.91 48.8
## 12  7.44 50.0
## 13  5.29 21.9
```
#### In this data set, how many of the suburbs average more thanseven rooms per dwelling? More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling.

#### 64 suburbs average more than 7 rooms per dwelling and 13 average more than 8. Those with more than 8 rooms per dwelling tend to have lower crime rate, the proportion of non-retail business acres tend to be on the lower side, most do not tract by the Charles River, the nox values seem to be distributed normally, most of the houses tend to be built prior to 1940, the distance from an employment center seems to be about normally distributed among the 13 suburbs, the tax seems to be lower than the average, the black population and ptratio aligns with the median, but the lower status of the population is on the low side compared to the median and average.
