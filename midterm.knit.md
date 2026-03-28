---
title: "Midterm Project- R Code and Outputs"
output: pdf_document
---

## Load in neccessary packages

``` r
library(tidymodels)
library(caret)
library(corrplot)
library(glmnet)
library(kableExtra)
library(earth)
```

# Part I

Using this dataset, please help Researcher A build a prediction model for PFS, aiming to understand how demographic, tumor, and treatment characteristics influence progression-free survival in breast cancer patients.

To answer this question, your report should at least include the following:

Model Training: Provide a detailed description of the model training procedure and how you obtained the model. Your description should include sufficient detail so that another statistician can reproduce the same model. Points will be deducted if the details provided are insufficient to reproduce your results.

## Import and partition data using 'tidymodels' package

``` r
load("data/datA.RData")

set.seed(2222)

datA_split = initial_split(datA, prop = 0.8)

#extract the training and test data
training_datA = training(datA_split)
testing_datA = testing(datA_split)

#training data
x_trainA = model.matrix(log_PFS ~ . - id, training_datA)[, -1]
y_trainA = training_datA$log_PFS

#test data
x_testA = model.matrix(log_PFS ~ . - id, testing_datA)[, -1]
y_testA = testing_datA$log_PFS
```

## Exploratory Data Analysis

``` r
# Changing the plot display
theme1 <- trellis.par.get()
theme1$plot.symbol$col <- rgb(.2, .4, .2, .5)
theme1$plot.symbol$pch <- 16
theme1$plot.line$col <- rgb(.8, .1, .1, 1)
theme1$plot.line$lwd <- 2
theme1$strip.background$col <- rgb(.0, .2, .6, .2)
trellis.par.set(theme1)

# Feature plots to look at trends between predictors and the outcome (log_PFS)
featurePlot(x = training_datA[ ,c(2, 5, 6, 12)],
            y = training_datA[ ,17],
            plot = "scatter",
            span = .5,
            labels = c("Predictors","log_PFS"),
            type = c("p", "smooth"),
            layout = c(2, 2))
```

![](midterm_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

``` r
# Calculates the correlation matrix for all the predictors
corrplot(cor(x_trainA), method = "circle", type = "full")
```

![](midterm_files/figure-latex/unnamed-chunk-3-2.pdf)<!-- --> 

This data set includes 7 binary predictors, 3 categorical predictors, 5 continuous predictors, and 1 continuous outcome in log scale. 

Chose elastic net because continuous variables formed a cloud, meaning we could assume linearity? homoscedasticity?

Correlation plot revealed that certain variables were highly correlated. Chose to address this with elastic net because we can address colinearity for [] and [] reasons


``` r
# Elastic Net

ctrl1 <- trainControl(method = "cv", number = 10) # set up cv
set.seed(2) # set seed for reproduction

# remove ID
training_datA <- training_datA |>  select(-"id")
testing_datA  <- testing_datA  |> select(-"id")

enet.fit <- train(log_PFS ~ ., 
                  data = training_datA, 
                  method = "glmnet",
                  tuneGrid = expand.grid(alpha = seq(0, 1, length = 21),
                                         lambda = exp(seq(-1, -7, length = 100))),
                  trControl = ctrl1)
```

```
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,
## : There were missing values in resampled performance measures.
```

``` r
enet.fit$bestTune
```

```
##      alpha     lambda
## 1944  0.95 0.01235197
```

``` r
myCol <- rainbow(25)
myPar <- list(superpose.symbol = list(col = myCol),
superpose.line = list(col = myCol))

plot(enet.fit, par.settings = myPar, xTrans = log)
```

![](midterm_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

``` r
# coefficients in the final model
coef(enet.fit$finalModel, enet.fit$bestTune$lambda)
```

```
## 16 x 1 sparse Matrix of class "dgCMatrix"
##                  s=0.01235197
## (Intercept)       2.656050011
## age              -0.001046560
## menopause         .          
## race              .          
## bmi               .          
## tumor_size       -0.057703954
## tumor_grade      -0.066041243
## lymph_nodes      -0.056993530
## ER                0.220684085
## PR                .          
## HER2             -0.176250264
## Ki67             -0.003932773
## stage             .          
## surgery           .          
## chemo             .          
## hormonal_therapy  0.137999051
```

Still to do: predict on testing_datA

``` r
# predict on testing_datA
enet.pred <- predict(enet.fit, newdata = testing_datA)

# test error (MSE)
mean((enet.pred - testing_datA[, "log_PFS"])^2)
```

```
## [1] 0.4955286
```



Results: Report the model. Interpret the model and assess its performance.

``` r
# Create a table with the model coefficients
coefs1 <- as.data.frame(as.matrix(coef(enet.fit$finalModel, enet.fit$bestTune$lambda)))
coefs1$Predictors <- rownames(coefs1)
colnames(coefs1)[1] <- "Coefficients"
coefs1 <- coefs1[, c("Predictors", "Coefficients")]
rownames(coefs1) <- NULL
coefs1 |>
  kable()
```



|Predictors       | Coefficients|
|:----------------|------------:|
|(Intercept)      |    2.6560500|
|age              |   -0.0010466|
|menopause        |    0.0000000|
|race             |    0.0000000|
|bmi              |    0.0000000|
|tumor_size       |   -0.0577040|
|tumor_grade      |   -0.0660412|
|lymph_nodes      |   -0.0569935|
|ER               |    0.2206841|
|PR               |    0.0000000|
|HER2             |   -0.1762503|
|Ki67             |   -0.0039328|
|stage            |    0.0000000|
|surgery          |    0.0000000|
|chemo            |    0.0000000|
|hormonal_therapy |    0.1379991|

``` r
# Create a table with the model coefficients exponentiated
coefs2 <- as.data.frame(as.matrix(coef(enet.fit$finalModel, enet.fit$bestTune$lambda)))
coefs2$Predictors <- rownames(coefs2)
colnames(coefs2)[1] <- "Exponentiated Coefficients"
coefs2 <- coefs2[, c("Predictors", "Exponentiated Coefficients")]
coefs2 <- coefs2 |>
  filter(Predictors %in% c("(Intercept)", "age", "tumor_size", "tumor_grade", "lymph_nodes", 
                           "ER", "HER2", "Ki67", "hormonal_therapy"))
coefs2$`Exponentiated Coefficients`<- exp(coefs2$`Exponentiated Coefficients`)
rownames(coefs2) <- NULL
coefs2 |>
  kable()
```



|Predictors       | Exponentiated Coefficients|
|:----------------|--------------------------:|
|(Intercept)      |                 14.2399303|
|age              |                  0.9989540|
|tumor_size       |                  0.9439294|
|tumor_grade      |                  0.9360923|
|lymph_nodes      |                  0.9446002|
|ER               |                  1.2469294|
|HER2             |                  0.8384081|
|Ki67             |                  0.9960750|
|hormonal_therapy |                  1.1479745|



# Part II

About a decade later, Researcher B conducted a separate study enrolling 2000 breast cancer patients diagnosed between 2000 and 2005, using the same study design and variable definitions. Like Researcher A’s study, all patients were followed until disease progression or death. Researcher B’s data are stored in datB.RData. Researcher A would like to evaluate whether the prediction model built on the original data generalizes well to Researcher B’s dataset. Does the model generalize well? If yes, provide justification. If not, build a new prediction model for the 2000–2005 cohort and discuss what has changed in prediction.

## Import and explore the full dataset

``` r
load("data/datB.RData")

# basic summary
summary(datB)
```

```
##       id                 age          menopause          race      
##  Length:2000        Min.   :25.00   Min.   :0.000   Min.   :1.000  
##  Class :character   1st Qu.:48.50   1st Qu.:0.000   1st Qu.:1.000  
##  Mode  :character   Median :56.00   Median :1.000   Median :1.000  
##                     Mean   :56.05   Mean   :0.681   Mean   :1.875  
##                     3rd Qu.:63.80   3rd Qu.:1.000   3rd Qu.:3.000  
##                     Max.   :85.00   Max.   :1.000   Max.   :4.000  
##       bmi          tumor_size     tumor_grade     lymph_nodes    
##  Min.   :16.00   Min.   :0.300   Min.   :1.000   Min.   : 0.000  
##  1st Qu.:24.00   1st Qu.:1.800   1st Qu.:1.000   1st Qu.: 0.000  
##  Median :27.30   Median :2.800   Median :2.000   Median : 2.000  
##  Mean   :27.44   Mean   :3.075   Mean   :2.079   Mean   : 2.318  
##  3rd Qu.:30.82   3rd Qu.:4.200   3rd Qu.:3.000   3rd Qu.: 4.000  
##  Max.   :45.90   Max.   :7.900   Max.   :3.000   Max.   :15.000  
##        ER              PR              HER2            Ki67      
##  Min.   :0.000   Min.   :0.0000   Min.   :0.000   Min.   : 1.00  
##  1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:11.70  
##  Median :1.000   Median :1.0000   Median :0.000   Median :19.85  
##  Mean   :0.674   Mean   :0.5045   Mean   :0.237   Mean   :20.37  
##  3rd Qu.:1.000   3rd Qu.:1.0000   3rd Qu.:0.000   3rd Qu.:28.80  
##  Max.   :1.000   Max.   :1.0000   Max.   :1.000   Max.   :62.30  
##      stage          surgery           chemo       hormonal_therapy
##  Min.   :1.000   Min.   :0.0000   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:1.000   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000  
##  Median :2.000   Median :0.0000   Median :1.000   Median :1.0000  
##  Mean   :1.894   Mean   :0.4415   Mean   :0.649   Mean   :0.5695  
##  3rd Qu.:2.000   3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:1.0000  
##  Max.   :3.000   Max.   :1.0000   Max.   :1.000   Max.   :1.0000  
##     log_PFS       
##  Min.   :-0.0866  
##  1st Qu.: 2.2536  
##  Median : 2.7440  
##  Mean   : 2.7424  
##  3rd Qu.: 3.2487  
##  Max.   : 5.2864
```

``` r
# compare distributions of key variables between datasets
par(mfrow = c(1, 2))
hist(datA$age, main = "Age - Training", xlab = "Age")
hist(datB$age, main = "Age - New Data", xlab = "Age")
```

![](midterm_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 

``` r
par(mfrow = c(1, 2))
hist(datA$tumor_size, main = "Tumor Size - Training", xlab = "Tumor Size")
hist(datB$tumor_size, main = "Tumor Size - New Data", xlab = "Tumor Size")
```

![](midterm_files/figure-latex/unnamed-chunk-7-2.pdf)<!-- --> 

``` r
par(mfrow = c(1, 2))
hist(datA$tumor_grade, main = "Tumor Grade - Training", xlab = "Tumor Grade")
hist(datB$tumor_grade, main = "Tumor Grade - New Data", xlab = "Tumor Grade")
```

![](midterm_files/figure-latex/unnamed-chunk-7-3.pdf)<!-- --> 

``` r
# remove id for matrix operations
datB_clean_eda <- datB |> select(-"id")
x_B_all <- model.matrix(log_PFS ~ ., datB_clean_eda)[, -1]
```


## Evaluate the model's performance on researcher B's dataset

``` r
# remove ID
datB <- datB |>  select(-"id")

# use all of datB to test generalizability of elastic net model
enet_pred_B <- predict(enet.fit, newdata = datB)

# compare performance (will update once model is tested on datA testing split)
# rmse_A <- RMSE(enet_pred_A,     testing_datA$log_PFS)
# r2_A   <- R2(enet_pred_A,       testing_datA$log_PFS)
rmse_B <- RMSE(enet_pred_B, datB$log_PFS)
r2_B   <- R2(enet_pred_B,   datB$log_PFS)

# cat("datA Test RMSE:", round(rmse_A, 4), "\n")
# cat("datA Test R²:  ", round(r2_A,   4), "\n\n")
cat("datB RMSE:     ", round(rmse_B, 4), "\n")
```

```
## datB RMSE:      0.8383
```

``` r
cat("datB R²:       ", round(r2_B,   4), "\n")
```

```
## datB R²:        0.1489
```

``` r
# will add adjusted r^2
```

The elastic net model explained approximately 15% of the variance in the log-transformed PFS in the 2000 to 2005 study cohort, suggesting weak  predictive performance.

## TO DO ONLY IF NEED TO CREATE NEW MODEL

## Import and partition data using 'tidymodels' package

``` r
set.seed(2)

datB_split = initial_split(datB, prop = 0.8)

#extract the training and test data
training_datB = training(datB_split)
testing_datB = testing(datB_split)

#training data
x_trainB = model.matrix(log_PFS ~ ., training_datB)[, -1]
y_trainB = training_datB$log_PFS

#test data
x_testB  = model.matrix(log_PFS ~ ., testing_datB)[, -1]
y_testB = testing_datB$log_PFS
```

## Exploratory Data Analysis

``` r
# Feature plots to look at trends between predictors and the outcome (log_PFS)
# featurePlot(x = training_datB[ ,2:16],
#             y = training_datB[ ,17],
#             plot = "scatter",
#             span = .5,
#             labels = c("Predictors","log_PFS"),
#             type = c("p", "smooth"),
#             layout = c(3, 2))
# 
# # Calculates the correlation matrix for all the predictors
# corrplot(cor(x_trainB), method = "circle", type = "full")
```

# GAM model

``` r
mars.fit <- train(log_PFS ~ .,
                  data = training_datA,
                  method = "earth",
                  tuneGrid = expand.grid(degree = 1:3,
                                        nprune = 2:20),
                  trControl = ctrl1)

mars.fit$bestTune
```

```
##   nprune degree
## 9     10      1
```

``` r
# evaluate on testing_datA (internal test split)
pred_mars_testA <- predict(mars.fit, newdata = testing_datA)
rmse_mars_testA <- sqrt(mean((pred_mars_testA - testing_datA$log_PFS)^2))
r2_mars_testA <- cor(pred_mars_testA, testing_datA$log_PFS)^2

# evaluate on datB (external validation)
pred_mars_B <- predict(mars.fit, newdata = datB)
rmse_mars_B <- sqrt(mean((pred_mars_B - datB$log_PFS)^2))
r2_mars_B <- cor(pred_mars_B, datB$log_PFS)^2
```



Run GAM or MARs if model isn't super generalizable?

