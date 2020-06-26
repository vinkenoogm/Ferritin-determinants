Confirmatory Factor Analysis
================
Marieke Vinkenoog
19/06/2020

``` r
data <- readRDS("X:/202003 Ferritine en omgeving/donordata_with_temps_env_cleaned.RDS")
```

# Variable selection - temperature measurement

There are several measurement options available for temperature:
average, minimum and maximum temperature, each for 1 day (the day before
the donation), 7-day average before donation, and 30-day average before
donation, so nine options total. For each measurement option we fit a
simple linear model that corrects for donor sex and age. The option that
results in the model with the lowest residual standard error will be
used in the analysis.

We hypothesize a temperature effect on ferritin because of its known
effect on hemoglobin. As sanity check, we also fit the same nine linear
models with Hb as outcome variable.

``` r
tempcheck <- matrix(0, nrow=0, ncol=2)
colnames(tempcheck) <- c('RSE ferritin', 'RSE Hb')

for (tempvar in c('AvgTemp', 'AvgTemp7', 'AvgTemp30', 
                  'MinTemp', 'MinTemp7', 'MinTemp30',
                  'MaxTemp', 'MaxTemp7', 'MaxTemp30')) {
   formula_fer <- as.formula(paste('Ferritin ~', tempvar, '* Sex * Age'))
   formula_hb <- as.formula(paste('Hb ~', tempvar, '* Sex * Age'))
   sd_fer <- summary(lm(formula_fer, data=data))$sigma
   sd_hb <- summary(lm(formula_hb, data=data))$sigma
   tempcheck <- rbind(tempcheck, c(sd_fer, sd_hb))
}
rownames(tempcheck) <- c('AvgTemp', 'AvgTemp7', 'AvgTemp30', 
                         'MinTemp', 'MinTemp7', 'MinTemp30',
                         'MaxTemp', 'MaxTemp7', 'MaxTemp30')
print(tempcheck)
```

    ##           RSE ferritin    RSE Hb
    ## AvgTemp       49.85587 0.5748550
    ## AvgTemp7      49.85814 0.5750489
    ## AvgTemp30     49.85865 0.5753589
    ## MinTemp       49.86380 0.5750563
    ## MinTemp7      49.85893 0.5752315
    ## MinTemp30     49.85430 0.5754459
    ## MaxTemp       49.84731 0.5746514
    ## MaxTemp7      49.85780 0.5749616
    ## MaxTemp30     49.86217 0.5753118

RSE is lowest using MaxTemp, for both ferritin and Hb (although
differences are very minimal). We will only use MaxTemp in the rest of
the analysis and disregard the other temperature variables.

``` r
data <- data[, c(1:12, 19, 22:33)]
```

# Variances

Standardizing variables is not required for SEM, and in fact discouraged
because it complicates interpretation of the parameter estimations.
However, when variances of the variables differ substantially due to
measuring scales, the model can become ill-conditioned. For the
algorithm to work optimally, all variances must be within factor 10 of
each other. This is done by dividing or multiplying variables by a
constant, this does not affect the parameter estimations, only the
interpretations.

``` r
numericols <- c('Age', 'Sex', 'Ferritin', 'TimeSincePrev', 'NumBefore',
                'BloodVolume', 'BMI', 'Duffy', 'MaxTemp', 'SES', 'PopDensity',
                'O3', 'PM25', 'PM10', 'EC', 'NO2', 'Time', 'Date_sin', 
                'Date_cos', 'DayOfYear')
varTable(data[, numericols])
```

    ##             name idx   nobs    type exo user    mean       var nlev lnam
    ## 1            Age   1 138012 numeric   0    0  40.142   249.909    0     
    ## 2            Sex   2 138012 numeric   0    0   0.575     0.244    0     
    ## 3       Ferritin   3 138012 numeric   0    0  58.406  2773.231    0     
    ## 4  TimeSincePrev   4  77476 numeric   0    0 174.977 23207.106    0     
    ## 5      NumBefore   5 138012 numeric   0    0   2.327     6.745    0     
    ## 6    BloodVolume   6 138012 numeric   0    0   4.874     0.695    0     
    ## 7            BMI   7 138012 numeric   0    0  25.018    14.929    0     
    ## 8          Duffy   8 138012 numeric   0    0   0.775     0.174    0     
    ## 9        MaxTemp   9 137386 numeric   0    0  15.508    56.036    0     
    ## 10           SES  10 137964 numeric   0    0  -0.099     1.305    0     
    ## 11    PopDensity  11 117580 numeric   0    0  87.719  4364.677    0     
    ## 12            O3  12 138012 numeric   0    0  46.935     6.377    0     
    ## 13          PM25  13 138012 numeric   0    0  11.112     2.192    0     
    ## 14          PM10  14 138012 numeric   0    0  18.366     3.383    0     
    ## 15            EC  15 138012 numeric   0    0   0.751     0.035    0     
    ## 16           NO2  16 138012 numeric   0    0  18.475    25.152    0     
    ## 17          Time  17 138012 numeric   0    0  15.424    12.261    0     
    ## 18      Date_sin  18 138012 numeric   0    0  -0.117     0.476    0     
    ## 19      Date_cos  19 138012 numeric   0    0   0.021     0.510    0     
    ## 20     DayOfYear  20 138012 numeric   0    0 204.010 10925.118    0

Variances range from 0.035 (EC) to 23207.106 (DaysSincePrev). We rescale
all variables to have a variance between 0.1-10.

``` r
data$Age <- data$Age / 10                        # Age in decades
data$Ferritin <- data$Ferritin / 100             # Ferritin in 100ng/ml
data$TimeSincePrev <- data$TimeSincePrev / 365   # Time in years
data$BMI <- data$BMI / 10                        # BMI in 10kg/m^2
data$MaxTemp <- data$MaxTemp / 10                # Temperature in dC
data$PopDensity <- data$PopDensity / 100         # Population in 100 inhabitants/km^2
data$EC <- data$EC * 10                          # Soot in 10ug/m^3
data$NO2 <- data$NO2 / 10                        # NO2 in 10ug/m^3
data$Time <- data$Time / 10                      # Time in tens of hours
data$DayOfYear <- data$DayOfYear / 100           # Day of year / 100

varTable(data[, numericols])
```

    ##             name idx   nobs    type exo user   mean   var nlev lnam
    ## 1            Age   1 138012 numeric   0    0  4.014 2.499    0     
    ## 2            Sex   2 138012 numeric   0    0  0.575 0.244    0     
    ## 3       Ferritin   3 138012 numeric   0    0  0.584 0.277    0     
    ## 4  TimeSincePrev   4  77476 numeric   0    0  0.479 0.174    0     
    ## 5      NumBefore   5 138012 numeric   0    0  2.327 6.745    0     
    ## 6    BloodVolume   6 138012 numeric   0    0  4.874 0.695    0     
    ## 7            BMI   7 138012 numeric   0    0  2.502 0.149    0     
    ## 8          Duffy   8 138012 numeric   0    0  0.775 0.174    0     
    ## 9        MaxTemp   9 137386 numeric   0    0  1.551 0.560    0     
    ## 10           SES  10 137964 numeric   0    0 -0.099 1.305    0     
    ## 11    PopDensity  11 117580 numeric   0    0  0.877 0.436    0     
    ## 12            O3  12 138012 numeric   0    0 46.935 6.377    0     
    ## 13          PM25  13 138012 numeric   0    0 11.112 2.192    0     
    ## 14          PM10  14 138012 numeric   0    0 18.366 3.383    0     
    ## 15            EC  15 138012 numeric   0    0  7.508 3.478    0     
    ## 16           NO2  16 138012 numeric   0    0  1.847 0.252    0     
    ## 17          Time  17 138012 numeric   0    0  1.542 0.123    0     
    ## 18      Date_sin  18 138012 numeric   0    0 -0.117 0.476    0     
    ## 19      Date_cos  19 138012 numeric   0    0  0.021 0.510    0     
    ## 20     DayOfYear  20 138012 numeric   0    0  2.040 1.093    0

``` r
saveRDS(data, "X:/202003 Ferritine en omgeving/donordata_with_temps_env_scaled.RDS")
```

Variances now range from 0.123 (Time) to 6.745 (NumBefore) which should
not be a problem.

# Confirmatory Factor Analysis

Before considering the full model including the relations between latent
constructs, we carry out a confirmatory factor analysis (CFA). This is
the measurement part of the Structural Equation Model (SEM), which is
later followed by the structural part. The CFA tests the construct
validity; whether the observed variables that the researcher has chosen
to measure the latent construct do so consistently. We must first
develop a hypothesis about which latent constructs are underlying the
measures that are available in the data set.

In the end we will compare four similar models that differ slightly in
both the measurement part and the structural part, but contain the same
observed variables. Constructs will be inspected for each model
separately. The data will also be split into new donors and repeat
donors, as the variable ‘time since previous donation’ is not available
for new donors, and ‘number of previous donations’ will always be zero.
The lack of information on these two variables make the ‘Donation’
construct useless for new donors, and CFA/SEM is not well-equipped to
deal with ‘missing’ data at this scale.

There are two facets to construct validity that need to be inspected:

  - Convergent validity: do the indicators of each construct converge,
    and share a high proportion of their variance?
  - Discriminant validity: is each construct truly distinct from all
    other constructs?

In other words, convergent validity measures whether variables that
*should* be related to each other (according to the hypothesis) are
observed to be related to each other, and discriminant validity measures
whether variables that *should not* be related to each other (according
to the hypothesis) are observed not to be related to each other.

Note: the default estimator is maximum likelihood (ML). However, we
choose to use diagonally weighted least squares (WLSMV) for two reasons:

1.  Its ability to incorporate non-continuous observed variables. There
    are only two of these in our data set (Sex and Duffy) but
    considering the known large impact of Sex on Ferritin levels, WLSMV
    becomes more attractive than ML.
2.  WLSMV makes no distributional assumptions on observed variables,
    unlike ML, which assumes normality. Many continuous variables in our
    data set are do not follow a normal distribution, e.g. Age which has
    a bimodal distribution, and BMI which is known to be positively
    skewed.

## Model A

<!--html_preserve-->

<div id="htmlwidget-b4c69180b14634b0d0e8" class="grViz html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-b4c69180b14634b0d0e8">{"x":{"diagram":"\n      digraph boxes_and_circles {\n      \n      graph [layout = neato, overlap=scale]\n      \n      node [shape = oval]\n      \"Donor characteristics\"; Donations; \"Physical environment\"; Pollution\n      \n      node [shape = box]\n      Soot; \"PM2.5\"; PM10; Ozone; \"Population density\"; Temperature; SES;\n      \"Number of donations\"; \"Time since previous donation\"; Sex; Age; \n      \"Blood volume\"; BMI; Duffy; Ferritin [penwidth = 3]\n      \n      \"Donor characteristics\"->Sex\n      \"Donor characteristics\"->Age\n      \"Donor characteristics\"->\"Blood volume\" \n      \"Donor characteristics\"-> BMI\n      \"Donor characteristics\"->Duffy\n      \n      Donations->\"Number of donations\"\n      Donations->\"Time since previous donation\"\n      \n      \"Physical environment\"->\"Population density\"\n      \"Physical environment\"->Temperature\n      \"Physical environment\"->SES\n      \n      Pollution->Soot\n      Pollution->\"PM2.5\"\n      Pollution->PM10\n      Pollution->Ozone\n      \n      \"Donor characteristics\"->Ferritin\n      Donations->Ferritin\n      \"Physical environment\"->Ferritin\n      Pollution->Ferritin\n      }\n      \n      \n      ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

Model A contains four latent constructs: Donor characteristics,
Donations, Physical environment and Pollution. The Donations construct
is not available for new donors.

``` r
data_r <- data[data$DonType == 'V', ]
data_n <- data[data$DonType == 'N', ]

modelA_n <- 'DonChar   =~ Sex + Age + BloodVolume + BMI + Duffy
             Env       =~ SES + PopDensity + MaxTemp
             Pollution =~ O3 + PM25 + PM10 + EC + NO2'

fitA_n <- cfa(modelA_n, data=data_n, check.gradient=FALSE, estimator="WLSMV")
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated lv
    ## variances are negative

``` r
summary(fitA_n, standardized=TRUE, fit.measures=TRUE)
```

    ## lavaan 0.6-6 ended normally after 95 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         29
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                         51947       59657
    ##                                                                   
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                              9002.725    7210.216
    ##   Degrees of freedom                                62          62
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.254
    ##   Shift parameter                                           28.782
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                            336633.336  162862.647
    ##   Degrees of freedom                                78          78
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  2.067
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.973       0.956
    ##   Tucker-Lewis Index (TLI)                       0.967       0.945
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.053       0.047
    ##   90 Percent confidence interval - lower         0.052       0.046
    ##   90 Percent confidence interval - upper         0.054       0.048
    ##   P-value RMSEA <= 0.05                          0.000       1.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.045       0.045
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   DonChar =~                                                            
    ##     Sex               1.000                               0.197    0.419
    ##     Age              -0.618    0.016  -38.175    0.000   -0.122   -0.109
    ##     BloodVolume      -7.119    0.244  -29.140    0.000   -1.401   -1.722
    ##     BMI              -0.552    0.007  -83.085    0.000   -0.109   -0.268
    ##     Duffy             0.000    0.006    0.074    0.941    0.000    0.000
    ##   Env =~                                                                
    ##     SES               1.000                                  NA       NA
    ##     PopDensity        3.948    0.310   12.749    0.000       NA       NA
    ##     MaxTemp           0.053    0.026    2.045    0.041       NA       NA
    ##   Pollution =~                                                          
    ##     O3                1.000                               1.996    0.810
    ##     PM25             -0.680    0.003 -230.389    0.000   -1.358   -0.921
    ##     PM10             -0.847    0.004 -214.213    0.000   -1.690   -0.917
    ##     EC               -0.916    0.003 -273.947    0.000   -1.828   -0.989
    ##     NO2              -0.233    0.001 -277.945    0.000   -0.466   -0.901
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   DonChar ~~                                                            
    ##     Env               0.001    0.000    7.670    0.000    0.041    0.041
    ##     Pollution        -0.015    0.001  -10.158    0.000   -0.037   -0.037
    ##   Env ~~                                                                
    ##     Pollution        -0.158    0.012  -13.115    0.000   -0.557   -0.557
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Sex               0.182    0.002  120.293    0.000    0.182    0.825
    ##    .Age               1.220    0.008  156.314    0.000    1.220    0.988
    ##    .BloodVolume      -1.300    0.064  -20.212    0.000   -1.300   -1.965
    ##    .BMI               0.152    0.002   98.029    0.000    0.152    0.928
    ##    .Duffy             0.141    0.001  130.055    0.000    0.141    1.000
    ##    .SES               1.530    0.011  136.474    0.000    1.530    1.013
    ##    .PopDensity        0.871    0.038   22.942    0.000    0.871    1.568
    ##    .MaxTemp           0.567    0.003  192.329    0.000    0.567    1.000
    ##    .O3                2.095    0.014  149.185    0.000    2.095    0.345
    ##    .PM25              0.328    0.003  113.280    0.000    0.328    0.151
    ##    .PM10              0.542    0.007   77.596    0.000    0.542    0.160
    ##    .EC                0.075    0.004   18.516    0.000    0.075    0.022
    ##    .NO2               0.050    0.000  138.356    0.000    0.050    0.189
    ##     DonChar           0.039    0.001   27.121    0.000    1.000    1.000
    ##     Env              -0.020    0.002  -13.191    0.000       NA       NA
    ##     Pollution         3.985    0.037  106.539    0.000    1.000    1.000

``` r
modelA_r <- 'DonChar   =~ Sex + Age + BloodVolume + BMI + Duffy
             Donation  =~ TimeSincePrev + NumBefore 
             Env       =~ SES + PopDensity + MaxTemp 
             Pollution =~ O3 + PM25 + PM10 + EC + NO2'

fitA_r <- cfa(modelA_r, data=data_r, check.gradient=FALSE, estimator="WLSMV")
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative
    
    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated lv
    ## variances are negative

``` r
summary(fitA_r, standardized=TRUE, fit.measures=TRUE)
```

    ## lavaan 0.6-6 ended normally after 92 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         36
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                         64388       78355
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                              16395.147   14941.917
    ##   Degrees of freedom                                 84          84
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.100
    ##   Shift parameter                                            38.514
    ##        simple second-order correction                              
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                            508861.577  242456.142
    ##   Degrees of freedom                               105         105
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  2.099
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.968       0.939
    ##   Tucker-Lewis Index (TLI)                       0.960       0.923
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.055       0.052
    ##   90 Percent confidence interval - lower         0.054       0.052
    ##   90 Percent confidence interval - upper         0.056       0.053
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.049       0.049
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   DonChar =~                                                            
    ##     Sex               1.000                               0.422    0.845
    ##     Age              -0.894    0.015  -61.448    0.000   -0.378   -0.257
    ##     BloodVolume      -1.746    0.010 -176.884    0.000   -0.738   -0.889
    ##     BMI              -0.256    0.004  -67.877    0.000   -0.108   -0.292
    ##     Duffy             0.077    0.005   16.778    0.000    0.033    0.074
    ##   Donation =~                                                           
    ##     TimeSincePrev     1.000                               0.213    0.510
    ##     NumBefore       -10.397    0.147  -70.518    0.000   -2.210   -1.042
    ##   Env =~                                                                
    ##     SES               1.000                                  NA       NA
    ##     PopDensity        1.751    0.072   24.231    0.000       NA       NA
    ##     MaxTemp           0.007    0.021    0.336    0.737       NA       NA
    ##   Pollution =~                                                          
    ##     O3                1.000                               2.028    0.834
    ##     PM25             -0.661    0.002 -282.406    0.000   -1.340   -0.936
    ##     PM10             -0.800    0.003 -259.119    0.000   -1.622   -0.920
    ##     EC               -0.851    0.002 -345.055    0.000   -1.726   -0.977
    ##     NO2              -0.206    0.001 -340.487    0.000   -0.417   -0.891
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   DonChar ~~                                                            
    ##     Donation          0.044    0.001   60.654    0.000    0.493    0.493
    ##     Env               0.011    0.001   15.414    0.000    0.206    0.206
    ##     Pollution        -0.066    0.004  -17.766    0.000   -0.078   -0.078
    ##   Donation ~~                                                           
    ##     Env               0.002    0.000    5.979    0.000    0.061    0.061
    ##     Pollution         0.020    0.002   11.070    0.000    0.046    0.046
    ##   Env ~~                                                                
    ##     Pollution        -0.260    0.010  -25.932    0.000   -1.061   -1.061
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Sex               0.072    0.001   68.212    0.000    0.072    0.287
    ##    .Age               2.017    0.008  247.245    0.000    2.017    0.934
    ##    .BloodVolume       0.145    0.003   49.944    0.000    0.145    0.210
    ##    .BMI               0.126    0.001  119.133    0.000    0.126    0.915
    ##    .Duffy             0.195    0.001  238.921    0.000    0.195    0.995
    ##    .TimeSincePrev     0.129    0.003   45.176    0.000    0.129    0.740
    ##    .NumBefore        -0.385    0.071   -5.440    0.000   -0.385   -0.086
    ##    .SES               1.257    0.009  136.498    0.000    1.257    1.012
    ##    .PopDensity        0.369    0.009   41.809    0.000    0.369    1.139
    ##    .MaxTemp           0.548    0.003  212.311    0.000    0.548    1.000
    ##    .O3                1.803    0.012  152.197    0.000    1.803    0.305
    ##    .PM25              0.254    0.002  119.068    0.000    0.254    0.124
    ##    .PM10              0.475    0.005  102.836    0.000    0.475    0.153
    ##    .EC                0.142    0.004   37.480    0.000    0.142    0.046
    ##    .NO2               0.045    0.000  149.807    0.000    0.045    0.206
    ##     DonChar           0.178    0.001  169.777    0.000    1.000    1.000
    ##     Donation          0.045    0.001   48.644    0.000    1.000    1.000
    ##     Env              -0.015    0.002   -9.458    0.000       NA       NA
    ##     Pollution         4.112    0.032  130.182    0.000    1.000    1.000

### Interpretation

We interpret the CFA models based on the TLI (relative measure of fit)
and the RMSEA (absolute measure of fit).

For new donors, TLI of the model is 0.945. Generally, a value of 0.95 or
higher indicates good fit. Our model falls just below this cutoff. The
robust RMSEA is 0.047 (90% CI 0.046-0.048). RMSEA below 0.08 represent a
good fit.

For repeat donors, TLI is 0.923, and RMSEA is 0.052 (90% CI
0.052-0.053). This model includes one more construct than the one for
new donors, yet fits the data slightly worse.

Next, we inspect the residual correlations between the observed
variables. The covariance matrix expected by the model is compared to
the covariance matrix observed in the data, the differences between
these are the residuals of the CFA model. Large residual covariances can
indicate that there is a relation between two variables that the model
doesn’t capture. Generally speaking, residuals \<|0.1| are considered
fine.

``` r
residuals(fitA_n, type="cor")
```

    ## $type
    ## [1] "cor.bollen"
    ## 
    ## $cov
    ##             Sex    Age    BldVlm BMI    Duffy  SES    PpDnst MaxTmp O3    
    ## Sex          0.000                                                        
    ## Age         -0.018  0.000                                                 
    ## BloodVolume -0.012 -0.046  0.000                                          
    ## BMI          0.074  0.220 -0.001  0.000                                   
    ## Duffy       -0.012 -0.016  0.000 -0.001  0.000                            
    ## SES          0.010  0.052  0.000 -0.062 -0.004  0.000                     
    ## PopDensity  -0.029 -0.113  0.015 -0.064  0.015  0.000  0.000              
    ## MaxTemp     -0.012  0.018  0.005  0.013  0.003 -0.007  0.004  0.000       
    ## O3           0.033  0.067 -0.009  0.039 -0.011  0.080  0.040 -0.011  0.000
    ## PM25        -0.035 -0.018  0.009 -0.040  0.016  0.052 -0.050  0.001  0.003
    ## PM10        -0.034 -0.027  0.006 -0.052  0.015  0.074  0.003 -0.006  0.045
    ## EC          -0.035 -0.057  0.002 -0.064  0.014 -0.028  0.005  0.003 -0.008
    ## NO2         -0.030 -0.080 -0.007 -0.081  0.008 -0.028  0.101 -0.002 -0.089
    ##             PM25   PM10   EC     NO2   
    ## Sex                                    
    ## Age                                    
    ## BloodVolume                            
    ## BMI                                    
    ## Duffy                                  
    ## SES                                    
    ## PopDensity                             
    ## MaxTemp                                
    ## O3                                     
    ## PM25         0.000                     
    ## PM10         0.128  0.000              
    ## EC           0.005 -0.022  0.000       
    ## NO2         -0.070 -0.038  0.010  0.000

``` r
residuals(fitA_r, type="cor")
```

    ## $type
    ## [1] "cor.bollen"
    ## 
    ## $cov
    ##               Sex    Age    BldVlm BMI    Duffy  TmSncP NumBfr SES    PpDnst
    ## Sex            0.000                                                        
    ## Age            0.051  0.000                                                 
    ## BloodVolume   -0.019 -0.101  0.000                                          
    ## BMI            0.164  0.128  0.174  0.000                                   
    ## Duffy         -0.018 -0.064  0.029  0.007  0.000                            
    ## TimeSincePrev  0.027 -0.024  0.032  0.031  0.016  0.000                     
    ## NumBefore     -0.048  0.077 -0.064 -0.042 -0.040  0.000  0.000              
    ## SES           -0.038  0.059  0.027 -0.055 -0.015 -0.009  0.019  0.000       
    ## PopDensity    -0.030 -0.201  0.017 -0.053  0.009 -0.010 -0.011  0.000  0.000
    ## MaxTemp        0.008 -0.003 -0.007  0.006  0.005 -0.004 -0.002 -0.023 -0.003
    ## O3             0.046  0.096 -0.014  0.022  0.000  0.030 -0.012  0.048  0.040
    ## PM25          -0.032 -0.072 -0.004 -0.032  0.005 -0.008 -0.019  0.038 -0.042
    ## PM10          -0.028 -0.080 -0.005 -0.045  0.004  0.005 -0.039  0.072  0.016
    ## EC            -0.044 -0.104  0.001 -0.040  0.006 -0.028  0.008 -0.037  0.001
    ## NO2           -0.047 -0.117  0.006 -0.053  0.006 -0.027  0.001 -0.010  0.095
    ##               MaxTmp O3     PM25   PM10   EC     NO2   
    ## Sex                                                    
    ## Age                                                    
    ## BloodVolume                                            
    ## BMI                                                    
    ## Duffy                                                  
    ## TimeSincePrev                                          
    ## NumBefore                                              
    ## SES                                                    
    ## PopDensity                                             
    ## MaxTemp        0.000                                   
    ## O3            -0.015  0.000                            
    ## PM25          -0.010  0.009  0.000                     
    ## PM10          -0.017  0.043  0.108  0.000              
    ## EC             0.002 -0.007  0.007 -0.023  0.000       
    ## NO2            0.004 -0.096 -0.071 -0.038  0.015  0.000

A few residual covariances spark interest. For new donors:

  - BMI and Age (0.220)
  - PopDensity and Age (-0.113)
  - PM2.5 and PM10 (0.128)
  - NO2 and PopDensity (0.101)

For repeat donors: \* BMI and Sex (0.164), Age (0.128) and BloodVolume
(0.174) \* PopDensity and Age (-0.201) \* PM2.5 and PM10 (0.108) \* EC
and Age (-0.104) \* NO2 and Age (-0.117)

We also look at the modification indices, which tell us for each fixed
parameter how much the model would improve if we freed that parameter
(i.e. added a covariance or relation between those variables into the
model).

Since CFA is a *confirmatory* model and not an *exploratory* one, adding
parameters based on modification indices or high residual variance
carries the risk of overfitting on the data. However, inspecting these
values is valuable to discover which parts of the model fit well and
which can be improved.

``` r
modindices(fitA_n, standardized=TRUE, sort=TRUE)
```

    ## Warning in sqrt(var.lhs.value * var.rhs.value): NaNs produced

    ## Warning in lav_start_check_cov(lavpartable = lavpartable, start = START): lavaan WARNING: starting values imply NaN for a correlation value;
    ##                   variables involved are: DonChar Env

    ##             lhs op        rhs      mi    epc sepc.lv sepc.all sepc.nox
    ## 75          Age ~~ PopDensity 955.749 -0.097  -0.097   -0.094   -0.094
    ## 42          Env =~        Age 877.874 -0.023      NA       NA       NA
    ## 131        PM25 ~~       PM10 826.157  0.599   0.599    1.420    1.420
    ## 121  PopDensity ~~        NO2 505.197  0.050   0.050    0.241    0.241
    ## 50          Env =~        NO2 448.615  0.012      NA       NA       NA
    ## 133        PM25 ~~        NO2 442.991 -0.111  -0.111   -0.862   -0.862
    ## 44          Env =~        BMI 421.813 -0.007      NA       NA       NA
    ## 130          O3 ~~        NO2 417.927 -0.182  -0.182   -0.561   -0.561
    ## 81          Age ~~        NO2 374.376 -0.046  -0.046   -0.186   -0.186
    ## 100         BMI ~~        NO2 348.686 -0.017  -0.017   -0.195   -0.195
    ## 111         SES ~~         O3 336.150  0.288   0.288    0.161    0.161
    ## 94          BMI ~~ PopDensity 318.945 -0.023  -0.023   -0.062   -0.062
    ## 113         SES ~~       PM10 303.096  0.212   0.212    0.233    0.233
    ## 77          Age ~~         O3 231.622  0.185   0.185    0.116    0.116
    ## 55    Pollution =~      Duffy 215.002 -0.012  -0.025   -0.065   -0.065
    ## 99          BMI ~~         EC 212.378 -0.048  -0.048   -0.450   -0.450
    ## 93          BMI ~~        SES 207.323 -0.032  -0.032   -0.066   -0.066
    ## 80          Age ~~         EC 184.159 -0.118  -0.118   -0.390   -0.390
    ## 46          Env =~         O3 183.569  0.031      NA       NA       NA
    ## 74          Age ~~        SES 148.420  0.071   0.071    0.052    0.052
    ## 118  PopDensity ~~       PM25 148.207 -0.075  -0.075   -0.141   -0.141
    ## 112         SES ~~       PM25 147.260  0.119   0.119    0.167    0.167
    ## 98          BMI ~~       PM10 146.389 -0.040  -0.040   -0.138   -0.138
    ## 128          O3 ~~       PM10 143.742  0.365   0.365    0.342    0.342
    ## 135        PM10 ~~        NO2 128.151 -0.075  -0.075   -0.452   -0.452
    ## 117  PopDensity ~~         O3 121.164  0.101   0.101    0.075    0.075
    ## 47          Env =~       PM25 102.247 -0.016      NA       NA       NA
    ## 97          BMI ~~       PM25  81.901 -0.024  -0.024   -0.107   -0.107
    ## 96          BMI ~~         O3  73.111  0.039   0.039    0.069    0.069
    ## 69          Sex ~~         EC  67.113 -0.031  -0.031   -0.269   -0.269
    ## 67          Sex ~~       PM25  64.696 -0.025  -0.025   -0.100   -0.100
    ## 68          Sex ~~       PM10  62.665 -0.030  -0.030   -0.096   -0.096
    ## 66          Sex ~~         O3  57.301  0.039   0.039    0.062    0.062
    ## 70          Sex ~~        NO2  48.404 -0.007  -0.007   -0.078   -0.078
    ## 114         SES ~~         EC  45.590 -0.084  -0.084   -0.248   -0.248
    ## 79          Age ~~       PM10  42.059 -0.056  -0.056   -0.069   -0.069
    ## 115         SES ~~        NO2  39.317 -0.022  -0.022   -0.078   -0.078
    ## 134        PM10 ~~         EC  35.167 -0.153  -0.153   -0.757   -0.757
    ## 40      DonChar =~        NO2  18.532  0.001   0.000    0.000    0.000
    ## 78          Age ~~       PM25  18.119 -0.030  -0.030   -0.047   -0.047
    ## 76          Age ~~    MaxTemp  16.791  0.015   0.015    0.018    0.018
    ## 33      DonChar =~        SES  14.325  0.002   0.000    0.000    0.000
    ## 41          Env =~        Sex  13.932 -0.002      NA       NA       NA
    ## 64          Sex ~~ PopDensity  13.092 -0.006  -0.006   -0.015   -0.015
    ## 48          Env =~       PM10  12.668  0.007      NA       NA       NA
    ## 105       Duffy ~~       PM25  12.653  0.009   0.009    0.040    0.040
    ## 45          Env =~      Duffy  12.319  0.001      NA       NA       NA
    ## 102       Duffy ~~ PopDensity  12.211  0.004   0.004    0.012    0.012
    ## 106       Duffy ~~       PM10  11.609  0.010   0.010    0.037    0.037
    ## 107       Duffy ~~         EC   9.884  0.010   0.010    0.093    0.093
    ## 95          BMI ~~    MaxTemp   8.231  0.004   0.004    0.013    0.013
    ## 122     MaxTemp ~~         O3   8.076 -0.024  -0.024   -0.022   -0.022
    ## 65          Sex ~~    MaxTemp   7.785 -0.004  -0.004   -0.013   -0.013
    ## 63          Sex ~~        SES   7.369  0.007   0.007    0.013    0.013
    ## 37      DonChar =~       PM25   6.607 -0.002   0.000    0.000    0.000
    ## 136          EC ~~        NO2   6.546  0.018   0.018    0.300    0.300
    ## 104       Duffy ~~         O3   5.729 -0.010  -0.010   -0.018   -0.018
    ## 88  BloodVolume ~~       PM25   5.576  0.014   0.014    0.021    0.021
    ## 35      DonChar =~    MaxTemp   5.273 -0.001   0.000    0.000    0.000
    ## 87  BloodVolume ~~         O3   4.436 -0.020  -0.020   -0.012   -0.012
    ## 129          O3 ~~         EC   4.078 -0.067  -0.067   -0.168   -0.168
    ## 108       Duffy ~~        NO2   3.701  0.002   0.002    0.019    0.019
    ## 84  BloodVolume ~~        SES   3.254 -0.008  -0.008   -0.006   -0.006
    ## 91  BloodVolume ~~        NO2   2.717 -0.003  -0.003   -0.013   -0.013
    ## 36      DonChar =~         O3   2.580  0.002   0.000    0.000    0.000
    ## 89  BloodVolume ~~       PM10   2.488  0.012   0.012    0.014    0.014
    ## 124     MaxTemp ~~       PM10   2.397 -0.010  -0.010   -0.018   -0.018
    ## 132        PM25 ~~         EC   2.040  0.030   0.030    0.188    0.188
    ## 120  PopDensity ~~         EC   1.298  0.009   0.009    0.036    0.036
    ## 38      DonChar =~       PM10   1.197 -0.001   0.000    0.000    0.000
    ## 86  BloodVolume ~~    MaxTemp   1.009  0.003   0.003    0.003    0.003
    ## 101       Duffy ~~        SES   0.964 -0.002  -0.002   -0.004   -0.004
    ## 39      DonChar =~         EC   0.697  0.001   0.000    0.000    0.000
    ## 103       Duffy ~~    MaxTemp   0.615  0.001   0.001    0.003    0.003
    ## 119  PopDensity ~~       PM10   0.542  0.006   0.006    0.009    0.009
    ## 125     MaxTemp ~~         EC   0.497  0.005   0.005    0.022    0.022
    ## 127          O3 ~~       PM25   0.453  0.017   0.017    0.020    0.020
    ## 90  BloodVolume ~~         EC   0.215  0.003   0.003    0.011    0.011
    ## 126     MaxTemp ~~        NO2   0.184 -0.001  -0.001   -0.005   -0.005
    ## 123     MaxTemp ~~       PM25   0.018  0.001   0.001    0.002    0.002
    ## 49          Env =~         EC   0.002  0.000      NA       NA       NA

``` r
modindices(fitA_r, standardized=TRUE, sort=TRUE)
```

    ## Warning in sqrt(var.lhs.value * var.rhs.value): NaNs produced
    
    ## Warning in sqrt(var.lhs.value * var.rhs.value): lavaan WARNING: starting values imply NaN for a correlation value;
    ##                   variables involved are: DonChar Env

    ##               lhs op           rhs           mi      epc  sepc.lv sepc.all
    ## 76      Pollution =~           Sex 1.822308e+12 1089.384 2209.176 4418.355
    ## 106           Age ~~    PopDensity 2.548172e+03   -0.191   -0.191   -0.222
    ## 65            Env =~           Age 2.366155e+03   -0.100       NA       NA
    ## 77      Pollution =~           Age 1.737446e+03    0.066    0.133    0.091
    ## 116   BloodVolume ~~     NumBefore 1.082548e+03   -0.423   -0.423   -1.791
    ## 112           Age ~~           NO2 8.512420e+02   -0.081   -0.081   -0.270
    ## 185          PM25 ~~          PM10 7.863770e+02    0.492    0.492    1.417
    ## 91            Sex ~~     NumBefore 7.754030e+02   -0.205   -0.205   -1.232
    ## 57       Donation =~    PopDensity 7.063320e+02    0.768    0.163    0.287
    ## 111           Age ~~            EC 6.810820e+02   -0.274   -0.274   -0.511
    ## 67            Env =~           BMI 6.220750e+02   -0.012       NA       NA
    ## 108           Age ~~            O3 6.048240e+02    0.347    0.347    0.182
    ## 184            O3 ~~           NO2 5.805940e+02   -0.177   -0.177   -0.618
    ## 104           Age ~~     NumBefore 5.344630e+02    0.295    0.295    0.335
    ## 187          PM25 ~~           NO2 5.070910e+02   -0.097   -0.097   -0.902
    ## 110           Age ~~          PM10 3.946190e+02   -0.208   -0.208   -0.213
    ## 129           BMI ~~    PopDensity 3.846960e+02   -0.017   -0.017   -0.079
    ## 175    PopDensity ~~           NO2 3.775880e+02    0.031    0.031    0.242
    ## 167           SES ~~          PM10 3.465510e+02    0.178    0.178    0.231
    ## 118   BloodVolume ~~    PopDensity 3.432800e+02   -0.051   -0.051   -0.222
    ## 109           Age ~~          PM25 3.257800e+02   -0.154   -0.154   -0.215
    ## 66            Env =~   BloodVolume 3.094840e+02   -0.028       NA       NA
    ## 75            Env =~           NO2 3.081180e+02    0.016       NA       NA
    ## 56       Donation =~           SES 2.888350e+02   -0.278   -0.059   -0.053
    ## 128           BMI ~~           SES 2.299950e+02   -0.026   -0.026   -0.065
    ## 71            Env =~            O3 2.152080e+02    0.055       NA       NA
    ## 135           BMI ~~           NO2 1.846780e+02   -0.009   -0.009   -0.124
    ## 105           Age ~~           SES 1.770850e+02    0.088    0.088    0.055
    ## 182            O3 ~~          PM10 1.741790e+02    0.345    0.345    0.373
    ## 93            Sex ~~    PopDensity 1.592570e+02    0.020    0.020    0.123
    ## 165           SES ~~            O3 1.583210e+02    0.159    0.159    0.106
    ## 99            Sex ~~           NO2 1.553950e+02   -0.012   -0.012   -0.212
    ## 95            Sex ~~            O3 1.493440e+02    0.061    0.061    0.170
    ## 115   BloodVolume ~~ TimeSincePrev 1.482450e+02    0.023    0.023    0.172
    ## 127           BMI ~~     NumBefore 1.482260e+02   -0.041   -0.041   -0.188
    ## 98            Sex ~~            EC 1.385640e+02   -0.043   -0.043   -0.429
    ## 133           BMI ~~          PM10 1.367030e+02   -0.030   -0.030   -0.123
    ## 189          PM10 ~~           NO2 1.318910e+02   -0.061   -0.061   -0.414
    ## 171    PopDensity ~~            O3 1.305410e+02    0.077    0.077    0.094
    ## 61       Donation =~          PM10 1.248880e+02    0.308    0.066    0.037
    ## 81      Pollution =~ TimeSincePrev 1.240610e+02    0.005    0.010    0.024
    ## 82      Pollution =~     NumBefore 1.240580e+02    0.052    0.105    0.049
    ## 160     NumBefore ~~          PM10 1.218460e+02   -0.176   -0.176   -0.411
    ## 137         Duffy ~~     NumBefore 1.146850e+02   -0.043   -0.043   -0.158
    ## 172    PopDensity ~~          PM25 1.073540e+02   -0.047   -0.047   -0.152
    ## 134           BMI ~~            EC 1.060960e+02   -0.027   -0.027   -0.200
    ## 90            Sex ~~ TimeSincePrev 9.939000e+01    0.011    0.011    0.116
    ## 166           SES ~~          PM25 9.466800e+01    0.077    0.077    0.136
    ## 41        DonChar =~ TimeSincePrev 9.433500e+01   -0.035   -0.015   -0.035
    ## 42        DonChar =~     NumBefore 9.430800e+01   -0.362   -0.153   -0.072
    ## 168           SES ~~            EC 9.080300e+01   -0.094   -0.094   -0.221
    ## 73            Env =~          PM10 8.985500e+01    0.030       NA       NA
    ## 79      Pollution =~           BMI 8.695900e+01    0.004    0.007    0.019
    ## 96            Sex ~~          PM25 7.120500e+01   -0.025   -0.025   -0.186
    ## 132           BMI ~~          PM25 6.767400e+01   -0.017   -0.017   -0.096
    ## 59       Donation =~            O3 6.657100e+01    0.303    0.064    0.026
    ## 64            Env =~           Sex 6.491600e+01    0.007       NA       NA
    ## 126           BMI ~~ TimeSincePrev 6.448100e+01    0.005    0.005    0.041
    ## 150 TimeSincePrev ~~            O3 5.611000e+01    0.032    0.032    0.066
    ## 97            Sex ~~          PM10 5.526100e+01   -0.027   -0.027   -0.147
    ## 153 TimeSincePrev ~~            EC 5.037900e+01   -0.022   -0.022   -0.161
    ## 188          PM10 ~~            EC 4.853300e+01   -0.148   -0.148   -0.571
    ## 72            Env =~          PM25 4.711600e+01   -0.017       NA       NA
    ## 154 TimeSincePrev ~~           NO2 4.487900e+01   -0.005   -0.005   -0.071
    ## 103           Age ~~ TimeSincePrev 4.383600e+01   -0.016   -0.016   -0.032
    ## 156     NumBefore ~~    PopDensity 3.398600e+01   -0.055   -0.055   -0.146
    ## 131           BMI ~~            O3 3.268400e+01    0.020    0.020    0.043
    ## 159     NumBefore ~~          PM25 2.969400e+01   -0.071   -0.071   -0.227
    ## 155     NumBefore ~~           SES 2.568600e+01    0.049    0.049    0.070
    ## 92            Sex ~~           SES 2.440200e+01   -0.011   -0.011   -0.037
    ## 46        DonChar =~            O3 2.362700e+01    0.017    0.007    0.003
    ## 62       Donation =~            EC 2.341800e+01   -0.136   -0.029   -0.016
    ## 178       MaxTemp ~~          PM10 2.288700e+01   -0.027   -0.027   -0.054
    ## 43        DonChar =~           SES 2.060600e+01   -0.007   -0.003   -0.003
    ## 136         Duffy ~~ TimeSincePrev 1.898700e+01    0.003    0.003    0.020
    ## 176       MaxTemp ~~            O3 1.744600e+01   -0.032   -0.032   -0.032
    ## 69            Env =~ TimeSincePrev 1.735500e+01   -0.002       NA       NA
    ## 70            Env =~     NumBefore 1.735400e+01   -0.025       NA       NA
    ## 48        DonChar =~          PM10 1.598000e+01    0.010    0.004    0.002
    ## 190            EC ~~           NO2 1.595300e+01    0.023    0.023    0.284
    ## 60       Donation =~          PM25 1.524000e+01    0.088    0.019    0.013
    ## 120   BloodVolume ~~            O3 1.446200e+01   -0.032   -0.032   -0.062
    ## 139         Duffy ~~    PopDensity 1.392200e+01    0.004    0.004    0.013
    ## 173    PopDensity ~~          PM10 1.251900e+01    0.020    0.020    0.049
    ## 138         Duffy ~~           SES 1.192900e+01   -0.007   -0.007   -0.014
    ## 158     NumBefore ~~            O3 1.083600e+01   -0.071   -0.071   -0.085
    ## 80      Pollution =~         Duffy 1.062000e+01    0.001    0.003    0.007
    ## 68            Env =~         Duffy 8.131000e+00    0.002       NA       NA
    ## 74            Env =~            EC 7.938000e+00   -0.009       NA       NA
    ## 177       MaxTemp ~~          PM25 7.501000e+00   -0.013   -0.013   -0.034
    ## 148 TimeSincePrev ~~    PopDensity 7.306000e+00   -0.003   -0.003   -0.013
    ## 181            O3 ~~          PM25 6.570000e+00    0.056    0.056    0.082
    ## 63       Donation =~           NO2 6.437000e+00   -0.018   -0.004   -0.008
    ## 45        DonChar =~       MaxTemp 6.262000e+00    0.002    0.001    0.001
    ## 169           SES ~~           NO2 6.111000e+00   -0.006   -0.006   -0.027
    ## 161     NumBefore ~~            EC 5.359000e+00    0.037    0.037    0.160
    ## 147 TimeSincePrev ~~           SES 5.080000e+00   -0.004   -0.004   -0.010
    ## 186          PM25 ~~            EC 4.726000e+00    0.038    0.038    0.199
    ## 183            O3 ~~            EC 4.131000e+00   -0.057   -0.057   -0.113
    ## 94            Sex ~~       MaxTemp 3.990000e+00    0.003    0.003    0.015
    ## 151 TimeSincePrev ~~          PM25 3.786000e+00   -0.005   -0.005   -0.027
    ## 119   BloodVolume ~~       MaxTemp 3.494000e+00   -0.005   -0.005   -0.016
    ## 117   BloodVolume ~~           SES 3.363000e+00    0.007    0.007    0.016
    ## 124   BloodVolume ~~           NO2 2.880000e+00    0.003    0.003    0.034
    ## 145         Duffy ~~           NO2 2.577000e+00    0.001    0.001    0.014
    ## 144         Duffy ~~            EC 2.213000e+00    0.005    0.005    0.028
    ## 122   BloodVolume ~~          PM10 1.953000e+00   -0.008   -0.008   -0.032
    ## 130           BMI ~~       MaxTemp 1.870000e+00    0.001    0.001    0.006
    ## 47        DonChar =~          PM25 1.680000e+00    0.003    0.001    0.001
    ## 140         Duffy ~~       MaxTemp 1.640000e+00    0.002    0.002    0.005
    ## 142         Duffy ~~          PM25 1.386000e+00    0.003    0.003    0.013
    ## 152 TimeSincePrev ~~          PM10 1.249000e+00    0.003    0.003    0.014
    ## 180       MaxTemp ~~           NO2 1.226000e+00    0.002    0.002    0.011
    ## 149 TimeSincePrev ~~       MaxTemp 1.170000e+00   -0.001   -0.001   -0.005
    ## 58       Donation =~       MaxTemp 1.121000e+00    0.011    0.002    0.003
    ## 121   BloodVolume ~~          PM25 1.027000e+00   -0.005   -0.005   -0.026
    ## 143         Duffy ~~          PM10 9.300000e-01    0.003    0.003    0.010
    ## 107           Age ~~       MaxTemp 6.610000e-01   -0.003   -0.003   -0.003
    ## 179       MaxTemp ~~            EC 5.050000e-01    0.004    0.004    0.015
    ## 157     NumBefore ~~       MaxTemp 2.570000e-01   -0.003   -0.003   -0.007
    ## 50        DonChar =~           NO2 2.010000e-01    0.000    0.000    0.000
    ## 49        DonChar =~            EC 1.250000e-01   -0.001    0.000    0.000
    ## 174    PopDensity ~~            EC 8.800000e-02    0.002    0.002    0.008
    ## 162     NumBefore ~~           NO2 4.500000e-02    0.001    0.001    0.007
    ## 123   BloodVolume ~~            EC 2.500000e-02    0.001    0.001    0.007
    ## 141         Duffy ~~            O3 7.000000e-03    0.000    0.000    0.001
    ##     sepc.nox
    ## 76  4418.355
    ## 106   -0.222
    ## 65        NA
    ## 77     0.091
    ## 116   -1.791
    ## 112   -0.270
    ## 185    1.417
    ## 91    -1.232
    ## 57     0.287
    ## 111   -0.511
    ## 67        NA
    ## 108    0.182
    ## 184   -0.618
    ## 104    0.335
    ## 187   -0.902
    ## 110   -0.213
    ## 129   -0.079
    ## 175    0.242
    ## 167    0.231
    ## 118   -0.222
    ## 109   -0.215
    ## 66        NA
    ## 75        NA
    ## 56    -0.053
    ## 128   -0.065
    ## 71        NA
    ## 135   -0.124
    ## 105    0.055
    ## 182    0.373
    ## 93     0.123
    ## 165    0.106
    ## 99    -0.212
    ## 95     0.170
    ## 115    0.172
    ## 127   -0.188
    ## 98    -0.429
    ## 133   -0.123
    ## 189   -0.414
    ## 171    0.094
    ## 61     0.037
    ## 81     0.024
    ## 82     0.049
    ## 160   -0.411
    ## 137   -0.158
    ## 172   -0.152
    ## 134   -0.200
    ## 90     0.116
    ## 166    0.136
    ## 41    -0.035
    ## 42    -0.072
    ## 168   -0.221
    ## 73        NA
    ## 79     0.019
    ## 96    -0.186
    ## 132   -0.096
    ## 59     0.026
    ## 64        NA
    ## 126    0.041
    ## 150    0.066
    ## 97    -0.147
    ## 153   -0.161
    ## 188   -0.571
    ## 72        NA
    ## 154   -0.071
    ## 103   -0.032
    ## 156   -0.146
    ## 131    0.043
    ## 159   -0.227
    ## 155    0.070
    ## 92    -0.037
    ## 46     0.003
    ## 62    -0.016
    ## 178   -0.054
    ## 43    -0.003
    ## 136    0.020
    ## 176   -0.032
    ## 69        NA
    ## 70        NA
    ## 48     0.002
    ## 190    0.284
    ## 60     0.013
    ## 120   -0.062
    ## 139    0.013
    ## 173    0.049
    ## 138   -0.014
    ## 158   -0.085
    ## 80     0.007
    ## 68        NA
    ## 74        NA
    ## 177   -0.034
    ## 148   -0.013
    ## 181    0.082
    ## 63    -0.008
    ## 45     0.001
    ## 169   -0.027
    ## 161    0.160
    ## 147   -0.010
    ## 186    0.199
    ## 183   -0.113
    ## 94     0.015
    ## 151   -0.027
    ## 119   -0.016
    ## 117    0.016
    ## 124    0.034
    ## 145    0.014
    ## 144    0.028
    ## 122   -0.032
    ## 130    0.006
    ## 47     0.001
    ## 140    0.005
    ## 142    0.013
    ## 152    0.014
    ## 180    0.011
    ## 149   -0.005
    ## 58     0.003
    ## 121   -0.026
    ## 143    0.010
    ## 107   -0.003
    ## 179    0.015
    ## 157   -0.007
    ## 50     0.000
    ## 49     0.000
    ## 174    0.008
    ## 162    0.007
    ## 123    0.007
    ## 141    0.001

The modification indices suggest the same relations as the residual
covariances, and also include suggestions for new relations between
latent constructs and observed variables.

A high residual covariance between two observed variables that load on
the same latent construct (e.g. BMI and Age, BMI and BloodVolume)
indicates that these variables are more tightly correlated to each other
than they are to the other variables related to that construct. A high
residual covariance between two observed variables that are not related
to the same construct suggests that maybe they do share an underlying
latent construct.

We choose not to incorporate new parameters based on the residual
covariances and modification indices, because the overall model fit is
reasonable and the residual covariances are not much higher than the
threshold for consideration. Most suggested relations to add between
variables that do not load on the same latent construct include age,
which may be explained by a non-uniform geographical distribution of age
among donors. E.g. perhaps most young donors are students living in
cities (higher pollution), while donors living are ‘rural’ areas are
more likely to be older. However, it does not make theoretical sense to
say that the observed variable Age should be related to the latent
construct Pollution.

Finally, we inspect the factor loadings (convergent validity) and
covariances between latent constructs (discriminant validity).

``` r
inspect(fitA_n, what="est")[c('lambda', 'psi')]
```

    ## $lambda
    ##             DonChr   Env Polltn
    ## Sex          1.000 0.000  0.000
    ## Age         -0.618 0.000  0.000
    ## BloodVolume -7.119 0.000  0.000
    ## BMI         -0.552 0.000  0.000
    ## Duffy        0.000 0.000  0.000
    ## SES          0.000 1.000  0.000
    ## PopDensity   0.000 3.948  0.000
    ## MaxTemp      0.000 0.053  0.000
    ## O3           0.000 0.000  1.000
    ## PM25         0.000 0.000 -0.680
    ## PM10         0.000 0.000 -0.847
    ## EC           0.000 0.000 -0.916
    ## NO2          0.000 0.000 -0.233
    ## 
    ## $psi
    ##           DonChr Env    Polltn
    ## DonChar    0.039              
    ## Env        0.001 -0.020       
    ## Pollution -0.015 -0.158  3.985

``` r
inspect(fitA_r, what="est")[c('lambda', 'psi')]
```

    ## $lambda
    ##               DonChr  Donatn   Env Polltn
    ## Sex            1.000   0.000 0.000  0.000
    ## Age           -0.894   0.000 0.000  0.000
    ## BloodVolume   -1.746   0.000 0.000  0.000
    ## BMI           -0.256   0.000 0.000  0.000
    ## Duffy          0.077   0.000 0.000  0.000
    ## TimeSincePrev  0.000   1.000 0.000  0.000
    ## NumBefore      0.000 -10.397 0.000  0.000
    ## SES            0.000   0.000 1.000  0.000
    ## PopDensity     0.000   0.000 1.751  0.000
    ## MaxTemp        0.000   0.000 0.007  0.000
    ## O3             0.000   0.000 0.000  1.000
    ## PM25           0.000   0.000 0.000 -0.661
    ## PM10           0.000   0.000 0.000 -0.800
    ## EC             0.000   0.000 0.000 -0.851
    ## NO2            0.000   0.000 0.000 -0.206
    ## 
    ## $psi
    ##           DonChr Donatn Env    Polltn
    ## DonChar    0.178                     
    ## Donation   0.044  0.045              
    ## Env        0.011  0.002 -0.015       
    ## Pollution -0.066  0.020 -0.260  4.112

We notice that the latent factors Environment and Pollution have a
relatively high covariance in both new and repeat donors. The second
hypothesized model combines the variables in both constructs into one
construct, and this high covariance suggests that the other model might
have a better fit. The Duffy variable doesn’t add any value to the model
for new donors and very little for repeat donors.

## Model B

We assess model B the same way as model A. Model B contains one fewer
construct: the Environment and Pollution constructs are combined into
one construct which we simply call Environment.

<!--html_preserve-->

<div id="htmlwidget-4a38f5e6c0256d8d4d1b" class="grViz html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-4a38f5e6c0256d8d4d1b">{"x":{"diagram":"\n      digraph boxes_and_circles {\n      \n      graph [layout = neato, overlap=scale]\n      \n      node [shape = oval]\n      \"Donor characteristics\"; Donations; Environment\n      \n      node [shape = box]\n      Soot; \"PM2.5\"; PM10; Ozone; \"Population density\"; Temperature; SES;\n      \"Number of donations\"; \"Time since previous donation\"; Sex; Age; \n      \"Blood volume\"; BMI; Duffy; Ferritin [penwidth = 3]\n      \n      \"Donor characteristics\"->Sex\n      \"Donor characteristics\"->Age\n      \"Donor characteristics\"->\"Blood volume\" \n      \"Donor characteristics\"->BMI\n      \"Donor characteristics\"->Duffy\n      \n      Donations->\"Number of donations\"\n      Donations->\"Time since previous donation\"\n      \n      Environment->\"Population density\"\n      Environment->Temperature\n      Environment->SES\n      Environment->Soot\n      Environment->\"PM2.5\"\n      Environment->PM10\n      Environment->Ozone\n      \n      \"Donor characteristics\"->Ferritin\n      Donations->Ferritin\n      Environment->Ferritin\n      }\n      \n      \n      ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

``` r
modelB_n <- 'DonChar   =~ Sex + Age + BloodVolume + BMI + Duffy
             Env       =~ SES + PopDensity + MaxTemp + O3 + PM25 + PM10 + EC + NO2'

fitB_n <- cfa(modelB_n, data=data_n, check.gradient=FALSE, estimator="WLSMV")
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

``` r
summary(fitB_n, standardized=TRUE, fit.measures=TRUE)
```

    ## lavaan 0.6-6 ended normally after 245 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         27
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                         51947       59657
    ##                                                                   
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                              9606.363    8009.563
    ##   Degrees of freedom                                64          64
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.204
    ##   Shift parameter                                           29.514
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                            336633.336  162862.647
    ##   Degrees of freedom                                78          78
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  2.067
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.972       0.951
    ##   Tucker-Lewis Index (TLI)                       0.965       0.941
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.054       0.049
    ##   90 Percent confidence interval - lower         0.053       0.048
    ##   90 Percent confidence interval - upper         0.054       0.050
    ##   P-value RMSEA <= 0.05                          0.000       0.978
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.047       0.047
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   DonChar =~                                                            
    ##     Sex               1.000                               0.195    0.414
    ##     Age              -0.612    0.016  -37.892    0.000   -0.119   -0.107
    ##     BloodVolume      -7.282    0.257  -28.386    0.000   -1.417   -1.742
    ##     BMI              -0.552    0.007  -83.156    0.000   -0.107   -0.265
    ##     Duffy             0.000    0.006    0.060    0.952    0.000    0.000
    ##   Env =~                                                                
    ##     SES               1.000                               0.065    0.053
    ##     PopDensity        4.810    0.461   10.429    0.000    0.313    0.419
    ##     MaxTemp           0.069    0.052    1.336    0.182    0.004    0.006
    ##     O3              -30.744    2.902  -10.593    0.000   -1.998   -0.810
    ##     PM25             20.892    1.953   10.698    0.000    1.358    0.921
    ##     PM10             26.005    2.432   10.692    0.000    1.690    0.916
    ##     EC               28.145    2.640   10.660    0.000    1.829    0.989
    ##     NO2               7.172    0.675   10.628    0.000    0.466    0.901
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   DonChar ~~                                                            
    ##     Env               0.000    0.000    7.353    0.000    0.038    0.038
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Sex               0.183    0.002  120.436    0.000    0.183    0.828
    ##    .Age               1.221    0.008  156.339    0.000    1.221    0.988
    ##    .BloodVolume      -1.347    0.068  -19.906    0.000   -1.347   -2.036
    ##    .BMI               0.153    0.002   98.002    0.000    0.153    0.930
    ##    .Duffy             0.141    0.001  130.055    0.000    0.141    1.000
    ##    .SES               1.505    0.011  138.567    0.000    1.505    0.997
    ##    .PopDensity        0.458    0.010   46.356    0.000    0.458    0.824
    ##    .MaxTemp           0.567    0.003  192.537    0.000    0.567    1.000
    ##    .O3                2.090    0.014  148.762    0.000    2.090    0.344
    ##    .PM25              0.329    0.003  113.335    0.000    0.329    0.151
    ##    .PM10              0.544    0.007   77.794    0.000    0.544    0.160
    ##    .EC                0.073    0.004   17.958    0.000    0.073    0.021
    ##    .NO2               0.050    0.000  138.192    0.000    0.050    0.188
    ##     DonChar           0.038    0.001   26.469    0.000    1.000    1.000
    ##     Env               0.004    0.001    5.319    0.000    1.000    1.000

``` r
modelB_r <- 'DonChar   =~ Sex + Age + BloodVolume + BMI + Duffy
             Env       =~ SES + PopDensity + MaxTemp + O3 + PM25 + PM10 + EC + NO2
             Donation  =~ TimeSincePrev + NumBefore'

fitB_r <- cfa(modelB_r, data=data_r, check.gradient=FALSE, estimator="WLSMV")
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

``` r
summary(fitB_r, standardized=TRUE, fit.measures=TRUE)
```

    ## lavaan 0.6-6 ended normally after 128 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         33
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                         64388       78355
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                              17121.130   15550.986
    ##   Degrees of freedom                                 87          87
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.104
    ##   Shift parameter                                            39.066
    ##        simple second-order correction                              
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                            508861.577  242456.142
    ##   Degrees of freedom                               105         105
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  2.099
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.967       0.936
    ##   Tucker-Lewis Index (TLI)                       0.960       0.923
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.055       0.053
    ##   90 Percent confidence interval - lower         0.054       0.052
    ##   90 Percent confidence interval - upper         0.056       0.053
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.050       0.050
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   DonChar =~                                                            
    ##     Sex               1.000                               0.423    0.845
    ##     Age              -0.885    0.015  -60.879    0.000   -0.374   -0.255
    ##     BloodVolume      -1.745    0.010 -176.807    0.000   -0.738   -0.889
    ##     BMI              -0.254    0.004  -67.518    0.000   -0.108   -0.290
    ##     Duffy             0.077    0.005   16.756    0.000    0.033    0.074
    ##   Env =~                                                                
    ##     SES               1.000                               0.124    0.111
    ##     PopDensity        1.805    0.078   23.120    0.000    0.224    0.393
    ##     MaxTemp          -0.005    0.024   -0.193    0.847   -0.001   -0.001
    ##     O3              -16.379    0.667  -24.568    0.000   -2.028   -0.834
    ##     PM25             10.820    0.434   24.929    0.000    1.340    0.936
    ##     PM10             13.100    0.526   24.915    0.000    1.622    0.920
    ##     EC               13.935    0.563   24.753    0.000    1.726    0.977
    ##     NO2               3.369    0.137   24.619    0.000    0.417    0.891
    ##   Donation =~                                                           
    ##     TimeSincePrev     1.000                               0.213    0.510
    ##     NumBefore       -10.386    0.147  -70.463    0.000   -2.208   -1.041
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   DonChar ~~                                                            
    ##     Env               0.004    0.000   15.018    0.000    0.082    0.082
    ##     Donation          0.044    0.001   60.643    0.000    0.494    0.494
    ##   Env ~~                                                                
    ##     Donation         -0.001    0.000   -9.241    0.000   -0.042   -0.042
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Sex               0.071    0.001   67.889    0.000    0.071    0.285
    ##    .Age               2.019    0.008  247.721    0.000    2.019    0.935
    ##    .BloodVolume       0.144    0.003   49.830    0.000    0.144    0.210
    ##    .BMI               0.126    0.001  119.229    0.000    0.126    0.916
    ##    .Duffy             0.195    0.001  238.930    0.000    0.195    0.995
    ##    .SES               1.227    0.009  139.950    0.000    1.227    0.988
    ##    .PopDensity        0.274    0.005   58.540    0.000    0.274    0.846
    ##    .MaxTemp           0.548    0.003  212.316    0.000    0.548    1.000
    ##    .O3                1.802    0.012  151.746    0.000    1.802    0.305
    ##    .PM25              0.253    0.002  118.562    0.000    0.253    0.124
    ##    .PM10              0.474    0.005  102.675    0.000    0.474    0.153
    ##    .EC                0.142    0.004   37.201    0.000    0.142    0.045
    ##    .NO2               0.045    0.000  149.896    0.000    0.045    0.206
    ##    .TimeSincePrev     0.129    0.003   45.145    0.000    0.129    0.740
    ##    .NumBefore        -0.380    0.071   -5.361    0.000   -0.380   -0.085
    ##     DonChar           0.179    0.001  170.071    0.000    1.000    1.000
    ##     Env               0.015    0.001   12.368    0.000    1.000    1.000
    ##     Donation          0.045    0.001   48.666    0.000    1.000    1.000

For new donors, TLI is 0.941 and RMSEA 0.049 (90% CI 0.048-0.050). For
repeat donors, TLI is 0.923 and RMSEA 0.053 (90% CI 0.052-0.053). These
are acceptable model fits. We check the residuals:

``` r
residuals(fitB_n, type="cor")
```

    ## $type
    ## [1] "cor.bollen"
    ## 
    ## $cov
    ##             Sex    Age    BldVlm BMI    Duffy  SES    PpDnst MaxTmp O3    
    ## Sex          0.000                                                        
    ## Age         -0.019  0.000                                                 
    ## BloodVolume -0.011 -0.045  0.000                                          
    ## BMI          0.071  0.220 -0.002  0.000                                   
    ## Duffy       -0.012 -0.016  0.000 -0.001  0.000                            
    ## SES          0.011  0.052 -0.004 -0.063 -0.004  0.000                     
    ## PopDensity  -0.022 -0.115 -0.011 -0.068  0.015 -0.110  0.000              
    ## MaxTemp     -0.012  0.018  0.005  0.013  0.003 -0.009 -0.006  0.000       
    ## O3           0.033  0.067 -0.011  0.039 -0.011  0.070  0.040 -0.011  0.000
    ## PM25        -0.035 -0.018  0.012 -0.039  0.016  0.063 -0.049  0.000  0.003
    ## PM10        -0.034 -0.027  0.009 -0.052  0.015  0.085  0.004 -0.007  0.045
    ## EC          -0.036 -0.057  0.004 -0.063  0.014 -0.017  0.005  0.002 -0.007
    ## NO2         -0.030 -0.080 -0.004 -0.081  0.008 -0.017  0.101 -0.002 -0.088
    ##             PM25   PM10   EC     NO2   
    ## Sex                                    
    ## Age                                    
    ## BloodVolume                            
    ## BMI                                    
    ## Duffy                                  
    ## SES                                    
    ## PopDensity                             
    ## MaxTemp                                
    ## O3                                     
    ## PM25         0.000                     
    ## PM10         0.128  0.000              
    ## EC           0.005 -0.022  0.000       
    ## NO2         -0.070 -0.038  0.009  0.000

``` r
residuals(fitB_r, type="cor")
```

    ## $type
    ## [1] "cor.bollen"
    ## 
    ## $cov
    ##               Sex    Age    BldVlm BMI    Duffy  SES    PpDnst MaxTmp O3    
    ## Sex            0.000                                                        
    ## Age            0.049  0.000                                                 
    ## BloodVolume   -0.018 -0.099  0.000                                          
    ## BMI            0.163  0.129  0.176  0.000                                   
    ## Duffy         -0.018 -0.065  0.029  0.007  0.000                            
    ## SES           -0.027  0.056  0.015 -0.059 -0.014  0.000                     
    ## PopDensity     0.008 -0.212 -0.022 -0.066  0.012 -0.084  0.000              
    ## MaxTemp        0.008 -0.003 -0.007  0.005  0.005 -0.023 -0.003  0.000       
    ## O3             0.049  0.096 -0.017  0.021  0.001  0.045  0.038 -0.016  0.000
    ## PM25          -0.035 -0.071  0.000 -0.031  0.004  0.042 -0.040 -0.008  0.009
    ## PM10          -0.031 -0.079 -0.002 -0.044  0.004  0.076  0.018 -0.015  0.043
    ## EC            -0.047 -0.104  0.004 -0.039  0.006 -0.033  0.003  0.004 -0.007
    ## NO2           -0.050 -0.116  0.010 -0.052  0.006 -0.006  0.097  0.006 -0.095
    ## TimeSincePrev  0.027 -0.025  0.032  0.031  0.016 -0.003  0.010 -0.004  0.032
    ## NumBefore     -0.047  0.079 -0.064 -0.041 -0.040  0.007 -0.052 -0.002 -0.015
    ##               PM25   PM10   EC     NO2    TmSncP NumBfr
    ## Sex                                                    
    ## Age                                                    
    ## BloodVolume                                            
    ## BMI                                                    
    ## Duffy                                                  
    ## SES                                                    
    ## PopDensity                                             
    ## MaxTemp                                                
    ## O3                                                     
    ## PM25           0.000                                   
    ## PM10           0.108  0.000                            
    ## EC             0.007 -0.023  0.000                     
    ## NO2           -0.071 -0.038  0.014  0.000              
    ## TimeSincePrev -0.010  0.003 -0.030 -0.028  0.000       
    ## NumBefore     -0.015 -0.035  0.012  0.005  0.000  0.000

A few residual covariances spark interest. For new donors:

  - BMI and Age (0.221)
  - PopDensity and Age (-0.115)
  - PM2.5 and PM10 (0.128)
  - NO2 and PopDensity (0.101)

For repeat donors: \* BMI and Sex (0.164), Age (0.128) and BloodVolume
(0.174) \* PopDensity and Age (-0.201) \* PM2.5 and PM10 (0.108) \* EC
and Age (-0.104) \* NO2 and Age (-0.117)

``` r
inspect(fitB_n, what="est")[c('lambda', 'psi')]
```

    ## $lambda
    ##             DonChr     Env
    ## Sex          1.000   0.000
    ## Age         -0.612   0.000
    ## BloodVolume -7.282   0.000
    ## BMI         -0.552   0.000
    ## Duffy        0.000   0.000
    ## SES          0.000   1.000
    ## PopDensity   0.000   4.810
    ## MaxTemp      0.000   0.069
    ## O3           0.000 -30.744
    ## PM25         0.000  20.892
    ## PM10         0.000  26.005
    ## EC           0.000  28.145
    ## NO2          0.000   7.172
    ## 
    ## $psi
    ##         DonChr Env  
    ## DonChar 0.038       
    ## Env     0.000  0.004

``` r
inspect(fitB_r, what="est")[c('lambda', 'psi')]
```

    ## $lambda
    ##               DonChr     Env  Donatn
    ## Sex            1.000   0.000   0.000
    ## Age           -0.885   0.000   0.000
    ## BloodVolume   -1.745   0.000   0.000
    ## BMI           -0.254   0.000   0.000
    ## Duffy          0.077   0.000   0.000
    ## SES            0.000   1.000   0.000
    ## PopDensity     0.000   1.805   0.000
    ## MaxTemp        0.000  -0.005   0.000
    ## O3             0.000 -16.379   0.000
    ## PM25           0.000  10.820   0.000
    ## PM10           0.000  13.100   0.000
    ## EC             0.000  13.935   0.000
    ## NO2            0.000   3.369   0.000
    ## TimeSincePrev  0.000   0.000   1.000
    ## NumBefore      0.000   0.000 -10.386
    ## 
    ## $psi
    ##          DonChr Env    Donatn
    ## DonChar   0.179              
    ## Env       0.004  0.015       
    ## Donation  0.044 -0.001  0.045
