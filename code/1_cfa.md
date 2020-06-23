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
data <- data[, c(1:12, 19, 22:32)]
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
                'Date_cos')
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

Variances range from 0.035 (EC) to 23207.106 (DaysSincePrev). We rescale
all variables to have a variance between 0.1-10.

``` r
data0 <- data
data <- data0

data$Age <- data$Age / 10                        # Age in decades
data$Ferritin <- data$Ferritin / 100             # Ferritin in 100ng/ml
data$TimeSincePrev <- data$TimeSincePrev / 365   # Time in years
data$BMI <- data$BMI / 10                        # BMI in 10kg/m^2
data$MaxTemp <- data$MaxTemp / 10                # Temperature in dC
data$PopDensity <- data$PopDensity / 100         # Population in 100 inhabitants/km^2
data$EC <- data$EC * 10                          # Soot in 10ug/m^3
data$NO2 <- data$NO2 / 10                        # NO2 in 10ug/m^3
data$Time <- data$Time / 10                      # Time in tens of hours

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

# Confirmatory Factor Analysis

Before considering the full model including the relations between latent
constructs, we carry out a confirmatory factor analysis (CFA). This is
the measurement part of the Structural Equation Model (SEM), which is
later followed by the structural part. The CFA tests whether the
observed variables that the researcher has chosen to measure the latent
construct do so consistently. We must first develop a hypothesis about
which latent constructs are underlying the measures that are available
in the data set.

In the end we will compare four similar models that differ slightly in
both the measurement part and the structural part, but contain the same
observed variables.

## Model A

<!--html_preserve-->

<div id="htmlwidget-66c869bf3519b8c3cc4d" class="grViz html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-66c869bf3519b8c3cc4d">{"x":{"diagram":"\n      digraph boxes_and_circles {\n      \n      graph [layout = neato, overlap=scale]\n      \n      node [shape = oval]\n      \"Donor characteristics\"; Donations; \"Physical environment\"; Pollution\n      \n      node [shape = box]\n      Soot; \"PM2.5\"; PM10; Ozone; \"Population density\"; Temperature; SES;\n      \"Number of donations\"; \"Time since previous donation\"; Sex; Age; \n      \"Blood volume\"; BMI; Duffy; Ferritin [penwidth = 3]\n      \n      \"Donor characteristics\"->Sex\n      \"Donor characteristics\"->Age\n      \"Donor characteristics\"->\"Blood volume\" \n      \"Donor characteristics\"-> BMI\n      \"Donor characteristics\"->Duffy\n      \n      Donations->\"Number of donations\"\n      Donations->\"Time since previous donation\"\n      \n      \"Physical environment\"->\"Population density\"\n      \"Physical environment\"->Temperature\n      \"Physical environment\"->SES\n      \n      Pollution->Soot\n      Pollution->\"PM2.5\"\n      Pollution->PM10\n      Pollution->Ozone\n      \n      \"Donor characteristics\"->Ferritin\n      Donations->Ferritin\n      \"Physical environment\"->Ferritin\n      Pollution->Ferritin\n      }\n      \n      \n      ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

Model A contains four latent constructs: Donor characteristics,
Donations, Physical environment and Pollution.

``` r
modelA <- 'DonChar   =~ Sex + Age + BloodVolume + BMI + Duffy
           Donations =~ NumBefore + TimeSincePrev + Time
           Env       =~ SES + PopDensity + MaxTemp
           Pollution =~ O3 + PM25 + PM10 + EC + NO2'
modelA_Donchar <- 'DonChar   =~ Sex + Age + BloodVolume + Duffy'

summary(cfa(modelA_Donchar, data=data))
```

    ## lavaan 0.6-6 ended normally after 28 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                          8
    ##                                                       
    ##   Number of observations                        138012
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                              1496.304
    ##   Degrees of freedom                                 2
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   DonChar =~                                          
    ##     Sex               1.000                           
    ##     Age              -0.867    0.011  -78.338    0.000
    ##     BloodVolume      -1.721    0.015 -112.280    0.000
    ##     Duffy             0.049    0.003   17.199    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Sex               0.062    0.002   38.579    0.000
    ##    .Age               2.362    0.009  260.318    0.000
    ##    .BloodVolume       0.155    0.005   32.698    0.000
    ##    .Duffy             0.174    0.001  262.589    0.000
    ##     DonChar           0.183    0.002  100.095    0.000

``` r
modelA_Donations <- 'Donations =~ NumBefore + YearsSincePrev + Time + Date_sin + Date_cos'
modelA_Env <- 'Env =~ SES + PopDensity + AvgTemp'
modelA_Pollution <- 'Pollution =~ O3 + PM25 + PM10 + EC + NO2'

fitA <- cfa(modelA, data=data, check.gradient=FALSE)
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated lv
    ## variances are negative

``` r
varTable(fitA)
```

    ##             name idx   nobs    type exo user   mean   var nlev lnam
    ## 1            Sex   3 138012 numeric   0    0  0.575 0.244    0     
    ## 2            Age   4 138012 numeric   0    0  4.014 2.499    0     
    ## 3    BloodVolume  10 138012 numeric   0    0  4.874 0.695    0     
    ## 4            BMI  11 138012 numeric   0    0  2.502 0.149    0     
    ## 5          Duffy  12 138012 numeric   0    0  0.775 0.174    0     
    ## 6      NumBefore   8 138012 numeric   0    0  2.327 6.745    0     
    ## 7  TimeSincePrev   7  77476 numeric   0    0  0.479 0.174    0     
    ## 8           Time  21 138012 numeric   0    0  1.542 0.123    0     
    ## 9            SES  14 137964 numeric   0    0 -0.099 1.305    0     
    ## 10    PopDensity  15 117580 numeric   0    0  0.877 0.436    0     
    ## 11       MaxTemp  13 137386 numeric   0    0  1.551 0.560    0     
    ## 12            O3  16 138012 numeric   0    0 46.935 6.377    0     
    ## 13          PM25  17 138012 numeric   0    0 11.112 2.192    0     
    ## 14          PM10  18 138012 numeric   0    0 18.366 3.383    0     
    ## 15            EC  19 138012 numeric   0    0  7.508 3.478    0     
    ## 16           NO2  20 138012 numeric   0    0  1.847 0.252    0

``` r
summary(fitA, fit.measures=TRUE)
```

    ## lavaan 0.6-6 ended normally after 205 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         38
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                         64388      138012
    ##                                                                   
    ## Model Test User Model:
    ##                                                         
    ##   Test statistic                              169243.245
    ##   Degrees of freedom                                  98
    ##   P-value (Chi-square)                             0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                            707515.508
    ##   Degrees of freedom                               120
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.761
    ##   Tucker-Lewis Index (TLI)                       0.707
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)           -1022957.515
    ##   Loglikelihood unrestricted model (H1)    -938335.892
    ##                                                       
    ##   Akaike (AIC)                             2045991.030
    ##   Bayesian (BIC)                           2046335.791
    ##   Sample-size adjusted Bayesian (BIC)      2046215.027
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.164
    ##   90 Percent confidence interval - lower         0.163
    ##   90 Percent confidence interval - upper         0.164
    ##   P-value RMSEA <= 0.05                          0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.082
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   DonChar =~                                          
    ##     Sex               1.000                           
    ##     Age               0.272    0.011   25.314    0.000
    ##     BloodVolume      -5.795    0.130  -44.503    0.000
    ##     BMI              -0.453    0.004 -109.044    0.000
    ##     Duffy             0.005    0.003    1.509    0.131
    ##   Donations =~                                        
    ##     NumBefore         1.000                           
    ##     TimeSincePrev    -0.098    0.005  -21.442    0.000
    ##     Time             -0.003    0.001   -4.944    0.000
    ##   Env =~                                              
    ##     SES               1.000                           
    ##     PopDensity        1.233    0.036   34.031    0.000
    ##     MaxTemp          -0.003    0.014   -0.226    0.822
    ##   Pollution =~                                        
    ##     O3                1.000                           
    ##     PM25             -0.744    0.002 -313.884    0.000
    ##     PM10             -0.899    0.003 -306.017    0.000
    ##     EC               -0.860    0.003 -285.046    0.000
    ##     NO2              -0.196    0.001 -230.562    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   DonChar ~~                                          
    ##     Donations        -0.028    0.001  -23.761    0.000
    ##     Env               0.001    0.000    3.038    0.002
    ##     Pollution        -0.013    0.001  -15.363    0.000
    ##   Donations ~~                                        
    ##     Env              -0.025    0.004   -6.918    0.000
    ##     Pollution        -0.099    0.016   -6.153    0.000
    ##   Env ~~                                              
    ##     Pollution        -0.313    0.009  -36.687    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Sex               0.195    0.002  124.489    0.000
    ##    .Age               2.155    0.012  179.692    0.000
    ##    .BloodVolume      -1.154    0.039  -29.903    0.000
    ##    .BMI               0.126    0.001  171.048    0.000
    ##    .Duffy             0.196    0.001  179.428    0.000
    ##    .NumBefore        -0.314    0.222   -1.413    0.158
    ##    .TimeSincePrev     0.128    0.002   57.299    0.000
    ##    .Time              0.116    0.001  179.430    0.000
    ##    .SES               1.263    0.007  170.321    0.000
    ##    .PopDensity        0.355    0.004   87.559    0.000
    ##    .MaxTemp           0.548    0.003  179.427    0.000
    ##    .O3                2.273    0.013  175.870    0.000
    ##    .PM25              0.036    0.001   48.746    0.000
    ##    .PM10              0.160    0.001  116.569    0.000
    ##    .EC                0.427    0.003  163.344    0.000
    ##    .NO2               0.080    0.000  175.531    0.000
    ##     DonChar           0.055    0.001   38.491    0.000
    ##     Donations         4.810    0.223   21.532    0.000
    ##     Env              -0.021    0.002  -10.571    0.000
    ##     Pollution         3.642    0.031  119.241    0.000
