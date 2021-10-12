Test de hipotesis
================
C. Lavalle
11-10-2021

1.  [Instalar librerias](#id1)  
2.  [Cargar librerias](#id2)  
3.  [Cargar base de datos](#id3)  
4.  [Test de 1 muestra](#id4)  
    4.1 Varianza poblacional desconocida  
    4.2 Varianza poblacional conocida  
    4.3 Test de proporcione  
5.  [Test de 2 muestras](#id5)  
    5.1 Varianza  
    5.2 Medias con varianzas iguales  
    5.3 Medias con varianzas distintas  
    5.4 Proporciones de dos muestras

<div id='id1' />

## 1\. Instalar librerias

``` r
install.packages("TeachingDemos")
```

<div id='id2' />

## 2\. Cargar librerias

``` r
library(TeachingDemos)
library(readxl)
```

<div id='id3' />

## 3\. Cargar base de datos

### a. General

``` r
Ceramica <- read_excel("Ceramica.xlsx")
head(Ceramica)
```

    ## # A tibble: 6 x 6
    ##   Site       Al    Fe    Mg    Ca    Na
    ##   <chr>   <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Pomaire  14.4  7     4.3   0.15  0.51
    ## 2 Pomaire  13.8  7.08  3.43  0.12  0.17
    ## 3 Pomaire  14.6  7.09  3.88  0.13  0.2 
    ## 4 Pomaire  11.5  6.37  5.64  0.16  0.14
    ## 5 Pomaire  13.8  7.06  5.34  0.2   0.2 
    ## 6 Pomaire  10.9  6.26  3.47  0.17  0.22

Este conjunto de datos corresponde a mediciones de metales en ceramicos
de distintas zonas

### b. Test de medias y varianzas

``` r
Pomaire <- Ceramica[Ceramica$Site == "Pomaire", ]
head(Pomaire)
```

    ## # A tibble: 6 x 6
    ##   Site       Al    Fe    Mg    Ca    Na
    ##   <chr>   <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Pomaire  14.4  7     4.3   0.15  0.51
    ## 2 Pomaire  13.8  7.08  3.43  0.12  0.17
    ## 3 Pomaire  14.6  7.09  3.88  0.13  0.2 
    ## 4 Pomaire  11.5  6.37  5.64  0.16  0.14
    ## 5 Pomaire  13.8  7.06  5.34  0.2   0.2 
    ## 6 Pomaire  10.9  6.26  3.47  0.17  0.22

``` r
Quinchamali <- Ceramica[Ceramica$Site == "Quinchamali", ]
head(Quinchamali)
```

    ## # A tibble: 5 x 6
    ##   Site           Al    Fe    Mg    Ca    Na
    ##   <chr>       <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Quinchamali  18.3  1.28  0.67  0.03  0.03
    ## 2 Quinchamali  15.8  2.39  0.63  0.01  0.04
    ## 3 Quinchamali  18    1.5   0.67  0.01  0.06
    ## 4 Quinchamali  18    1.88  0.68  0.01  0.04
    ## 5 Quinchamali  20.8  1.51  0.72  0.07  0.1

### c. Test de proporciones

``` r
Y1 <- ifelse(Ceramica$Site == "Pomaire",1,0)
exitos1 <- sum(Y1)
n1 <- length(Y1)

Y2 <- ifelse(Ceramica$Site == "Quinchamali",1,0)
exitos2 <- sum(Y2)
n2 <- length(Y2)
```

Se generan proporciones para el test de proporciones, mediante la
funcion ifelse() se asginan valores 1 si la zona de observacion es
pomaire, 0 en su defecto. debido a que pomaire se asigna como 1, su suma
(sum()) es el total de registros cuya zona es igual a pomaire, el
comando length() mide el numero de observaciones, asi la division de las
variables “exitos” y “n” nos da la proporcion de observaciones
pertenecientes a pomaire.

<div id='id4' />

## 4\. Test de 1 muestra

``` r
mean(Pomaire$Al)
```

    ## [1] 12.56429

``` r
boxplot(Pomaire$Al)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### a. Varianza poblacional desconocida

Argumentos t.test():  
\- datos para el analisis  
\- la hipotesis alternativa  
\- nivel de confianza  
\- media a comparar

#### 1\. Two.sided

``` r
t.test(Pomaire$Al,alternative = "two.sided",conf.level = 0.95, mu = 17)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Pomaire$Al
    ## t = -12.052, df = 13, p-value = 1.983e-08
    ## alternative hypothesis: true mean is not equal to 17
    ## 95 percent confidence interval:
    ##  11.76919 13.35938
    ## sample estimates:
    ## mean of x 
    ##  12.56429

  - H1: media muestral \!= media poblacional  
  - p-value \< significancia, se rechaza H0  
  - H1: media muestral \!= media poblacional (17)

#### 2\. Less

``` r
t.test(Pomaire$Al,alternative = "less",mu = 18, conf.level = 0.95)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Pomaire$Al
    ## t = -14.769, df = 13, p-value = 8.355e-10
    ## alternative hypothesis: true mean is less than 18
    ## 95 percent confidence interval:
    ##      -Inf 13.21605
    ## sample estimates:
    ## mean of x 
    ##  12.56429

  - H1: media muestral \< media poblacional
  - p-value \< significancia, se rechaza H0
  - H1: media muestral \< media poblacional (18)

#### 3\. Greater

``` r
t.test(Pomaire$Al,alternative = "greater", mu = 16, conf.level = 0.95)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Pomaire$Al
    ## t = -9.3352, df = 13, p-value = 1
    ## alternative hypothesis: true mean is greater than 16
    ## 95 percent confidence interval:
    ##  11.91252      Inf
    ## sample estimates:
    ## mean of x 
    ##  12.56429

  - H1: media muestral \> media poblacional
  - p-value \> significancia, se acepta H0
  - no existe evidencia de que la media muestral sea significativamente
    superior a la media poblacional (16)

#### 4\. Intervalo de confianza

``` r
Test_1 <- t.test(Pomaire$Al,alternative = "two.sided",conf.level = 0.95)
as.numeric(Test_1$conf.int)
```

    ## [1] 11.76919 13.35938

### b. Varianza poblacional conocida

Argumentos z.test():  
\- datos para el analisis  
\- desviacion estandar poblacional  
\- hipotesis alternativa  
\- nivel de confianza  
\- media a comparar

#### 1\. Calculo de la desviacion estandar

``` r
sigma <- sd(Quinchamali$Al) # desviacion estandar "poblacional"
```

#### 2\. Test con varianza poblacional conocida

``` r
z.test(Quinchamali$Al, sd = sigma, alternative = "two.sided",conf.level = 0.95, mu = 17) 
```

    ## 
    ##  One Sample z-test
    ## 
    ## data:  Quinchamali$Al
    ## z = 1.4862, n = 5.00000, Std. Dev. = 1.77539, Std. Dev. of the sample
    ## mean = 0.79398, p-value = 0.1372
    ## alternative hypothesis: true mean is not equal to 17
    ## 95 percent confidence interval:
    ##  16.62383 19.73617
    ## sample estimates:
    ## mean of Quinchamali$Al 
    ##                  18.18

  - H1: media muestral \!= media poblacional
  - p-value \> significancia, se acepta H0
  - H0: media muestral == media poblacional (17)

#### 3\. Intervalo de confianza

``` r
Test_2 <-z.test(Quinchamali$Al, sd = sigma, alternative = "two.sided", conf.level = 0.95)
as.numeric(Test_2$conf.int) # Entrega el intervalo deconfianza
```

    ## [1] 16.62383 19.73617

### c. Test de proporciones

  - exitos1: total de registros cuya zona es igual a pomaire  
  - n1: numero total de registros

<!-- end list -->

``` r
prop.test(exitos1, n1, alternative = "two.sided", conf.level = 0.95) 
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  exitos1 out of n1, null probability 0.5
    ## X-squared = 0.038462, df = 1, p-value = 0.8445
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.3374768 0.7286162
    ## sample estimates:
    ##         p 
    ## 0.5384615

  - Si no se coloca valor de p = 0.5  
  - H0: P == 0.5 | H1: P \!= 0.5  
  - p-value \> significancia, se acepta H0  
  - La proporcion de ceramicas de Quinchamali es igual a 0.5

### Intervalo de confianza

``` r
Test_3 <- prop.test(exitos1,  n1, alternative = "two.sided",conf.level = 0.95)
as.numeric(Test_3$conf.int)
```

    ## [1] 0.3374768 0.7286162

### d. Test de varianza

``` r
sigma.test(Pomaire$Al, alternative = "two.sided", conf.level = 0.95) # Test de una varianza
```

    ## 
    ##  One sample Chi-squared test for variance
    ## 
    ## data:  Pomaire$Al
    ## X-squared = 24.652, df = 13, p-value = 0.05127
    ## alternative hypothesis: true variance is not equal to 1
    ## 95 percent confidence interval:
    ##  0.9966258 4.9218149
    ## sample estimates:
    ## var of Pomaire$Al 
    ##          1.896319

La varianza no es igual a 1

``` r
Test_4 <- sigma.test(Pomaire$Al, alternative = "two.sided",conf.level = 0.95)
as.numeric(Test_4$conf.int) # Entrega el intervalo deconfianza
```

    ## [1] 0.9966258 4.9218149

<div id='id5' />

## 5\. Test de 2 muestras

``` r
mean(Pomaire$Al); mean(Quinchamali$Al)
```

    ## [1] 12.56429

    ## [1] 18.18

``` r
boxplot(Pomaire$Al, Quinchamali$Al)
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### a. Varianza

``` r
var.test(Pomaire$Al, Quinchamali$Al, alternative = "two.sided",conf.level = 0.95) # test de 2 varianzas
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  Pomaire$Al and Quinchamali$Al
    ## F = 0.60162, num df = 13, denom df = 4, p-value = 0.4366
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.06903318 2.40402766
    ## sample estimates:
    ## ratio of variances 
    ##          0.6016239

Tienen varianzas iguales

### b. Medias con varianzas iguales

``` r
t.test(Pomaire$Al, Quinchamali$Al, alternative = "two.sided",conf.level = 0.95, var.equal = TRUE)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  Pomaire$Al and Quinchamali$Al
    ## t = -7.2808, df = 17, p-value = 1.284e-06
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -7.243023 -3.988405
    ## sample estimates:
    ## mean of x mean of y 
    ##  12.56429  18.18000

Las medias no son iguales

### c. Medias con varianzas distintas

``` r
t.test(Pomaire$Al,Quinchamali$Al, alternative = "two.sided",conf.level = 0.95, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  Pomaire$Al and Quinchamali$Al
    ## t = -6.417, df = 5.8209, p-value = 0.000762
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -7.773151 -3.458278
    ## sample estimates:
    ## mean of x mean of y 
    ##  12.56429  18.18000

OJO\! Este es solo un ejemplo de uso del script

### d. Proporciones de dos muestras

``` r
prop.test(x = c(exitos1, exitos2), n = c(n1, n2), alternative = "two.sided", conf.level = 0.95)
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  c(exitos1, exitos2) out of c(n1, n2)
    ## X-squared = 5.3078, df = 1, p-value = 0.02123
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  0.06342272 0.62888497
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.5384615 0.1923077

Las proporciones nos son iguales

``` r
prop.test(x = c(exitos1, exitos2), n = c(n1, n2), alternative = "greater", conf.level = 0.95)
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  c(exitos1, exitos2) out of c(n1, n2)
    ## X-squared = 5.3078, df = 1, p-value = 0.01061
    ## alternative hypothesis: greater
    ## 95 percent confidence interval:
    ##  0.1026948 1.0000000
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.5384615 0.1923077

La proporcion de ceramicas de Quinchamali es mayor que las de Pomaire
