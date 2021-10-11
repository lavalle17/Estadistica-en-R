Test de hipotesis
================
C. Lavalle
11-10-2021

## Instalar librerias

``` r
install.packages("TeachingDemos")
```

# Cargar librerias

``` r
library(TeachingDemos)
```

    ## Warning: package 'TeachingDemos' was built under R version 4.1.1

``` r
library(readxl)
```

# Cargar base de datos

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

# Base para test de medias y varianzas

``` r
Pomaire <- Ceramica[Ceramica$Site == "Pomaire", ]
Quinchamali <- Ceramica[Ceramica$Site == "Quinchamali", ]
```

# Bases para test de proporciones

``` r
Y1 <- ifelse(Ceramica$Site == "Pomaire",1,0)
exitos1 <- sum(Y1)
n1 <- length(Y1)

Y2 <- ifelse(Ceramica$Site == "Quinchamali",1,0)
exitos2 <- sum(Y2)
n2 <- length(Y2)
```

# Test hipotesis media

``` r
mean(Pomaire$Al)
```

    ## [1] 12.56429

# Varianza poblacional desconocida

``` r
t.test(Pomaire$Al,alternative = "two.sided",conf.level = 0.95, mu = 17) # Realiza el test para mu = 17 
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

Si es igual a 17

``` r
t.test(Pomaire$Al,alternative = "less",mu = 18, conf.level = 0.95) # Realiza el test para mu < 18
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

No es menor a 18

``` r
t.test(Pomaire$Al,alternative = "greater", mu = 16, conf.level = 0.95) # Realiza el test para mu > 16
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

No es mayor a 16

``` r
Test_1 <- t.test(Pomaire$Al,alternative = "two.sided",conf.level = 0.95)
as.numeric(Test_1$conf.int) # Entrega el intervalo deconfianza
```

    ## [1] 11.76919 13.35938

# Varianza poblacional conocida

``` r
sigma <- sd(Quinchamali$Al) # Varianza "poblacional"
```

``` r
z.test(Quinchamali$Al, sd = sigma, alternative = "two.sided",conf.level = 0.95, mu = 17) # test con varianza POBLACIONAL conocida
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

No es igual a 17

``` r
Test_2 <-z.test(Quinchamali$Al, sd = sigma, alternative = "two.sided", conf.level = 0.95)
as.numeric(Test_2$conf.int) # Entrega el intervalo deconfianza
```

    ## [1] 16.62383 19.73617

# Test de proporciones

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

Si no se coloca valor de p H0: P = 0.5 | H1: P NO ES 0.5 Se asume
criterio de varianza maxima (p=0.5) La proporcion de ceramicas de
Quinchamali es igual a 0.5

``` r
Test_3 <- prop.test(exitos1,  n1, alternative = "two.sided",conf.level = 0.95)
as.numeric(Test_3$conf.int)
```

    ## [1] 0.3374768 0.7286162

# Test de varianza

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

# Comparacion de 2 muestras

## Varianza

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

## Medias con varianzas iguales

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

## Medias con varianzas distintas

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

OJO\! Este resultado no es correcto porque no tienen varainzas iguales
(SOLO EJEMPLO)

## Proporciones de dos muestras

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
