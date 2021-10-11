# Base de datos y paquetes ####
install.packages("TeachingDemos")
library(TeachingDemos)
library(readxl)
Ceramica <- read_excel("Ceramica.xlsx")

# Base para test de medias y varianzas####
Pomaire <- Ceramica[Ceramica$Site == "Pomaire", ]
Quinchamali <- Ceramica[Ceramica$Site == "Quinchamali", ]

# Bases para test de proporciones
Y1 <- ifelse(Ceramica$Site == "Pomaire",1,0)
exitos1 <- sum(Y1)
n1 <- length(Y1)

Y2 <- ifelse(Ceramica$Site == "Quinchamali",1,0)
exitos2 <- sum(Y2)
n2 <- length(Y2)

# Test hipotesis media ####
mean(Pomaire$Al)
  # Varianza poblacional desconocida:####
t.test(Pomaire$Al,alternative = "two.sided",conf.level = 0.95, mu = 17) # Realiza el test para mu = 17
# Si es igual a 17

t.test(Pomaire$Al,alternative = "less",mu = 18, conf.level = 0.95) # Realiza el test para mu < 18
# No es menor a 18

t.test(Pomaire$Al,alternative = "greater", mu = 16, conf.level = 0.95) # Realiza el test para mu > 16
# No es mayor a 16

Test_1 <- t.test(Pomaire$Al,alternative = "two.sided",conf.level = 0.95)
as.numeric(Test_1$conf.int) # Entrega el intervalo deconfianza

  # Varianza poblacional conocida:####
sigma <- sd(Quinchamali$Al) # Varianza "poblacional"

z.test(Quinchamali$Al, sd = sigma, alternative = "two.sided",conf.level = 0.95, mu = 17) # test con varianza POBLACIONAL conocida
# No es igual a 17

Test_2 <-z.test(Quinchamali$Al, sd = sigma, alternative = "two.sided", conf.level = 0.95)
as.numeric(Test_2$conf.int) # Entrega el intervalo deconfianza
# Test de proporciones ####
prop.test(exitos1, n1, alternative = "two.sided", conf.level = 0.95) 

# Si no se coloca valor de p
# H0: P = 0.5 | H1: P NO ES 0.5
# Se asume criterio de varianza maxima (p=0.5)
# La proporcion de ceramicas de Quinchamali es igual a 0.5

Test_3 <- prop.test(exitos1,  n1, alternative = "two.sided",conf.level = 0.95)
as.numeric(Test_3$conf.int)

# Test de varianza ####
sigma.test(Pomaire$Al, alternative = "two.sided", conf.level = 0.95) # Test de una varianza
# La varianza no es igual a 1

Test_4 <- sigma.test(Pomaire$Al, alternative = "two.sided",conf.level = 0.95)
as.numeric(Test_4$conf.int) # Entrega el intervalo deconfianza
# Comparacion de 2 muestras ####
# Varianza:
var.test(Pomaire$Al, Quinchamali$Al, alternative = "two.sided",conf.level = 0.95) # test de 2 varianzas
# Tienen varainzas iguales

# Medias con varainzas iguales:
t.test(Pomaire$Al, Quinchamali$Al, alternative = "two.sided",conf.level = 0.95, var.equal = TRUE)
# Las medias no son iguales

# Medias con varianzas distintas:
t.test(Pomaire$Al,Quinchamali$Al, alternative = "two.sided",conf.level = 0.95, var.equal = FALSE)
# OJO! Este resultado no es correcto porque no tienen varainzas iguales (SOLO EJEMPLO)

# Proporciones de dos muestras:
prop.test(x = c(exitos1, exitos2), n = c(n1, n2), alternative = "two.sided", conf.level = 0.95)
# Las proporciones nos son iguales

prop.test(x = c(exitos1, exitos2), n = c(n1, n2), alternative = "greater", conf.level = 0.95)
# La proporcion de ceramicas de Quinchamali es mayor que las de Pomaire
