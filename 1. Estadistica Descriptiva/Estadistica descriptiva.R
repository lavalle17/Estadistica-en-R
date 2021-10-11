# instalar librerias
install.packages("modeest") # Instala el paquete que permite calcular la moda
install.packages("moments") # Instala el paquete para poder calcular los estadisticos de forma
library(modeest) # Carga el paquete que permite calcular la moda
library(readxl) # Carga base datos
library(moments) # Carga el paquete para poder calcular los estadisticos de forma

# nuevo anuncio

# Cargar bases de datos ####
base <- read_excel("Base de datos/Base Litiasis.xls")

# Filtros ####
base[base$FUMA == 1, ] # Selecciona solo a las personas que fuman

fumadores <- base[base$FUMA == 1, ] # Selecciona a los fumadores y los guarda como un objeto

menores_de_35 <- base[base$EDAD <= 35, ] # Selecciona todos los menores o iguales a 35 años

peso_menores_de_35 <- base$PESO[base$EDAD <= 35] # Selecciona el peso de las personas menores a 35 años
peso_menores_de_35 <- base[base$EDAD <= 35,]$PESO # Selecciona el peso de las personas menores a 35 años

## *** Recomendacion *** ##
base_filtrada <- base[base$FUMA == 1, ] # Primero filtrar la base y guardar como objeto
Peso_fumadores <- base_filtrada$PESO # Luego seleccionar la variable deseada

# Estadisticos de centro ####
# Media
mean(base$COLHDL) # Entrega el promedio del colesterol HDL de los pacientes
diabetes <- base[base$DIABETES == 1, ]
mean(diabetes$COLHDL) # Entrega el promedio del colesterol HDL de los pacientes con diabetes

# Mediana
median(base$TRIGLIC) # Entrega la mediana de los trigliceridos de los pacientes
pacientes_positivos <- base[base$LITIASIS == "SI", ] # Selecciona solo personas con Litiasis
median(pacientes_positivos$TRIGLIC) # Entrega la mediana de los trigliceridos de los pacientes con litiasis

# Moda
mfv(base$TALLA) # Entrega la (o las) modas de la Talla

# Estadisticos de  posicion ####
quantile(base$COLTOT) # Entrega  los cuarteles de la variable colesterol total
quantile(base$COLHDL, prob=seq(0,1,length=101)) # Entrega todos los percentiles de colesterol total

# Estadisticos de variabilidad ####
# Rango
range(fumadores$EDAD) # Entrega le rango de la edad de los fumadores

# Rango intercuartil
edad_fumadores <- fumadores$EDAD # Selecciona la edad de los fumadores
IQR(edad_fumadores) # Entrega el RIC de la edad de los fumadores

# Desviacion estandar
sd(diabetes$COLTOT) # Entrega la desviacion estandar en el colesterol total de las personas con diabetes

# Varianza
var(diabetes$COLTOT) # Entrega la varainza en el colesterol total de las personas con diabetes
sd(diabetes$COLTOT)^2 # La varianza es el cuadrado de la desviacion estandar

# Coeficiente de variacion 
sd(diabetes$COLTOT)/mean(diabetes$COLTOT) # Entrega el coeficiente de varaicion

# Estadisticos de forma ####
# Coeficiente de asimetria
skewness(base$COLHDL) # Coeficiente de asimetria

# Curtosis
kurtosis(base$COLHDL) # Calcula la curtosis

# Tablas ####

# Tabla simple
tabla_ulcera_1 <- table(base$ULCERA) # Crea una tabla que cuenta personas con ulceras
tabla_ulcera_1 # Muestra la tabla 1

# Tabla de doble entrada
tabla_ulcera_2 <- table(base$ULCERA, base$FUMA) # Cuenta personas que fuman y con ulceras
tabla_ulcera_2 # Muestra la tabla 2

# Sumas totales
addmargins(tabla_ulcera_2) # Agrega las sumas totales
addmargins(tabla_ulcera_2, margin = 1) # Agrega las sumas por COLUMNAS (ULCERA en este caso)
addmargins(tabla_ulcera_2, margin = 2) # Agrega las sumas por FILAS (FUMA en este caso)

# Tabla de proporciones
prop.table(tabla_ulcera_2) # Agrega las proporciones totales
prop.table(tabla_ulcera_2, margin = 1) # Agrega las proporciones por FILAS (FUMA en este caso)
prop.table(tabla_ulcera_2, margin = 2) # # Agrega las proporciones por COLUMNAS (ULCERA en este caso)

# Graficos ####

# Histogramas
hist(base$TALLA) # Genera el histograma de la Talla

# Grafico de cajas
boxplot(base$PESO) # Entrega el grafico de caja del peso
boxplot(base$PESO ~ base$GENERO)# Entrega el grafico de caja del peso por genero

# Grafico circular
pie(table(base$FUMA)) # Entrega el grafico circular de los fumadores

# Grafico de barras
barplot(table(base$FUMA,base$GENERO))

# Grafico de barras apilado
tabla_genero <- table(base$FUMA,base$GENERO)
barplot(prop.table(tabla_genero,margin = 2))

# Graficos de dispercion
plot(base$PAD ~ base$PAS)
