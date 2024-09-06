# 2. Instala y carga la librería socviz, en esta librería se encuentra el conjunto de datos “organdata”, 
#carga esta base datos y realiza el siguiente ejercicio. (Para conocer el significado de las variables 
#solo ejecuta en la consola la instrucción help(organdata) y te llevara a su descriptor, es necesario 
#cargar la librería primero).

#Esta base de datos contiene información de 17 países de la OCDE sobre trasplantes de órganos realizados
#entre 1991 y 2002. Además, contiene información sobre muertes ocurridas, población y PIB.#

install.packages("tseries")
install.packages("socviz")
install.packages("lmtest")
install.packages("sandwich")
install.packages("dplyr")
install.packages("modelr")
install.packages("tidyr")
install.packages("MuMIn")
install.packages("MASS")


library(lmtest)
library(tseries)
library(dplyr)
library(socviz)
library(sandwich)
library(modelr)
library(tidyr)
library(MuMIn)
library(MASS)

data("organdata")
help("organdata")

###############
# Modelos AIC #
###############
#####

#a) Realiza la propuesta de un modelo de regresión que sirva para explicar la relación multiples que exista
#entre la tasa de donación de órganos y las variables que consideres son importantes.
###########################################################################################################
  # Eliminar filas con valores faltantes -- Se necesita
  organdata_clean <- organdata %>% drop_na()
  
  # Convertir columnas numéricas de notación científica a formato numérico normal
  organdata_clean <- organdata_clean %>%
    mutate(across(where(is.numeric), ~ as.numeric(format(., scientific = FALSE))))

  # Eliminar columnas no numéricas del conjunto de datos
  organdata_numeric <- organdata_clean[, sapply(organdata_clean, is.numeric)]
  
  # Calcular la matriz de correlación
  correlation_matrix <- cor(organdata_numeric)


colnames(organdata_numeric)
options(scipen = 999)


#####
#1
#####
#b) Estima la ecuación propuesta (Ecuación de regresión con sus variables). lol
modelo1 <- lm(donors ~ pubhealth, data = organdata_clean)
modelo1

summary(modelo1)

#c) Analiza los residuales para determinar que no se violen los supuestos, y en caso de ser necesario,
#realiza las correcciones pertinentes.
######################################################################################################

###Revision de supuestos
residuos1 <- organdata_clean<- organdata_clean %>%
  add_residuals(modelo1)

# Agregar residuales al conjunto de datos
organdata_clean <- organdata_clean %>% add_residuals(modelo1, var = "residuos1")

hist(organdata_clean$residuos1)

par(mfrow=c(2,2))
plot(modelo1)

###Prueba de normalidad
jarque.bera.test(na.omit(organdata_clean$residuos1))

##Prueba de varianza lol
bptest(modelo1)

organdata_clean <- organdata_clean %>%mutate(lgdp=log(roads))                   #Transformar una variable con logaritmo.

summary(modelo1)


################################################################################
organdata_clean <- organdata_clean %>% add_residuals(modelo1, var = "residuos2")#Eliminación de la columna 'resid' y actualización del conjunto de datos con residuales del modelo

hist(organdata_clean$residuos1, 
     main="Histograma de organdata$residuos1", 
     col="#8AC641", 
     xlab="organdata$residuos1", 
     ylab="Frecuencia")

par(mfrow=c(2,2))
# Residuals vs Fitted
plot(modelo1, which = 1, 
     main="Residuos vs Ajustados", 
     col="#8AC641")

# Q-Q plot
plot(modelo1, which = 2, 
     main="Q-Q Residuos", 
     col="#8AC641")

# Scale-Location
plot(modelo1, which = 3, 
     main="Escala-Ubicación", 
     col="#8AC641")

# Residuals vs Leverage
plot(modelo1, which = 5, 
     main="Residuos vs Apalancamiento", 
     col="#8AC641")



################################################################################
jarque.bera.test(na.omit(organdata_clean$resid))

bptest(modelo1)

summary(modelo1)

coeftest(modelo1, vcov = vcovHC(modelo1, type="HC1"))                           #Prueba de significancia de los coeficientes

#####
#2
#####
#b) Estima la ecuación propuesta (Ecuación de regresión con sus variables). lol
modelo2 <- lm(donors ~ external, data = organdata_clean)
modelo2

summary(modelo2)


#c) Analiza los residuales para determinar que no se violen los supuestos, y en caso de ser necesario,
#realiza las correcciones pertinentes.
######################################################################################################

###Revision de supuestos
residuos2 <- organdata_clean<- organdata_clean %>%
  add_residuals(modelo2)

# Agregar residuales al conjunto de datos
organdata_clean <- organdata_clean %>%
  add_residuals(modelo2, var = "residuos2")

hist(organdata_clean$residuos2, main = "Histograma de organdata_clean$residuos2", xlab = "organdata_clean$residuos2")

par(mfrow=c(2,2))
plot(modelo2)

###Prueba de normalidad
jarque.bera.test(na.omit(organdata_clean$residuos2))

##Prueba de varianza lol
bptest(modelo2)

organdata_clean <- organdata_clean %>%mutate(lgdp=log(roads))                   #Transformar una variable con logaritmo.

summary(modelo2)


################################################################################
organdata_clean <- organdata_clean %>% add_residuals(modelo2, var = "residuos2del2")#Eliminación de la columna 'resid' y actualización del conjunto de datos con residuales del modelo

hist(organdata_clean$residuos2,
     main="Histograma de organdata$residuos2",
     col="#8AC641", 
     xlab="organdata$residuos2", 
     ylab="Frecuencia")

par(mfrow=c(2,2))
# Residuals vs Fitted
plot(modelo2, which = 1, 
     main="Residuos vs Ajustados", 
     col="#8AC641")

# Q-Q plot
plot(modelo2, which = 2, 
     main="Q-Q Residuos", 
     col="#8AC641")

# Scale-Location
plot(modelo2, which = 3, 
     main="Escala-Ubicación", 
     col="#8AC641")

# Residuals vs Leverage
plot(modelo2, which = 5, 
     main="Residuos vs Apalancamiento", 
     col="#8AC641")


################################################################################
jarque.bera.test(na.omit(organdata_clean$resid))

bptest(modelo2)


summary(modelo2)

coeftest(modelo2, vcov = vcovHC(modelo2, type="HC1"))                           #Prueba de significancia de los coeficientes


#####
#3
#####
#b) Estima la ecuación propuesta (Ecuación de regresión con sus variables). lol
modelo3 <- lm(donors ~ pop + pop_dens + gdp + gdp_lag + health + health_lag + pubhealth + roads + cerebvas + assault + external + txp_pop, data = organdata_clean, na.action = na.fail)
modelo3

summary(modelo3)

# Calcular el AIC para el modelo
AIC (modelo3)

# Realizar la selección de modelos usando dredge
cand.md <- dredge(modelo3)
summary(cand.md)
print(cand.md)

# Realizar la selección de modelos usando stepAIC
modelo3Final = stepAIC(modelo3, direction="both")
summary(modelo3Final)


################################################################################

###Revision de supuestos
organdata_clean <- organdata_clean %>%
  mutate(residuos3 = residuals(modelo3Final))

hist(organdata_clean$residuos3)

###Prueba de normalidad
jarque.bera.test(na.omit(organdata_clean$residuos3))

##Prueba de varianza lol
bptest(modelo3Final)

organdata_clean <- organdata_clean %>% mutate(lgdp = log(roads))                #Transformar una variable con logaritmo.

summary(modelo3Final)


################################################################################
organdata_clean <- organdata_clean %>%
  mutate(residuos2 = residuals(modelo3Final))                                   #Eliminación de la columna 'resid' y actualización del conjunto de datos con residuales del modelo

hist(organdata_clean$residuos3,
     main="Histograma de organdata$residuos2",
     col="#8AC641", 
     xlab="organdata$residuos2", 
     ylab="Frecuencia")

par(mfrow=c(2,2))
# Residuals vs Fitted
plot(modelo3Final, which = 1, 
     main="Residuos vs Ajustados", 
     col="#8AC641")

# Q-Q plot
plot(modelo3Final, which = 2, 
     main="Q-Q Residuos", 
     col="#8AC641")

# Scale-Location
plot(modelo3Final, which = 3, 
     main="Escala-Ubicación", 
     col="#8AC641")

# Residuals vs Leverage
plot(modelo3Final, which = 5, 
     main="Residuos vs Apalancamiento", 
     col="#8AC641")


################################################################################
jarque.bera.test(na.omit(organdata_clean$residuos2))

bptest(modelo3Final)

summary(modelo3Final)

coeftest(modelo3Final, vcov = vcovHC(modelo3Final, type="HC1"))                           #Prueba de significancia de los coeficientes
