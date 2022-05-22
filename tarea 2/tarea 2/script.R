library(dplyr)
library(stats)
library(readr)
library(MASS)
library(tidyr)
library(skimr)
library(ggplot2)
library(ggpubr)
library(tree)
library(rpart)
library(rpart.plot)
library(tidymodels)
library(ranger)
library(doParallel)
library(Rcpp)



set.seed(10)
base <- read_csv("FINAL_USO-1.csv")

cant_datos = dim(base)[1]

colnames(base)

indice = seq(1,1718)
indice_entrenamiento = sample(indice, 1032,replace = FALSE)
indice = indice[-indice_entrenamiento]
indice2 = seq(1,length(indice))
indice2 = sample(indice2, 343, replace = FALSE)
indice_val = indice[indice2]
indice_test = indice[-indice2]

base = base[,c(6:dim(base)[2])]
colnames(base)[1] = "Adj_close"

base_entrenamiento = data.frame(base[indice_entrenamiento,])
base_testeo = data.frame(base[indice_test,])
base_validacion = data.frame(base[indice_val,])
adj_close = base_validacion$Adj_close
adj_close2 = base_testeo$Adj_close
adj_close3 = base_entrenamiento$Adj_close


base_entrenamiento2 <- base_entrenamiento[,-1] %>% mutate_all (~ ( scale (.)%>% as.vector ))
base_testeo2 <- base_testeo[,-1] %>% mutate_all (~ ( scale (.)%>% as.vector ))
base_validacion2 <- base_validacion[,-1] %>% mutate_all (~ ( scale (.)%>% as.vector ))


# Pregunta 1.1 --------------------------------------------------------------
# Ocupamos la base de validacion para calibrar el valor de R

matriz_validacion = as.matrix(base_validacion2)

descomposicion = svd(matriz_validacion)

vector_ECM = c()


for (i in 1:75){
  if(i==1) {
    matriz_nueva = descomposicion$u[,1]*descomposicion$d[1]
    regresion = lm(adj_close~. , data = cbind(adj_close,as.data.frame(matriz_nueva)))
    error = sum(regresion$residuals^2)
    vector_ECM = c(vector_ECM, error)}
  else {
    matriz_nueva = descomposicion$u[,1:i]%*%diag(descomposicion$d[1:i])
    regresion = lm(adj_close~. , data = cbind(adj_close,as.data.frame(matriz_nueva)))
    error = sum(regresion$residuals^2)/(dim(matriz_nueva)[1])
    vector_ECM = c(vector_ECM, error)}
  }

plot(1:75, vector_ECM)
plot(3:75, vector_ECM[3:75])


# 1.2) --------------------------------------------------------------------

#Elegimos r = 30, ya que despues de este, el ECM baja relativamente poco

matriz_entrenamiento = as.matrix(base_entrenamiento2)
descomposicion2 = svd(matriz_entrenamiento)
matriz_nueva2 = descomposicion2$u[,1:30]%*%diag(descomposicion2$d[1:30])
regresion_u = lm(adj_close3~. , data = cbind(adj_close3,as.data.frame(matriz_nueva2)))


Xvalr = as.matrix(base_validacion2)%*%descomposicion2$v[,1:30]
ygorro = (Xvalr%*%(regresion_u$coefficients[2:31])) + regresion_u$coefficients[1]

ECM1_2 = mean((ygorro - adj_close)^2)




# 2) ----------------------------------------------------------------------




# A) ----------------------------------------------------------------------


skim(datos)

arbol_regresion1 <- rpart(
  formula = Adj_close~ .,
  data    = base_entrenamiento,
  method = 'anova',
  cp = 0)

summary(arbol_regresion1)
rpart.plot(arbol_regresion1)


#Podemos notar que no se aprecia nada bien, por lo que cambiaremos un poco el parametro "cp" para lograr identificar valores

arbol_regresion2 <- rpart(
  formula = Adj_close~ .,
  data    = base_entrenamiento,
  method = 'anova',
  cp = 0.0005)

summary(arbol_regresion2)
rpart.plot(arbol_regresion2)


# B) ----------------------------------------------------------------------
# Para esto, tomamos nuestro arbol mas completo para ir realizando cross validation y analizar el mejor valor del alpha
# esto lo calculamos con el menor valor de Xerror

minimo_valor = min(arbol_regresion1$cptable[,4])
minimo_valor = round(minimo_valor,6)
indice = which(round(as.numeric(arbol_regresion1$cptable[,4]),6) == minimo_valor)

valor_cp = arbol_regresion1$cptable[indice,1]

arbol_podado = rpart(
  formula = Adj_close~ .,
  data    = base_entrenamiento,
  method = 'anova',
  cp = valor_cp)

  

# C) ----------------------------------------------------------------------

#Exportamos un pdf con la imagen para poder apreciar todos los valores


pdf("tree.pdf")
rpart.plot(arbol_podado, type = 4)
dev.off()


prediccion_c = predict(arbol_podado, newdata = base_testeo[1,])

# Nos arroja un valor de 156.2525, el valor real es 157.16, lo cual es un valor bastante cercano


# 4) ----------------------------------------------------------------------

prediccion_d = predict(arbol_podado, newdata = base_testeo)

ECM2_4 = mean((base_testeo[,1]-prediccion_d)^2) # Valor de 3.68


# 5) ----------------------------------------------------------------------

random_forest  <- ranger(
  formula   = Adj_close ~ .,
  data      = base_entrenamiento,
  num.trees = 100,
  seed      = 123
)


predicciones_e = predict(
  random_forest,
  data = base_testeo
)

predicciones_e = predicciones_e$predictions
MSE_e = mean((predicciones_e - base_testeo$Adj_close)^2)



# 6) ----------------------------------------------------------------------

ECM1_2
ECM2_4
MSE_e


