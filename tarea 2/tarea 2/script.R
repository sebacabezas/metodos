library(dplyr)
library(stats)




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

base_entrenamiento = as.data.frame(base[indice_entrenamiento,])
base_testeo = as.data.frame(base[indice_test,])
base_validacion = as.data.frame(base[indice_val,])
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

matriz_nueva2 = descomposicion2$u%*%diag(descomposicion2$d)
regresion_u = lm(adj_close3~. , data = cbind(adj_close3,as.data.frame(matriz_nueva2)))
sum(regresion_u$residuals^2)
