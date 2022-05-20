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


base_entrenamiento2 <- base_entrenamiento %>% mutate_all (~ ( scale (.)%>% as.vector ))
base_testeo2 <- base_testeo %>% mutate_all (~ ( scale (.)%>% as.vector ))
base_validacion2 <- base_validacion %>% mutate_all (~ ( scale (.)%>% as.vector ))


# Pregunta 1 --------------------------------------------------------------
# Ocupamos la base de validacion para calibrar el valor de R

matriz_validacion = as.matrix(base_validacion2)

descomposicion = svd(matriz_validacion)

vector_ECM = c()

for (i in 1:76) {
  matriz_nueva = descomposicion$u[1:i,1:i]%*%diag(descomposicion$d[1:i]) %*% t(descomposicion$v[1:i,1:i])
  
  
}
