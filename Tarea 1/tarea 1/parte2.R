library(readr)

set.seed(10)
base <- read_csv("FINAL_USO.csv")

cant_datos = dim(base)[1]
cant_datos*0.6

colnames(base)

indice = seq(1,1718)
indice_entrenamiento = sample(indice, 1032,replace = FALSE)
indice = indice[-indice_entrenamiento]
indice2 = seq(1,length(indice))
indice2 = sample(indice2, 343, replace = FALSE)
indice_val = indice[indice2]
indice_test = indice[-indice2]


