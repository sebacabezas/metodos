library(readr)
library(dplyr)
library(R.matlab)
library(stats)
library(mgcv)
library(car)
library(MASS)




# 1) ----------------------------------------------------------------------



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



# 2) ----------------------------------------------------------------------

base = base[,c(6:dim(base)[2])]
colnames(base)[1] = "Adj_close"

ridge = lm.ridge( Adj_close~ . , data=base, lambda = c(12,3,5,7,10,15,20,30,40,50,60,70,80,90,100,200,300,500,1000,2000))
plot(ridge)

