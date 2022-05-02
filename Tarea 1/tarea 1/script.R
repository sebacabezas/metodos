library(R.matlab)

datos <- readMat("represa-1.mat")

regresion1 = lm(datos$ytest~datos$Xtest)

plot(datos$ytest, datos$Xtest)
abline(regresion1$coefficients[[1]], regresion1$coefficients[[2]])


# B) ----------------------------------------------------------------------

# Modelo nulo
lm(datos$)
