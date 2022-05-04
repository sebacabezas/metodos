library(R.matlab)
library(stats)
library(mgcv)


datos <- readMat("represa-1.mat")

regresion1 = lm(datos$y~datos$X)

plot(datos$y, datos$X)
abline(regresion1$coefficients[[1]], regresion1$coefficients[[2]])


# B) ----------------------------------------------------------------------

# Modelo nulo



ECM1 = c(0)
ECM2 = c(mean((rep(datos$y[1],length(datos$yval)) - datos$yval)^2))

for (i in 2:length(datos$X)) {
  x1 = datos$X[1:i]
  y1 = datos$y[1:i]
  reg = lm(y1~x1)
  pred1 = predict(reg, data.frame("x1" = datos$X[1:i]))
  pred2 = predict(reg, newdata = data.frame("x1" = datos$Xval))
  
  ECM1 = c(ECM1, mean((pred1 - datos$y[1:i])^2))
  ECM2 = c(ECM2, mean((pred2 - datos$yval)^2))

}

plot(seq(1,12),  ECM1, xlim = c(-0,16), ylim = c(0,300))
lines(ECM2)

# Podemos ver que convergen al mismo valor

# C) ----------------------------------------------------------------------

base_potencia_8 = poly(rbind(datos$X, datos$Xval, datos$Xtest), degree = 8, raw = TRUE, simple = TRUE)

c1 = colSums(base_potencia_8)
media_por_cols = apply(base_potencia_8,2, mean)
var_por_cols = apply(base_potencia_8,2,var)

base_potencia_8 = scale(base_potencia_8, center = FALSE, scale = c1)


# D) ----------------------------------------------------------------------

regresion_4 = lm(datos$y ~ poly(datos$X, degree = 8, raw = TRUE, simple = TRUE))

grilla = seq(-50,50,length=100000)

plot(datos$X, datos$y, ylim = c(-60,60))
lines(grilla, regresion_4$coefficients[[1]] + regresion_4$coefficients[[2]]*grilla + regresion_4$coefficients[[3]]*grilla^2 +
        regresion_4$coefficients[[4]]*grilla^3 + regresion_4$coefficients[[5]]*grilla^4 + 
        regresion_4$coefficients[[6]]*grilla^5 + regresion_4$coefficients[[7]]*grilla^6 + 
        regresion_4$coefficients[[8]]*grilla^7 + regresion_4$coefficients[[9]]*grilla^8, col = "green")



# E) ----------------------------------------------------------------------

# Modelo nulo


base_poly = as.data.frame(poly(datos$X, degree = 8, raw = TRUE, simple = TRUE))
base_poly = cbind(datos$y, base_poly)
colnames(base_poly) = c("y","x11","x12","x13","x14","x15","x16","x17","x18")
base_poly_2 = as.data.frame(poly(datos$Xval, degree = 8, raw = TRUE, simple = TRUE))
colnames(base_poly_2) = c("x11","x12","x13","x14","x15","x16","x17","x18")

ECM1 = c(0)
ECM2 = c(mean((rep(datos$y[1],length(datos$yval)) - datos$yval)^2))

#Modelos con menos de 9 datos

for (i in 2:8) {
  data_temp = base_poly[1:i,]
  reg = lm(y~., data = data_temp)
  pred1 = predict(reg, base_poly[1:(i),])
  pred2 = predict(reg, base_poly_2)
  
  ECM1 = c(ECM1, mean((pred1 - datos$y[1:i])^2))
  ECM2 = c(ECM2, mean((pred2 - datos$yval)^2))
  
}


#Modelo con 9 datos o más


for (i in 9:length(datos$X)) {
  x1 = datos$X[1:i]
  y1 = datos$y[1:i]
  reg = lm(y1~x1)
  pred1 = predict(reg, data.frame("x1" = datos$X[1:i]))
  pred2 = predict(reg, newdata = as.data.frame("x1" = datos$Xval))
  
  ECM1 = c(ECM1, mean((pred1 - datos$y[1:i])^2))
  ECM2 = c(ECM2, mean((pred2 - datos$yval)^2))
  
}

plot(seq(1,12),  ECM1, xlim = c(-0,16), ylim = c(0,300))
lines(ECM2)

# Podemos ver que convergen al mismo valor













lm(datos$y[c(1,2)] ~ poly(datos$X, degree = 8, raw = TRUE, simple = TRUE)[1:2,])

