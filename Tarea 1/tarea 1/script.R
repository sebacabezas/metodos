library(R.matlab)
library(stats)
library(mgcv)
library(car)
library(MASS)


datos <- readMat("represa-1.mat")

regresion1 = lm(datos$y~datos$X)

plot(datos$X,datos$y)
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


for (i in 2:length(datos$X)) {
  data_temp = base_poly[1:i,]
  reg = lm(y~., data = data_temp)
  pred1 = predict(reg, base_poly[1:(i),])
  pred2 = predict(reg, base_poly_2)
  
  ECM1 = c(ECM1, mean((pred1 - datos$y[1:i])^2))
  ECM2 = c(ECM2, mean((pred2 - datos$yval)^2))
}



plot(seq(1,12),  ECM1, xlim = c(0,12), ylim = c(0,250))
lines(ECM2)




# F) ----------------------------------------------------------------------

# Lambda = 10
modelo_ridge <- lm.ridge( y~ ., data=base_poly, lambda = 10/28)

grilla = seq(-50,50,length=1000)
plot(datos$X, datos$y)
lines(grilla, coef(modelo_ridge)[[1]] + coef(modelo_ridge)[[2]]*grilla + coef(modelo_ridge)[[3]]*grilla^2 +
        coef(modelo_ridge)[[4]]*grilla^3 + coef(modelo_ridge)[[5]]*grilla^4 + 
        coef(modelo_ridge)[[6]]*grilla^5 + coef(modelo_ridge)[[7]]*grilla^6 + 
        coef(modelo_ridge)[[8]]*grilla^7 + coef(modelo_ridge)[[9]]*grilla^8, col = "green")
  

# Lambda = 100
modelo_ridge2 <- lm.ridge( y~ ., data=base_poly, lambda = 100/28)

grilla = seq(-50,50,length=1000)
plot(datos$X, datos$y)
lines(grilla, coef(modelo_ridge2)[[1]] + coef(modelo_ridge2)[[2]]*grilla + coef(modelo_ridge2)[[3]]*grilla^2 +
        coef(modelo_ridge2)[[4]]*grilla^3 + coef(modelo_ridge2)[[5]]*grilla^4 + 
        coef(modelo_ridge2)[[6]]*grilla^5 + coef(modelo_ridge2)[[7]]*grilla^6 + 
        coef(modelo_ridge2)[[8]]*grilla^7 + coef(modelo_ridge2)[[9]]*grilla^8, col = "green")

# G) ----------------------------------------------------------------------


# ECM de modelo lineal con 1 predictor

data_ml1 = data.frame("y" = datos$y, "x" = datos$X)
data_testeo = data.frame("y" = datos$ytest, "x" = datos$Xtest)
ml1 = lm(y~x, data = data_ml1)
prediccion_mod1 = predict(ml1, data_testeo)
ECM_mod1 = mean((prediccion_mod1-datos$ytest)^2)


# ECM de modelo lineal polinomial

base_poly_test = poly(datos$Xtest, degree = 8, raw = TRUE, simple = TRUE)
ml2 = lm(y~., data = base_poly)
prediccion_mod2 = predict(ml2, base_poly_2)
mean((prediccion_mod2-datos$ytest)^2)


# ECM de modelo lineal polinomial con penalizacion lambda = 10



modelo_ridge <- lm.ridge( y~ ., data=base_poly, lambda = 10/28)
prediccion_ridge = as.matrix(cbind(rep(1,length(datos$Xtest)),as.matrix(base_poly_test[,]))) %*% coef(modelo_ridge)
ECM_ridge = mean((prediccion_ridge-datos$ytest)^2)
ECM_ridge













