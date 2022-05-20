library(readr)
library(dplyr)
library(R.matlab)
library(stats)
library(mgcv)
library(car)
library(MASS)
library(faraway)
library(tidyverse)
library(DataExplorer)
library(scales)
library(corrr)
library(glmnet)
library(pls)




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

base_entrenamiento = base[indice_entrenamiento,]
base_testeo = base[indice_test,]

ridge = lm.ridge( Adj_close~ . , data=base_entrenamiento, lambda = exp(c(1,2,3,4,5,6,7,8,9,10,12,13)))
plot(ridge, type = "l")

x_train <- model.matrix(Adj_close~., data = base_entrenamiento)[, -1]
y_train <- base_entrenamiento$Adj_close

x_test = model.matrix(Adj_close~., data = base_testeo)[, -1]
y_test = base_testeo$Adj_close

modelo_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda     = seq(1,100,0.5),
  standardize = TRUE
)

regularizacion <- modelo_ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_ridge$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")


# 3) ----------------------------------------------------------------------


x_train <- model.matrix(Adj_close~., data = base_entrenamiento)[, -1]
y_train <- base_entrenamiento$Adj_close



modelo_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda     = seq(1,50,0.5),
  standardize = TRUE
)

regularizacion <- modelo_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

# 4) ----------------------------------------------------------------------

cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)



# 5) ----------------------------------------------------------------------


cv_error2 <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error2)
paste("Mejor valor de lambda encontrado:", cv_error2$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error2$lambda.1se)


# 6) ----------------------------------------------------------------------

modelo_ridge_final = glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda     = cv_error$lambda.min,
  standardize = TRUE
)
predicciones_test <- predict(modelo_ridge_final, newx = x_test)

test_mse_ridge <- mean((predicciones_test - y_test)^2)
paste("Error (mse) de test:", test_mse_ridge)




modelo_lasso_final = glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda     = cv_error2$lambda.min,
  standardize = TRUE
)
predicciones_test2 <- predict(modelo_lasso_final, newx = x_test)

test_mse_lasso <- mean((predicciones_test2 - y_test)^2)
paste("Error (mse) de test:", test_mse_lasso)


plot(y_test)
lines(predicciones_test2)
