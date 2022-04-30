import numpy as np
import pandas as pd
from mat4py import loadmat

import matplotlib.pyplot as plt
from matplotlib import style
import seaborn as sns

from scipy.stats import pearsonr
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy import stats

# Configuracion plots
plt.rcParams['image.cmap'] = "bwr"
#plt.rcParams['figure.dpi'] = "100"
plt.rcParams['savefig.bbox'] = "tight"
style.use('ggplot') or plt.style.use('ggplot')
##############################################


data = loadmat('represa-1.mat')
# diccionario con X, Y, Xtest, ytest, Xval, yval

datos = pd.DataFrame({"X": data["X"], "Y": data["y"]})
datos_test = pd.DataFrame({"Xtest": data["Xtest"], "Ytest": data["ytest"]})
datos_val = pd.DataFrame({"Xval": data["Xval"], "Yval": data["yval"]})

modelo_lineal = LinearRegression()
modelo_lineal.fit(X = data["Xtest"], y = data["ytest"])

print("Intercepto:", modelo_lineal.intercept_)
print("Coeficientes:", modelo_lineal.coef_)

print(datos_test.head())