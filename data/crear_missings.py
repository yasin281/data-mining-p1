import pandas as pd
import numpy as np

# Leer el archivo CSV
df = pd.read_csv("muestra.csv")

# Obtener número de filas y columnas
n_filas, n_columnas = df.shape

# Calcular el número total de celdas en el DataFrame
total_celdas = n_filas * n_columnas

# Verificar que haya suficientes celdas para insertar 300 missings
if total_celdas < 300:
    raise ValueError("El archivo no contiene suficientes celdas para insertar 300 missing values.")

# Seleccionar 300 índices únicos (índice plano) de celdas al azar
indices = np.random.choice(total_celdas, 300, replace=False)

# Convertir los índices planos a índices de fila y columna
filas, columnas = np.unravel_index(indices, (n_filas, n_columnas))

# Asignar np.nan a las celdas seleccionadas
df.values[filas, columnas] = np.nan

# Guardar el DataFrame modificado en un nuevo archivo CSV
df.to_csv("muestras_missing.csv", index=False)

print("Se han agregado 300 missing values de forma aleatoria en 'muestras_missing.csv'.")
