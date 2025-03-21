'''
original data set, num of cols per year(2013-2023):

2013: 20490 -> 9.06348%
2014: 20532 -> 9.08206%
2015: 20800 -> 9.20060%
2016: 20848 -> 9.22184%
2017: 20668 -> 9.14222%
2018: 20336 -> 8.99536%
2019: 20482 -> 9.05994%
2020: 20366 -> 9.00863%
2021: 20350 -> 9.00155%
2022: 20530 -> 9.08117%
2023: 20670 -> 9.14310%

Total: 226K
____________________________________

reduced data set por MD:

2013: 453
2014: 454
2015: 460
2016: 461
2017: 457
2018: 450
2019: 453
2020: 451
2021: 450
2022: 454
2023: 457

Total: 5000
'''

import pandas as pd


def sample_rows_per_year(input_csv, output_csv, start_year=2013, end_year=2023, sample_sizes=None):
    if sample_sizes is None:
        raise ValueError(
            "Se requiere un diccionario con el número de muestras por año.")

    # Leemos el archivo CSV
    df = pd.read_csv(input_csv)

    # Creamos un DataFrame vacío para acumular los resultados
    df_sampled = pd.DataFrame()

    # Recorremos cada año en el rango deseado
    for year in range(start_year, end_year + 1):
        # Filtramos filas para ese año
        df_year = df[df['Year'] == year]

        # Determinamos el número de muestras para este año
        n = sample_sizes.get(year, 450)  # Si no hay valor, usa 450 por defecto

        # Nos aseguramos de que existan suficientes filas
        if len(df_year) >= n:
            # Tomamos una muestra aleatoria de n filas
            df_sampled_year = df_year.sample(n=n, random_state=42)
        else:
            # Si el año no tiene suficientes filas, tomamos todas las disponibles
            df_sampled_year = df_year

        # Concatenamos al DataFrame final
        df_sampled = pd.concat(
            [df_sampled, df_sampled_year], ignore_index=True)

    # Guardamos la muestra resultante a un nuevo CSV
    df_sampled.to_csv(output_csv, index=False)


# Diccionario con el número de muestras por año
sample_sizes = {
    2013: 453,
    2014: 454,
    2015: 460,
    2016: 461,
    2017: 457,
    2018: 450,
    2019: 453,
    2020: 451,
    2021: 450,
    2022: 454,
    2023: 457
}

# Ejecución de ejemplo
sample_rows_per_year(
    input_csv='bike_sales_data_world_2013_2023.csv',
    output_csv='reduced_data.csv',
    start_year=2013,
    end_year=2023,
    sample_sizes=sample_sizes
)
