import pandas as pd

def sample_rows_per_year(input_csv, output_csv, start_year=2013, end_year=2023, n=450):
    # Leemos el archivo CSV
    df = pd.read_csv(input_csv)
    
    # Creamos un DataFrame vacío para acumular los resultados
    df_sampled = pd.DataFrame()
    
    # Recorremos cada año en el rango deseado
    for year in range(start_year, end_year + 1):
        # Filtramos filas para ese año
        df_year = df[df['Year'] == year]
        
        # Nos aseguramos de que existan suficientes filas
        if len(df_year) >= n:
            # Tomamos una muestra aleatoria de n filas
            df_sampled_year = df_year.sample(n=n, random_state=42)
        else:
            # Si el año no llega a 300 filas, definimos qué hacer.
            # Aquí, por ejemplo, podríamos tomar todas las filas disponibles para ese año.
            # Ajusta según tu necesidad.
            df_sampled_year = df_year
        
        # Concatenamos al DataFrame final
        df_sampled = pd.concat([df_sampled, df_sampled_year], ignore_index=True)
    
    # Guardamos la muestra resultante a un nuevo CSV
    df_sampled.to_csv(output_csv, index=False)

# Ejecución de ejemplo
sample_rows_per_year(
    input_csv='ventas.csv',
    output_csv='muestra.csv',
    start_year=2013,
    end_year=2023,
    n=450
)
