# Título: Procesamiento Estricto de 32 Estados (Sin Archivos Extra)
# Autor: Adaptado para Mtro. José César Romero Galván

library(jsonlite)
library(tidyverse)
library(scales)

# 1. CONFIGURACIÓN
# ----------------------------------------------------------
# Ajusta tu ruta aquí
setwd("/Users/cesarromero/Documents/Data_Civica/Proyectos/Partes/A/Base de datos")
files_path <- "/Users/cesarromero/Documents/Data_Civica/Proyectos/Partes/A/Base de datos"

# Vector oficial de 32 Estados (Orden INEGI)
state_names <- c(
  "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", 
  "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", 
  "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", 
  "México", "Michoacán", "Morelos", "Nayarit", "Nuevo León", 
  "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", 
  "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
  "Veracruz", "Yucatán", "Zacatecas"
)

final_list <- list()

# 2. BUCLE EXACTO (1 al 32)
# ----------------------------------------------------------
# En lugar de leer todo lo que hay en la carpeta, forzamos un ciclo del 1 al 32.

for (i in 1:32) {
  
  # Construimos el nombre del archivo esperado: "datos_estado_1.json", etc.
  file_name <- paste0("datos_estado_", i, ".json")
  full_path <- file.path(files_path, file_name)
  
  # Obtenemos el nombre del estado correspondiente al índice i
  state_name <- state_names[i]
  
  # Verificamos si el archivo existe antes de intentar leerlo
  if (file.exists(full_path)) {
    
    message(paste("Procesando:", state_name, "(Archivo:", file_name, ")"))
    
    tryCatch({
      json_data <- fromJSON(full_path)
      age_section <- json_data$por_edad
      
      # Función de procesamiento (Igual que antes)
      process_sex <- function(age_list, sex_label) {
        if (is.null(age_list) || length(age_list) == 0) return(NULL)
        
        df <- enframe(unlist(age_list), name = "Edad_Raw", value = "Total")
        df %>%
          mutate(
            Sexo = sex_label,
            Total = as.numeric(Total),
            Age_Num = suppressWarnings(as.numeric(Edad_Raw))
          ) %>%
          filter(!is.na(Age_Num))
      }
      
      df_men <- process_sex(age_section$Hombres, "Hombre")
      df_women <- process_sex(age_section$Mujeres, "Mujer")
      df_state_temp <- bind_rows(df_men, df_women)
      
      if (nrow(df_state_temp) > 0) {
        df_state_temp <- df_state_temp %>%
          mutate(
            Grupo_Edad = case_when(
              Age_Num >= 0 & Age_Num <= 9   ~ "Infancias (0-9)",
              Age_Num >= 10 & Age_Num <= 19 ~ "Adolescentes (10-19)",
              Age_Num >= 20 & Age_Num <= 35 ~ "Jóvenes (20-35)",
              Age_Num >= 36 & Age_Num <= 59 ~ "Adultos (36-59)",
              Age_Num >= 60                 ~ "Adultos mayores (60+)",
              TRUE                          ~ "Otro"
            ),
            Entidad = state_name
          ) %>%
          group_by(Entidad, Sexo, Grupo_Edad) %>%
          summarise(Total_Desaparecidos = sum(Total), .groups = 'drop')
        
        final_list[[state_name]] <- df_state_temp
      }
      
    }, error = function(e) {
      message(paste("Error leyendo:", file_name, "-", e$message))
    })
    
  } else {
    message(paste("ADVERTENCIA: No se encontró el archivo:", file_name))
  }
}

# 3. UNIFICACIÓN Y EXPORTACIÓN
# ----------------------------------------------------------
df_base <- bind_rows(final_list)

# --- Generación de Totales (Copiado de tu estructura original) ---

# Totales por Estado y Sexo
df_state_sex_total <- df_base %>%
  group_by(Entidad, Sexo) %>%
  summarise(Total_Desaparecidos = sum(Total_Desaparecidos), .groups = 'drop') %>%
  mutate(Grupo_Edad = "TOTAL (Todas las edades)")

# Totales por Estado Globales
df_state_grand_total <- df_base %>%
  group_by(Entidad) %>%
  summarise(Total_Desaparecidos = sum(Total_Desaparecidos), .groups = 'drop') %>%
  mutate(Sexo = "AMBOS SEXOS", Grupo_Edad = "TOTAL (Todas las edades)")

# Totales Nacionales (OJO: Solo sumará los 32 estados procesados)
df_national_sex_total <- df_base %>%
  group_by(Sexo) %>%
  summarise(Total_Desaparecidos = sum(Total_Desaparecidos), .groups = 'drop') %>%
  mutate(Entidad = "TOTAL NACIONAL", Grupo_Edad = "TOTAL (Todas las edades)")

df_national_grand_total <- df_base %>%
  summarise(Total_Desaparecidos = sum(Total_Desaparecidos)) %>%
  mutate(Entidad = "TOTAL NACIONAL", Sexo = "AMBOS SEXOS", Grupo_Edad = "TOTAL (Todas las edades)")

df_national_age_sex <- df_base %>%
  group_by(Sexo, Grupo_Edad) %>%
  summarise(Total_Desaparecidos = sum(Total_Desaparecidos), .groups = 'drop') %>%
  mutate(Entidad = "TOTAL NACIONAL")

df_national_age_total <- df_base %>%
  group_by(Grupo_Edad) %>%
  summarise(Total_Desaparecidos = sum(Total_Desaparecidos), .groups = 'drop') %>%
  mutate(Entidad = "TOTAL NACIONAL", Sexo = "AMBOS SEXOS")

# Unificación Final
df_final_completo <- bind_rows(
  df_base, df_state_sex_total, df_state_grand_total, 
  df_national_sex_total, df_national_grand_total, 
  df_national_age_sex, df_national_age_total
) %>%
  mutate(Grupo_Edad = factor(Grupo_Edad, levels = c(
    "Infancias (0-9)", "Adolescentes (10-19)", "Jóvenes (20-35)",
    "Adultos (36-59)", "Adultos mayores (60+)", "Otro", "TOTAL (Todas las edades)"
  ))) %>%
  arrange(desc(Entidad == "TOTAL NACIONAL"), Entidad, Sexo, Grupo_Edad)

# Guardar
write_csv(df_final_completo, "df_desaparecidos_32_estados.csv")

message("Proceso terminado. Se generó 'df_desaparecidos_32_estados.csv'")
message(paste("Total Nacional Calculado (Solo 32 estados):", comma(sum(df_base$Total_Desaparecidos))))