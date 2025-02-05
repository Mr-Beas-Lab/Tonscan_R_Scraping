library(tidyverse)

table_data <- table_data %>%
  mutate(
    #Quitar la primera parte de Maker
    Maker = str_sub(Maker, 29), 
    # Dividir Value en dos partes (líneas)
    Value_1 = str_extract(Value, "^[^\n]+"),  # Primera línea
    Value_2 = str_extract(Value, "(?<=\n).+"),  # Segunda línea
    
    # Extraer Value_1_num y Value_1_units
    Value_1_num = str_extract(Value_1, "^\\d+(\\.\\d+)?") %>% 
      str_replace_all(" ", "") %>%  # Eliminar espacios
      as.numeric(),
    Value_1_units = str_extract(Value_1, "[A-Z]+"),
    
    # Extraer Value_2_num y Value_2_units
    Value_2_num = str_extract(Value_2, "\\d[\\d\\s]*(\\.\\d+)?") %>%  # Ajuste: permitir números enteros
      str_replace_all(" ", "") %>%  # Eliminar espacios
      as.numeric(),
    Value_2_units = str_extract(Value_2, "[A-Z]+"),
    # Procesar minutos
    Transaction_Time = case_when(
      str_detect(Age, "minutes? ago") ~ now() - minutes(as.numeric(str_extract(Age, "\\d+"))),
      str_detect(Age, "hours? ago") ~ now() - hours(as.numeric(str_extract(Age, "\\d+"))),
      str_detect(Age, "days? ago") ~ now() - days(as.numeric(str_extract(Age, "\\d+"))),
      TRUE ~ NA_POSIXct_  # Manejo de casos no especificados
    )
  ) 

table_data <- table_data %>%
  mutate(Value_TON = ifelse(Value_1_units == "TON", Value_1_num, Value_2_num),
         Value_MRB = ifelse(Value_2_units == "MRB", Value_2_num, Value_1_num))

head(table_data)

# Guardar los datos en un archivo CSV
write.csv(table_data, "extracted_table_data.csv", row.names = FALSE)


write.csv(MarketCap_value, "MarketCap_value.csv", row.names = FALSE)
