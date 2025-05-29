library(tidyverse)

# ine_consumo_comunidad_autonoma

datos <- read_csv2("datos/ine_en_bruto/ine_consumo_comunidad_autonoma.csv")

datos <- datos |> 
  janitor::clean_names() |> 
  filter(!is.na(comunidades_y_ciudades_autonomas)) |> 
  mutate(comunidades_y_ciudades_autonomas = 
           str_replace(comunidades_y_ciudades_autonomas, "\\d+\\s+", "")) |> 
  mutate(divisiones_coicop = 
           str_replace(divisiones_coicop, "\\d+[.]\\s+", "")) |> 
  select(!totales_territoriales) |> 
  rename(anno = periodo)

write_csv(datos, "datos/ine_consumo_comunidad_autonoma.csv")

# ine_desempleo_edad_nivel_estudios

datos <- read_csv2("datos/ine_en_bruto/ine_desempleo_edad_nivel_estudios.csv")

datos <- datos |> 
  janitor::clean_names() |> 
  rename(porcentaje = total)

datos <- datos |> 
  separate(periodo, into = c("anno", "trimestre"), sep = "T")

datos <- datos |> 
  group_by(anno, nivel_de_formacion_alcanzado, grupo_de_edad) |> 
  summarize(porcentaje = median(porcentaje))

write_csv(datos, "datos/ine_desempleo_edad_nivel_estudios.csv")

# ine_desempleo_sexo_nivel_estudios

datos <- read_csv2("datos/ine_en_bruto/ine_desempleo_sexo_nivel_estudios.csv")

datos <- datos |> 
  janitor::clean_names() |> 
  rename(porcentaje = total)

datos <- datos |> 
  separate(periodo, into = c("anno", "trimestre"), sep = "T") |> 
  filter(sexo != "Ambos sexos")

datos <- datos |> 
  group_by(anno, nivel_de_formacion_alcanzado, sexo) |> 
  summarize(porcentaje = median(porcentaje))

write_csv(datos, "datos/ine_desempleo_sexo_nivel_estudios.csv")

# ine_desigualdad_comunidad_autonoma

datos <- read_csv2("datos/ine_en_bruto/ine_desigualdad_comunidad_autonoma.csv")

datos <- datos |> 
  janitor::clean_names()  

datos <- datos |> 
  filter(comunidades_y_ciudades_autonomas != "Total Nacional") |> 
  mutate(comunidades_y_ciudades_autonomas = 
           str_replace(comunidades_y_ciudades_autonomas, "\\d+\\s+", "")) |> 
  rename(anno = periodo) |> 
  filter(desigualdad_en_la_distribucion_de_ingresos_s80_s20_y_coeficiente_de_gini == "Gini") |> 
  select(!desigualdad_en_la_distribucion_de_ingresos_s80_s20_y_coeficiente_de_gini) |> 
  rename(Gini = total) |> 
  mutate(Gini = Gini/100)

write_csv(datos, "datos/ine_desigualdad_comunidad_autonoma.csv")

# ine_poblacion_sexo_edad_pais_nacimiento

datos <- read_csv2("datos/ine_en_bruto/ine_poblacion_sexo_edad_pais_nacimiento.csv")

datos <- datos |> 
  janitor::clean_names()  

datos <- datos |> 
  filter(pais_de_nacimiento != "Total" & sexo != "Total" & edad != "Todas las edades") |> 
  rename(anno = periodo) |> 
  mutate(edad = parse_number(edad)) 

write_csv(datos, "datos/ine_poblacion_sexo_edad_pais_nacimiento.csv")

# ine_pobreza_nivel_estudios

datos <- read_csv2("datos/ine_en_bruto/ine_pobreza_nivel_estudios.csv")

datos <- datos |> 
  janitor::clean_names()  

datos <- datos |> 
  filter(!(nivel_de_formacion_alcanzado %in% c("Total", "No consta" ))) |> 
  rename(anno = periodo) |> 
  rename(porcentaje = total) |> 
  mutate(porcentaje = 
           parse_number(porcentaje, 
                        locale = locale(decimal_mark = ",", grouping_mark = ".")
                        )
         )

write_csv(datos, "datos/ine_pobreza_nivel_estudios.csv")
