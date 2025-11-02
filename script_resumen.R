library(dplyr)

datos <- read.csv("data.csv")

resumen <- datos %>%
  group_by(ramo, TIPO_CLIENTE, numero_polizas) %>%
  summarise(
    Q1 = quantile(PRIMA_MES_TRIM_SEMES_SOLES, 0.25, na.rm = TRUE),
    Q3 = quantile(PRIMA_MES_TRIM_SEMES_SOLES, 0.75, na.rm = TRUE),
    mediana = median(PRIMA_MES_TRIM_SEMES_SOLES, na.rm = TRUE),
    limite_superior = Q3 + 1.5 * (Q3 - Q1),
    total_clientes = n(),
    clientes_fuera = sum(PRIMA_MES_TRIM_SEMES_SOLES > (Q3 + 1.5 * (Q3 - Q1)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(porcentaje_fuera = round((clientes_fuera / total_clientes) * 100, 2))

saveRDS(resumen, "resumen.rds")

