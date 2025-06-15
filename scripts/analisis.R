# ============================
# PROYECTO: Análisis de Encuesta BU
# Autor: Miguel Pérez
# ============================

# ==== Librerías ====
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ==== Configuración ====
mostrar_en_consola <- TRUE
dir.create("plots", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# ==== Cargar Datos ====
datos <- read_excel("data/Respuestas_Encuesta.xlsx")

# ==== Vista previa ====
if (mostrar_en_consola) {
  print(head(datos))
  str(datos)
  summary(datos)
}

# ==== Limpieza y transformación ====
programas <- datos %>% 
  select(starts_with('¿A qué programa')) %>%
  unite("Programa", everything(), na.rm = TRUE, sep = ", ")

actividades <- datos %>%
  select(starts_with('¿A qué actividad asistes')) %>%
  unite("Actividad", everything(), na.rm = TRUE, sep = ", ")

datos <- datos %>%
  select(-starts_with('¿A qué programa'), -starts_with('¿A qué actividad'), -'Marca temporal') %>%
  bind_cols(actividades, programas)

datos <- datos %>%
  relocate(Programa, .after = '¿A qué facultad perteneces?') %>%
  relocate(Actividad, .after = '¿A qué tipo de actividad asistes con mayor frecuencia?')

colnames(datos) <- c("Edad", "Genero", "Facultad", "Programa", "Semestre", "PromedioAcumulado",
                     "ParticipacionBU", "RazonNoParticipa", "Motivacion", "NumeroActividadesParticipa",
                     "DiasParticipacion", "TipoActividad", "Actividad", "ImpactoBU",
                     "BienestarGeneral", "MedioInformacion")

# ==== Normalizar motivaciones ====
datos <- datos %>%
  mutate(Motivacion = case_when(
    Motivacion %in% c("Mejorar mi bienestar físico", "Mejorar tambien en mi capacidad fisica") ~ "Bienestar físico",
    Motivacion == "Reducir el estrés académico" ~ "Reducir estrés",
    Motivacion == "Socializar y conocer nuevas personas" ~ "Socializar",
    Motivacion == "Mejorar mi salud mental" ~ "Salud mental",
    Motivacion == "Ganarme la camiseta de BU" ~ "Incentivo material",
    Motivacion %in% c("Aprender nuevas habilidades", "Mejorar mis habilidades musicales") ~ "Aprender habilidades",
    Motivacion == "Todas las anteriores" ~ "Multimotivación",
    is.na(Motivacion) ~ NA_character_,
    TRUE ~ "Otro"
  ))

# ==== Función para guardar y mostrar gráficos ====
guardar_plot <- function(plot, nombre) {
  ggsave(filename = paste0("plots/", nombre, ".png"), plot = plot, width = 8, height = 6)
  if (mostrar_en_consola) print(plot)
}

# ==== Gráficos ====
# 1. Programas con mayor participación
p1 <- ggplot(filter(datos, ParticipacionBU == "Sí"), aes(x = Programa)) +
  geom_bar(fill = "darkorange") +
  coord_flip() +
  labs(title = "Programas con mayor participación en BU", x = "Programa", y = "Estudiantes")
guardar_plot(p1, "programas_participacion")

# 2. Participación por semestre
p2 <- ggplot(datos, aes(x = Semestre)) +
  geom_bar(fill = "violet") +
  labs(title = "Respuestas por semestre", x = "Semestre", y = "Respuestas")
guardar_plot(p2, "participacion_por_semestre")

# 3. Actividades más asistidas
p3 <- ggplot(filter(datos, !is.na(Actividad) & Actividad != ""), aes(x = Actividad)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Actividades más asistidas", x = "Actividad", y = "Cantidad")
guardar_plot(p3, "actividades_mas_asistidas")

# 4. Participación por tipo de actividad
p4 <- ggplot(filter(datos, !is.na(TipoActividad) & TipoActividad != ""), aes(x = TipoActividad)) +
  geom_bar(fill = "lightblue") +
  coord_flip() +
  labs(title = "Tipo de actividad vs Participación", x = "Tipo de actividad", y = "Cantidad")
guardar_plot(p4, "tipo_actividad_participacion")

# 5. Frecuencia de días de participación
p5 <- ggplot(datos, aes(x = DiasParticipacion)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Frecuencia semanal de participación", x = "Días", y = "Cantidad")
guardar_plot(p5, "dias_participacion")

# 6. Número de actividades
p6 <- ggplot(datos, aes(x = NumeroActividadesParticipa)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Número de actividades por estudiante", x = "Actividades", y = "Cantidad")
guardar_plot(p6, "numero_actividades")

# 7. Participación vs semestre
p7 <- ggplot(datos, aes(x = as.factor(Semestre), fill = ParticipacionBU)) +
  geom_bar(position = "fill") +
  labs(title = "Participación según semestre", x = "Semestre", y = "Proporción")
guardar_plot(p7, "participacion_vs_semestre")

# 8. Motivaciones
p8 <- ggplot(filter(datos, !is.na(Motivacion)), aes(x = Motivacion)) +
  geom_bar(fill = "#4682B4") +
  labs(title = "Motivaciones para participar", x = "Motivación", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar_plot(p8, "motivaciones")

# 9. Participación por género
p9 <- ggplot(datos, aes(x = ParticipacionBU, fill = Genero)) +
  geom_bar(position = "dodge") +
  labs(title = "Participación según género", x = "Participa", y = "Cantidad")
guardar_plot(p9, "participacion_genero")

# 10. Barreras para participar
p10 <- ggplot(filter(datos, !is.na(RazonNoParticipa)), aes(x = RazonNoParticipa)) +
  geom_bar(fill = "firebrick") +
  coord_flip() +
  labs(title = "Barreras para la participación", x = NULL, y = "Cantidad")
guardar_plot(p10, "barreras_participacion")

# ==== Estadísticas ====
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

resumen_estadistico <- list(
  promedio_general = mean(datos$PromedioAcumulado, na.rm = TRUE),
  desviacion_general = sd(datos$PromedioAcumulado, na.rm = TRUE),
  porcentaje_participacion = mean(datos$ParticipacionBU == "Sí", na.rm = TRUE) * 100,
  promedio_dias_participacion = mean(datos$DiasParticipacion[datos$ParticipacionBU == "Sí"], na.rm = TRUE)
)

if (mostrar_en_consola) {
  cat("\n--- RESUMEN ESTADÍSTICO ---\n")
  print(resumen_estadistico)
}

writeLines(
  paste(
    "Promedio acumulado:", resumen_estadistico$promedio_general,
    "\nDesviación estándar:", resumen_estadistico$desviacion_general,
    "\n% de participación:", resumen_estadistico$porcentaje_participacion,
    "\nPromedio días participación:", resumen_estadistico$promedio_dias_participacion
  ),
  con = "results/resumen_estadistico.txt"
)

message("Script ejecutado con éxito. Ver 'plots/' para gráficas y 'results/' para resúmenes.")

