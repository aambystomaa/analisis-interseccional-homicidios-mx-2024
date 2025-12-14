# ============================================================
# Share bar (100%): Peso de "No especificado" en presuntos homicidios de mujeres
# Grupos: Total mujeres | Mujeres indígenas | Mujeres 60+
# Fuente: INEGI EDR 2024
# Autora: Natalia Ruiz
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(foreign)
})

# ---------------------------
# 1) Rutas / carpetas
# ---------------------------
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

dbf_path <- "data/raw/DEFUN24.dbf"
stopifnot(file.exists(dbf_path))

# ---------------------------
# 2) Cargar datos
# ---------------------------
df <- read.dbf(dbf_path, as.is = TRUE)

# ---------------------------
# 3) Helpers
# ---------------------------

# Presunto homicidio: CIE-10 X85–Y09
is_presunto_homicidio <- function(cie10) {
  cie10 <- toupper(trimws(as.character(cie10)))
  code3 <- str_extract(cie10, "^[A-Z][0-9]{2}")
  (!is.na(code3)) & (
    (str_starts(code3, "X") & as.integer(str_sub(code3, 2, 3)) >= 85) |
      (str_starts(code3, "Y") & as.integer(str_sub(code3, 2, 3)) <= 9)
  )
}

# Definición de "No especificado"
# Y09 = Agresión por medios no especificados
is_no_especificado <- function(cie10) {
  code3 <- str_extract(toupper(trimws(as.character(cie10))), "^[A-Z][0-9]{2}")
  code3 == "Y09"
}

# ---------------------------
# 4) Limpieza base
# ---------------------------
df_w <- df %>%
  mutate(
    SEXO = suppressWarnings(as.integer(SEXO)),
    CONINDIG = suppressWarnings(as.integer(CONINDIG)),
    ANIO_NACIM = suppressWarnings(as.integer(ANIO_NACIM)),
    ANIO_OCUR = suppressWarnings(as.integer(ANIO_OCUR)),
    ANIO_REGIS = suppressWarnings(as.integer(ANIO_REGIS)),
    CAUSA_DEF = as.character(CAUSA_DEF)
  ) %>%
  filter(SEXO == 2) %>%
  filter(is_presunto_homicidio(CAUSA_DEF)) %>%
  mutate(
    anio_muerte = if_else(!is.na(ANIO_OCUR), ANIO_OCUR, ANIO_REGIS),
    edad_est = anio_muerte - ANIO_NACIM
  ) %>%
  filter(!is.na(edad_est), edad_est >= 0, edad_est <= 110)

# ---------------------------
# 5) Construir grupos
# ---------------------------
df_groups <- bind_rows(
  df_w %>% mutate(grupo = "Total mujeres"),
  df_w %>% filter(CONINDIG == 1) %>% mutate(grupo = "Mujeres indígenas"),
  df_w %>% filter(edad_est >= 60) %>% mutate(grupo = "Mujeres 60+")
)

# ---------------------------
# 6) Tabla share bar
# ---------------------------
tab <- df_groups %>%
  mutate(
    especificacion = if_else(
      is_no_especificado(CAUSA_DEF),
      "No especificado",
      "Especificado"
    )
  ) %>%
  group_by(grupo, especificacion) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

tab <- tab %>%
  mutate(
    grupo = factor(grupo, levels = c("Total mujeres", "Mujeres indígenas", "Mujeres 60+")),
    especificacion = factor(especificacion, levels = c("Especificado", "No especificado")),
    label = if_else(
      especificacion == "No especificado",
      percent(pct, accuracy = 0.1),
      ""
    )
  )

# ---------------------------
# 7) Gráfica share bar
# ---------------------------
pal <- c(
  "Especificado" = "#1B9E77",   # verde
  "No especificado" = "#542788" # morado
)

p <- ggplot(tab, aes(x = grupo, y = pct, fill = especificacion)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_fill_manual(values = pal) +
  coord_flip() +
  labs(
    title = "Lo no especificado como señal de desigualdad institucional",
    subtitle = "Proporción de presuntos homicidios de mujeres con forma de agresión no especificada\n(México, 2024)",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas (EDR) 2024. Presunto homicidio: CIE-10 X85–Y09."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    
    # CENTRAR TÍTULO Y SUBTÍTULO
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    
    # más aire arriba para que no se corte
    plot.margin = margin(t = 16, r = 12, b = 10, l = 12)
  )

out_png <- "outputs/sharebar_no_especificado_por_grupo_2024.png"
ggsave(out_png, p, width = 7.8, height = 5.2, dpi = 320)
message("Guardada: ", out_png)

# ---------------------------
# 8) Tabla soporte
# ---------------------------
out_csv <- "outputs/tabla_sharebar_no_especificado_por_grupo_2024.csv"
write.csv(tab, out_csv, row.names = FALSE)
message("Guardada: ", out_csv)