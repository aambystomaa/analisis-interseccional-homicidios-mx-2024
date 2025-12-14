# ============================================================
# Barras comparativas (%): Total mujeres vs Mujeres indígenas
# Presunto homicidio (CIE-10 X85–Y09), INEGI EDR 2024
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

# Presunto homicidio (agresiones) CIE-10: X85–Y09
is_presunto_homicidio <- function(cie10) {
  cie10 <- toupper(trimws(as.character(cie10)))
  code3 <- str_extract(cie10, "^[A-Z][0-9]{2}")
  (!is.na(code3)) & (
    (str_starts(code3, "X") & as.integer(str_sub(code3, 2, 3)) >= 85) |
      (str_starts(code3, "Y") & as.integer(str_sub(code3, 2, 3)) <= 9)
  )
}

# Clasifica formas (6 categorías: 4 + no especificado + otras)
clasificar_forma_6 <- function(cie10) {
  cie10 <- toupper(trimws(as.character(cie10)))
  code3 <- str_extract(cie10, "^[A-Z][0-9]{2}")
  
  case_when(
    is.na(code3) ~ NA_character_,
    
    # Arma de fuego (agresión)
    code3 %in% c("X93", "X94", "X95") ~ "Arma de fuego",
    
    # Arma blanca / punzocortante
    code3 == "X99" ~ "Arma blanca / punzocortante",
    
    # Asfixia / estrangulamiento
    code3 == "X91" ~ "Asfixia / estrangulamiento",
    
    # Objeto contundente
    code3 == "Y00" ~ "Objeto contundente",
    
    # No especificado (agresión no especificada)
    code3 == "Y09" ~ "No especificado",
    
    # Todo lo demás dentro de X85–Y09
    TRUE ~ "Otras"
  )
}

# ---------------------------
# 4) Filtrar a mujeres + presunto homicidio
# ---------------------------
df_w <- df %>%
  mutate(
    SEXO = suppressWarnings(as.integer(SEXO)),
    CONINDIG = suppressWarnings(as.integer(CONINDIG)),
    CAUSA_DEF = as.character(CAUSA_DEF)
  ) %>%
  filter(SEXO == 2) %>%
  filter(is_presunto_homicidio(CAUSA_DEF)) %>%
  mutate(
    grupo = if_else(CONINDIG == 1, "Mujeres indígenas", "Total mujeres"),
    forma6 = clasificar_forma_6(CAUSA_DEF)
  ) %>%
  filter(!is.na(forma6))

# Denominadores por grupo
denoms <- df_w %>%
  group_by(grupo) %>%
  summarise(N = n(), .groups = "drop")

# Tabla % por grupo
tab <- df_w %>%
  group_by(grupo, forma6) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(denoms, by = "grupo") %>%
  mutate(pct = n / N)

# Orden de categorías (para lectura)
orden_forma <- c(
  "Arma de fuego",
  "Arma blanca / punzocortante",
  "Asfixia / estrangulamiento",
  "Objeto contundente",
  "No especificado",
  "Otras"
)

tab <- tab %>%
  mutate(
    forma6 = factor(forma6, levels = orden_forma),
    grupo = factor(grupo, levels = c("Total mujeres", "Mujeres indígenas")),
    label = percent(pct, accuracy = 0.1)
  )

# ---------------------------
# 5) Gráfica: barras lado a lado + etiquetas
# ---------------------------
# Paleta verde → morado (2 grupos)
pal_grupos <- c(
  "Total mujeres" = "#1B9E77",
  "Mujeres indígenas" = "#542788"
)

p <- ggplot(tab, aes(x = forma6, y = pct, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.8),
    vjust = -0.35,
    size = 3.2
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.12))) +
  scale_fill_manual(values = pal_grupos) +
  labs(
    title = "Formas de violencia letal en mujeres (presunto homicidio, 2024)",
    subtitle = "Comparación porcentual: Total mujeres vs Mujeres indígenas (CONINDIG==1)",
    x = NULL,
    y = "Porcentaje dentro del grupo",
    fill = NULL,
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas (EDR) 2024. Presunto homicidio: CIE-10 X85–Y09."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

out_png <- "outputs/barras_forma_violencia_indigenas_vs_total_2024.png"
ggsave(out_png, p, width = 10, height = 6.2, dpi = 320)
message("Guardada: ", out_png)

# Export opcional de tabla
out_csv <- "outputs/tabla_barras_forma_violencia_indigenas_vs_total_2024.csv"
write.csv(tab %>% arrange(forma6, grupo), out_csv, row.names = FALSE)
message("Guardada: ", out_csv)
