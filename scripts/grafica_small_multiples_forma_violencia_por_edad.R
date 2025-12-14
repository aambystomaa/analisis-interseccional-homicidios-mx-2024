# ============================================================
# Small multiples (100% stacked): distribución de formas de violencia letal
# Mujeres víctimas de presunto homicidio (INEGI EDR 2024)
# Paneles: Total mujeres vs Niñas/adolescentes (<18) vs Mujeres 60+
# Autora: Natalia Ruiz
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(foreign)
})

if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

dbf_path <- "data/raw/DEFUN24.dbf"
stopifnot(file.exists(dbf_path))

df <- read.dbf(dbf_path, as.is = TRUE)

# Presunto homicidio (agresiones) CIE-10: X85–Y09
is_presunto_homicidio <- function(cie10) {
  cie10 <- toupper(trimws(as.character(cie10)))
  code <- str_extract(cie10, "^[A-Z][0-9]{2}")
  (!is.na(code)) & (
    (str_starts(code, "X") & as.integer(str_sub(code, 2, 3)) >= 85) |
      (str_starts(code, "Y") & as.integer(str_sub(code, 2, 3)) <= 9)
  )
}

# Clasificación (4 + Otras)
clasificar_forma <- function(cie10) {
  cie10 <- toupper(trimws(as.character(cie10)))
  code <- str_extract(cie10, "^[A-Z][0-9]{2}")
  
  case_when(
    is.na(code) ~ NA_character_,
    code %in% c("X93", "X94", "X95") ~ "Arma de fuego",
    code %in% c("X99") ~ "Arma blanca / punzocortante",
    code %in% c("X91") ~ "Asfixia / estrangulamiento",
    code %in% c("Y00") ~ "Objeto contundente",
    TRUE ~ "Otras"
  )
}

df2 <- df %>%
  mutate(
    SEXO = suppressWarnings(as.integer(SEXO)),
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
  filter(!is.na(edad_est), edad_est >= 0, edad_est <= 110) %>%
  mutate(
    grupo_edad = case_when(
      edad_est < 18 ~ "Niñas y adolescentes (<18)",
      edad_est >= 60 ~ "Mujeres 60+",
      TRUE ~ "Otras edades (18–59)"
    ),
    forma = clasificar_forma(CAUSA_DEF)
  ) %>%
  filter(!is.na(forma))

tab <- df2 %>%
  mutate(panel = case_when(
    grupo_edad == "Niñas y adolescentes (<18)" ~ "Niñas y adolescentes (<18)",
    grupo_edad == "Mujeres 60+" ~ "Mujeres 60+",
    TRUE ~ "Total mujeres"
  )) %>%
  group_by(panel, forma) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

orden_forma <- c(
  "Arma de fuego",
  "Arma blanca / punzocortante",
  "Asfixia / estrangulamiento",
  "Objeto contundente",
  "Otras"
)

tab <- tab %>%
  mutate(
    forma = factor(forma, levels = orden_forma),
    panel = factor(panel, levels = c("Total mujeres", "Niñas y adolescentes (<18)", "Mujeres 60+"))
  )

# Paleta verde → morado
pal <- c(
  "Arma de fuego" = "#1B9E77",
  "Arma blanca / punzocortante" = "#66A61E",
  "Asfixia / estrangulamiento" = "#7570B3",
  "Objeto contundente" = "#542788",
  "Otras" = "#B2ABD2"
)

# Etiquetas dentro: solo si >= 6%
tab_labels <- tab %>%
  mutate(label = if_else(pct >= 0.06, percent(pct, accuracy = 1), ""))

p <- ggplot(tab, aes(x = 1, y = pct, fill = forma)) +
  geom_col(width = 0.65) +
  geom_text(
    data = tab_labels,
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3.2
  ) +
  coord_flip() +
  facet_wrap(~ panel, ncol = 1) +
  scale_fill_manual(values = pal) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Violencia letal en mujeres víctimas de presunto homicidio (México, 2024)",
    subtitle = "Distribución porcentual por grupo etario (microdatos INEGI, EDR 2024)",
    fill = NULL,
    caption = "Nota: Categorías agrupadas a partir de CIE-10 (X85–Y09). 'Otras' agrupa el resto de agresiones."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # quitar ejes (lo que pediste)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(18, "pt"),
    plot.margin = margin(t = 10, r = 12, b = 18, l = 12)
  )

out_png <- "outputs/small_multiples_forma_violencia_por_edad_2024.png"
ggsave(out_png, p, width = 7.6, height = 9.2, dpi = 320)
message("Guardada: ", out_png)

out_csv <- "outputs/tabla_small_multiples_forma_violencia_por_edad_2024.csv"
write.csv(tab %>% arrange(panel, forma), out_csv, row.names = FALSE)
message("Guardada: ", out_csv)
