# ============================================================
# Análisis interseccional — Mujeres víctimas de presunto homicidio
# INEGI | Estadísticas de Defunciones Registradas (EDR) 2024
#
# Autora: Natalia Ruiz
#
# ============================================================

# -------------------------
# 0) Paquetes
# -------------------------
if (!requireNamespace("foreign", quietly = TRUE)) install.packages("foreign")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(foreign)
library(dplyr)
library(ggplot2)

# -------------------------
# 1) Working directory (raíz del repo)
# -------------------------
setwd("~/Documents/analisis-interseccional-homicidios-mx-2024")

# -------------------------
# 2) Outputs + log
# -------------------------
dir.create("outputs", showWarnings = FALSE)

log_path <- "outputs/log_mujeres_presunto_homicidio_2024.txt"
sink(log_path, split = TRUE)

cat("========================================\n")
cat("Mujeres víctimas de presunto homicidio — análisis interseccional\n")
cat("Autora: Natalia Ruiz\n")
cat("Fecha:", Sys.Date(), "\n")
cat("Working directory:", getwd(), "\n")
cat("Edad: ANIO_OCUR/ANIO_REGIS - ANIO_NACIM (NO EDAD)\n")
cat("========================================\n\n")

# -------------------------
# 3) Leer DBF
# -------------------------
dbf_path <- "data/raw/DEFUN24.dbf"
cat("DBF esperado:", normalizePath(dbf_path, mustWork = FALSE), "\n")
cat("¿Existe DBF?:", file.exists(dbf_path), "\n\n")
stopifnot(file.exists(dbf_path))

df <- read.dbf(dbf_path, as.is = TRUE)
cat("DBF leído correctamente.\n")
cat("Dimensiones df:", dim(df)[1], "filas x", dim(df)[2], "columnas\n\n")

# -------------------------
# 4) Preparación de tipos + CIE-10 (cie3)
# -------------------------
df <- df %>%
  mutate(
    SEXO_chr = as.character(SEXO),
    CONINDIG_chr = as.character(CONINDIG),
    cie3 = substr(CAUSA_DEF, 1, 3),
    ANIO_NACIM_num = suppressWarnings(as.numeric(as.character(ANIO_NACIM))),
    ANIO_OCUR_num  = suppressWarnings(as.numeric(as.character(ANIO_OCUR))),
    ANIO_REGIS_num = suppressWarnings(as.numeric(as.character(ANIO_REGIS)))
  )

# Chequeos rápidos de años (para el log)
cat("Chequeo ANIO_NACIM (summary):\n")
print(summary(df$ANIO_NACIM_num))
cat("\nChequeo ANIO_OCUR (summary):\n")
print(summary(df$ANIO_OCUR_num))
cat("\nChequeo ANIO_REGIS (summary):\n")
print(summary(df$ANIO_REGIS_num))
cat("\n")

# -------------------------
# 5) Filtrar presuntos homicidios (X85–Y09) + mujeres
# -------------------------
mujeres <- df %>%
  filter(!is.na(cie3)) %>%
  filter(cie3 >= "X85" & cie3 <= "Y09") %>%
  filter(SEXO_chr == "2")

cat("Total mujeres víctimas de presunto homicidio (X85–Y09):", nrow(mujeres), "\n\n")

# -------------------------
# 6) Construir año base de muerte y edad estimada
# -------------------------
# Preferimos ANIO_OCUR; si falta, usamos ANIO_REGIS
mujeres <- mujeres %>%
  mutate(
    anio_muerte = ifelse(!is.na(ANIO_OCUR_num), ANIO_OCUR_num, ANIO_REGIS_num)
  )

# Limpiar años implausibles:
# - Nacimiento razonable: 1900–2024 (ajusta si quieres ampliar)
# - Año muerte razonable: 2024 (idealmente) pero dejamos 2023–2025 por posibles rezagos/errores
mujeres <- mujeres %>%
  mutate(
    ANIO_NACIM_ok = !is.na(ANIO_NACIM_num) & ANIO_NACIM_num >= 1900 & ANIO_NACIM_num <= 2024,
    anio_muerte_ok = !is.na(anio_muerte) & anio_muerte >= 2023 & anio_muerte <= 2025,
    edad_est = ifelse(ANIO_NACIM_ok & anio_muerte_ok, anio_muerte - ANIO_NACIM_num, NA_real_),
    # Edad plausible: 0–110
    edad_est = ifelse(!is.na(edad_est) & edad_est >= 0 & edad_est <= 110, edad_est, NA_real_)
  )

cat("--- CHECK EDAD (desde años) ---\n")
cat("Año muerte (anio_muerte) summary:\n"); print(summary(mujeres$anio_muerte))
cat("\nEdad estimada (edad_est) summary:\n"); print(summary(mujeres$edad_est))
cat("\nEdad estimada percentiles:\n")
print(quantile(mujeres$edad_est, probs = c(0, .01, .05, .25, .5, .75, .95, .99, 1), na.rm = TRUE))
cat("\nConteo edad_est NA:\n"); print(table(is.na(mujeres$edad_est)))
cat("--- FIN CHECK EDAD ---\n\n")

# -------------------------
# 7) Variables interseccionales (con edad_est)
# -------------------------
mujeres <- mujeres %>%
  mutate(
    menor_18 = !is.na(edad_est) & edad_est < 18,
    mayor_60 = !is.na(edad_est) & edad_est >= 60,
    indigena = CONINDIG_chr == "1"
  )

# -------------------------
# 8) Porcentajes generales
# -------------------------
n_total <- nrow(mujeres)
n_edad_valida <- sum(!is.na(mujeres$edad_est))

pct_menor18 <- 100 * sum(mujeres$menor_18, na.rm = TRUE) / n_total
pct_60mas   <- 100 * sum(mujeres$mayor_60, na.rm = TRUE) / n_total

# Indígena: denominador solo CONINDIG especificado (1 o 2); excluye no especificado
mujeres_conindig <- mujeres %>% filter(CONINDIG_chr %in% c("1", "2"))
pct_indig <- ifelse(nrow(mujeres_conindig) > 0,
                    100 * sum(mujeres_conindig$indigena, na.rm = TRUE) / nrow(mujeres_conindig),
                    NA_real_)

cat("PORCENTAJES GENERALES\n")
cat("Total mujeres (denominador):", n_total, "\n")
cat("Edad válida (edad_est):", n_edad_valida, "de", n_total, "\n")
cat("CONINDIG especificado (para % indígena):", nrow(mujeres_conindig), "de", n_total, "\n\n")
cat(sprintf("%% Menores de 18 (sobre total): %.2f%% (n=%d)\n",
            pct_menor18, sum(mujeres$menor_18, na.rm = TRUE)))
cat(sprintf("%% Mujeres 60+ (sobre total): %.2f%% (n=%d)\n",
            pct_60mas, sum(mujeres$mayor_60, na.rm = TRUE)))
cat(sprintf("%% Indígenas (CONINDIG especificado): %.2f%% (n=%d de %d)\n\n",
            pct_indig, sum(mujeres_conindig$indigena, na.rm = TRUE), nrow(mujeres_conindig)))

resumen <- data.frame(
  indicador = c(
    "Total mujeres presunto homicidio",
    "Edad válida (conteo, edad_est)",
    "CONINDIG especificado (conteo)",
    "% Menores de 18 (sobre total mujeres)",
    "% Mujeres 60+ (sobre total mujeres)",
    "% Mujeres indígenas (sobre CONINDIG especificado)"
  ),
  valor = c(
    n_total,
    n_edad_valida,
    nrow(mujeres_conindig),
    round(pct_menor18, 2),
    round(pct_60mas, 2),
    round(pct_indig, 2)
  )
)

write.csv(resumen, "outputs/resumen_porcentajes_mujeres_2024.csv", row.names = FALSE)
cat("Resumen guardado en outputs/resumen_porcentajes_mujeres_2024.csv\n\n")

# -------------------------
# 9) Forma de defunción (X85–Y09 completa, sin 'otras')
# -------------------------
mujeres <- mujeres %>%
  mutate(
    forma_muerte = case_when(
      cie3 == "X85" ~ "Envenenamiento (drogas/medicamentos)",
      cie3 == "X86" ~ "Sustancia corrosiva",
      cie3 == "X87" ~ "Pesticidas",
      cie3 == "X88" ~ "Gases/vapores",
      cie3 == "X89" ~ "Otros químicos",
      cie3 == "X90" ~ "Envenenamiento no especificado",
      cie3 == "X91" ~ "Asfixia / estrangulamiento",
      cie3 == "X92" ~ "Ahogamiento",
      cie3 == "X93" ~ "Arma de fuego (arma corta)",
      cie3 == "X94" ~ "Arma de fuego (rifle/escopeta)",
      cie3 == "X95" ~ "Arma de fuego (no especificada)",
      cie3 == "X96" ~ "Explosivos",
      cie3 == "X97" ~ "Fuego / llamas",
      cie3 == "X98" ~ "Objetos calientes",
      cie3 == "X99" ~ "Arma blanca / objeto cortante",
      cie3 == "Y00" ~ "Objeto contundente",
      cie3 == "Y01" ~ "Empujón desde lugar elevado",
      cie3 == "Y02" ~ "Empujón frente a objeto en movimiento",
      cie3 == "Y03" ~ "Colisión vehicular intencional",
      cie3 == "Y04" ~ "Golpes / fuerza corporal",
      cie3 == "Y05" ~ "Agresión sexual con fuerza física",
      cie3 == "Y06" ~ "Negligencia / abandono",
      cie3 == "Y07" ~ "Otros maltratos",
      cie3 == "Y08" ~ "Otras agresiones especificadas",
      cie3 == "Y09" ~ "Agresión no especificada",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(forma_muerte))

# -------------------------
# 10) Función para gráfica TOP 10 (porcentaje + etiquetas) con guardado seguro
# -------------------------
plot_top10 <- function(data, filename, titulo, color_barra) {
  if (nrow(data) == 0) {
    cat("AVISO: No hay registros para:", titulo, "\n")
    return(invisible(NULL))
  }
  
  tab <- data %>%
    count(forma_muerte) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    arrange(desc(pct)) %>%
    slice(1:10)
  
  if (nrow(tab) == 0) {
    cat("AVISO: Tabla vacía tras conteo para:", titulo, "\n")
    return(invisible(NULL))
  }
  
  tab <- tab %>%
    mutate(
      forma_muerte = factor(forma_muerte, levels = rev(forma_muerte)),
      label = paste0(round(pct, 1), "%")
    )
  
  p <- ggplot(tab, aes(x = forma_muerte, y = pct)) +
    geom_col(fill = color_barra) +
    geom_text(aes(label = label), hjust = -0.05, size = 3) +
    coord_flip() +
    labs(
      title = titulo,
      x = "",
      y = "Porcentaje del total del grupo (Top 10)"
    ) +
    scale_y_continuous(limits = c(0, max(tab$pct) * 1.15)) +
    theme_minimal()
  
  out_path <- file.path("outputs", filename)
  ggsave(out_path, p, width = 11, height = 6, dpi = 300)
  cat("Gráfica guardada:", normalizePath(out_path, mustWork = FALSE), "\n")
  invisible(p)
}

# -------------------------
# 11) Colores rosa → morado
# -------------------------
col_general <- "#E91E63"
col_indig   <- "#C2185B"
col_menor18 <- "#9C27B0"
col_60mas   <- "#6A1B9A"

# -------------------------
# 12) Subconjuntos + 4 gráficas (TOP 10)
# -------------------------
plot_top10(
  mujeres,
  "top10_forma_muerte_mujeres_general_2024.png",
  "Top 10 formas de defunción — Mujeres (México, 2024)",
  col_general
)

plot_top10(
  mujeres %>% filter(indigena),
  "top10_forma_muerte_mujeres_indigenas_2024.png",
  "Top 10 formas de defunción — Mujeres indígenas (México, 2024)",
  col_indig
)

plot_top10(
  mujeres %>% filter(menor_18),
  "top10_forma_muerte_mujeres_menores_18_2024.png",
  "Top 10 formas de defunción — Niñas y adolescentes (México, 2024)",
  col_menor18
)

plot_top10(
  mujeres %>% filter(mayor_60),
  "top10_forma_muerte_mujeres_60_mas_2024.png",
  "Top 10 formas de defunción — Mujeres 60+ (México, 2024)",
  col_60mas
)

cat("\nArchivos en outputs:\n")
print(list.files("outputs"))
cat("\nEjecución finalizada.\n")
sink()
