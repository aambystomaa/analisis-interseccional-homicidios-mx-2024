### Análisis interseccional a partir de datos del INEGI

Este repositorio presenta un análisis exploratorio de los **presuntos homicidios de mujeres en México durante 2024**, utilizando los microdatos de las **Estadísticas de Defunciones Registradas (EDR) del INEGI**.

El objetivo del ejercicio es **identificar diferencias en las formas de violencia letal contra las mujeres** según condiciones estructurales como la edad y la condición indígena, desde una **perspectiva de derechos humanos e interseccional**.

---

## Contenido del repositorio

homicidios-mx-2024/
│
├── data/
│ ├── raw/
│ │ └── DEFUN24.dbf # Microdatos EDR INEGI 2024
│ └── processed/
│
├── outputs/
│ ├── log_mujeres_presunto_homicidio_2024.txt
│ ├── resumen_porcentajes_mujeres_2024.csv
│ ├── top10_forma_muerte_mujeres_general_2024.png
│ ├── top10_forma_muerte_mujeres_indigenas_2024.png
│ ├── top10_forma_muerte_mujeres_menores_18_2024.png
│ └── top10_forma_muerte_mujeres_60_mas_2024.png
│
├── scripts/
│ └── analisis_mujeres_presunto_homicidio_2024.R
│
├── homicidios-mx-2024.Rproj
└── README.md


---

## Fuente de datos

- **Instituto Nacional de Estadística y Geografía (INEGI)**
- **Estadísticas de Defunciones Registradas (EDR)**
- Microdatos 2024
- Archivo: `DEFUN24.dbf`

Los datos son de acceso público y pueden consultarse en:
https://www.inegi.org.mx/programas/mortalidad/

---

## Alcance del análisis

### Definición de presunto homicidio
Se consideran **presuntos homicidios** las defunciones cuya causa básica de muerte corresponde a los códigos **X85–Y09 de la CIE-10**, que agrupan agresiones intencionales.

### Población analizada
- Mujeres (`SEXO == 2`)
- Presuntos homicidios (CIE-10 X85–Y09)
- Año 2024

### Grupos analíticos
El análisis compara cuatro grupos:
1. Total de mujeres víctimas de presunto homicidio  
2. Mujeres indígenas (`CONINDIG == 1`)  
3. Niñas y adolescentes (menores de 18 años)  
4. Mujeres de 60 años y más  

---

## Consideraciones metodológicas

### Estimación de la edad
La variable `EDAD` en los microdatos presenta una codificación que no permite interpretarla directamente como años cumplidos. Para evitar errores, **la edad se estimó a partir de los años de nacimiento y muerte**:

edad estimada = ANIO_OCUR (o ANIO_REGIS) − ANIO_NACIM


Se validaron rangos plausibles (0–110 años). Los registros fuera de estos rangos se excluyen del análisis por edad.

### Forma de defunción
Las formas de agresión se clasifican con base en los códigos CIE-10 (X85–Y09).  
Para cada grupo se presentan las **10 formas más frecuentes**, expresadas como **porcentaje del total del grupo**, lo que permite una comparación entre poblaciones.

---

## Productos analíticos

- Porcentajes generales de:
  - niñas y adolescentes
  - mujeres de 60 años y más
  - mujeres indígenas (con denominador específico)
- Gráficas comparativas de las principales formas de defunción por grupo
- Registro completo de ejecución (log) para asegurar transparencia y reproducibilidad

Todos los productos se generan automáticamente en la carpeta `outputs/`.

---

## Enfoque de derechos humanos

Este análisis parte de la premisa de que la violencia letal contra las mujeres **no es uniforme** y que su expresión está atravesada por desigualdades estructurales.

Los resultados muestran diferencias relevantes en:
- el uso de armas de fuego,
- la presencia de violencia de proximidad (asfixia, arma blanca, golpes),
- y la frecuencia de categorías no especificadas, que reflejan limitaciones institucionales en el registro y la investigación de los homicidios.

El objetivo no es individualizar responsabilidades, sino **visibilizar patrones estructurales** que deben informar políticas públicas de prevención, atención y justicia.

---

## Reproducibilidad

Para reproducir el análisis:

1. Clona este repositorio
2. Descarga el archivo `DEFUN24.dbf` del INEGI
3. Colócalo en `data/raw/`
4. Abre `homicidios-mx-2024.Rproj` en RStudio
5. Ejecuta el script:
scripts/analisis_mujeres_presunto_homicidio_2024.R


---

## Autoría

**Natalia Ruiz**  
Análisis de datos con enfoque en derechos humanos, género e interseccionalidad.

Ejercicio realizado como simulación de trabajo analítico alineado a la metodología y valores de interseccionalidad.

---

## Nota ética

Este repositorio utiliza datos públicos, anonimizados y agregados.  
Los resultados deben interpretarse con responsabilidad, evitando lecturas punitivas, estigmatizantes o descontextualizadas de la violencia.

---

## Licencia

Este proyecto se comparte con fines analíticos y educativos.  
Cualquier reutilización debe respetar las condiciones de uso de los microdatos del INEGI y citar la fuente.
