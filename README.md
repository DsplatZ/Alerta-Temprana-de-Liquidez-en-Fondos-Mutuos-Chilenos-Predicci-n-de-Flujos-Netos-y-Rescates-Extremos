# Alerta Temprana de Liquidez en Fondos Mutuos Chilenos: Predicción de Flujos Netos y Rescates Extremos (2024–2026)

**Proyecto Integrado Grupal (TIG) — Ciencia de Datos (`ICOM801`)**  
**Institución:** Universidad Andrés Bello (UNAB) — Primavera 2026  
**Docente:** Felipe Cárcamo Jiménez  
**Integrantes:** Valentina Cid · Luis Jiménez · Diego Zapata  
**Repositorio Oficial:** [https://github.com/DsplatZ/Alerta-Temprana-de-Liquidez-en-Fondos-Mutuos-Chilenos-Predicci-n-de-Flujos-Netos-y-Rescates-Extremos](https://github.com/DsplatZ/Alerta-Temprana-de-Liquidez-en-Fondos-Mutuos-Chilenos-Predicci-n-de-Flujos-Netos-y-Rescates-Extremos)

---

## 1. Descripción del Proyecto y Problemática

En el sistema financiero chileno, los fondos mutuos —especialmente aquellos orientados a instrumentos de deuda de corto y mediano plazo— operan como vehículos de gestión de liquidez diaria para tesorerías corporativas, inversionistas institucionales y personas naturales. Frente a variaciones en tasas de interés, movimientos cambiarios o requerimientos estacionales de caja, la industria enfrenta jornadas de rescates netos abruptos que presionan la liquidez de las Administradoras Generales de Fondos (AGF).

Este proyecto aborda la siguiente pregunta aplicada: **¿Con qué precisión es posible anticipar hoy, al cierre de la jornada hábil ($t - 1$), el flujo neto diario relativo y la ocurrencia de retiros extremos del día siguiente ($t$) utilizando exclusivamente datos públicos macrofinancieros, composición de cartera y efectos calendario?**

### Hipótesis Contrastadas
* **$H_1$ (Señal Macrofinanciera Directa):** Las variaciones rezagadas de la Tasa de Política Monetaria (TPM), el dólar observado (USD/CLP) y la Unidad de Fomento (UF) contienen señal predictiva estadísticamente significativa sobre los flujos netos diarios. *(Rechazada en forma lineal aislada).*
* **$H_2$ (No Linealidad y Efectos Calendario):** Un algoritmo basado en ensambles de árboles de decisión (**Random Forest**) supera a los modelos lineales tradicionales (**OLS** y **Regresión Logística**) fuera de muestra al capturar interacciones no lineales entre inercia de flujos, rotación entre categorías y estacionalidad intrasemanal (miércoles de rescates y viernes de aportes). *(Confirmada fuera de muestra en 2026).*

---

## 2. Resumen de Resultados Empíricos (Test Set 2026)

Evaluado sobre **133 días hábiles inéditos** (21 de abril al 30 de octubre de 2026):
* **Pista A — Regresión (Estimación continua del flujo neto relativo %):** Random Forest reduce el error cuadrático medio respecto a Regresión Lineal OLS (**RMSE = 0,320%** vs. 0,331%; **$R^2$ = 28,1%**).
* **Pista B — Clasificación (Alerta temprana de retiro extremo $\le P_{10}$):** Mediante calibración del umbral de decisión por validación cruzada temporal expansiva (`rolling_origin`) maximizando el Índice de Youden ($J$), Random Forest (corte $= 0,16$) alcanza un **ROC-AUC de 0,894**, anticipando el **100 % (11 de 11)** de los días reales de retiro extremo con una especificidad del **70,5 %** (reduciendo en un 25 % las falsas alarmas frente a Regresión Logística).

---

## 3. Estructura Recomendada del Repositorio

```text
Alerta-Temprana-de-Liquidez-en-Fondos-Mutuos-Chilenos-Predicci-n-de-Flujos-Netos-y-Rescates-Extremos/
├── README.md                                   # Documentación principal del proyecto
├── TIG_codigo_github.R                         # Pipeline completo en R (ETL, EDA, Modelado y Gráficos)
├── data/                                       # Conjuntos de datos procesados listos para ejecución
│   ├── tabla_analitica_fondos.csv              # Panel maestro consolidado (663 días hábiles, 2024–2026)
│   ├── uf_2024_2026.csv                        # Serie diaria de la Unidad de Fomento (BCCh)
│   ├── tpm_2024_2026.csv                       # Serie diaria de la Tasa de Política Monetaria (BCCh)
│   └── base_IPSA_DOLAR.csv                     # Cotizaciones diarias USD/CLP y proxy IPSA (Yahoo Finance)
├── outputs/                                    # Gráficos de alta resolución generados por el script
│   ├── grafico_importancia_variables.png
│   ├── grafico_reg_lineal.png
│   ├── grafico_roc_clasificacion.png
│   └── grafico_alerta_temporal_test.jpg
└── docs/                                       # Entregables académicos finales
    ├── informe_tig_liquidez_fondos_mutuos.pdf
    └── PPT_FondosMutuos.pptx
```

---

## 4. Fuentes de Datos Públicas y Verificables

Todas las series utilizadas provienen de portales públicos oficiales, integradas en un panel diario libre de fuga de información (*no data leakage*):

1. **Comisión para el Mercado Financiero (CMF):**
   * *Contenido:* Cartolas diarias por serie (`RUN_FM`, `SERIE`, `FECHA_INF`, `MONEDA`, cuotas aportadas/rescatadas, valor cuota y patrimonio neto) y archivo maestro de identificación (`fm_ident`) con **1.353 fondos mutuos únicos**.
   * *Acceso público:* [Estadísticas de Fondos Mutuos - CMF Chile](https://www.cmfchile.cl/portal/estadisticas/617/w3-propertyvalue-28917.html)
2. **Banco Central de Chile (BCCh - Base de Datos Estadísticos):**
   * *Contenido:* Serie diaria de la Tasa de Política Monetaria (`TPM_C1`, % anual) y valor diario de la Unidad de Fomento (`UF_IVP_DIARIO`, en CLP).
   * *Acceso público:* [Base de Datos Estadísticos (BDE) - Banco Central de Chile](https://si3.bcentral.cl/Siete)
3. **Yahoo Finance (Series Financieras Públicas):**
   * *Contenido:* Precios diarios de cierre para el dólar observado (`USDCLP=X`) y el ETF local proxy continuo del índice bursátil IPSA (`CFMITNIPSA.SN`).
   * *Acceso público:* [Yahoo Finance](https://finance.yahoo.com/)

*Nota sobre reproducibilidad:* Debido al gran volumen de archivos planos diarios `.txt` publicados por la CMF, la carpeta `data/` incluye directamente la tabla consolidada `tabla_analitica_fondos.csv` y las series auxiliares procesadas, permitiendo ejecutar la etapa de EDA, modelamiento y evaluación de manera inmediata.

---

## 5. Requisitos e Instrucciones de Ejecución en R

### Dependencias
El proyecto fue desarrollado en **R (>= 4.3.0)** utilizando los siguientes paquetes del ecosistema `tidyverse` y `tidymodels`:
* Manipulación y series de tiempo: `tidyverse`, `readxl`, `lubridate`, `quantmod`, `zoo`, `skimr`
* Modelamiento y validación: `tidymodels`, `ranger`

Para instalar las dependencias faltantes en R, ejecute una única vez:
```r
install.packages(c("tidyverse", "tidymodels", "ranger", "readxl", "lubridate", "quantmod", "zoo", "skimr"))
```

### Pasos para Reproducir los Resultados
1. Clone o descargue este repositorio y asegúrese de que las carpetas `data/` y `outputs/` existan en el directorio raíz del proyecto.
2. Abra el archivo `TIG_codigo_github.R` en **RStudio** (estableciendo el directorio de trabajo en la raíz del repositorio mediante `Session > Set Working Directory > To Source File Location`).
3. Ejecute el script:
   * Si cuenta con los archivos crudos locales en `data/raw/`, el bloque **Parte 1 (ETL)** reconstruirá desde cero `tabla_analitica_fondos.csv`.
   * Si utiliza directamente los archivos incluidos en `data/`, el script omitirá la lectura de archivos crudos y ejecutará íntegramente la **Parte 2 (EDA, Partición Cronológica 80/20, Entrenamiento OLS/Logística/Random Forest, Validación Cruzada Temporal `rolling_origin` y Exportación de Gráficos)** en `outputs/`.

---

## 6. Licencia y Declaración de Uso de Inteligencia Artificial

* **Licencia:** El código fuente de este repositorio se distribuye bajo licencia abierta **MIT**, respetando los términos de uso público de los portales estadísticos de la CMF y el Banco Central de Chile.
* **Transparencia y Uso de IA:** Conforme a las directrices de integridad académica de la asignatura Ciencia de Datos (`ICOM801`), se declara el uso de herramientas de Inteligencia Artificial generativa (Gemini, ChatGPT y Claude) como apoyo en la estructuración del código en R, depuración de sintaxis y edición de redacción técnica. La definición metodológica, ejecución de experimentos, validación estadística e interpretación financiera fueron realizadas y verificadas íntegramente por los autores.