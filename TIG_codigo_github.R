# ============================================================
# Proyecto: "Alerta Temprana de Liquidez en Fondos Mutuos Chilenos:
#            Predicción de Flujos Netos y Rescates Extremos (2024–2026)"
# Asignatura: Ciencia de Datos (ICOM801) - Universidad Andrés Bello
# Docente: Felipe Cárcamo Jiménez
# Autores: Valentina Cid, Luis Jiménez y Diego Zapata
# Repositorio: https://github.com/DsplatZ/Alerta-Temprana-de-Liquidez-en-Fondos-Mutuos-Chilenos-Predicci-n-de-Flujos-Netos-y-Rescates-Extremos
# ============================================================

# ============================================================
# 0. GESTIÓN DE DEPENDENCIAS Y RUTAS RELATIVAS DEL REPOSITORIO
# ============================================================
paquetes_requeridos <- c(
  "tidyverse", "tidymodels", "ranger", "readxl",
  "lubridate", "quantmod", "zoo", "skimr"
)

paquetes_faltantes <- paquetes_requeridos[!(paquetes_requeridos %in% installed.packages()[, "Package"])]
if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes)
}

library(tidyverse)
library(tidymodels)
library(ranger)
library(readxl)
library(lubridate)
library(quantmod)
library(zoo)
library(skimr)

# Rutas relativas respecto a la raíz del repositorio
ruta_data    <- "data"
ruta_raw     <- file.path(ruta_data, "raw")
ruta_cmf     <- file.path(ruta_raw, "CMF")
ruta_bcch    <- file.path(ruta_raw, "Banco_Central")
ruta_uf      <- file.path(ruta_raw, "UF_CLP_diario")
ruta_outputs <- "outputs"

if (!dir.exists(ruta_data))    dir.create(ruta_data, recursive = TRUE)
if (!dir.exists(ruta_outputs)) dir.create(ruta_outputs, recursive = TRUE)

# ============================================================
# PRIMERA PARTE: ETL Y CONSTRUCCIÓN DE TABLA ANALÍTICA (2024–2026)
# ============================================================
# Nota: Si los archivos crudos de CMF/BCCh no están descargados localmente
# en data/raw/, el script utiliza directamente data/tabla_analitica_fondos.csv
# incluido en este repositorio para garantizar reproducibilidad inmediata.

archivo_ident <- list.files(ruta_raw, pattern = "^fm_ident_.*\\.txt$", full.names = TRUE) |>
  sort() |>
  tail(1)

if (length(archivo_ident) > 0 && dir.exists(ruta_cmf)) {
  message("Ejecutando Parte 1 (ETL completo desde archivos crudos)...")

  # PASO 1. MAESTRO DE IDENTIFICACIÓN DE FONDOS MUTUOS (CMF)
  fm_ident_df <- read_delim(
    file = archivo_ident,
    delim = ";",
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) |>
    transmute(
      RUN_FM         = as.integer(`RUN Fondo`),
      administradora = str_squish(`Raz. Social Administradora`),
      nombre_fondo   = str_squish(`Nombre Fondo`),
      tipo_fm_cod    = as.integer(`Tipo de Fondo Mutuo`),
      categoria_fm   = case_when(
        tipo_fm_cod %in% c(1, 2)    ~ "Deuda_CP",      # Money Market y Deuda <= 365d
        tipo_fm_cod == 3            ~ "Deuda_MLP",     # Deuda Mediano y Largo Plazo
        tipo_fm_cod == 4            ~ "Mixto",
        tipo_fm_cod == 5            ~ "Accionario",
        tipo_fm_cod %in% c(6, 7, 8) ~ "Otros_Estr_Calif",
        TRUE                        ~ "Sin_Clasificar"
      ),
      moneda_ri      = str_squish(Moneda)
    ) |>
    distinct(RUN_FM, .keep_all = TRUE)

  cat("Maestro CMF cargado:", nrow(fm_ident_df), "fondos únicos.\n")

  # PASO 2. UNIDAD DE FOMENTO DIARIA (BANCO CENTRAL DE CHILE)
  archivos_uf <- list.files(ruta_uf, pattern = "^UF_IVP_DIARIO_.*\\.xlsx$", full.names = TRUE)

  uf_df <- archivos_uf |>
    map(\(f) read_excel(
      path = f,
      sheet = "Cuadro",
      skip = 3,
      col_names = c("fecha_raw", "uf_raw"),
      col_types = "text"
    )) |>
    list_rbind() |>
    mutate(
      fecha = suppressWarnings(if_else(
        str_detect(fecha_raw, "^[0-9]{5}$"),
        as.Date(as.numeric(fecha_raw), origin = "1899-12-30"),
        as_date(ymd_hms(fecha_raw, truncated = 3))
      )),
      uf_clp = as.numeric(str_replace(uf_raw, ",", "."))
    ) |>
    select(fecha, uf_clp) |>
    drop_na(fecha, uf_clp) |>
    distinct(fecha, .keep_all = TRUE) |>
    arrange(fecha)

  write_csv(uf_df, file.path(ruta_data, "uf_2024_2026.csv"))
  cat("Serie UF cargada:", nrow(uf_df), "días corridos.\n")

  # PASO 3. CONSOLIDACIÓN DE FONDOS MUTUOS (CMF) + VALORIZACIÓN CLP
  archivos_cmf <- list.files(ruta_cmf, pattern = "\\.txt$", full.names = TRUE)

  cmf_detalle <- archivos_cmf |>
    map(\(f) read_delim(
      f,
      delim = ";",
      col_select = c(RUN_FM, SERIE, FECHA_INF, MONEDA,
                     CUOTAS_APORTADAS, CUOTAS_RESCATADAS,
                     VALOR_CUOTA, PATRIMONIO_NETO),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )) |>
    list_rbind() |>
    distinct() |>
    filter(MONEDA %in% c("$$", "UF")) |>
    mutate(
      RUN_FM           = as.integer(RUN_FM),
      fecha            = ymd(FECHA_INF),
      cuotas_aportadas = parse_number(CUOTAS_APORTADAS, locale = locale(decimal_mark = ".")),
      cuotas_rescates  = parse_number(CUOTAS_RESCATADAS, locale = locale(decimal_mark = ".")),
      valor_cuota      = parse_number(VALOR_CUOTA, locale = locale(decimal_mark = ".")),
      patrimonio_neto  = parse_number(PATRIMONIO_NETO, locale = locale(decimal_mark = "."))
    ) |>
    left_join(uf_df, by = "fecha", relationship = "many-to-one") |>
    left_join(fm_ident_df |> select(RUN_FM, tipo_fm_cod, categoria_fm),
              by = "RUN_FM", relationship = "many-to-one") |>
    mutate(
      categoria_fm   = replace_na(categoria_fm, "Sin_Clasificar"),
      factor_clp     = if_else(MONEDA == "UF", uf_clp, 1),
      patrimonio_clp = patrimonio_neto * factor_clp,
      aportes_clp    = cuotas_aportadas * valor_cuota * factor_clp,
      rescates_clp   = cuotas_rescates * valor_cuota * factor_clp
    )

  cmf_raw <- cmf_detalle |>
    group_by(fecha) |>
    summarise(
      patrimonio_mm = sum(patrimonio_clp, na.rm = TRUE) / 1e6,
      aportes_mm    = sum(aportes_clp, na.rm = TRUE) / 1e6,
      rescates_mm   = sum(rescates_clp, na.rm = TRUE) / 1e6,
      .groups = "drop"
    )

  cmf_cat_raw <- cmf_detalle |>
    filter(categoria_fm %in% c("Deuda_CP", "Deuda_MLP", "Accionario")) |>
    group_by(fecha, categoria_fm) |>
    summarise(
      pat_cat_mm = sum(patrimonio_clp, na.rm = TRUE) / 1e6,
      flu_cat_mm = sum(aportes_clp - rescates_clp, na.rm = TRUE) / 1e6,
      .groups = "drop"
    )

  # PASO 4. DESCARGA DE MERCADO: IPSA Y DÓLAR (YAHOO FINANCE)
  fecha_inicio <- "2024-01-02"

  IPSA <- tryCatch(
    getSymbols("^IPSA", src = "yahoo", from = fecha_inicio, auto.assign = FALSE),
    error = function(e) NULL
  )

  if (is.null(IPSA) || nrow(IPSA) < 100) {
    message("Usando ETF CFMITNIPSA.SN como proxy continuo del IPSA...")
    IPSA <- getSymbols("CFMITNIPSA.SN", src = "yahoo", from = fecha_inicio, auto.assign = FALSE)
  }

  DOLAR <- getSymbols("USDCLP=X", src = "yahoo", from = fecha_inicio, auto.assign = FALSE)

  yahoo_df <- full_join(
    tibble(fecha = as_date(index(IPSA)),  ipsa  = as.numeric(Cl(IPSA))),
    tibble(fecha = as_date(index(DOLAR)), dolar = as.numeric(Cl(DOLAR))),
    by = "fecha"
  ) |>
    arrange(fecha)

  write_csv(yahoo_df, file.path(ruta_data, "base_IPSA_DOLAR.csv"))

  # PASO 5. TASA DE POLÍTICA MONETARIA (BANCO CENTRAL DE CHILE)
  archivos_tpm <- list.files(ruta_bcch, pattern = "^TPM_C1.*\\.xlsx$", full.names = TRUE)

  tpm_df <- archivos_tpm |>
    map(\(f) read_excel(
      path = f,
      sheet = "Cuadro",
      skip = 3,
      col_names = c("fecha_raw", "tpm_raw"),
      col_types = "text"
    )) |>
    list_rbind() |>
    mutate(
      fecha = suppressWarnings(if_else(
        str_detect(fecha_raw, "^[0-9]{5}$"),
        as.Date(as.numeric(fecha_raw), origin = "1899-12-30"),
        as_date(ymd_hms(fecha_raw, truncated = 3))
      )),
      tpm = as.numeric(str_replace(tpm_raw, ",", "."))
    ) |>
    select(fecha, tpm) |>
    drop_na(fecha, tpm) |>
    distinct(fecha, .keep_all = TRUE) |>
    arrange(fecha)

  write_csv(tpm_df, file.path(ruta_data, "tpm_2024_2026.csv"))

  # PASO 6. ALINEACIÓN CONTABLE Y CONSTRUCCIÓN DE TABLA ANALÍTICA
  dias_habiles_bcch <- tpm_df |>
    select(fecha) |>
    mutate(fecha_habil = fecha)

  cmf_habil <- cmf_raw |>
    arrange(fecha) |>
    left_join(dias_habiles_bcch, by = "fecha") |>
    fill(fecha_habil, .direction = "down") |>
    drop_na(fecha_habil) |>
    group_by(fecha_habil) |>
    summarise(
      patrimonio_mm = last(patrimonio_mm, order_by = fecha),
      aportes_mm    = sum(aportes_mm, na.rm = TRUE),
      rescates_mm   = sum(rescates_mm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(fecha = fecha_habil)

  cmf_cat_habil <- cmf_cat_raw |>
    arrange(categoria_fm, fecha) |>
    left_join(dias_habiles_bcch, by = "fecha") |>
    group_by(categoria_fm) |>
    fill(fecha_habil, .direction = "down") |>
    drop_na(fecha_habil) |>
    group_by(fecha_habil, categoria_fm) |>
    summarise(
      pat_cat_mm = last(pat_cat_mm, order_by = fecha),
      flu_cat_mm = sum(flu_cat_mm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(fecha = fecha_habil) |>
    arrange(categoria_fm, fecha) |>
    group_by(categoria_fm) |>
    mutate(flujo_rel_cat = (flu_cat_mm / lag(pat_cat_mm, 1)) * 100) |>
    ungroup() |>
    select(fecha, categoria_fm, pat_cat_mm, flujo_rel_cat) |>
    pivot_wider(
      names_from  = categoria_fm,
      values_from = c(pat_cat_mm, flujo_rel_cat)
    )

  datos_analiticos <- cmf_habil |>
    inner_join(tpm_df, by = "fecha", relationship = "one-to-one") |>
    left_join(uf_df, by = "fecha", relationship = "one-to-one") |>
    left_join(yahoo_df, by = "fecha", relationship = "one-to-one") |>
    left_join(cmf_cat_habil, by = "fecha", relationship = "one-to-one") |>
    arrange(fecha) |>
    fill(ipsa, dolar, uf_clp, .direction = "down") |>
    mutate(
      flujo_neto            = aportes_mm - rescates_mm,
      patrimonio_uf_mil     = (patrimonio_mm * 1e6) / (uf_clp * 1e3),
      flujo_neto_uf_mil     = (flujo_neto * 1e6) / (uf_clp * 1e3),
      flujo_relativo        = (flujo_neto / lag(patrimonio_mm, 1)) * 100,
      flujo_lag1            = lag(flujo_relativo, 1),
      flujo_lag2            = lag(flujo_relativo, 2),
      flujo_prom5           = zoo::rollmean(lag(flujo_relativo, 1), k = 5, fill = NA, align = "right"),
      d_tpm_lag1            = lag(tpm, 1) - lag(tpm, 2),
      ret_dolar_lag1        = (lag(dolar, 1) / lag(dolar, 2) - 1) * 100,
      ret_ipsa_lag1         = (lag(ipsa, 1) / lag(ipsa, 2) - 1) * 100,
      var_uf_lag1           = (lag(uf_clp, 1) / lag(uf_clp, 2) - 1) * 100,
      var_uf_20d_lag1       = (lag(uf_clp, 1) / lag(uf_clp, 21) - 1) * 100,
      tpm_real_lag1         = lag(tpm, 1) - (var_uf_20d_lag1 * 12),
      flujo_deuda_cp_lag1   = lag(flujo_rel_cat_Deuda_CP, 1),
      flujo_deuda_mlp_lag1  = lag(flujo_rel_cat_Deuda_MLP, 1),
      flujo_accionario_lag1 = lag(flujo_rel_cat_Accionario, 1),
      share_deuda_cp_lag1   = (lag(pat_cat_mm_Deuda_CP, 1) / lag(patrimonio_mm, 1)) * 100,
      dia_semana            = factor(wday(fecha, label = TRUE)),
      es_fin_mes            = factor(if_else(day(fecha) >= 25, "Si", "No"))
    ) |>
    drop_na()

  write_csv(datos_analiticos, file.path(ruta_data, "tabla_analitica_fondos.csv"))
} else {
  message("Archivos crudos no detectados en data/raw/. Cargando tabla analítica consolidada desde data/tabla_analitica_fondos.csv...")
}

# ============================================================
# SEGUNDA PARTE: EDA, MODELADO SUPERVISADO Y EVALUACIÓN
# ============================================================

# PASO 1. CARGA DE DATOS Y ANÁLISIS EXPLORATORIO (EDA)
archivo_analitico <- if (file.exists(file.path(ruta_data, "tabla_analitica_fondos.csv"))) {
  file.path(ruta_data, "tabla_analitica_fondos.csv")
} else {
  "tabla_analitica_fondos.csv"
}

datos <- read_csv(archivo_analitico, show_col_types = FALSE) |>
  arrange(fecha) |>
  mutate(
    dia_semana = factor(dia_semana),
    es_fin_mes = factor(es_fin_mes, levels = c("No", "Si"))
  )

cat("\nObservaciones finales:", nrow(datos), "\n")
cat("Período:", as.character(min(datos$fecha)), "a", as.character(max(datos$fecha)), "\n")

cat("\n=== RESUMEN EXPLORATORIO (EDA) ===\n")
skim(datos |> select(
  flujo_relativo, flujo_lag1, flujo_prom5, d_tpm_lag1,
  tpm_real_lag1, var_uf_lag1, ret_dolar_lag1, ret_ipsa_lag1,
  flujo_deuda_cp_lag1, flujo_deuda_mlp_lag1, share_deuda_cp_lag1
))

# PASO 2. PARTICIÓN CRONOLÓGICA ESTRICTA (80% TRAIN / 20% TEST)
set.seed(2026)
split_temp <- initial_time_split(datos, prop = 0.80)
train_data <- training(split_temp)
test_data  <- testing(split_temp)

cat("\nTrain:", nrow(train_data), "días (", as.character(min(train_data$fecha)), "a", as.character(max(train_data$fecha)), ")\n")
cat("Test :", nrow(test_data),  "días (", as.character(min(test_data$fecha)),  "a", as.character(max(test_data$fecha)),  ")\n")

# Umbrales calculados EXCLUSIVAMENTE sobre train_data (evita fuga de datos)
umbral_p10 <- quantile(train_data$flujo_relativo, probs = 0.10)
piso_p005  <- quantile(train_data$flujo_relativo, probs = 0.005)

cat("Umbral de Alerta de Retiro Extremo (P10 en train):", round(umbral_p10, 4), "%\n\n")

train_data <- train_data |>
  mutate(
    flujo_reg     = pmax(flujo_relativo, piso_p005),
    alerta_retiro = factor(if_else(flujo_relativo <= umbral_p10, "Si", "No"), levels = c("No", "Si"))
  )

test_data <- test_data |>
  mutate(
    flujo_reg     = pmax(flujo_relativo, piso_p005),
    alerta_retiro = factor(if_else(flujo_relativo <= umbral_p10, "Si", "No"), levels = c("No", "Si"))
  )

cat("Distribución de clases (Alerta de Retiro):\n")
print(table(Train = train_data$alerta_retiro))
print(table(Test  = test_data$alerta_retiro))

# PASO 3. PISTA A: REGRESIÓN (Magnitud del flujo neto relativo)
receta_reg <- recipe(
  flujo_reg ~ flujo_lag1 + flujo_lag2 + flujo_prom5 +
    d_tpm_lag1 + tpm_real_lag1 + var_uf_lag1 +
    ret_dolar_lag1 + ret_ipsa_lag1 +
    flujo_deuda_cp_lag1 + flujo_deuda_mlp_lag1 +
    flujo_accionario_lag1 + share_deuda_cp_lag1 +
    dia_semana + es_fin_mes,
  data = train_data
) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

mod_lm <- linear_reg() |>
  set_engine("lm")

mod_rf_reg <- rand_forest(trees = 500, min_n = 5) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

wf_lm     <- workflow() |> add_recipe(receta_reg) |> add_model(mod_lm)     |> fit(train_data)
wf_rf_reg <- workflow() |> add_recipe(receta_reg) |> add_model(mod_rf_reg) |> fit(train_data)

cat("\n=== COEFICIENTES REGRESIÓN LINEAL (CONTRASTE H1) ===\n")
print(summary(extract_fit_engine(wf_lm)))

pred_reg <- test_data |>
  select(fecha, flujo_reg) |>
  bind_cols(
    predict(wf_lm, test_data)     |> rename(pred_lm = .pred),
    predict(wf_rf_reg, test_data) |> rename(pred_rf = .pred)
  )

metricas_reg <- bind_rows(
  pred_reg |> metrics(truth = flujo_reg, estimate = pred_lm) |> mutate(modelo = "Regresión Lineal (OLS)"),
  pred_reg |> metrics(truth = flujo_reg, estimate = pred_rf) |> mutate(modelo = "Random Forest")
) |>
  filter(.metric %in% c("rmse", "mae", "rsq")) |>
  select(modelo, .metric, .estimate)

cat("\n=== MÉTRICAS FUERA DE MUESTRA: REGRESIÓN ===\n")
print(metricas_reg)

# PASO 4. PISTA B: CLASIFICACIÓN (Alerta temprana de retiro)
receta_cls <- recipe(
  alerta_retiro ~ flujo_lag1 + flujo_lag2 + flujo_prom5 +
    d_tpm_lag1 + tpm_real_lag1 + var_uf_lag1 +
    ret_dolar_lag1 + ret_ipsa_lag1 +
    flujo_deuda_cp_lag1 + flujo_deuda_mlp_lag1 +
    flujo_accionario_lag1 + share_deuda_cp_lag1 +
    dia_semana + es_fin_mes,
  data = train_data
) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

mod_log <- logistic_reg() |>
  set_engine("glm")

mod_rf_cls <- rand_forest(trees = 500, min_n = 5) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

wf_log_spec    <- workflow() |> add_recipe(receta_cls) |> add_model(mod_log)
wf_rf_cls_spec <- workflow() |> add_recipe(receta_cls) |> add_model(mod_rf_cls)

wf_log    <- wf_log_spec    |> fit(train_data)
wf_rf_cls <- wf_rf_cls_spec |> fit(train_data)

pred_cls <- test_data |>
  select(fecha, alerta_retiro) |>
  bind_cols(
    predict(wf_log, test_data)                 |> rename(clase_log = .pred_class),
    predict(wf_log, test_data, type = "prob")  |> select(prob_log = .pred_Si),
    predict(wf_rf_cls, test_data)              |> rename(clase_rf = .pred_class),
    predict(wf_rf_cls, test_data, type = "prob") |> select(prob_rf = .pred_Si)
  )

metricas_cls <- bind_rows(
  tibble(
    modelo   = "Regresión Logística",
    recall   = recall_vec(pred_cls$alerta_retiro, pred_cls$clase_log, event_level = "second"),
    roc_auc  = roc_auc_vec(pred_cls$alerta_retiro, pred_cls$prob_log, event_level = "second"),
    accuracy = accuracy_vec(pred_cls$alerta_retiro, pred_cls$clase_log)
  ),
  tibble(
    modelo   = "Random Forest",
    recall   = recall_vec(pred_cls$alerta_retiro, pred_cls$clase_rf, event_level = "second"),
    roc_auc  = roc_auc_vec(pred_cls$alerta_retiro, pred_cls$prob_rf, event_level = "second"),
    accuracy = accuracy_vec(pred_cls$alerta_retiro, pred_cls$clase_rf)
  )
)

cat("\n=== MÉTRICAS FUERA DE MUESTRA: CLASIFICACIÓN (UMBRAL DEFAULT 0.50) ===\n")
print(metricas_cls)

# PASO 5. IMPORTANCIA DE VARIABLES Y GRÁFICOS DE REGRESIÓN
imp_df <- wf_rf_reg |>
  extract_fit_engine() |>
  pluck("variable.importance") |>
  enframe(name = "predictor", value = "importancia") |>
  arrange(desc(importancia))

cat("\n=== RANKING DE IMPORTANCIA DE PREDICTORES (RANDOM FOREST) ===\n")
print(imp_df)

etiquetas_variables <- c(
  flujo_lag2            = "Flujo neto hace 2 días",
  flujo_prom5           = "Promedio móvil flujo (5 días)",
  dia_semana_mié        = "Efecto calendario: Miércoles",
  dia_semana_vie        = "Efecto calendario: Viernes",
  share_deuda_cp_lag1   = "Participación Deuda Corto Plazo (%)",
  flujo_deuda_mlp_lag1  = "Flujo Deuda Mediano/Largo Plazo (ayer)",
  flujo_accionario_lag1 = "Flujo Fondos Accionarios (ayer)",
  ret_ipsa_lag1         = "Retorno diario IPSA (ayer)",
  flujo_lag1            = "Flujo neto de ayer",
  flujo_deuda_cp_lag1   = "Flujo Deuda Corto Plazo (ayer)",
  ret_dolar_lag1        = "Variación Dólar observado (ayer)",
  tpm_real_lag1         = "TPM real ex-ante (ajustada UF)",
  var_uf_lag1           = "Variación diaria UF (ayer)",
  es_fin_mes_Si         = "Efecto fin de mes (días 25-31)",
  dia_semana_lun        = "Efecto calendario: Lunes",
  dia_semana_mar        = "Efecto calendario: Martes",
  d_tpm_lag1            = "Cambio puntual TPM (ayer)"
)

g_vip <- imp_df |>
  mutate(predictor_limpio = recode(predictor, !!!etiquetas_variables)) |>
  ggplot(aes(x = reorder(predictor_limpio, importancia), y = importancia)) +
  geom_col(fill = "#1f4e78", width = 0.7) +
  coord_flip() +
  labs(
    title    = "Importancia de Predictores en Flujos Netos Diarios (2024–2026)",
    subtitle = "Random Forest (Impureza de Varianza) - Información disponible al cierre de ayer",
    x        = NULL,
    y        = "Importancia Relativa"
  ) +
  theme_minimal()

print(g_vip)
ggsave(file.path(ruta_outputs, "grafico_importancia_variables.png"), g_vip, width = 8, height = 5, dpi = 300)

g_pred <- pred_reg |>
  pivot_longer(cols = c(flujo_reg, pred_lm, pred_rf), names_to = "serie", values_to = "valor") |>
  mutate(serie = recode(serie,
    flujo_reg = "Flujo Real (%)",
    pred_lm   = "Predicción OLS",
    pred_rf   = "Predicción Random Forest"
  )) |>
  ggplot(aes(x = fecha, y = valor, color = serie, linetype = serie)) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  scale_color_manual(values = c(
    "Flujo Real (%)"           = "black",
    "Predicción OLS"           = "#d95f02",
    "Predicción Random Forest" = "#1b9e77"
  )) +
  scale_linetype_manual(values = c(
    "Flujo Real (%)"           = "solid",
    "Predicción OLS"           = "dashed",
    "Predicción Random Forest" = "solid"
  )) +
  labs(
    title    = "Desempeño Predictivo Fuera de Muestra (Test Set 2026)",
    subtitle = "Comparación entre Regresión Lineal y Random Forest",
    x        = "Fecha",
    y        = "Flujo Neto Relativo (% Patrimonio)",
    color    = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g_pred)
ggsave(file.path(ruta_outputs, "grafico_reg_lineal.png"), g_pred, width = 9, height = 4.5, dpi = 300)

# PASO 6. CALIBRACIÓN DE UMBRAL (CV TEMPORAL EN TRAIN) Y GRÁFICOS DE CLASIFICACIÓN
set.seed(2026)
folds_temp <- rolling_origin(
  data       = train_data,
  initial    = 280,
  assess     = 45,
  skip       = 25,
  cumulative = TRUE
)

ctrl_oof <- control_resamples(save_pred = TRUE)

oof_log <- fit_resamples(wf_log_spec,    resamples = folds_temp, control = ctrl_oof) |>
  collect_predictions() |>
  mutate(modelo = "Regresión Logística")

oof_rf  <- fit_resamples(wf_rf_cls_spec, resamples = folds_temp, control = ctrl_oof) |>
  collect_predictions() |>
  mutate(modelo = "Random Forest")

buscar_umbral_optimo <- function(df_oof) {
  grid_umbrales <- seq(0.05, 0.50, by = 0.01)
  map_dfr(grid_umbrales, function(u) {
    pred_u <- factor(if_else(df_oof$.pred_Si >= u, "Si", "No"), levels = c("No", "Si"))
    rec    <- recall_vec(df_oof$alerta_retiro, pred_u, event_level = "second")
    esp    <- specificity_vec(df_oof$alerta_retiro, pred_u, event_level = "second")
    tibble(
      umbral     = u,
      recall_cv  = rec,
      especif_cv = esp,
      youden_j   = rec + esp - 1
    )
  }) |>
    arrange(desc(youden_j), desc(recall_cv)) |>
    slice(1)
}

opt_log <- buscar_umbral_optimo(oof_log)
opt_rf  <- buscar_umbral_optimo(oof_rf)

cat("\n=== UMBRALES ÓPTIMOS CALIBRADOS EN CV TEMPORAL (TRAIN) ===\n")
print(bind_rows(
  opt_log |> mutate(modelo = "Regresión Logística", .before = 1),
  opt_rf  |> mutate(modelo = "Random Forest", .before = 1)
))

pred_cls_cal <- pred_cls |>
  mutate(
    clase_log_cal = factor(if_else(prob_log >= opt_log$umbral, "Si", "No"), levels = c("No", "Si")),
    clase_rf_cal  = factor(if_else(prob_rf  >= opt_rf$umbral,  "Si", "No"), levels = c("No", "Si"))
  )

metricas_cls_cal <- bind_rows(
  tibble(
    modelo        = "Regresión Logística",
    umbral_opt    = opt_log$umbral,
    recall        = recall_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$clase_log_cal, event_level = "second"),
    especificidad = specificity_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$clase_log_cal, event_level = "second"),
    precision     = precision_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$clase_log_cal, event_level = "second"),
    roc_auc       = roc_auc_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$prob_log, event_level = "second"),
    accuracy      = accuracy_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$clase_log_cal)
  ),
  tibble(
    modelo        = "Random Forest",
    umbral_opt    = opt_rf$umbral,
    recall        = recall_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$clase_rf_cal, event_level = "second"),
    especificidad = specificity_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$clase_rf_cal, event_level = "second"),
    precision     = precision_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$clase_rf_cal, event_level = "second"),
    roc_auc       = roc_auc_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$prob_rf, event_level = "second"),
    accuracy      = accuracy_vec(pred_cls_cal$alerta_retiro, pred_cls_cal$clase_rf_cal)
  )
)

cat("\n=== MÉTRICAS FUERA DE MUESTRA: CLASIFICACIÓN CALIBRADA (TEST SET) ===\n")
print(metricas_cls_cal)

cat("\n=== MATRIZ DE CONFUSIÓN CALIBRADA: REGRESIÓN LOGÍSTICA ===\n")
print(conf_mat(pred_cls_cal, truth = alerta_retiro, estimate = clase_log_cal))

cat("\n=== MATRIZ DE CONFUSIÓN CALIBRADA: RANDOM FOREST ===\n")
print(conf_mat(pred_cls_cal, truth = alerta_retiro, estimate = clase_rf_cal))

lbl_log <- sprintf("Regresión Logística (AUC = %.3f)", metricas_cls_cal$roc_auc[1])
lbl_rf  <- sprintf("Random Forest (AUC = %.3f)", metricas_cls_cal$roc_auc[2])

roc_df <- bind_rows(
  roc_curve(pred_cls_cal, truth = alerta_retiro, prob_log, event_level = "second") |> mutate(modelo = lbl_log),
  roc_curve(pred_cls_cal, truth = alerta_retiro, prob_rf,  event_level = "second") |> mutate(modelo = lbl_rf)
) |>
  mutate(modelo = factor(modelo, levels = c(lbl_log, lbl_rf)))

puntos_op <- tibble(
  modelo   = factor(c(lbl_log, lbl_rf), levels = c(lbl_log, lbl_rf)),
  fpr      = 1 - metricas_cls_cal$especificidad,
  tpr      = metricas_cls_cal$recall,
  etiqueta = sprintf("Corte = %.2f", metricas_cls_cal$umbral_opt),
  nudge_x  = c(0.12, -0.01),
  nudge_y  = c(-0.06, -0.10)
)

g_roc <- roc_df |>
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = modelo)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
  geom_line(linewidth = 1.1) +
  geom_point(data = puntos_op, aes(x = fpr, y = tpr), size = 3.2) +
  geom_text(data = puntos_op, aes(x = fpr + nudge_x, y = tpr + nudge_y, label = etiqueta),
            size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = setNames(c("#d95f02", "#1b9e77"), c(lbl_log, lbl_rf))) +
  coord_equal() +
  labs(
    title    = "Curva ROC Fuera de Muestra (Test Set 2026)",
    subtitle = "Logística vs. Random Forest (puntos: umbral calibrado CV)",
    x        = "Tasa de Falsas Alarmas (1 - Especificidad)",
    y        = "Sensibilidad (Recall)",
    color    = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 9.5),
    axis.title.y    = element_text(size = 10, margin = margin(r = 8)),
    axis.title.x    = element_text(size = 10, margin = margin(t = 6)),
    plot.margin     = margin(12, 16, 12, 16)
  )

print(g_roc)
ggsave(file.path(ruta_outputs, "grafico_roc_clasificacion.png"), g_roc, width = 7, height = 5.5, dpi = 300)

eventos_reales <- pred_cls_cal |>
  filter(alerta_retiro == "Si")

g_cls_temp <- pred_cls_cal |>
  select(fecha, prob_log, prob_rf) |>
  pivot_longer(cols = c(prob_log, prob_rf), names_to = "modelo", values_to = "probabilidad") |>
  mutate(modelo = recode(modelo,
    prob_log = "Prob. Regresión Logística",
    prob_rf  = "Prob. Random Forest"
  )) |>
  ggplot(aes(x = fecha, y = probabilidad, color = modelo, linetype = modelo)) +
  geom_vline(data = eventos_reales, aes(xintercept = fecha),
             color = "#b2182b", alpha = 0.25, linewidth = 1.2) +
  geom_line(linewidth = 0.85, alpha = 0.9) +
  geom_hline(yintercept = opt_rf$umbral, linetype = "dashed", color = "#1b9e77", linewidth = 0.6) +
  scale_color_manual(values = c(
    "Prob. Regresión Logística" = "#d95f02",
    "Prob. Random Forest"       = "#1b9e77"
  )) +
  scale_linetype_manual(values = c(
    "Prob. Regresión Logística" = "dashed",
    "Prob. Random Forest"       = "solid"
  )) +
  labs(
    title    = "Sistema de Alerta Temprana de Retiros Extremos (Test Set 2026)",
    subtitle = "Franjas rojas = días reales de retiro extremo (P10) | Línea verde horizontal = umbral calibrado RF",
    x        = "Fecha",
    y        = "Probabilidad Predicha de Retiro Extremo",
    color    = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g_cls_temp)
ggsave(file.path(ruta_outputs, "grafico_alerta_temporal_test.jpg"), g_cls_temp, width = 9, height = 4.5, dpi = 300)