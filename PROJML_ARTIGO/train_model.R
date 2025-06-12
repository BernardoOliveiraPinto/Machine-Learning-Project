# Carregar bibliotecas essenciais para análise e modelagem
library(corrplot)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(lubridate)
library(fastDummies)
library(caret)
library(xgboost)
library(Metrics)
# 1. Leitura do dataset inicial
# ------------------------------------------------------------------------------
dados <- usinas_completo_com_meteo 

# 2. Visão geral dos dados
# ------------------------------------------------------------------------------
summary(dados)

# 3. Ajuste de pressão atmosférica para resolver problema de unidade
# ------------------------------------------------------------------------------
dados$`meteo_PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)` <- dados$`meteo_PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)` / 10
dados$`meteo_PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)` <- dados$`meteo_PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)` / 10
dados$`meteo_PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)` <- dados$`meteo_PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)` / 10

# 4. Correção de registros errados de pressão (< 891.5 então multiplicar por 10)
# ------------------------------------------------------------------------------
col <- "meteo_PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)"
dados[[col]] <- ifelse(
  dados[[col]] < 891.5,
  dados[[col]] * 10,
  dados[[col]]
)

# 5. Seleção inicial de colunas a remover (reduzir dimensionalidade)
# ------------------------------------------------------------------------------
initial_columns_drop = c(
  "meteo_TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)",
  "meteo_TEMPERATURA DO PONTO DE ORVALHO (°C)",
  "meteo_TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)",
  "meteo_TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)",
  "meteo_TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)",
  "meteo_TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)",
  "meteo_Unnamed: 19",
  "nom_subsistema",
  "nom_estado",
  "nom_tipocombustivel",
  "id_ons",
  "ceg",
  "source_file",
  "latitude",
  "longitude",
  "latitude_estacao",
  "longitude_estacao",
  "nom_tipousina",
  "meteo_source_file",
  "date",
  "hour",
  "meteo_PRECIPITAÇÃO TOTAL, HORÁRIO (mm)",
  "cod_modalidadeoperacao"
)

# Aplicação da remoção de colunas no dataset
dados <- dados[, !(names(dados) %in% initial_columns_drop)]

# 6. Imputação de valores ausentes (NA) por mediana em colunas meteorológicas
# ------------------------------------------------------------------------------
meteo_cols <- grep("^meteo", names(dados), value = TRUE)
for (col in meteo_cols) {
  dados[[col]] <- as.numeric(gsub(",", ".", dados[[col]]))
  dados[[col]][is.na(dados[[col]])] <- median(dados[[col]], na.rm = TRUE)
}

# 7. Renomeação de colunas para nomes mais curtos e claros
# ------------------------------------------------------------------------------
dados <- dados %>% 
  rename(
    atmos_press       = `meteo_PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)`,
    min_atmos_press   = `meteo_PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)`,
    max_atmos_press   = `meteo_PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)`,
    global_rad        = `meteo_RADIACAO GLOBAL (Kj/m²)`,
    max_rel_humid      = `meteo_UMIDADE REL. MAX. NA HORA ANT. (AUT) (%)`,
    min_rel_humid      = `meteo_UMIDADE REL. MIN. NA HORA ANT. (AUT) (%)`,
    rel_humid          = `meteo_UMIDADE RELATIVA DO AR, HORARIA (%)`,
    wind_dir         = `meteo_VENTO, DIREÇÃO HORARIA (gr) (° (gr))`,
    max_wind_gust     = `meteo_VENTO, RAJADA MAXIMA (m/s)`,
    wind_speed       = `meteo_VENTO, VELOCIDADE HORARIA (m/s)`,
    gen_value          = val_geracao,
    state_id           = id_estado,
    plant_name         = nom_usina,
    sub_id             = id_subsistema,
    nearest_w_station  = estacao_proxima
  )

# 8. Análise inicial de correlação entre variáveis numéricas
# ------------------------------------------------------------------------------
numeric_cols <- sapply(dados, is.numeric)
dados_numericos = dados[, numeric_cols]
corr_mat <- cor(dados_numericos)

melted <- reshape2::melt(corr_mat)
melted$txt_col <- ifelse(melted$value > 0.40, "white", "black")

ggplot(melted, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "grey90") +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlGnBu"),
    limits = c(-1, 1)
  ) +
  geom_text(aes(label = sprintf("%.2f", value), color = txt_col),
            size = 3) +
  scale_color_identity() +             
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title   = element_blank(),
    panel.grid   = element_blank()
  )

# 9. Remoção de variáveis altamente correlacionadas para evitar multicolinearidade
# ------------------------------------------------------------------------------
columns_drop_corr = c(
  "max_rel_humid",
  "min_rel_humid",
  "global_rad",
  "max_atmos_press",
  "min_atmos_press"
)
dados <- dados[, !(names(dados) %in% columns_drop_corr)]

# 10. Revisão estatística após limpezas e imputações
# ------------------------------------------------------------------------------
summary(dados)

# 11. Segunda análise de correlação após remoções
# ------------------------------------------------------------------------------
numeric_cols2    <- sapply(dados, is.numeric)
dados_numericos2 <- dados[, numeric_cols2]
corr_mat2        <- cor(dados_numericos2)

melted2 <- reshape2::melt(corr_mat2)
melted2$txt_col <- ifelse(melted2$value > 0.40, "white", "black")

ggplot(melted2, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "grey90") +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlGnBu"),
    limits = c(-1, 1)
  ) +
  geom_text(aes(label = sprintf("%.2f", value), color = txt_col),
            size = 3) +
  scale_color_identity() +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title   = element_blank(),
    panel.grid   = element_blank()
  )

# 12. Criação de variável categórica de estação do ano a partir do mês
# ------------------------------------------------------------------------------
dados <- dados %>% 
  mutate(
    # extrai o mês numérico (1–12)
    mes = month(din_instante),
    # mapeia para a estação
    seasons = case_when(
      mes %in% c(12, 1, 2)   ~ "Summer",
      mes %in% 3:5           ~ "Autumn",
      mes %in% 6:8           ~ "Winter",
      mes %in% 9:11          ~ "Spring",
      TRUE                   ~ NA_character_
    )
  ) %>% 
  select(-mes)

# 13. Visualização de outliers por boxplot
# ------------------------------------------------------------------------------
dados_numericos2_without_genValue <- dados_numericos2[, colnames(dados_numericos2) != "gen_value"] 
boxplot(
  dados_numericos2_without_genValue,
  las=2,
  col="lightblue",
  main="Boxplot of Numeric Variables",
  cex.axis = 0.7
)

# 14. Análise de outliers específicos de direção do vento
# ------------------------------------------------------------------------------
dados_outlier_wind <- dados %>%
  filter(`wind_dir` > 200)

ggplot(dados_outlier_wind, aes(x = nearest_w_station)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Wind Direction Outlier Numbers by Nearest Weather Station",
    x     = "Nearest Station",
    y     = "Wind Direction Outlier Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(hjust = 0.5)
  )

# Removal of nearest_w_station column as will not bring anything to our model
nearest_column = c("nearest_w_station")
dados <- dados[, !(names(dados) %in% nearest_column)]

# 15. Tratamento de outliers via winsorização (IQR)
# ------------------------------------------------------------------------------
Q1 <- quantile(dados$wind_dir, probs = 0.25)
Q3 <- quantile(dados$wind_dir, probs = 0.75)
IQR_val = Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

dados$wind_dir_winsor <- pmin(
  pmax(dados$wind_dir, lower_bound),
  upper_bound
)

# --- salvar bounds para uso na API ---
bounds <- list(
  wind_dir_lower = lower_bound,
  wind_dir_upper = upper_bound
)
saveRDS(bounds, "bounds.rds")

# 16. One-hot encoding de variáveis categóricas selecionadas
# ------------------------------------------------------------------------------
non_dummy_cols <- names(dados)[sapply(dados, is.numeric)]

dados <- dummy_cols(
  dados,
  select_columns = c("plant_name","sub_id","state_id","seasons"),
  remove_selected_columns = TRUE,
  remove_first_dummy     = FALSE
)
# Capturar nomes das colunas dummy geradas e salvar
dummy_names <- setdiff(names(dados), c(non_dummy_cols, "gen_value"))
saveRDS(dummy_names, "dummy_names.rds")


# 17. Normalização (Min-Max) de variáveis numéricas (exceto target)
# ------------------------------------------------------------------------------
predictor_nums <- setdiff(names(dados)[sapply(dados, is.numeric)], "gen_value")
pre_proc <- preProcess(dados[, predictor_nums, drop = FALSE], method = "range")
dados[, predictor_nums] <- predict(pre_proc, dados[, predictor_nums])
saveRDS(pre_proc,   "pre_proc.rds")
# 18. Remoção de coluna de data/hora para evitar vazamento de informação
drop_date = c(
  "din_instante"
)
dados <- dados[, !(names(dados) %in% drop_date)]

# 19. Particionamento em treino (70%) e teste (30%)
# ------------------------------------------------------------------------------
train_idx <- createDataPartition(dados$gen_value, p = 0.7, list = FALSE)
train_data <- dados[train_idx, ]
test_data  <- dados[-train_idx, ]

# 20. Preparação para XGBoost: criação de matrizes DMatrix
# ------------------------------------------------------------------------------
# Nome da variável target
predictor_cols <- setdiff(
  names(train_data)[sapply(train_data, is.numeric)],
  "gen_value"
)

train_matrix <- as.matrix(train_data[, predictor_cols])
train_label  <- train_data$gen_value

test_matrix  <- as.matrix(test_data[, predictor_cols])
test_label   <- test_data$gen_value

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest  <- xgb.DMatrix(data = test_matrix,  label = test_label)

# 21. Definição de parâmetros do modelo XGBoost
# ------------------------------------------------------------------------------
params <- list(
  booster        = "gbtree",
  objective      = "reg:squarederror",  # regressão
  eval_metric    = "rmse",              # métrica de avaliação
  eta            = 0.1,                 # taxa de aprendizado
  max_depth      = 6,                   # profundidade máxima
  subsample      = 0.8,                 # amostragem de linhas
  colsample_bytree = 0.8                # amostragem de colunas
)

# 22. Treinamento com watchlist para early stopping
# ------------------------------------------------------------------------------
watchlist <- list(train = dtrain, eval = dtest)
n_rounds  <- 100

xgb_model <- xgb.train(
  params        = params,
  data          = dtrain,
  nrounds       = n_rounds,
  watchlist     = watchlist,
  early_stopping_rounds = 10,    # para parar se não melhorar
  print_every_n = 10             # exibe progresso a cada 10 iterações
)
# Salvar modelo 
saveRDS(xgb_model, "train_model.rds")
# 23. Previsão e avaliação do modelo
# ------------------------------------------------------------------------------
preds <- predict(xgb_model, dtest)

# Cálculo do RMSE usando o pacote Metrics
rmse_value <- rmse(test_label, preds)
cat("RMSE no conjunto de teste:", round(rmse_value, 4), "\n")

# 24. Análise de importância de variáveis
# ------------------------------------------------------------------------------
importance_matrix <- xgb.importance(
  feature_names = colnames(train_matrix),
  model         = xgb_model
)

# Seleciona as 20 features mais importantes
imp <- importance_matrix[1:20, ]

# Gráfico de barras horizontais
ggplot(imp, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Feature Importance (Top 20)",
    x     = "Feature",
    y     = "Gain"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# 25. Impressão dos resultados finais
# ------------------------------------------------------------------------------

res <- caret::postResample(pred = preds, obs = test_label)
mae_value <- Metrics::mae(test_label, preds)

r2_value <- caret::R2(preds, test_label)

cat("RMSE: ",   round(res["RMSE"], 4),   "\n")  # Exibe RMSE
cat("R-squared: ", round(r2_value,       4), "\n")  # Exibe R²
cat("MAE: ",    round(mae_value,       4), "\n")  # Exibe MAE

