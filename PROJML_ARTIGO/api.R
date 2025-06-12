# api.R (Plumber API adaptada para fastDummies, sem winsorização ativa)

library(plumber)
library(fastDummies)
library(caret)
library(xgboost)

# Carregando artefatos
model       <- readRDS("train_model.rds")
pre_proc    <- readRDS("pre_proc.rds")
dummy_names <- readRDS("dummy_names.rds")

# Remover din_instante dos dummy_names se estiver presente
dummy_names <- dummy_names[dummy_names != "din_instante"]

#* @apiTitle Predição de Geração de Energia Elétrica (XGBoost)
#* @post /predicao
function(atmos_press, rel_humid, wind_dir, max_wind_gust, wind_speed,
         plant_name, sub_id, state_id, seasons) {
  # 1. Montar data.frame de entrada
  df <- data.frame(
    atmos_press   = as.numeric(atmos_press),
    rel_humid     = as.numeric(rel_humid),
    wind_dir      = as.numeric(wind_dir),
    max_wind_gust = as.numeric(max_wind_gust),
    wind_speed    = as.numeric(wind_speed),
    plant_name    = plant_name,
    sub_id        = sub_id,
    state_id      = state_id,
    seasons       = seasons,
    stringsAsFactors = FALSE
  )
  # 2. Criar wind_dir_winsor igual a wind_dir (sem corte)
  df$wind_dir_winsor <- df$wind_dir
  
  # 3. One-hot encoding dinâmico com fastDummies
  df_api <- dummy_cols(
    df,
    select_columns = c("plant_name","sub_id","state_id","seasons"),
    remove_selected_columns = TRUE,
    remove_first_dummy     = FALSE
  )
  # Garantir todas as colunas dummy
  for(col in dummy_names) {
    if(!col %in% names(df_api)) df_api[[col]] <- 0
  }
  
  # 4. Preparar matriz final de predição - INCLUIR wind_dir
  num_fixed <- df[, c("atmos_press","rel_humid","wind_dir","max_wind_gust","wind_speed","wind_dir_winsor")]
  dummies   <- df_api[, dummy_names]
  df_enc    <- cbind(num_fixed, dummies)
  
  # 5. Normalizar e prever
  df_scaled <- predict(pre_proc, df_enc)
  pred      <- predict(model, newdata = as.matrix(df_scaled))
  list(gen_value = as.numeric(pred))
}