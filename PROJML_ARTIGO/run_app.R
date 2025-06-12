# Script para executar a aplicação Shiny
library(shiny)

# Verificar se todos os arquivos necessários existem
required_files <- c("train_model.rds", "pre_proc.rds", "dummy_names.rds", "plant_mapping.rds")

for (file in required_files) {
  if (!file.exists(file)) {
    stop(paste("Arquivo necessário não encontrado:", file))
  }
}

cat("=== Todos os arquivos necessários foram encontrados ===\n")
cat("Iniciando aplicação Shiny...\n")

# Executar aplicação (usando shiny.R)
runApp("shiny.R", host = "127.0.0.1", port = 3838, launch.browser = TRUE) 