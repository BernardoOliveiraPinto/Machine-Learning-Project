# Script para instalar dependências da aplicação Shiny
# Execute este script antes de rodar a aplicação

# Configurar CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Lista de pacotes necessários
packages <- c(
  "shiny",
  "shinydashboard", 
  "DT",
  "plotly",
  "shinycssloaders",
  "fastDummies",
  "caret",
  "xgboost",
  "ggplot2",
  "dplyr",
  "shinyjs"
)

# Função para instalar pacotes se não estiverem instalados
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Instalando pacote:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    cat("Pacote", pkg, "já está instalado\n")
  }
}

# Instalar todos os pacotes
cat("=== Verificando e instalando dependências ===\n")
lapply(packages, install_if_missing)

cat("\n=== Todas as dependências foram verificadas! ===\n")
cat("Agora você pode executar a aplicação:\n")
cat("  1. Execute: Rscript run_app.R\n")
cat("  OU\n")
cat("  2. No R: source('run_app.R')\n") 