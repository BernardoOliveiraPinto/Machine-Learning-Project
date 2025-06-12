# 🌪️ Aplicação Shiny - Predição de Geração Eólica

## 📋 Descrição

Esta aplicação Shiny foi desenvolvida para predizer a geração de energia elétrica de usinas eólicas brasileiras usando um modelo XGBoost. A aplicação oferece uma interface intuitiva com visualizações interativas e funcionalidades avançadas.

## 🚀 Funcionalidades

### ✨ Principais Características

1. **Autocomplete Inteligente**: Digite o nome da usina e o sistema automaticamente preenche o subsistema e estado
2. **Descrições Detalhadas**: Cada campo de entrada possui descrições claras sobre o que representa
3. **Visualizações Interativas**: 
   - Gauge de medição da predição
   - Comparação com médias históricas
   - Análise de sensibilidade à velocidade do vento
4. **Interface Moderna**: Design responsivo e amigável

### 📊 Variáveis de Entrada

- **🏭 Nome da Usina**: Seleção com autocomplete de 136 usinas disponíveis
- **⚡ Subsistema**: Preenchido automaticamente (NE ou S)
- **🗺️ Estado**: Preenchido automaticamente (BA, CE, PB, PE, PI, RN, RS, SC)
- **🌡️ Pressão Atmosférica**: Em milibares (900-1100 mB)
- **💧 Umidade Relativa**: Percentual (0-100%)
- **🧭 Direção do Vento**: Em graus (0-360°)
- **💨 Rajada Máxima**: Em metros por segundo (0-50 m/s)
- **🌬️ Velocidade do Vento**: Em metros por segundo (0-30 m/s)
- **🗓️ Estação do Ano**: Verão, Outono, Inverno, Primavera

## 🛠️ Como Usar

### 1. Instalação das Dependências

```r
# Execute este comando no R ou RStudio
source("install_dependencies.R")
```

### 2. Executar a Aplicação

**Opção 1 - Via script:**
```bash
Rscript run_app.R
```

**Opção 2 - No R/RStudio:**
```r
source("run_app.R")
```

### 3. Usando a Aplicação

1. **Selecione a Usina**: Digite o nome da usina no campo de autocomplete
2. **Verificar Dados**: Os campos de subsistema e estado serão preenchidos automaticamente
3. **Ajustar Parâmetros**: Modifique os valores meteorológicos conforme necessário
4. **Fazer Predição**: Clique no botão "🔮 Fazer Predição"
5. **Visualizar Resultados**: Explore as diferentes abas de visualização

## 📁 Arquivos Necessários

Certifique-se de que os seguintes arquivos estão presentes:

- `train_model.rds` - Modelo XGBoost treinado
- `pre_proc.rds` - Pré-processador de dados
- `dummy_names.rds` - Nomes das variáveis dummy
- `plant_mapping.rds` - Mapeamento de usinas para subsistemas/estados

## 📊 Visualizações

### 1. Gauge de Medição
- Mostra a predição atual em formato de velocímetro
- Faixas de cores indicando diferentes níveis de geração
- Linha de threshold para indicar limites críticos

### 2. Comparação Histórica
- Compara a predição atual com médias históricas
- Inclui médias por estação e máximo histórico
- Gráfico de barras colorido para fácil interpretação

### 3. Análise de Sensibilidade
- Mostra como a predição varia com a velocidade do vento
- Ponto destacado para o valor atual
- Útil para entender o impacto das variáveis meteorológicas

## 🎯 Exemplos de Uso

### Exemplo 1: Usina no Nordeste
- **Usina**: Conj. Caetité
- **Estado**: BA (preenchido automaticamente)
- **Subsistema**: NE (preenchido automaticamente)
- **Condições típicas de verão**

### Exemplo 2: Usina no Sul
- **Usina**: Elebrás Cidreira 1
- **Estado**: RS (preenchido automaticamente)
- **Subsistema**: S (preenchido automaticamente)
- **Condições típicas de inverno**

## 🔧 Personalização

A aplicação pode ser customizada modificando:

- **Cores e Estilos**: Edite a seção CSS no arquivo `shiny_app.R`
- **Gráficos**: Modifique as funções `renderPlotly()` para diferentes visualizações
- **Dados**: Atualize os arquivos `.rds` com novos dados de treinamento

## 📱 Compatibilidade

- **Navegadores**: Chrome, Firefox, Safari, Edge
- **Dispositivos**: Desktop, tablet, mobile (responsivo)
- **R Versões**: R 4.0 ou superior

## 🐛 Troubleshooting

### Problemas Comuns

1. **Erro de pacotes**: Execute `install_dependencies.R` novamente
2. **Arquivos não encontrados**: Verifique se todos os `.rds` estão no diretório
3. **Porta ocupada**: Modifique a porta no arquivo `run_app.R`

### Suporte

Para problemas ou sugestões, verifique:
- Console do R para mensagens de erro
- Log do navegador (F12) para erros JavaScript
- Verifique se todos os arquivos necessários existem

## 🎉 Resultado Esperado

Após executar a aplicação, você terá:
- Interface web acessível via navegador
- Predições em tempo real
- Visualizações interativas
- Sistema de autocomplete funcional
- Análises de sensibilidade automáticas

A aplicação estará disponível em: `http://127.0.0.1:3838` 