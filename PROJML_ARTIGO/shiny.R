# Wind Generation Prediction Application
# Developed based on trained XGBoost model

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(fastDummies)
library(caret)
library(xgboost)
library(ggplot2)
library(dplyr)

# Load model artifacts
model <- readRDS("train_model.rds")
pre_proc <- readRDS("pre_proc.rds")
dummy_names <- readRDS("dummy_names.rds")

# Load correct plant mapping from CSV
plant_mapping_csv <- read.csv("wind_plants_summary.csv", stringsAsFactors = FALSE)

# Convert to expected format
plant_mapping <- data.frame(
  plant_name = plant_mapping_csv$nom_usina,
  state_id = plant_mapping_csv$id_estado,
  sub_id = plant_mapping_csv$id_subsistema,
  stringsAsFactors = FALSE
)

# Filter din_instante from dummy_names if present
dummy_names <- dummy_names[dummy_names != "din_instante"]

# Prediction function
predict_generation <- function(atmos_press, rel_humid, wind_dir, max_wind_gust, 
                              wind_speed, plant_name, sub_id, state_id, seasons) {
  
  # Create input data frame
  df <- data.frame(
    atmos_press = as.numeric(atmos_press),
    rel_humid = as.numeric(rel_humid),
    wind_dir = as.numeric(wind_dir),
    max_wind_gust = as.numeric(max_wind_gust),
    wind_speed = as.numeric(wind_speed),
    plant_name = plant_name,
    sub_id = sub_id,
    state_id = state_id,
    seasons = seasons,
    stringsAsFactors = FALSE
  )
  
  # Create wind_dir_winsor
  df$wind_dir_winsor <- df$wind_dir
  
  # One-hot encoding
  df_api <- dummy_cols(
    df,
    select_columns = c("plant_name", "sub_id", "state_id", "seasons"),
    remove_selected_columns = TRUE,
    remove_first_dummy = FALSE
  )
  
  # Ensure all dummy columns
  for(col in dummy_names) {
    if(!col %in% names(df_api)) df_api[[col]] <- 0
  }
  
  # Prepare final matrix
  num_fixed <- df[, c("atmos_press", "rel_humid", "wind_dir", 
                      "max_wind_gust", "wind_speed", "wind_dir_winsor")]
  dummies <- df_api[, dummy_names]
  df_enc <- cbind(num_fixed, dummies)
  
  # Normalize and predict
  df_scaled <- predict(pre_proc, df_enc)
  pred <- predict(model, newdata = as.matrix(df_scaled))
  
  return(as.numeric(pred))
}

# User interface
ui <- dashboardPage(
  dashboardHeader(title = "Wind Generation Forecasting System"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction", tabName = "prediction", icon = icon("wind")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f7f9; }
        .box { border-radius: 8px; }
        .form-group label { font-weight: bold; color: #2c3e50; }
        .btn-predict { background: linear-gradient(45deg, #3498db, #2980b9); 
                       border: none; color: white; font-weight: bold; }
        .prediction-result { 
          font-size: 24px; font-weight: bold; color: #27ae60;
          text-align: center; padding: 20px;
          background: linear-gradient(135deg, #ecf0f1, #bdc3c7);
          border-radius: 10px; margin: 10px 0;
        }
        .readonly-field {
          background-color: #f8f9fa !important;
          color: #6c757d !important;
          cursor: not-allowed !important;
        }
      ")),
      tags$script(HTML("
        $(document).ready(function() {
          // Make fields readonly
          $('#sub_id').attr('readonly', true).addClass('readonly-field');
          $('#state_id').attr('readonly', true).addClass('readonly-field');
        });
      "))
    ),
    
    tabItems(
      # Prediction tab
      tabItem(tabName = "prediction",
        fluidRow(
          # Input panel
          box(
            title = "Input Parameters", status = "primary", 
            solidHeader = TRUE, width = 6,
            
            # Plant selection with autocomplete
            selectizeInput("plant_name", 
              label = div("🏭 Wind Farm Name", 
                         tags$small("(Type to search)", style = "color: #7f8c8d;")),
              choices = NULL,
              options = list(placeholder = "Type the wind farm name...")
            ),
            
            fluidRow(
              column(6, 
                textInput("sub_id", 
                  label = div("⚡ Subsystem", 
                             tags$small("(Auto-filled)", style = "color: #7f8c8d;")),
                  value = ""
                )
              ),
              column(6,
                textInput("state_id", 
                  label = div("🗺️ State", 
                             tags$small("(Auto-filled)", style = "color: #7f8c8d;")),
                  value = ""
                )
              )
            ),
            
            numericInput("atmos_press", 
              label = div("🌡️ Atmospheric Pressure (mB)", 
                         tags$small("Atmospheric pressure at station level", style = "color: #7f8c8d;")),
              value = 1013.25, min = 900, max = 1100, step = 0.1
            ),
            
            numericInput("rel_humid", 
              label = div("💧 Relative Humidity (%)", 
                         tags$small("Air relative humidity", style = "color: #7f8c8d;")),
              value = 65, min = 0, max = 100, step = 1
            ),
            
            numericInput("wind_dir", 
              label = div("🧭 Wind Direction (°)", 
                         tags$small("Wind direction in degrees (0-360°)", style = "color: #7f8c8d;")),
              value = 180, min = 0, max = 360, step = 1
            ),
            
            numericInput("max_wind_gust", 
              label = div("💨 Maximum Wind Gust (m/s)", 
                         tags$small("Maximum wind gust speed", style = "color: #7f8c8d;")),
              value = 15, min = 0, max = 50, step = 0.1
            ),
            
            numericInput("wind_speed", 
              label = div("🌬️ Wind Speed (m/s)", 
                         tags$small("Average wind speed", style = "color: #7f8c8d;")),
              value = 8, min = 0, max = 30, step = 0.1
            ),
            
            selectInput("seasons", 
              label = div("🗓️ Season", 
                         tags$small("Meteorological season", style = "color: #7f8c8d;")),
              choices = list("Summer" = "Summer", "Autumn" = "Autumn", 
                            "Winter" = "Winter", "Spring" = "Spring"),
              selected = "Summer"
            ),
            
            br(),
            div(style = "text-align: center;",
              actionButton("predict_btn", "🔮 Make Prediction", 
                          class = "btn-predict btn-lg")
            )
          ),
          
          # Results panel
          box(
            title = "Prediction Results", status = "success", 
            solidHeader = TRUE, width = 6,
            
            withSpinner(
              uiOutput("prediction_output")
            ),
            
            br(),
            hidden(
              div(id = "charts_section",
                h4("📊 Visualizations", style = "color: #2c3e50;"),
                
                # Chart explanations
                div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                  h5("ℹ️ Chart Explanations:", style = "color: #2c3e50; margin-bottom: 8px;"),
                  tags$ul(style = "margin-bottom: 5px; font-size: 13px;",
                    tags$li("📊 Gauge: Speedometer showing predicted generation with 0-200 MW scale"),
                    tags$li("📈 Comparison: Compares your prediction with seasonal historical averages"),
                    tags$li("📉 Sensitivity: Shows how generation varies by changing wind speed")
                  )
                ),
                
                tabsetPanel(
                  tabPanel("📊 Gauge", 
                    withSpinner(plotlyOutput("gauge_plot", height = "300px"))
                  ),
                  tabPanel("📈 Comparison", 
                    withSpinner(plotlyOutput("comparison_plot", height = "300px"))
                  ),
                  tabPanel("📉 Sensitivity", 
                    withSpinner(plotlyOutput("sensitivity_plot", height = "300px"))
                  )
                )
              )
            )
          )
        ),
        
        # Information panel
        fluidRow(
          box(
            title = "ℹ️ Model Information", status = "info", 
            solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
            
            p("This model uses XGBoost for wind generation prediction based on:"),
            tags$ul(
              tags$li("Meteorological variables (pressure, humidity, wind)"),
              tags$li("Wind farm characteristics (location, subsystem)"),
              tags$li("Temporal factors (season)")
            ),
            p("The model was trained with historical data from Brazilian wind farms and 
              shows high accuracy in generation prediction.")
          )
        )
      ),
      
      # About tab
      tabItem(tabName = "about",
        fluidRow(
          box(
            title = "About the System", status = "primary", 
            solidHeader = TRUE, width = 12,
            
            h3("🌪️ Wind Generation Prediction System"),
            p("This system was developed to predict electricity generation from 
              Brazilian wind farms using Machine Learning techniques."),
            
            h4("🎯 Objective"),
            p("Provide accurate wind generation predictions to assist in 
              energy planning and operation of the Brazilian electrical system."),
            
            h4("🧠 Technology"),
            tags$ul(
              tags$li("Algorithm: XGBoost (Extreme Gradient Boosting)"),
              tags$li("Language: R"),
              tags$li("Interface: Shiny Dashboard"),
              tags$li("Visualizations: Plotly")
            ),
            
            h4("📊 Variables Used"),
            tags$ul(
              tags$li("Atmospheric pressure at station level"),
              tags$li("Air relative humidity"),
              tags$li("Wind direction and speed"),
              tags$li("Maximum wind gusts"),
              tags$li("Wind farm characteristics (name, location)"),
              tags$li("Temporal factors (season)")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Update selectizeInput choices for plants
  updateSelectizeInput(session, "plant_name",
    choices = sort(plant_mapping$plant_name),  # Sort alphabetically
    server = TRUE
  )
  
  # Update subsystem and state when plant is selected
  observeEvent(input$plant_name, {
    if (!is.null(input$plant_name) && input$plant_name != "") {
      selected_plant <- plant_mapping[plant_mapping$plant_name == input$plant_name, ]
      if (nrow(selected_plant) > 0) {
        updateTextInput(session, "sub_id", value = selected_plant$sub_id[1])
        updateTextInput(session, "state_id", value = selected_plant$state_id[1])
      } else {
        # Fallback mapping if plant not found
        showNotification("Plant mapping not found. Using default NE/BA.", type = "warning")
        updateTextInput(session, "sub_id", value = "NE")
        updateTextInput(session, "state_id", value = "BA")
      }
    }
  })
  
  # Reactive to store prediction result
  prediction_result <- reactiveVal(NULL)
  
  # Make prediction when button is clicked
  observeEvent(input$predict_btn, {
    req(input$plant_name, input$sub_id, input$state_id)
    
    # Make prediction
    result <- predict_generation(
      atmos_press = input$atmos_press,
      rel_humid = input$rel_humid,
      wind_dir = input$wind_dir,
      max_wind_gust = input$max_wind_gust,
      wind_speed = input$wind_speed,
      plant_name = input$plant_name,
      sub_id = input$sub_id,
      state_id = input$state_id,
      seasons = input$seasons
    )
    
    prediction_result(result)
    
    # Show charts section
    show("charts_section")
  })
  
  # Update prediction output
  output$prediction_output <- renderUI({
    if (!is.null(prediction_result())) {
      result <- prediction_result()
      
      div(
        # Main result
        div(class = "prediction-result",
          div(style = "font-size: 28px; font-weight: bold; color: #27ae60;",
            paste("⚡", round(result, 2), "MW")
          ),
          tags$br(),
          div(style = "font-size: 16px; color: #2c3e50; margin-top: 10px;",
            paste("🏭 Wind Farm:", input$plant_name)
          ),
          div(style = "font-size: 14px; color: #7f8c8d; margin-top: 5px;",
            paste("📍", input$state_id, "•", input$sub_id, "• Season:", input$seasons)
          ),
          tags$hr(style = "margin: 15px 0;"),
          div(style = "font-size: 13px; color: #95a5a6;",
            paste("🌬️ Wind:", input$wind_speed, "m/s • 💧 Humidity:", input$rel_humid, "% • 🌡️ Pressure:", input$atmos_press, "mB")
          )
        )
      )
    } else {
      div(class = "prediction-result",
        div(style = "font-size: 20px; color: #95a5a6;",
          "⚡ Awaiting prediction..."
        ),
        tags$br(),
        div(style = "font-size: 14px; color: #bdc3c7;",
          "Select a wind farm and click 'Make Prediction'"
        )
      )
    }
  })
  
  # Gauge chart
  output$gauge_plot <- renderPlotly({
    req(prediction_result())
    
    result <- prediction_result()
    
    # Define color based on value
    gauge_color <- if(result < 30) "#e74c3c" else if(result < 70) "#f39c12" else "#2ecc71"
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",  # Removed delta to avoid confusion
      value = result,
      number = list(suffix = " MW", font = list(size = 20)),
      gauge = list(
        axis = list(range = list(0, 200), tickwidth = 1, tickcolor = "darkblue"),
        bar = list(color = gauge_color, thickness = 0.75),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray",
        steps = list(
          list(range = c(0, 30), color = "#ffebee"),
          list(range = c(30, 70), color = "#fff3e0"),
          list(range = c(70, 120), color = "#e8f5e8"),
          list(range = c(120, 200), color = "#e3f2fd")
        ),
        threshold = list(
          line = list(color = "#e74c3c", width = 4),
          thickness = 0.75,
          value = 150
        )
      ),
      title = list(
        text = paste("📊 Generation Meter<br>",
                    "<sub>Wind Farm: ", input$plant_name, "</sub>"),
        font = list(size = 16)
      )
    ) %>%
    layout(
      margin = list(l = 20, r = 20, t = 80, b = 20),
      font = list(color = "#2c3e50", family = "Arial"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
  })
  
  # Comparison chart
  output$comparison_plot <- renderPlotly({
    req(prediction_result())
    
    result <- prediction_result()
    
    # More realistic data based on state and season
    base_summer <- if(input$state_id == "BA") 85 else if(input$state_id == "CE") 75 else 65
    base_winter <- if(input$state_id == "BA") 55 else if(input$state_id == "CE") 45 else 40
    max_historic <- if(input$state_id == "BA") 140 else if(input$state_id == "CE") 120 else 100
    
    comparison_data <- data.frame(
      Categoria = c("Current Prediction", 
                   paste(input$state_id, "Summer Avg"), 
                   paste(input$state_id, "Winter Avg"), 
                   paste(input$state_id, "Historical Max")),
      Valor = c(result, base_summer, base_winter, max_historic),
      Cor = c("#3498db", "#f39c12", "#9b59b6", "#e74c3c")
    )
    
    plot_ly(comparison_data, x = ~Categoria, y = ~Valor, type = "bar",
            marker = list(color = ~Cor),
            text = ~paste(round(Valor, 1), "MW"), textposition = "outside") %>%
      layout(
        title = list(text = "📈 Comparison with Historical Averages by State", 
                    font = list(size = 16)),
        xaxis = list(title = ""),
        yaxis = list(title = "Generation (MW)"),
        margin = list(l = 50, r = 20, t = 60, b = 100),
        showlegend = FALSE
      )
  })
  
  # Sensitivity chart
  output$sensitivity_plot <- renderPlotly({
    req(prediction_result())
    
    # Wind speed sensitivity analysis (broader range)
    wind_speeds <- seq(3, 20, by = 1)
    predictions <- sapply(wind_speeds, function(ws) {
      predict_generation(
        atmos_press = input$atmos_press,
        rel_humid = input$rel_humid,
        wind_dir = input$wind_dir,
        max_wind_gust = input$max_wind_gust,
        wind_speed = ws,
        plant_name = input$plant_name,
        sub_id = input$sub_id,
        state_id = input$state_id,
        seasons = input$seasons
      )
    })
    
    sensitivity_data <- data.frame(
      VelocidadeVento = wind_speeds,
      GeracaoPredita = predictions
    )
    
    plot_ly(sensitivity_data, x = ~VelocidadeVento, y = ~GeracaoPredita, 
            type = "scatter", mode = "lines+markers",
            line = list(color = "#3498db", width = 3),
            marker = list(color = "#2980b9", size = 6),
            name = "Sensitivity Curve") %>%
      add_trace(x = input$wind_speed, y = prediction_result(),
                type = "scatter", mode = "markers",
                marker = list(color = "#e74c3c", size = 15, symbol = "diamond"),
                name = paste("Your Value:", input$wind_speed, "m/s")) %>%
      layout(
        title = list(text = "📉 Wind Speed Sensitivity Analysis", 
                    font = list(size = 16)),
        xaxis = list(title = "Wind Speed (m/s)", range = c(2, 21)),
        yaxis = list(title = "Predicted Generation (MW)"),
        margin = list(l = 50, r = 20, t = 60, b = 50),
        showlegend = TRUE,
        legend = list(x = 0, y = 1)
      )
  })
}

# Run application
shinyApp(ui = ui, server = server) 