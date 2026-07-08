library(shiny)
library(ggplot2)
library(gramEvol)
library(bslib)

# --- 1. Load and Prep Data ---
df <- read.csv("parkinsons_updrs.data")
features <- c("motor_UPDRS", "age", "Jitter.Abs.", "Shimmer", "HNR", "PPE")
df_subset <- df[, features]
colnames(df_subset) <- c("motor_UPDRS", "age", "JitterAbs", "Shimmer", "HNR", "PPE")

set.seed(42)
df_sample <- df_subset[sample(nrow(df_subset), 300), ]
train_idx <- sample(seq_len(nrow(df_sample)), size = floor(0.8 * nrow(df_sample)))
train_data <- df_sample[train_idx, ]
test_data  <- df_sample[-train_idx, ]

# --- 2. Premium UI Design ---
my_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary = "#00d2ff",
  secondary = "#3a7bd5",
  base_font = font_google("Inter"),
  heading_font = font_google("Outfit")
)

ui <- page_sidebar(
  title = "Symbolic Regression vs Traditional Models",
  theme = my_theme,
  sidebar = sidebar(
    title = "Controls",
    h5("Symbolic Regression"),
    numericInput("popSize", "Population Size:", value = 50, min = 10, max = 500),
    numericInput("generations", "Generations:", value = 15, min = 5, max = 100),
    actionButton("runModels", "Run & Compare Models", class = "btn-primary", style="font-weight:bold; font-size:16px;"),
    hr(),
    p("Clicking this button trains both a traditional Linear Regression and an Evolutionary Symbolic Regression side-by-side.")
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    
    # --- LINEAR REGRESSION COLUMN ---
    card(
      full_screen = TRUE,
      card_header(class = "bg-secondary text-white", "Traditional Linear Regression"),
      card_body(
        layout_columns(
          value_box(title = "Test RMSE", value = textOutput("lmRmse"), theme = "secondary"),
          value_box(title = "Complexity", value = "High (Fixed)", theme = "secondary")
        ),
        h5("Model Equation:"),
        verbatimTextOutput("lmEquation"),
        plotOutput("lmPlot", height = "300px"),
        plotOutput("lmResid", height = "200px")
      )
    ),
    
    # --- SYMBOLIC REGRESSION COLUMN ---
    card(
      full_screen = TRUE,
      card_header(class = "bg-primary text-white", "Evolutionary Symbolic Regression"),
      card_body(
        layout_columns(
          value_box(title = "Test RMSE", value = textOutput("gpRmse"), theme = "primary"),
          value_box(title = "Complexity", value = "Low (Evolved)", theme = "primary")
        ),
        h5("Discovered Equation:"),
        verbatimTextOutput("gpEquation"),
        plotOutput("gpPlot", height = "300px"),
        plotOutput("gpResid", height = "200px")
      )
    )
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  observeEvent(input$runModels, {
    showNotification("Training Linear Model...", type = "message")
    
    # Linear Model
    lm_mod <- lm(motor_UPDRS ~ ., data = train_data)
    lm_preds <- predict(lm_mod, newdata = test_data)
    lm_rmse <- sqrt(mean((test_data$motor_UPDRS - lm_preds)^2))
    
    output$lmRmse <- renderText({ paste(round(lm_rmse, 3)) })
    output$lmEquation <- renderPrint({ round(coef(lm_mod), 3) })
    
    output$lmPlot <- renderPlot({
      ggplot(data.frame(Actual = test_data$motor_UPDRS, Predicted = lm_preds), aes(x=Actual, y=Predicted)) +
        geom_point(color="#3a7bd5", size=3, alpha=0.7) +
        geom_abline(intercept=0, slope=1, color="white", linetype="dashed", size=1) +
        theme_dark() + labs(title="Actual vs Predicted (Linear)") +
        theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text = element_text(color="white"), axis.text = element_text(color="white"))
    })
    
    output$lmResid <- renderPlot({
      ggplot(data.frame(Residuals = test_data$motor_UPDRS - lm_preds), aes(x=Residuals)) +
        geom_density(fill="#3a7bd5", alpha=0.5) +
        theme_dark() + labs(title="Error Distribution (Linear)") +
        theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text = element_text(color="white"), axis.text = element_text(color="white"))
    })
    
    # Symbolic Model
    showNotification("Evolving Equations via Genetic Algorithm... Please wait!", type = "warning", duration = NULL, id = "gp_notif")
    
    ruleDef <- list(
      expr = grule(op(expr, expr), func(expr), var, const),
      op   = grule('+', '-', '*', '/'),
      func = grule(sin, cos, log, abs, sqrt),
      var  = grule(age, JitterAbs, Shimmer, HNR, PPE),
      const= grule(c1, c2, c3)
    )
    grammarDef <- CreateGrammar(ruleDef)
    
    fitnessFunction <- function(expr) {
      result <- tryCatch({
        eval_env <- list(age=train_data$age, JitterAbs=train_data$JitterAbs, Shimmer=train_data$Shimmer, HNR=train_data$HNR, PPE=train_data$PPE, c1=1.5, c2=0.5, c3=10.0)
        preds <- eval(expr, envir = eval_env)
        if (any(is.na(preds)) || any(is.infinite(preds))) return(Inf)
        return(sqrt(mean((train_data$motor_UPDRS - preds)^2)))
      }, error = function(e) { return(Inf) })
      return(result)
    }
    
    ge <- GrammaticalEvolution(grammarDef, fitnessFunction, iterations = input$generations, popSize = input$popSize)
    removeNotification(id = "gp_notif")
    
    best_expr <- ge$best$expression
    eval_env <- list(age=test_data$age, JitterAbs=test_data$JitterAbs, Shimmer=test_data$Shimmer, HNR=test_data$HNR, PPE=test_data$PPE, c1=1.5, c2=0.5, c3=10.0)
    gp_preds <- eval(best_expr, envir = eval_env)
    
    if (any(is.na(gp_preds)) || any(is.infinite(gp_preds))) {
      gp_rmse <- Inf
    } else {
      gp_rmse <- sqrt(mean((test_data$motor_UPDRS - gp_preds)^2))
    }
    
    output$gpRmse <- renderText({ ifelse(is.infinite(gp_rmse), "Error", paste(round(gp_rmse, 3))) })
    output$gpEquation <- renderPrint({ print(best_expr) })
    
    output$gpPlot <- renderPlot({
      if (is.infinite(gp_rmse)) return(plot.new())
      ggplot(data.frame(Actual = test_data$motor_UPDRS, Predicted = gp_preds), aes(x=Actual, y=Predicted)) +
        geom_point(color="#00d2ff", size=3, alpha=0.7) +
        geom_abline(intercept=0, slope=1, color="white", linetype="dashed", size=1) +
        theme_dark() + labs(title="Actual vs Predicted (Symbolic)") +
        theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text = element_text(color="white"), axis.text = element_text(color="white"))
    })
    
    output$gpResid <- renderPlot({
      if (is.infinite(gp_rmse)) return(plot.new())
      ggplot(data.frame(Residuals = test_data$motor_UPDRS - gp_preds), aes(x=Residuals)) +
        geom_density(fill="#00d2ff", alpha=0.5) +
        theme_dark() + labs(title="Error Distribution (Symbolic)") +
        theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text = element_text(color="white"), axis.text = element_text(color="white"))
    })
  })
}

shinyApp(ui = ui, server = server)
