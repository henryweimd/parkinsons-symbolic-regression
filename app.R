library(shiny)
library(ggplot2)
library(gramEvol)
library(bslib)

# --- 1. Load and Prep Data ---
df <- read.csv("parkinsons_updrs.data")
features <- c("motor_UPDRS", "age", "Jitter.Abs.", "Shimmer", "HNR", "PPE")
df_subset <- df[, features]
colnames(df_subset) <- c("motor_UPDRS", "age", "JitterAbs", "Shimmer", "HNR", "PPE")

# The Parkinson's dataset is notoriously noisy. R-squared on just 5 vocal features is naturally low.
set.seed(42)
df_sample <- df_subset[sample(nrow(df_subset), 400), ]
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
  title = "Discovering Medical Math: Linear vs. Symbolic Regression",
  theme = my_theme,
  tags$head(
    tags$style(HTML("
      .value-box { box-shadow: 0 4px 12px rgba(0,0,0,0.5); border-radius: 12px; transition: transform 0.2s; }
      .value-box:hover { transform: translateY(-5px); }
      .value-box .value { font-size: 2.2rem; font-weight: 700; text-shadow: 1px 1px 2px rgba(0,0,0,0.5); }
      pre { font-size: 1.15rem; background-color: #1a1a1a; border: 1px solid #333; font-weight: 600; padding: 15px; border-radius: 8px; white-space: pre-wrap; }
      #lmEquation { color: #8bb3f4; border-left: 5px solid #3a7bd5; }
      #gpEquation { color: #66e0ff; border-left: 5px solid #00d2ff; }
      .card-header { font-size: 1.25rem; font-weight: 600; letter-spacing: 0.5px; }
      h5 { font-weight: 600; margin-top: 15px; margin-bottom: 10px; color: #ffffff; }
      p { font-size: 1.05rem; line-height: 1.6; color: #dcdcdc; }
      .btn-primary { box-shadow: 0 0 15px rgba(0, 210, 255, 0.4); border-radius: 8px; transition: all 0.3s; }
      .btn-primary:hover { box-shadow: 0 0 25px rgba(0, 210, 255, 0.7); transform: scale(1.05); }
    "))
  ),
  sidebar = sidebar(
    title = "Algorithm Controls",
    h5("Evolutionary Parameters"),
    p("Give the AI more time and a larger population to find a better mathematical fit."),
    numericInput("popSize", "Population Size:", value = 250, min = 10, max = 1000, step=10),
    numericInput("generations", "Generations:", value = 40, min = 5, max = 200, step=5),
    numericInput("mutationChance", "Mutation Rate:", value = 0.1, min = 0.01, max = 0.5, step=0.01),
    
    h5("Interpretability Control"),
    sliderInput("complexity_penalty", "Penalty for Complexity:", min=0, max=0.5, value=0.00, step=0.01),
    p(style="font-size: 0.85em; color: #aaaaaa;", "Higher penalties force the AI to invent shorter, more human-readable equations by punishing long, messy formulas."),
    
    actionButton("runModels", "Run & Compare Models", class = "btn-primary", style="font-weight:bold; font-size:16px; margin-top: 15px;")
  ),
  
  # Interpretability & Black Box Explanation
  card(
    card_header("Why Symbolic Regression? The Power of Interpretability", class="bg-dark text-white"),
    card_body(
      markdown("
      In modern medicine, AI models like **Neural Networks** are highly accurate, but they are 'Black Boxes'. They use millions of hidden weights, making it impossible for a doctor to understand *why* a prediction was made. 

      **Symbolic Regression bridges the gap between machine accuracy and human understanding.**
      Instead of hiding its logic, it evolves an exact, readable mathematical formula. A scientist can look at the discovered formula (e.g., `UPDRS = Age * log(Jitter)`) and form new biological theories about *why* those specific variables interact that way. You can even force the AI to keep the equation short and interpretable using the 'Complexity Penalty' slider on the left!
      ")
    )
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    
    # --- LINEAR REGRESSION COLUMN ---
    card(
      full_screen = TRUE,
      card_header(class = "bg-secondary text-white", "1. Traditional Linear Regression"),
      card_body(
        p("Linear Regression assumes everything is a simple, straight-line weighted average. It is perfectly interpretable, but completely inflexible."),
        layout_columns(
          value_box(title = "Test RMSE (Lower = Better)", value = textOutput("lmRmse"), theme = "secondary"),
          value_box(title = "R-Squared (1.0 = Perfect)", value = textOutput("lmR2"), theme = "secondary")
        ),
        h5("The Final Equation:"),
        verbatimTextOutput("lmEquation"),
        hr(),
        p("If predictions are perfect, all dots will hug the white dashed line."),
        plotOutput("lmPlot", height = "300px"),
        p("Error Distribution (We want a tall, narrow spike exactly at 0):"),
        plotOutput("lmResid", height = "200px")
      )
    ),
    
    # --- SYMBOLIC REGRESSION COLUMN ---
    card(
      full_screen = TRUE,
      card_header(class = "bg-primary text-white", "2. Evolutionary Symbolic Regression"),
      card_body(
        p("Symbolic Regression invents a custom, free-form equation. It naturally discovers non-linear biological thresholds, compounding effects, and ratios."),
        layout_columns(
          value_box(title = "Test RMSE (Lower = Better)", value = textOutput("gpRmse"), theme = "primary"),
          value_box(title = "R-Squared (1.0 = Perfect)", value = textOutput("gpR2"), theme = "primary")
        ),
        h5("The Discovered Equation:"),
        verbatimTextOutput("gpEquation"),
        hr(),
        p("Evolutionary Progress (How the math improved over generations):"),
        plotOutput("gpHistory", height = "200px"),
        hr(),
        p("Does the custom math equation pull the dots tighter to the white line?"),
        plotOutput("gpPlot", height = "300px"),
        p("Error Distribution (A narrower spike means fewer wild mistakes):"),
        plotOutput("gpResid", height = "200px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$runModels, {
    showNotification("Training Linear Model...", type = "message")
    
    # Linear Model
    lm_mod <- lm(motor_UPDRS ~ ., data = train_data)
    lm_preds <- predict(lm_mod, newdata = test_data)
    lm_rmse <- sqrt(mean((test_data$motor_UPDRS - lm_preds)^2))
    lm_r2 <- cor(test_data$motor_UPDRS, lm_preds)^2
    
    cfs <- round(coef(lm_mod), 3)
    eq_str <- paste0("UPDRS = ", cfs[1], 
                     "\n      + (", cfs[2], " * Age)",
                     "\n      + (", cfs[3], " * JitterAbs)",
                     "\n      + (", cfs[4], " * Shimmer)",
                     "\n      + (", cfs[5], " * HNR)",
                     "\n      + (", cfs[6], " * PPE)")
    
    output$lmRmse <- renderText({ paste(round(lm_rmse, 2)) })
    output$lmR2 <- renderText({ paste(round(lm_r2, 3)) })
    output$lmEquation <- renderPrint({ cat(eq_str) })
    
    output$lmPlot <- renderPlot({
      ggplot(data.frame(Actual = test_data$motor_UPDRS, Predicted = lm_preds), aes(x=Actual, y=Predicted)) +
        geom_point(color="#3a7bd5", size=3, alpha=0.7) +
        geom_abline(intercept=0, slope=1, color="white", linetype="dashed", linewidth=1) +
        theme_dark() + labs(title="Actual vs Predicted (Linear)", x="Actual UPDRS Score", y="Predicted UPDRS Score") +
        theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text = element_text(color="white"), axis.text = element_text(color="white"))
    })
    
    output$lmResid <- renderPlot({
      ggplot(data.frame(Residuals = test_data$motor_UPDRS - lm_preds), aes(x=Residuals)) +
        geom_density(fill="#3a7bd5", alpha=0.5) +
        theme_dark() + labs(title="", x="Prediction Error", y="Density") +
        theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text = element_text(color="white"), axis.text = element_text(color="white"))
    })
    
    # Symbolic Model
    
    ruleDef <- list(
      expr = grule(op(expr, expr), func(expr), var, const),
      op   = grule('+', '-', '*', '/'),
      func = grule(sin, cos, log, abs, sqrt),
      var  = grule(age, JitterAbs, Shimmer, HNR, PPE),
      const= grule(0.1, 0.5, 1.0, 2.0, 5.0, 10.0)
    )
    grammarDef <- CreateGrammar(ruleDef)
    
    # Pre-compute environment and targets to prevent memory explosion
    train_env <- list(age=train_data$age, JitterAbs=train_data$JitterAbs, Shimmer=train_data$Shimmer, HNR=train_data$HNR, PPE=train_data$PPE)
    train_updrs <- train_data$motor_UPDRS
    
    fitnessFunction <- function(expr) {
      result <- tryCatch({
        preds <- eval(expr, envir = train_env)
        if (!is.numeric(preds) || any(is.na(preds)) || any(is.infinite(preds))) return(Inf)
        
        # Calculate RMSE
        rmse <- sqrt(mean((train_updrs - preds)^2))
        
        # Apply the Interpretability (Complexity) Penalty
        expr_str <- deparse(expr)
        complexity <- sum(nchar(expr_str))
        penalty <- input$complexity_penalty * complexity
        
        return(rmse + penalty)
      }, error = function(e) { return(Inf) })
      return(result)
    }
    
    history_costs <- numeric(0)
    monitor_fn <- function(result) {
      cost <- result$best$cost
      history_costs <<- c(history_costs, cost)
      # This progress update actively flushes the websocket, acting as a heartbeat 
      # so Posit Connect Cloud doesn't think the server froze!
      shiny::incProgress(1, detail = paste("Best Score so far:", round(cost, 2)))
    }
    
    # Run the genetic algorithm with a generic progress bar
    withProgress(message = paste0('Evolving ', format(input$generations * input$popSize, big.mark=","), ' Equations...'), value = 0, max = input$generations, {
      ge <- GrammaticalEvolution(grammarDef, fitnessFunction, 
                                 iterations = input$generations, 
                                 popSize = input$popSize,
                                 mutationChance = input$mutationChance,
                                 monitorFunc = monitor_fn)
    })
    
    best_expr <- ge$best$expression
    eval_env <- list(age=test_data$age, JitterAbs=test_data$JitterAbs, Shimmer=test_data$Shimmer, HNR=test_data$HNR, PPE=test_data$PPE)
    gp_preds <- eval(best_expr, envir = eval_env)
    
    if (!is.numeric(gp_preds) || any(is.na(gp_preds)) || any(is.infinite(gp_preds))) {
      gp_rmse <- Inf
      gp_r2 <- NA
    } else {
      # Handle case where the AI discovers a simple constant (e.g., UPDRS = c3)
      if (length(gp_preds) == 1) {
        gp_preds <- rep(gp_preds, length(test_data$motor_UPDRS))
      }
      gp_rmse <- sqrt(mean((test_data$motor_UPDRS - gp_preds)^2))
      
      # R-squared calculation crashes if predictions have zero variance (i.e. a flat line)
      if (sd(gp_preds) == 0) {
        gp_r2 <- NA
      } else {
        gp_r2 <- cor(test_data$motor_UPDRS, gp_preds)^2
      }
    }
    
    output$gpRmse <- renderText({ ifelse(is.infinite(gp_rmse), "Error", paste(round(gp_rmse, 2))) })
    output$gpR2 <- renderText({ ifelse(is.na(gp_r2), "Error", paste(round(gp_r2, 3))) })
    
    # Neatly format the discovered expression (removing the 'expression()' wrapper)
    eq_formatted <- paste("UPDRS =\n", paste(deparse(best_expr[[1]]), collapse = " \n"))
    output$gpEquation <- renderPrint({ cat(eq_formatted) })
    
    output$gpHistory <- renderPlot({
      if (length(history_costs) == 0) return(plot.new())
      df_hist <- data.frame(Generation = seq_along(history_costs), Cost = history_costs)
      ggplot(df_hist, aes(x=Generation, y=Cost)) +
        geom_line(color="#00d2ff", linewidth=1) +
        geom_point(color="#ffffff", size=2) +
        theme_dark() + labs(title="Survival of the Fittest", x="Generation", y="Best Score") +
        theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text = element_text(color="white"), axis.text = element_text(color="white"))
    })
    
    output$gpPlot <- renderPlot({
      if (is.infinite(gp_rmse)) return(plot.new())
      ggplot(data.frame(Actual = test_data$motor_UPDRS, Predicted = gp_preds), aes(x=Actual, y=Predicted)) +
        geom_point(color="#00d2ff", size=3, alpha=0.7) +
        geom_abline(intercept=0, slope=1, color="white", linetype="dashed", linewidth=1) +
        theme_dark() + labs(title="Actual vs Predicted (Symbolic)", x="Actual UPDRS Score", y="Predicted UPDRS Score") +
        theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text = element_text(color="white"), axis.text = element_text(color="white"))
    })
    
    output$gpResid <- renderPlot({
      if (is.infinite(gp_rmse)) return(plot.new())
      ggplot(data.frame(Residuals = test_data$motor_UPDRS - gp_preds), aes(x=Residuals)) +
        geom_density(fill="#00d2ff", alpha=0.5) +
        theme_dark() + labs(title="", x="Prediction Error", y="Density") +
        theme(plot.background = element_rect(fill = "#222222"), panel.background = element_rect(fill = "#333333"), text = element_text(color="white"), axis.text = element_text(color="white"))
    })
  })
}
shinyApp(ui = ui, server = server)
