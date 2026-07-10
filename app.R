library(shiny)
library(bslib)
library(gramEvol)

# --- Load Datasets Globally ---
# Dataset 1: Parkinson's
df_park <- read.csv("parkinsons_updrs.data")
df_park <- df_park[, c("motor_UPDRS", "age", "Jitter.Abs.", "Shimmer", "HNR", "PPE")]
colnames(df_park) <- c("motor_UPDRS", "age", "JitterAbs", "Shimmer", "HNR", "PPE")

# Dataset 2: Medical Insurance (Synthesized version with non-linear interaction)
df_ins <- read.csv("insurance.csv")
# Columns: age, bmi, children, smoker, charges

# Custom theme (Neon Blue/Dark)
my_theme <- bs_theme(
  bg = "#121212", 
  fg = "#e0e0e0", 
  primary = "#00d2ff", 
  secondary = "#444444",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- page_sidebar(
  theme = my_theme,
  title = "AI Model Face-Off: Linear vs Symbolic Regression",
  
  # Custom CSS for the value boxes
  tags$head(
    tags$style(HTML("
      .value-box-primary { background: linear-gradient(135deg, #00d2ff, #3a7bd5) !important; color: white !important; }
      .value-box-secondary { background: #2a2a2a !important; color: #00d2ff !important; border: 1px solid #444; }
      .bslib-value-box .value-box-value { font-weight: 800 !important; font-size: 2.5rem !important; }
      .bslib-value-box .value-box-title { font-weight: 600 !important; font-size: 1.1rem !important; text-transform: uppercase; letter-spacing: 1px; }
      .btn-primary { background: #00d2ff; color: #000; border: none; font-weight: bold; }
      .btn-primary:hover { background: #00a8cc; color: #fff; }
    "))
  ),
  
  sidebar = sidebar(
    title = "Algorithm Controls",
    
    h5("Dataset Selection"),
    selectInput("dataset_choice", "Choose Dataset:", 
                choices = c("Parkinson's Telemonitoring" = "parkinsons", 
                            "US Medical Insurance Costs" = "insurance"), 
                selected = "parkinsons"),
    hr(),
    
    h5("Evolutionary Parameters"),
    p("Give the AI more time and a larger population to find a better mathematical fit."),
    numericInput("popSize", "Population Size:", value = 250, min = 10, max = 1000, step=10),
    numericInput("generations", "Generations:", value = 50, min = 5, max = 200, step=5),
    numericInput("mutationChance", "Mutation Rate:", value = 0.1, min = 0.01, max = 0.5, step=0.01),
    
    h5("Interpretability Control"),
    sliderInput("complexity_penalty", "Penalty for Complexity:", min=0, max=0.5, value=0.00, step=0.01),
    p(style="font-size: 0.85em; color: #aaaaaa;", "Higher penalties force the AI to invent shorter, more human-readable equations by punishing long, messy formulas."),
    
    actionButton("runModels", "Run & Compare Models", class = "btn-primary", style="font-weight:bold; font-size:16px; margin-top: 15px;"),
    
    hr(style="border-top: 1px solid #555; margin-top: 30px;"),
    tags$a(href="https://github.com/henryweimd/parkinsons-symbolic-regression", target="_blank", style="color: #00d2ff; text-decoration: none; font-weight: bold; display: block; text-align: center;", icon("github"), " View Source on GitHub")
  ),
  
  # Python App Banner
  card(
    class = "text-white",
    style = "margin-bottom: 15px; background: linear-gradient(135deg, #00d2ff, #3a7bd5); border-radius: 10px;",
    card_body(
      HTML("
        <div style='display: flex; align-items: center; justify-content: space-between;'>
          <div>
            <h4 style='margin: 0; font-weight: bold;'>🐍 Try the Python version!</h4>
            <p style='margin: 5px 0 0 0; font-size: 1.0em;'>A high-performance version of this dashboard built with <b>Streamlit</b> and <b>PySR</b>.</p>
          </div>
          <div>
            <a href='https://symbolic-regression-demo.streamlit.app/' target='_blank' class='btn btn-light' style='font-weight: bold; margin-right: 10px; color: #3a7bd5;'>🚀 Live Python Demo (Streamlit)</a>
            <a href='https://github.com/henryweimd/symbolic-regression-python-demo' target='_blank' class='btn btn-outline-light' style='font-weight: bold; color: white; border-color: white;'>View Python Source on GitHub</a>
          </div>
        </div>
      ")
    )
  ),
  
  # Intro, Context, and Citation
  card(
    card_header(textOutput("context_title"), class="bg-dark text-white"),
    card_body(
      uiOutput("dataset_context")
    )
  ),
  
  # Results Comparison
  layout_columns(
    col_widths = c(6, 6),
    
    # Linear Regression Results
    card(
      card_header("Traditional AI: Linear Regression", class="bg-dark text-white"),
      card_body(
        p("Linear Regression assumes everything is a simple, straight-line weighted average. It is perfectly interpretable, but completely inflexible."),
        layout_columns(
          value_box(title = "Test RMSE (Lower = Better)", value = textOutput("lmRmse"), theme = "secondary"),
          value_box(title = "R-Squared (1.0 = Perfect)", value = textOutput("lmR2"), uiOutput("lmR2_eval"), theme = "secondary")
        ),
        h5("The Final Equation:"),
        verbatimTextOutput("lmEquation"),
        hr(),
        p("Actual vs Predicted: If the formula is perfectly accurate, all dots will hug the straight white line."),
        plotOutput("lmPlot", height = "400px")
      )
    ),
    
    # Symbolic Regression Results
    card(
      card_header("Evolutionary AI: Symbolic Regression", class="bg-primary text-white"),
      card_body(
        p("Symbolic Regression invents a custom, free-form equation. It naturally discovers non-linear biological thresholds, compounding effects, and ratios."),
        layout_columns(
          value_box(title = "Test RMSE (Lower = Better)", value = textOutput("gpRmse"), theme = "primary"),
          value_box(title = "R-Squared (1.0 = Perfect)", value = textOutput("gpR2"), uiOutput("gpR2_eval"), theme = "primary")
        ),
        h5("The Discovered Equation:"),
        verbatimTextOutput("gpEquation"),
        hr(),
        p("Actual vs Predicted: If the formula is perfectly accurate, all dots will hug the straight white line."),
        plotOutput("gpPlot", height = "400px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamic Text for Context
  output$context_title <- renderText({
    if (input$dataset_choice == "parkinsons") {
      "Why Symbolic Regression? (Parkinson's Telemonitoring Dataset)"
    } else {
      "Why Symbolic Regression? (US Medical Cost Dataset)"
    }
  })
  
  output$dataset_context <- renderUI({
    if (input$dataset_choice == "parkinsons") {
      markdown("
      **Neural Networks** are highly accurate but act as unreadable 'Black Boxes'. **Symbolic Regression** solves this by evolving a clear, readable mathematical formula from scratch. It naturally discovers non-linear biological relationships that doctors can actually read and verify in a lab.
      
      **The Context:** We are predicting Parkinson's disease severity (UPDRS) using non-invasive voice recordings via the [UCI Parkinson's Telemonitoring Dataset](https://archive.ics.uci.edu/dataset/189/parkinsons+telemonitoring). The AI is challenged to find a math equation linking four vocal biomarkers: **Jitter & Shimmer** (variations in pitch/volume), **HNR** (vocal clarity), and **PPE** (dysphonia/hoarseness).
      ")
    } else {
      markdown("
      **Neural Networks** are highly accurate but act as unreadable 'Black Boxes'. **Symbolic Regression** solves this by evolving a clear, readable mathematical formula from scratch. It naturally discovers hidden non-linear conditions (like multipliers) that linear models completely miss!
      
      **The Context:** We are predicting yearly patient medical costs based on the [US Medical Insurance Cost Dataset](https://www.kaggle.com/datasets/mirichoi0218/insurance). The AI uses: **Age**, **BMI**, **Children**, and **Smoker** (1=Yes, 0=No). 
      
      **Why this dataset?** Linear regression struggles here because high BMI only spikes costs *if* the patient is also a smoker. Symbolic Regression can discover this non-linear interaction naturally, demonstrating exactly why Evolutionary AI is so powerful.
      ")
    }
  })
  
  # Intuitive Performance Explainer
  get_performance_emoji <- function(val, is_noisy_dataset) {
    if (is.na(val)) return(p("⚠️ Error (Flatline)", style="margin:0; font-size:1.1rem; color:#ffd700;"))
    
    if (is_noisy_dataset) {
      if (val < 0.15) return(p("🔴 Weak (High Biological Noise)", style="margin:0; font-size:1.1rem; color:#ff6b6b;"))
      if (val < 0.30) return(p("🟠 Fair (Typical for Voice Data)", style="margin:0; font-size:1.1rem; color:#ffa502;"))
      if (val < 0.50) return(p("🟡 Good Signal", style="margin:0; font-size:1.1rem; color:#eccc68;"))
      return(p("🟢 Strong Signal", style="margin:0; font-size:1.1rem; color:#7bed9f;"))
    } else {
      if (val < 0.50) return(p("🔴 Poor Fit", style="margin:0; font-size:1.1rem; color:#ff6b6b;"))
      if (val < 0.75) return(p("🟠 Fair Fit (Missed Non-linearities?)", style="margin:0; font-size:1.1rem; color:#ffa502;"))
      if (val < 0.85) return(p("🟡 Good Signal", style="margin:0; font-size:1.1rem; color:#eccc68;"))
      return(p("🟢 Excellent fit!", style="margin:0; font-size:1.1rem; color:#7bed9f;"))
    }
  }

  observeEvent(input$runModels, {
    
    is_parkinsons <- (input$dataset_choice == "parkinsons")
    
    # 1. Setup Data and Grammar
    if (is_parkinsons) {
      full_df <- df_park
      target_col <- "motor_UPDRS"
      
      ruleDef <- list(
        expr = grule(op(expr, expr), func(expr), var, const),
        op   = grule('+', '-', '*', safe_div, safe_pow),
        func = grule(sin, cos, abs, safe_log, safe_sqrt, safe_exp),
        var  = grule(age, JitterAbs, Shimmer, HNR, PPE),
        const= grule(0.1, 0.5, 1.0, 2.0, 5.0, 10.0)
      )
    } else {
      full_df <- df_ins
      target_col <- "charges"
      
      ruleDef <- list(
        expr = grule(op(expr, expr), var, const),
        op   = grule('+', '-', '*'),
        var  = grule(age, bmi, smoker),
        const= grule(0.1, 0.25, 0.5, 1.0, 5.0, 10.0)
      )
    }
    
    grammarDef <- CreateGrammar(ruleDef)
    
    # --- Dynamic Scaling ---
    # Automatically scale large targets so the AI doesn't have to search for massive numbers
    target_mean <- mean(full_df[[target_col]], na.rm=TRUE)
    if (target_mean > 500) {
      scale_factor <- 10^(floor(log10(target_mean)) - 1)
      if (scale_factor < 1) scale_factor <- 1000
      full_df[[target_col]] <- full_df[[target_col]] / scale_factor
      target_disp <- paste0(target_col, " (Scaled / ", scale_factor, ")")
    } else {
      target_disp <- target_col
    }
    
    # Downsample for speed
    set.seed(42)
    df_sample <- full_df[sample(nrow(full_df), min(400, nrow(full_df))), ]
    
    train_idx <- sample(seq_len(nrow(df_sample)), size = floor(0.8 * nrow(df_sample)))
    train_data <- df_sample[train_idx, ]
    test_data  <- df_sample[-train_idx, ]
    
    train_targets <- train_data[[target_col]]
    test_targets <- test_data[[target_col]]
    
    # Inject protected math functions into evaluation environments to prevent NaN domain errors
    safe_env_vars <- list(
      safe_log = function(x) log(abs(x) + 1e-6),
      safe_sqrt = function(x) sqrt(abs(x)),
      safe_div = function(a, b) ifelse(abs(b) < 1e-6, a, a/b),
      safe_exp = function(x) exp(pmin(pmax(x, -50), 50)),
      safe_pow = function(a, b) abs(a)^pmin(pmax(b, -10), 10)
    )
    
    if (is_parkinsons) {
      train_env <- c(list(age=train_data$age, JitterAbs=train_data$JitterAbs, Shimmer=train_data$Shimmer, HNR=train_data$HNR, PPE=train_data$PPE), safe_env_vars)
      eval_env <- c(list(age=test_data$age, JitterAbs=test_data$JitterAbs, Shimmer=test_data$Shimmer, HNR=test_data$HNR, PPE=test_data$PPE), safe_env_vars)
    } else {
      train_env <- c(list(age=train_data$age, bmi=train_data$bmi, smoker=train_data$smoker), safe_env_vars)
      eval_env <- c(list(age=test_data$age, bmi=test_data$bmi, smoker=test_data$smoker), safe_env_vars)
    }
    
    # 2. Linear Regression Model
    lm_mod <- lm(as.formula(paste(target_col, "~ .")), data = train_data)
    lm_preds <- predict(lm_mod, newdata = test_data)
    
    lm_rmse <- sqrt(mean((test_targets - lm_preds)^2))
    ss_tot <- sum((test_targets - mean(train_targets))^2)
    ss_res <- sum((test_targets - lm_preds)^2)
    lm_r2 <- max(0, 1 - (ss_res / ss_tot))
    
    output$lmRmse <- renderText({ paste(round(lm_rmse, 2)) })
    output$lmR2 <- renderText({ paste(round(lm_r2, 3)) })
    output$lmR2_eval <- renderUI({ get_performance_emoji(lm_r2, is_parkinsons) })
    
    # Dynamically build the linear equation string so nothing is hard-coded
    cfs <- round(coef(lm_mod), 3)
    cfs[is.na(cfs)] <- 0
    intercept <- cfs[1]
    term_names <- names(cfs)[-1]
    term_vals <- cfs[-1]
    
    eq_str <- paste0(target_disp, " =\n      ", intercept)
    for (i in seq_along(term_names)) {
      eq_str <- paste0(eq_str, "\n      + (", term_vals[i], " * ", term_names[i], ")")
    }
    
    output$lmEquation <- renderPrint({ cat(eq_str) })
    
    output$lmPlot <- renderPlot({
      par(bg = "#121212", col.axis = "#e0e0e0", col.lab = "#e0e0e0", fg = "#444444", mar = c(4, 4, 1, 1))
      plot(test_targets, lm_preds, 
           xlab = paste("Actual", target_disp), 
           ylab = paste("Predicted", target_disp), 
           pch = 16, col = "#00d2ff88", cex = 1.2)
      abline(a = 0, b = 1, col = "white", lwd = 2, lty = 2)
    })
    
    # 3. Symbolic Regression via Grammatical Evolution
    fitnessFunction <- function(expr) {
      preds <- tryCatch({
        res <- eval(expr, envir = train_env)
        if (length(res) == 1) rep(res, length(train_targets)) else res
      }, error = function(e) return(Inf))
      
      if (any(is.na(preds)) || any(is.infinite(preds))) return(Inf)
      
      mse <- mean((train_targets - preds)^2)
      if (is.na(mse) || is.nan(mse)) return(Inf)
      
      complexity <- length(all.names(expr))
      penalty <- input$complexity_penalty * complexity * sd(train_targets)
      
      return(mse + penalty)
    }
    
    withProgress(message = 'Evolving Equations...', value = 0, {
      
      monitorFunc <- function(result) {
        frac <- result$generation / input$generations
        incProgress(1 / input$generations, detail = paste("Generation", result$generation, "/", input$generations))
      }
      
      ge_res <- tryCatch({
        GrammaticalEvolution(
          grammarDef, 
          fitnessFunction, 
          optimizer = "ga",
          popSize = input$popSize, 
          iterations = input$generations,
          mutationChance = input$mutationChance,
          monitorFunc = monitorFunc
        )
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(ge_res)) {
        output$gpEquation <- renderPrint({ cat("Error: Evolution failed. Try adjusting parameters.") })
        return()
      }
      
      best_expr <- ge_res$best$expressions
      
      gp_preds <- tryCatch({
        res <- eval(best_expr, envir = eval_env)
        if (length(res) == 1) rep(res, length(test_targets)) else res
      }, error = function(e) rep(NA, length(test_targets)))
      
      if (any(is.na(gp_preds))) {
        gp_rmse <- Inf
        gp_r2 <- NA
      } else {
        gp_rmse <- sqrt(mean((test_targets - gp_preds)^2))
        gp_ss_res <- sum((test_targets - gp_preds)^2)
        gp_r2 <- max(0, 1 - (gp_ss_res / ss_tot))
      }
      
      output$gpRmse <- renderText({ ifelse(is.infinite(gp_rmse), "Error", paste(round(gp_rmse, 2))) })
      output$gpR2 <- renderText({ ifelse(is.na(gp_r2), "Error", paste(round(gp_r2, 3))) })
      output$gpR2_eval <- renderUI({ get_performance_emoji(gp_r2, is_parkinsons) })
      
      eq_prefix <- paste0(target_disp, " =\n")
      eq_formatted <- paste(eq_prefix, paste(deparse(best_expr[[1]]), collapse = " \n"))
      output$gpEquation <- renderPrint({ cat(eq_formatted) })
      
      output$gpPlot <- renderPlot({
        par(bg = "#121212", col.axis = "#e0e0e0", col.lab = "#e0e0e0", fg = "#444444", mar = c(4, 4, 1, 1))
        plot(test_targets, gp_preds, 
             xlab = paste("Actual", target_disp), 
             ylab = paste("Predicted", target_disp), 
             pch = 16, col = "#00d2ff88", cex = 1.2)
        abline(a = 0, b = 1, col = "white", lwd = 2, lty = 2)
      })
      
    })
  })
}

shinyApp(ui, server)
