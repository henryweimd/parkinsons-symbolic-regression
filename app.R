library(shiny)
library(ggplot2)
library(gramEvol)
library(bslib)

# --- 1. Load and Prep Data ---
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/telemonitoring/parkinsons_updrs.data"
df <- read.csv(url)

features <- c("motor_UPDRS", "age", "Jitter.Abs.", "Shimmer", "HNR", "PPE")
df_subset <- df[, features]
colnames(df_subset) <- c("motor_UPDRS", "age", "JitterAbs", "Shimmer", "HNR", "PPE")

# Random sample of 300 rows for speed
set.seed(42)
df_sample <- df_subset[sample(nrow(df_subset), 300), ]

# Base R train/test split (no caret package needed)
train_idx <- sample(seq_len(nrow(df_sample)), size = floor(0.8 * nrow(df_sample)))
train_data <- df_sample[train_idx, ]
test_data  <- df_sample[-train_idx, ]

# --- 2. Define the UI ---
ui <- page_navbar(
  title = "Symbolic Regression vs Traditional Models",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel(title = "1. Data Overview",
    fluidRow(
      column(12, 
             h3("Parkinson's Telemonitoring Dataset"),
             p("Predicting 'motor_UPDRS' clinical score using patient age and vocal cord measurements."),
             tableOutput("dataTable")
      )
    )
  ),
  
  nav_panel(title = "2. Linear Regression",
    fluidRow(
      column(4,
             actionButton("runLm", "Train Linear Regression", class = "btn-primary"),
             hr(),
             h4("Model Summary:"),
             verbatimTextOutput("lmSummary"),
             h4("Test Set RMSE:"),
             textOutput("lmRmse")
      ),
      column(8, plotOutput("lmPlot"))
    )
  ),
  
  nav_panel(title = "3. Symbolic Regression (gramEvol)",
    fluidRow(
      column(4,
             numericInput("popSize", "Population Size:", value = 50, min = 10, max = 500),
             numericInput("generations", "Generations:", value = 10, min = 5, max = 100),
             actionButton("runGp", "Run Evolutionary Search", class = "btn-success"),
             hr(),
             h4("Best Equation Found:"),
             verbatimTextOutput("gpEquation"),
             h4("Test Set RMSE:"),
             textOutput("gpRmse")
      ),
      column(8, plotOutput("gpPlot"))
    )
  )
)

# --- 3. Define the Server Logic ---
server <- function(input, output, session) {
  output$dataTable <- renderTable({ head(df_sample, 15) })
  
  lm_model <- eventReactive(input$runLm, { lm(motor_UPDRS ~ ., data = train_data) })
  
  output$lmSummary <- renderPrint({ summary(lm_model()) })
  
  output$lmRmse <- renderText({
    preds <- predict(lm_model(), newdata = test_data)
    paste(round(sqrt(mean((test_data$motor_UPDRS - preds)^2)), 4))
  })
  
  output$lmPlot <- renderPlot({
    preds <- predict(lm_model(), newdata = test_data)
    plot_df <- data.frame(Actual = test_data$motor_UPDRS, Predicted = preds)
    ggplot(plot_df, aes(x=Actual, y=Predicted)) +
      geom_point(color="blue", alpha=0.6) +
      geom_abline(intercept=0, slope=1, color="red", linetype="dashed") +
      theme_minimal() + labs(title="Linear Regression: Actual vs Predicted")
  })
  
  gp_model <- eventReactive(input$runGp, {
    showNotification("Evolving Equations... Please wait!", type = "warning", id = "gp_notif")
    
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
    return(ge)
  })
  
  output$gpEquation <- renderPrint({ print(gp_model()$best$expression) })
  
  output$gpRmse <- renderText({
    best_expr <- gp_model()$best$expression
    eval_env <- list(age=test_data$age, JitterAbs=test_data$JitterAbs, Shimmer=test_data$Shimmer, HNR=test_data$HNR, PPE=test_data$PPE, c1=1.5, c2=0.5, c3=10.0)
    preds <- eval(best_expr, envir = eval_env)
    if (any(is.na(preds)) || any(is.infinite(preds))) return("Error: Invalid math on test data.")
    paste(round(sqrt(mean((test_data$motor_UPDRS - preds)^2)), 4))
  })
  
  output$gpPlot <- renderPlot({
    best_expr <- gp_model()$best$expression
    eval_env <- list(age=test_data$age, JitterAbs=test_data$JitterAbs, Shimmer=test_data$Shimmer, HNR=test_data$HNR, PPE=test_data$PPE, c1=1.5, c2=0.5, c3=10.0)
    preds <- eval(best_expr, envir = eval_env)
    if (any(is.na(preds)) || any(is.infinite(preds))) { plot.new(); title("Invalid test predictions"); return() }
    
    plot_df <- data.frame(Actual = test_data$motor_UPDRS, Predicted = preds)
    ggplot(plot_df, aes(x=Actual, y=Predicted)) +
      geom_point(color="green4", alpha=0.6) +
      geom_abline(intercept=0, slope=1, color="red", linetype="dashed") +
      theme_minimal() + labs(title="Symbolic Regression: Actual vs Predicted")
  })
}

shinyApp(ui = ui, server = server)
