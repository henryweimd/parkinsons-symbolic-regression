library(shiny)
library(ggplot2)
library(gramEvol)
library(bslib)

df <- read.csv("parkinsons_updrs.data")
features <- c("motor_UPDRS", "age", "Jitter.Abs.", "Shimmer", "HNR", "PPE")
df_subset <- df[, features]
colnames(df_subset) <- c("motor_UPDRS", "age", "JitterAbs", "Shimmer", "HNR", "PPE")

set.seed(42)
df_sample <- df_subset[sample(nrow(df_subset), 300), ]
train_idx <- sample(seq_len(nrow(df_sample)), size = floor(0.8 * nrow(df_sample)))
train_data <- df_sample[train_idx, ]
test_data  <- df_sample[-train_idx, ]

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
  sidebar = sidebar(
    title = "Algorithm Controls",
    h5("Symbolic Regression"),
    p("These controls tell the algorithm how hard to look for the perfect math equation. More generations = more breeding of math formulas."),
    numericInput("popSize", "Population Size (Formulas per generation):", value = 50, min = 10, max = 500),
    numericInput("generations", "Generations (How many times to evolve):", value = 15, min = 5, max = 100),
    actionButton("runModels", "Run & Compare Models", class = "btn-primary", style="font-weight:bold; font-size:16px;"),
    hr(),
    p(strong("What is this?")),
    p("We are trying to predict a Parkinson's disease severity score ('Motor UPDRS') using just a patient's age and vocal cord measurements. We are comparing two AIs: one that forces a straight line, and one that invents its own math.")
  ),
  
  # Introductory Text for Non-Technical Audience
  card(
    card_header("How do algorithms turn data into predictions?", class="bg-dark text-white"),
    card_body(
      markdown("
      When doctors or scientists want to predict a patient's health score based on biological measurements, they usually rely on an algorithm to find the hidden math connecting the variables.

      *   **Traditional Linear Regression** is the standard workhorse. It assumes the relationship is a simple, straight-line weighted average (like adding up a receipt). It figures out *how much weight* to give each variable, but it can't handle complex, winding, or compounding relationships.
      *   **Evolutionary Symbolic Regression** assumes nothing. It dumps variables, numbers, and math symbols (`+`, `-`, `*`, `/`, `sin`, `log`) into a digital arena. It pieces them together randomly to create thousands of math equations, tests them against the data, kills the bad ones, and 'breeds' the good ones together over multiple generations to invent a completely custom formula.
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
        p("Linear Regression produces a 'weighted sum' equation. It assigns a multiplier to every single variable and adds them up. It is very easy to read, but very rigid."),
        layout_columns(
          value_box(title = "Test RMSE (Lower is better)", value = textOutput("lmRmse"), theme = "secondary"),
          value_box(title = "R-Squared (Closer to 1 is better)", value = textOutput("lmR2"), theme = "secondary")
        ),
        h5("The Final Equation:"),
        verbatimTextOutput("lmEquation"),
        hr(),
        p("If the predictions are perfect, all dots will fall exactly on the white dashed line."),
        plotOutput("lmPlot", height = "300px"),
        p("This shows the distribution of the errors (how far off the predictions were). We want a tall, narrow spike exactly at 0."),
        plotOutput("lmResid", height = "200px")
      )
    ),
    
    # --- SYMBOLIC REGRESSION COLUMN ---
    card(
      full_screen = TRUE,
      card_header(class = "bg-primary text-white", "2. Evolutionary Symbolic Regression"),
      card_body(
        p("Symbolic Regression invents a custom, free-form equation. It might nest variables inside logarithms, divide them by each other, or ignore some variables entirely to find the perfect biological fit."),
        layout_columns(
          value_box(title = "Test RMSE (Lower is better)", value = textOutput("gpRmse"), theme = "primary"),
          value_box(title = "R-Squared (Closer to 1 is better)", value = textOutput("gpR2"), theme = "primary")
        ),
        h5("The Discovered Equation:"),
        verbatimTextOutput("gpEquation"),
        hr(),
        p("Compare this scatter plot to the Linear Regression. Which one hugs the white line better?"),
        plotOutput("gpPlot", height = "300px"),
        p("Compare this error spread. A narrower spike at 0 means more consistent, accurate predictions."),
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
    
    # Format Linear Equation nicely
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
      gp_r2 <- NA
    } else {
      gp_rmse <- sqrt(mean((test_data$motor_UPDRS - gp_preds)^2))
      gp_r2 <- cor(test_data$motor_UPDRS, gp_preds)^2
    }
    
    output$gpRmse <- renderText({ ifelse(is.infinite(gp_rmse), "Error", paste(round(gp_rmse, 2))) })
    output$gpR2 <- renderText({ ifelse(is.na(gp_r2), "Error", paste(round(gp_r2, 3))) })
    
    eq_formatted <- paste("UPDRS =", paste(deparse(best_expr), collapse = " "))
    output$gpEquation <- renderPrint({ cat(eq_formatted) })
    
    output$gpPlot <- renderPlot({
      if (is.infinite(gp_rmse)) return(plot.new())
      ggplot(data.frame(Actual = test_data$motor_UPDRS, Predicted = gp_preds), aes(x=Actual, y=Predicted)) +
        geom_point(color="#00d2ff", size=3, alpha=0.7) +
        geom_abline(intercept=0, slope=1, color="white", linetype="dashed", linewidth=1) +
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
