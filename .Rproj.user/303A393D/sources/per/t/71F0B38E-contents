# Install packages
packages <- c("tidyverse", "broom", "dplyr", "haven", "lmtest", "lfe");for(pack in packages){
  if(!require(pack, character.only = TRUE)){
    install.packages(pack, dependencies = TRUE)
    require(pack, character.only = TRUE)
  }
  # Clean up the workspace
  rm(pack)
}

# Load the stata file
df <- read_dta("Session 3.3 RCT/Codes/ipa-neda_rctexercise.dta")

# Balance and Outcome variables
balance_vars <- c("age", "hhmemnum", "educ", "lit", "hunger6m")
outcome_vars <- c("life_sat", "future_life_sat", "hhincome30", "lh_asset", "loans")

# Function to perform detailed summary and regression
areg <- function(vars, data) {
  results <- list()
  
  for (var in vars) {
    cat("\nAnalysis Results for:", var, "\n")
    
    # Summary statistics
    print(summary(data[[var]]))
    
    # Absorption regression including intercept
    model <- felm(as.formula(paste(var, "~ treatment | community")), data = data)
    print(summary(model))
    
    # Extract the intercept
    icpt <- getfe(model, ef='zm2', robust = TRUE)
    icpt$t_value <- icpt$effect/icpt$se
    icpt$p_value <- 2 * pt(-abs(icpt$t_value), df = model$df)
    cons <- c(icpt["icpt.1", c("effect", "se", "t_value", "p_value")])
    
    # Print model summary with constant
    summary_model <- rbind(summary(model)$coefficients,cons)
    cat("\n Computing for the constant:\n")
    print(summary_model)
    
    results[[var]] <- list(model_summary = tidy(model), with_cons = summary_model)
  }
  
  return(results)
}

# Execute the analysis for balance and outcome variables
balance_results <- areg(balance_vars, df)
outcome_results <- areg(outcome_vars, df)
