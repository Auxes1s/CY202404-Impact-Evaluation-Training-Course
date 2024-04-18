# Install packages
packages <- c("tidyverse", "dplyr", "haven", "lfe");for(pack in packages){
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
areg <- function(vars, df){
  for(i in vars){
    cat("\n Analysis for: ", i, "\n")
    print(summary(df[[i]]))
    
    #NOTE: Since FE_LM is weird inside functions, we redundantly called it for the model summary
    formula_for_fn <- as.formula(paste(i, '~ treatment | community'))
    fe_lm <- do.call("felm", list(formula=formula_for_fn,
                                  data=df))
    print(summary(felm(formula_for_fn, data = df)))

    icpt <- getfe(obj = fe_lm, se = TRUE, robust = TRUE,  ef = 'zm2', bN = 5000)
    
    icpt <- icpt %>%
      mutate(t_value = effect/robustse,
             p_value = 2 * pt(-abs(t_value), df = fe_lm$df))
    cons <- c(icpt["icpt.1", c("effect", "robustse", "t_value", "p_value")])
    
    # Print model summary with constant
    summary_model <- rbind(summary(fe_lm)$coefficients,cons)
    cat("\n Computing for the constant:\n")
    print(summary_model)
  }
}

# Execute the analysis for balance and outcome variables
#Balance
areg(balance_vars, df)

#Outcomes
areg(outcome_vars, df)
