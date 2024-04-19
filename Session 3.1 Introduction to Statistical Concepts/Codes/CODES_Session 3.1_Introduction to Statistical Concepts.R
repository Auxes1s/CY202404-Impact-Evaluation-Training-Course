# Install packages, do not remove the packages
packages <- c("tidyverse", "dplyr", "ggplot2", "jtools", "reshape2", "wooldridge", 
              "vtable", "gtsummary", "olsrr", "lmtest", "sandwich");for(pack in packages){
  if(!require(pack, character.only = TRUE)){
    install.packages(pack, dependencies = TRUE)
    require(pack, character.only = TRUE)
  }
  # Clean up the workspace
  rm(pack)
}

# Load the 'wage1' dataset from the 'wooldridge' package
data("wage1")

# sum wage educ - Summarize the distribution of 'wage' and 'educ' variables
wage1 %>%
  select(wage, educ) %>%
  st()

#reg wage educ -  Regression Analysis: Wage vs. Education
# Model 1: Linear regression of wage on education
model_1 <- lm(wage ~ educ, wage1)
model_1 %>%
  summary()

# Create a table summarizing the 'educ' variable
wage1 %>%
  select(educ) %>%
  mutate(educ = as.factor(educ)) %>% #Force tbl_summary to treat educ as a categorical variable
  tbl_summary()

#reg lwage educ - Model 2: Linear regression of log(wage) on education
model_2 <- lm(lwage ~ educ, wage1)

##Output the summary of the linear regression
model_2 %>%
  summary()


# Example: Wage Equation with Multiple Predictors
#sum wage lwage educ exper tenure - Summarize selected variables
wage1 %>%
  select(wage, lwage, educ, exper, tenure) %>%
  st()

# Model 3: Multiple linear regression with education, experience, and tenure
model_3 <- lm(lwage ~ educ + exper + tenure, wage1)

model_3 %>%
  summary()

#################
# Regression Diagnostics for Model 3
#Component-residual plots (Partial Residual plots)
resid_model_3 <- resid(model_3)
par(mfrow=c(2,2))
plot(x = model_3$model$exper, y = resid_model_3) ; abline(h = 0, lty = 2)
plot(x = model_3$model$educ, y = resid_model_3) ; abline(h = 0,  lty = 2)
plot(x = model_3$model$tenure, y = resid_model_3) ; abline(h = 0, lty = 2)

#Model DFBetas
dfbetas <-  dfbetas(model_3) %>%
  as.data.frame() %>%
  select(-"(Intercept)") %>% #Let's drop the intercepts
  melt(variable.name = "Predictor", value.name = "DFBETA")
  
ggplot(dfbetas, aes(Predictor, DFBETA, fill = Predictor, color =  Predictor)) +
  geom_boxplot() +
  theme_apa()

# Model Variance Inflation Factor (VIF)
model_3 %>%
  ols_coll_diag()

# Test for Heteroscedasticity
model_3 %>%
  bptest()

#Robust Standard Errors in R c/o sandwich package
robust_se <- vcovHC(model_3, type = "HC1")
coeftest(model_3, vcov = robust_se)

#Model Extensions and Other Issues - Model 4: Including 'female' as a predictor
model_4 <- lm(lwage ~ educ + exper + tenure + female, wage1)
model_4 %>%
  summary()
