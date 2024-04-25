#INSTALL THE PACKAGES
packages <- c("tidyverse", "haven", "gtsummary", "ggpubr", "ggplot2", "rdrobust", "rddensity");for(pack in packages){
  if(!require(pack, character.only = TRUE, quietly = TRUE)){
    install.packages(pack, dependencies = TRUE)
    require(pack, character.only = TRUE)
  }
  # Clean up the workspace
  rm(pack)
};theme_set(theme_pubclean())


#################################################################
#################################################################
######################SLIDE 2####################################
#Load dataset
hisp_data <- read_dta("evaluation.dta")
hisp_data <- hisp_data %>%
  mutate(D = ifelse(poverty_index > 58, "> 58", "<= 58"))

#Check whether there is any evidence of manipulation of the eligibility index 
ggplot(hisp_data, aes(x = poverty_index)) + 
  geom_vline(xintercept = 58, type = '-') +
  geom_density()+
  ggtitle("KDE Plot of Poverty index")

rdplotdensity(rdd = rddensity(hisp_data$poverty_index, c = 58), 
              X = hisp_data$poverty_index, 
              type = "both")


#################################################################
#################################################################
######################SLIDE 3####################################
#Categorize > 58 and <= 58
hisp_data_treatment <- hisp_data %>%
  filter(treatment_locality == 1) 

#Can you apply a sharp RDD? 
rdplot(y = hisp_data_treatment$enrolled,
       x = hisp_data_treatment$poverty_index,
       c = 58,
       p = 1,
       nbins = c(58, 42),
       x.label = "Running variable (Poverty index)",
       y.label = "Outcome variable (Enrolled)"
)

# Plotting
ggplot(hisp_data_treatment, aes(y = enrolled, x = poverty_index, color = factor(D))) +
  geom_vline(xintercept = 58) +
  geom_point() +
  labs(x = "Poverty Index", y = "Enrolled")




#################################################################
#################################################################
######################SLIDE 4####################################
# Normalize the poverty index at "0"
hisp_data_model <- hisp_data %>%
  mutate(poverty_index_left = ifelse(poverty_index <= 58, poverty_index - 58, 0),
         poverty_index_right = ifelse(poverty_index > 58, poverty_index - 58, 0)) %>%
  filter(round == 1 & treatment_locality == 1)

# Apply regression on a subset of data
reg_model <- lm(health_expenditures ~ poverty_index_left + poverty_index_right + eligible,
                data = hisp_data_model)

# Extract fitted values
hisp_data_model$he_pred1<- reg_model$fitted.values

# Plotting
ggplot(hisp_data_model, aes(x = poverty_index, y = he_pred1, color = factor(D))) +
  geom_point() + labs(x = "Poverty Index", y = "Predicted Health Expenditures")


#################################################################
#################################################################
######################SLIDE 5####################################
rdd <- rdrobust(
  y = hisp_data_model$health_expenditures,
  x = hisp_data_model$poverty_index,
  c = 58
)

summary(rdd)

#################################################################
#################################################################
######################SLIDE 6####################################
# Define the model formula
model_formula <- health_expenditures ~ eligible + poverty_index_left + poverty_index_right +
  age_hh + age_sp + educ_hh + educ_sp + female_hh + indigenous +
  hhsize + dirtfloor + bathroom + land + hospital_distance

# Filter the data and run the regression
reg_model_cov <- lm(model_formula, data = hisp_data_model)

# Summarize the regression results
summary(reg_model_cov)


#################################################################
#################################################################
######################SLIDE 7####################################
# Generating higher order terms
hisp_data_model <- hisp_data_model %>%
  mutate(
    poverty_index_left2 = poverty_index_left^2,
    poverty_index_right2 = poverty_index_right^2,
    poverty_index_left3 = poverty_index_left^3,
    poverty_index_right3 = poverty_index_right^3,
    poverty_index_left4 = poverty_index_left^4,
    poverty_index_right4 = poverty_index_right^4
  )

# Define the model formula for regression with higher order terms
model_formula <- health_expenditures ~ eligible + poverty_index_left + poverty_index_right +
  poverty_index_left2 + poverty_index_right2 +
  poverty_index_left3 + poverty_index_right3 +
  poverty_index_left4 + poverty_index_right4

# Filter the data and run the regression
reg_model_cov_polynomial <- lm(model_formula, data = hisp_data_model)

# Summarize the regression results
summary(reg_model_cov_polynomial)



#################################################################
#################################################################
######################SLIDE 8####################################
hisp_data_model$he_pred2 <- reg_model_cov_polynomial$fitted.values

# Plotting
ggplot(hisp_data_model, aes(x = poverty_index, y = he_pred2, color = factor(D))) +
  geom_point() + labs(x = "Poverty Index", y = "Predicted Health Expenditures")



#################################################################
#################################################################
######################SLIDE 9####################################
# Filter the data
filtered_data <- filter(hisp_data, round == 0 & treatment_locality == 1)

# Regression model
reg_model_false <- lm(health_expenditures ~ eligible + poverty_index, data = filtered_data)

# Generate predictions
filtered_data$he_pred3 <- reg_model_false$fitted.values

# Plotting the predictions
ggplot(filtered_data, aes(x = poverty_index, y = he_pred3, color = factor(D))) +
  geom_point() +
  labs(x = "Poverty Index", y = "Predicted Health Expenditures",
       title = "Predicted Health Expenditures vs. Poverty Index")



#################################################################
#################################################################
######################SLIDE 10####################################
rdd <- rdrobust(
  y = filtered_data$health_expenditures,
  x = filtered_data$poverty_index,
  c = 58
)

summary(rdd)
