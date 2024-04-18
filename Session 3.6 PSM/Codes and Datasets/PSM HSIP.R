# Loading of packages, data, and seed setting here
packages <- c("tidyverse", "dplyr", "haven", "here", "broom", "estimatr", "fishmethods", "haven", "kableExtra", 
              "MatchIt", "modelsummary", "pwr", "rddensity", "skimr", 
              "texreg", "tidyverse");for(pack in packages){
  if(!require(pack, character.only = TRUE)){
    install.packages(pack, dependencies = TRUE)
    require(pack, character.only = TRUE)
  }
  # Clean up the workspace
  rm(pack)
}

theme_set(ggpubr::theme_pubclean())
setwd(here("Session 3.6 PSM/Codes and Datasets"))

# Load datasets
data <- file.path(getwd(), "evaluation.dta")
df <- read_dta(data)
vars <- read_csv("variable_desc.csv")

# subset data to only "eligible" units
df_w <- df %>%
  pivot_wider(names_from = round, # variable that determines new columns
              # variables that should be made "wide"
              values_from = c(health_expenditures, 
                              poverty_index, age_hh, age_sp,
                              educ_hh, educ_sp, female_hh,
                              indigenous, hhsize, dirtfloor,
                              bathroom, land, hospital_distance,
                              hospital)) %>%
  # remove the household that has missing values
  # as missing values are not allowed when using matchit
  filter(!is.na(health_expenditures_0)) 


# PSM Estimation (Probit predicted scores) --------------------------------
psm_r <- matchit(enrolled ~ age_hh_0 + educ_hh_0,
                 data = df_w %>% dplyr::select(-hospital_0, -hospital_1), 
                 distance = "probit")

psm_ur <- matchit(enrolled ~ age_hh_0 + educ_hh_0 + age_sp_0 + educ_sp_0 +
                    female_hh_0 + indigenous_0 + hhsize_0 + dirtfloor_0 +
                    bathroom_0 + land_0 + hospital_distance_0,
                  data = df_w %>% dplyr::select(-hospital_0, -hospital_1), 
                  distance = "probit")

psm_r2 <- matchit(enrolled ~ age_hh_0 + educ_hh_0,
                 data = df_w %>% dplyr::select(-hospital_0, -hospital_1), 
                 distance = "logit")

psm_ur2 <- matchit(enrolled ~ age_hh_0 + educ_hh_0 + age_sp_0 + educ_sp_0 +
                    female_hh_0 + indigenous_0 + hhsize_0 + dirtfloor_0 +
                    bathroom_0 + land_0 + hospital_distance_0,
                  data = df_w %>% dplyr::select(-hospital_0, -hospital_1), 
                  distance = "logit")

htmlreg(list(psm_r$model, psm_ur$model, psm_r2$model, psm_ur2$model),
        file="PSM Probit and Logit.doc",
        doctype = T,
        custom.coef.map = list('age_hh_0' = "Age (HH) at Baseline",
                               'educ_hh_0' = "Education (HH) at Baseline",
                               'age_sp_0' = "Age (Spouse) at Baseline",
                               'educ_sp_0' = "Education (Spouse) at Baseline",
                               'female_hh_0' = "Female Head of Household (HH) at Baseline",
                               'indigenous_0' = "Indigenous Language Spoken at Baseline",
                               'hhsize_0' = "Number of Household Members at Baseline",
                               'dirtfloor_0' = "Dirt floor at Baseline",
                               'bathroom_0' = "Private Bathroom at Baseline",
                               'land_0' = "Hectares of Land at Baseline",
                               'hospital_distance_0' = "Distance From Hospital at Baseline"),
        caption = "Estimating the Propensity Score Based on Baseline Observed Characteristics",
        custom.model.names = c("Limited Set (Probit)", "Full Set (Probit)",
                               "Limited Set (Logit)", "Full Set (Logit)"))

# Common Support Region ---------------------------------------------------
df_w <- df_w %>%
  mutate(ps_r = psm_r$model$fitted.values,
         ps_ur = psm_ur$model$fitted.values,
         ps_r2 = psm_r2$model$fitted.values,
         ps_ur2 = psm_ur2$model$fitted.values)

df_w %>% #Common support for Probit UR
  mutate(enrolled_lab = ifelse(enrolled == 1, "Enrolled", "Not Enrolled")) %>%
  ggplot(aes(x = ps_ur,
             group = enrolled_lab, colour = enrolled_lab, fill = enrolled_lab)) +
  geom_density(alpha = I(0.4)) +
  xlab("Propensity Score") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  scale_colour_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")

df_w %>% #Common support for Logit UR
  mutate(enrolled_lab = ifelse(enrolled == 1, "Enrolled", "Not Enrolled")) %>%
  ggplot(aes(x = ps_ur2,
             group = enrolled_lab, colour = enrolled_lab, fill = enrolled_lab)) +
  geom_density(alpha = I(0.4)) +
  xlab("Propensity Score") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  scale_colour_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")


# Balance Tests -----------------------------------------------------------
kable(summary(psm_ur)$sum.all,
      caption = "Balance Before Matching") %>%
  kable_styling() #Pre-treatment probit

kable(summary(psm_ur)$sum.matched,
      caption = "Balance After Matching") %>%
  kable_styling() #Post-treatment probit

kable(summary(psm_ur2)$sum.all,
      caption = "Balance Before Matching") %>%
  kable_styling() #Pre-treatment

kable(summary(psm_ur2)$sum.matched,
      caption = "Balance After Matching") %>%
  kable_styling() #Post-treatment

# Obtain PSM Impact Estimate --------------------------------------------------
match_df_r <- match.data(psm_r) #obtain matched data (Probit restricted)
match_df_ur <- match.data(psm_ur) #obtain matched data (Probit unrestricted)
match_df_r2 <- match.data(psm_r2) #obtain matched data (Logit restricted)
match_df_ur2 <- match.data(psm_ur2) #obtain matched data (Logit unrestricted)

out_lm_r <- lm_robust(health_expenditures_1 ~ enrolled,
                      data = match_df_r, clusters = locality_identifier,
                      weights = weights)

out_lm_ur <- lm_robust(health_expenditures_1 ~ enrolled,
                       data = match_df_ur, clusters = locality_identifier,
                       weights = weights)

out_lm_r2 <- lm_robust(health_expenditures_1 ~ enrolled,
                      data = match_df_r2, clusters = locality_identifier,
                      weights = weights)

out_lm_ur2 <- lm_robust(health_expenditures_1 ~ enrolled,
                       data = match_df_ur2, clusters = locality_identifier,
                       weights = weights)

htmlreg(list(out_lm_r, out_lm_ur,
             out_lm_r2, out_lm_ur2),
        file="PSM Impact Estimates.doc",
        doctype = T,
        caption = "Evaluating HISP: Matching on Baseline Characteristics and Regression Analysis",
        custom.model.names = c("Limited Set (Probit)", 
                               "Full Set (Probit)",
                               "Limited Set (Logit)", 
                               "Full Set (Logit)"))


# DiD with PSM ------------------------------------------------------------
df_long_match_r <- df %>%
  left_join(match_df_r %>% dplyr::select(household_identifier, weights)) %>%
  filter(!is.na(weights))

df_long_match_ur <- df %>%
  left_join(match_df_ur %>% dplyr::select(household_identifier, weights)) %>%
  filter(!is.na(weights))

df_long_match_r2 <- df %>%
  left_join(match_df_r2 %>% dplyr::select(household_identifier, weights)) %>%
  filter(!is.na(weights))

df_long_match_ur2 <- df %>%
  left_join(match_df_ur2 %>% dplyr::select(household_identifier, weights)) %>%
  filter(!is.na(weights))

did_reg_r <- lm_robust(health_expenditures ~ enrolled + round + enrolled * round,
                       data = df_long_match_r, weights = weights,
                       clusters = locality_identifier)

did_reg_ur <- lm_robust(health_expenditures ~ enrolled + round + enrolled * round,
                        data = df_long_match_ur, weights = weights,
                        clusters = locality_identifier)

did_reg_r2 <- lm_robust(health_expenditures ~ enrolled + round + enrolled * round,
                       data = df_long_match_r2, weights = weights,
                       clusters = locality_identifier)

did_reg_ur2 <- lm_robust(health_expenditures ~ enrolled + round + enrolled * round,
                        data = df_long_match_ur2, weights = weights,
                        clusters = locality_identifier)

htmlreg(list(did_reg_r, did_reg_ur,
             did_reg_r2, did_reg_ur2), 
        file="DiD-PSM Impact Estimates.doc",
        doctype = T,
        custom.coef.map = list('enrolled' = "Enrollment",
                               'round' = "Round",
                               'enrolled:round' = "Enrollment X Round"),
        caption = "Evaluating HISP: Difference-in-Differences Regression Combined With Matching",
        custom.model.names = c("Limited Set (Probit)", 
                               "Full Set (Probit)",
                               "Limited Set (Logit)",
                               "Full Set (Logit)"),
        caption.above = TRUE)
