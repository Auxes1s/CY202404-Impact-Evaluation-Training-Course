#INSTALL THE PACKAGES
packages <- c("tidyverse", "haven", "gtsummary", "AER");for(pack in packages){
  if(!require(pack, character.only = TRUE, quietly = TRUE)){
    install.packages(pack, dependencies = TRUE)
    require(pack, character.only = TRUE)
  }
  # Clean up the workspace
  rm(pack)
}


#Read the data
hisp_data <- read_dta("evaluation.dta")


#What is the proportion of HH who got the promotion? 
hisp_data %>%
  select(promotion_locality) %>%
  tbl_summary()


#Are there differences in the baseline characteristics of HH who got the 
#promotion and those who did not? Compare the baseline health expenditures, 
#poverty_index and hhsize and other characteristics of the two populations

#hhsize
hisp_data %>%
  filter(round == 0) %>%
  select(hhsize, promotion_locality) %>%
  t.test(hhsize ~ promotion_locality, data = .)

#poverty_indexs
hisp_data %>%
  filter(round == 0) %>%
  select(poverty_index, promotion_locality) %>%
  t.test(poverty_index ~ promotion_locality, data = .)

#health_expenditures
hisp_data %>%
  filter(round == 0) %>%
  select(poverty_index, promotion_locality) %>%
  t.test(poverty_index ~ promotion_locality, data = .)


#Was there a difference in enrollment rates between the promoted and 
#non-promoted villages? 
#health_expenditures
(ttest_enrolled <- hisp_data %>%
  filter(round == 1) %>%
  select(enrolled_rp, promotion_locality) %>%
  t.test(enrolled_rp ~ promotion_locality, data = .))

#What is the ITT impact on health expenditures between the promoted and
#non-promoted localities? 
(ttest_hex <- hisp_data %>%
  filter(round == 1) %>%
  select(health_expenditures, promotion_locality) %>%
  t.test(health_expenditures ~ promotion_locality, data = .))

#What is the LATE impact
ITT_hex <- ttest_hex$estimate[1] - ttest_hex$estimate[2]
ITT_ERP <- ttest_enrolled$estimate[1] - ttest_enrolled$estimate[2]

hisp_data %>%
  filter(round == 1) %>%
  mutate(promotion_locality = ifelse(promotion_locality == 1, "Promoted locality", "Non-promoted locality")) %>%
  group_by(promotion_locality) %>%
  summarise(Mean_HEX = mean(health_expenditures), Mean_ERP = mean(enrolled_rp)) %>%
  add_row(promotion_locality = "ITT (Difference of Means)", Mean_HEX = ITT_hex, Mean_ERP = ITT_ERP) %>%
  mutate(LATE = ifelse(promotion_locality == "ITT (Difference of Means)", ITT_hex / ifelse(ITT_ERP == 0, NA, ITT_ERP), NA))


#What is LATE impact using IV regression? 
hisp_round1 <- subset(hisp_data, round == 1)
model_iv <- ivreg(health_expenditures ~ enrolled_rp | promotion_locality, data = hisp_round1)
summary(model_iv)



#What is the LATE impact with controls  (age of the household head and spouse) ? 
model_iv_control <- ivreg(health_expenditures ~ enrolled_rp + age_hh + age_sp | promotion_locality + age_hh + age_sp, data = hisp_round1)
summary(model_iv_control)



#Including baseline covariates:  age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance  
# Perform the instrumental variables regression including several additional covariates
model_iv_cov <- ivreg(health_expenditures ~ enrolled_rp + age_hh + age_sp + educ_hh + educ_sp +
                    female_hh + indigenous + hhsize + dirtfloor + bathroom + land +
                    hospital_distance | promotion_locality + age_hh + age_sp +
                    educ_hh + educ_sp + female_hh + indigenous + hhsize + dirtfloor +
                    bathroom + land + hospital_distance, data = hisp_round1)

summary(model_iv_cov)
