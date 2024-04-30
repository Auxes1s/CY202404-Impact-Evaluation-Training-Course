# Packages Required -------------------------------------------------------
library(tidyverse) #for cleaning data, generating variables
library(haven) #for importing the dataset
library(texreg) #for clean tables in felm() results
library(plm) #for plm() function, used to replicate xtreg 
library(sandwich) #for robust standard errors
library(clubSandwich) #for vcovCR() function, used to replicate STATA robust standard errors
library(sjlabelled) #for labelling variables
library(lfe) #for felm() function, to replicate cluster robust standard errors in STATA
library(stargazer) #for clean tables in placebo tests

# Loading the Dataset -----------------------------------------------------
#Choosing a file manually, if you have trouble locating your working directory, : remove the # at the beginning of the next two lines, run the first line then a pop-up will then show 
#stata_local_rct2 <- choose.files() #make sure to go to the folder where you placed the dataset and select the dataset "DID data_KFA Apr 25 2023.dta"
#did.dta <- read_dta(stata_local_rct) #remove the # if you have trouble locating your working directory

#Getting a stata file from local PC using codes
stata_local_rct <- file.path(getwd(), "DID data_KFA Apr 25 2023.dta")
did.dta <- read_dta(stata_local_rct)

# Simple DiD --------------------------------------------------------------
simple_did <- did.dta %>%
  group_by(psgc) %>% 
  mutate(evercity = max(city))

simple_did_descstats <- simple_did %>%
  filter(year %in% c(1992,2015) & (cyear>2000)) %>%
  group_by(year, evercity) %>%
  summarize(mean_lncinc_pcr = mean(lninc_pcr, na.rm=TRUE)) #note missing values for 2015 (STATA automatically drops these)
simple_did_descstats
print.data.frame(simple_did_descstats) #to display decimals based on your options set with options(digits = 7), 7 is the default

(7.913204-5.914218)-(7.433907-5.927306) #simple DiD

#Simple DiD with Regressions
simple_did_regress <- simple_did %>%
  filter(year %in% c(1992,2015) & (cyear>2000)) %>%
  mutate(year = as.factor(year),
         evercity = as.factor(evercity))

did_reg1 <- lm(lninc_pcr ~ year*evercity, data = simple_did_regress) #R's equivalent of factor interactions with STATA is *
summary(did_reg1)
stargazer(did_reg1, type = "text")


# Full Sample -------------------------------------------------------------
#replicating results of running : xtreg lninc_pcr i.year i.cityx, fe vce(cluster prv)
did_felm1 <- felm(lninc_pcr ~ as.factor(year) + as.factor(cityx) | psgc | 0 | prv, simple_did)
summary(did_felm1)
#to replicate computation of intercept (average value of fixed effects) in STATA:
did_reg2 <- plm(lninc_pcr ~ as.factor(year) + as.factor(cityx),
      simple_did, index = c("psgc", "year"), model = "within")
within_intercept(did_reg2, vcov = function(x) vcovCR(did_reg2,
                                                     type = "CR1S", 
                                                     cluster = simple_did$prv))

#replicating results of running : xtreg lninc_pcr i.year i.cityx lninclo_r3 lnpopx3, fe vce(cluster prv)
did_felm2 <- felm(lninc_pcr ~ as.factor(year) + as.factor(cityx) + lninclo_r3 + lnpopx3 | psgc | 0 | prv, simple_did)
summary(did_felm2)
#to replicate computation of intercept (average value of fixed effects) in STATA:
did_reg3 <- plm(lninc_pcr ~ as.factor(year) + as.factor(cityx) + lninclo_r3 + lnpopx3,
                simple_did, index = c("psgc", "year"), model = "within")
within_intercept(did_reg3, vcov = function(x) vcovCR(did_reg3, #to replicate computation of intercept (average value of fixed effects) in STATA
                                                     type = "CR1S", 
                                                     cluster = simple_did$prv))
#Summarizing Results
screenreg(list(did_felm1, did_felm2), include.fstatistic = T, digits = 3 ,
          custom.coef.map = list( "as.factor(cityx)1" = "cityx", "lninclo_r3" = "lninclo_r3","lnpopx3" = "lnpopx3"))


# Placebo Tests -----------------------------------------------------------

# Tests 1 : Early Ratification by 3 Years ---------------------------------
placebo.tests1 <- simple_did %>%
  group_by(psgc) %>% 
  mutate(lninclo.r.lag1 = ifelse(year-dplyr::lag(year)==1, dplyr::lag(lninclo_r, n = 1, order_by = year), NA), #mutate(lag1 = ifelse(time - lag(time) == 1, lag(value), NA))
         lnpopx.lag1 = ifelse(year-dplyr::lag(year)==1, dplyr::lag(lnpopx, n = 1, order_by = year), NA)) %>% 
  ungroup() %>% 
  mutate(cityx3 = ifelse(year>=(cyear-3),1,0),
         lnrain.pcr = precip_dlr) %>%
  rename(lninc.pcr = lninc_pcr,
         lnexp.pcr = lnexp_pcr) %>% 
  var_labels(lnrain.pcr = "Rainfall, standardized to long-run normal",
             cityx3 = "Earlier city ratification, 3 years") %>% 
  filter(cityx == 0)

#Loop over them and create model for each
outcome_list <- list("lninc.pcr", "lnexp.pcr", "lnrain.pcr")
felm_nolags = lapply(outcome_list, function(x){
  felm(as.formula(paste0(x, "~ as.factor(year) + as.factor(cityx3)| psgc | 0 | prv")), data = placebo.tests1)
})

felm_withlags = lapply(outcome_list, function(x){
  felm(as.formula(paste0(x, "~ as.factor(year) + as.factor(cityx3) + lninclo.r.lag1 + lnpopx.lag1 | psgc | 0 | prv")), data = placebo.tests1)
})
stargazer(felm_nolags, felm_withlags, type = "text", keep = "cityx3",
          dep.var.labels=c("No Lags", "With Lags"),
          column.labels = as.character(outcome_list))


# Test 2 : Random Assignment ----------------------------------------------
set.seed(1986) #unlikely to generate same results as STATA
placebo.tests2 <- simple_did %>%
  group_by(psgc) %>% 
  mutate(lninclo.r.lag1 = ifelse(year-dplyr::lag(year)==1, dplyr::lag(lninclo_r, n = 1, order_by = year), NA), #mutate(lag1 = ifelse(time - lag(time) == 1, lag(value), NA))
         lnpopx.lag1 = ifelse(year-dplyr::lag(year)==1, dplyr::lag(lnpopx, n = 1, order_by = year), NA)) %>% 
  ungroup() %>% 
  mutate(rand = ifelse(year==2015, 1900+floor(runif(39827)*(2015-1900))+1, NA),
         cityr = ifelse(year==2015, ifelse(runif(39827), 1,0), NA)) %>%
  group_by(id) %>%
  mutate(randx = max(rand, na.rm = TRUE),
         cityx = max(cityr, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(cyear_rand = randx*cityx+9999*(ifelse(cityx==0, 1,0)),
         city_rand = ifelse(year > cyear_rand, 1,0),
         lnrain.pcr = precip_dlr) %>% 
  rename(lninc.pcr = lninc_pcr,
         lnexp.pcr = lnexp_pcr)

felm_nolags2 = lapply(outcome_list, function(x){
  felm(as.formula(paste0(x, "~ as.factor(year) + as.factor(city_rand)| psgc | 0 | prv")), data = placebo.tests2)
})

felm_withlags2 = lapply(outcome_list, function(x){
  felm(as.formula(paste0(x, "~ as.factor(year) + as.factor(city_rand) + lninclo.r.lag1 + lnpopx.lag1 | psgc | 0 | prv")), data = placebo.tests2)
})
stargazer(felm_nolags2, felm_withlags2, type = "text", keep = "city_rand",
          dep.var.labels=c("No Lags", "With Lags"),
          column.labels = as.character(outcome_list))


# Test 3 : Unrelated Outcome Rainfall -------------------------------------
placebo.tests3 <- simple_did %>%
  group_by(psgc) %>% 
  mutate(lninclo.r.lag1 = ifelse(year-dplyr::lag(year)==1, dplyr::lag(lninclo_r, n = 1, order_by = year), NA), #mutate(lag1 = ifelse(time - lag(time) == 1, lag(value), NA))
         lnpopx.lag1 = ifelse(year-dplyr::lag(year)==1, dplyr::lag(lnpopx, n = 1, order_by = year), NA)) %>% 
  ungroup() %>% 
  rename(lninc.pcr = lninc_pcr,
         lnexp.pcr = lnexp_pcr)

felm_nolags3 <- felm(precip_dlr ~ as.factor(year) + as.factor(cityx)| psgc | 0 | prv, data = placebo.tests3)
felm_withlags3 <- felm(precip_dlr ~ as.factor(year) + as.factor(cityx) + lninclo.r.lag1 + lnpopx.lag1 | psgc | 0 | prv, data = placebo.tests3)
stargazer(felm_nolags3, felm_withlags3, type = "text", keep = "cityx")


# Parallel Trends ---------------------------------------------------------
pt_graph <- did.dta %>%
  group_by(id) %>% 
  mutate(city1 = max(city*(ifelse(cyear<=2000, 1,0)), na.rm = TRUE),
         city2 = max(city*(ifelse(cyear>=2001, 1,0)), na.rm = TRUE),
         city3 = 1*city1 + 2*city2) %>%
  ungroup() %>% 
  select(city3, year, lninc_pcr, lnexp_pcr) %>%
  rename(inc = lninc_pcr,
         exp = lnexp_pcr) %>% 
  group_by(city3, year) %>% 
  summarize(mean_inc = mean(inc, na.rm = TRUE),
            mean_exp = mean(exp, na.rm = TRUE))

#reshape wide inc exp, i(year) j(city3)
pt_graph2 <- pt_graph %>% 
  pivot_wider(names_from = c("city3"), values_from = c("mean_inc","mean_exp")) 
pt_graph2

#Graph Per Capita Income
pt_labels <- c("Cities ratified post-2001", "Cities ratified pre-2001","Municipalities")
pt_colors <- c("Cities ratified post-2001" = "green", "Cities ratified pre-2001" = "red", "Municipalities"= "blue")
pt_lines <- c("Municipalities" = 1, "Cities ratified pre-2001" = 2, "Cities ratified post-2001" = 1)
pt_graph2 %>% 
  ggplot() +
  geom_line(aes(x = year, y = mean_inc_2, color = "Cities ratified post-2001", linetype = "Cities ratified post-2001"), linewidth = 1.25) +
  geom_line(aes(x = year, y = mean_inc_1, color = "Cities ratified pre-2001", linetype = "Cities ratified pre-2001"), linewidth = 1.25) +
  geom_line(aes(x = year, y = mean_inc_0, color = "Municipalities", linetype = "Municipalities"), linewidth = 1.25) +
  geom_point(aes(x = year, y = mean_inc_2, color = "Cities ratified post-2001"), alpha = 0.25, size = 5) +
  geom_vline(xintercept = 2001, linetype = "dotted") +
  annotate("text", x=2001, y=log(3000), label= "RA9009")+
  xlab("year") +
  ylab("Constant 20000 PhP, log scale") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust=0.5, size = "25", face = "bold"),
    axis.ticks = element_line(size = 0.5),
    axis.ticks.length = unit(0.25, "cm")) +
  scale_color_manual(name = "Legend", labels = pt_labels, values = pt_colors) +
  scale_linetype_manual(name = "Legend", labels = pt_labels, values = pt_lines)+
  scale_x_continuous(limits=c(1990,2015), breaks = seq(1990, 2015, by = 5)) +
  scale_y_continuous(breaks = c(log(300), log(550), log(1000), log(1700), log(3000)),
                     labels = c("300", "550", "1000", "1700", "3000"))

#Graph Per Capita Expenditure
pt_graph2 %>% 
  ggplot() +
  geom_line(aes(x = year, y = mean_exp_2, color = "Cities ratified post-2001", linetype = "Cities ratified post-2001"), linewidth = 1.25) +
  geom_line(aes(x = year, y = mean_exp_1, color = "Cities ratified pre-2001", linetype = "Cities ratified pre-2001"), linewidth = 1.25) +
  geom_line(aes(x = year, y = mean_exp_0, color = "Municipalities", linetype = "Municipalities"), linewidth = 1.25) +
  geom_point(aes(x = year, y = mean_exp_2, color = "Cities ratified post-2001"), alpha = 0.25, size = 5) +
  geom_vline(xintercept = 2001, linetype = "dotted") +
  annotate("text", x=2001, y=log(3000), label= "RA9009")+
  xlab("year") +
  ylab("Constant 20000 PhP, log scale") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust=0.5, size = "25", face = "bold"),
    axis.ticks = element_line(size = 0.5),
    axis.ticks.length = unit(0.25, "cm")) +
  scale_color_manual(name = "Legend", labels = pt_labels, values = pt_colors) +
  scale_linetype_manual(name = "Legend", labels = pt_labels, values = pt_lines)+
  scale_x_continuous(limits=c(1990,2015), breaks = seq(1990, 2015, by = 5)) +
  scale_y_continuous(breaks = c(log(300), log(550), log(1000), log(1700), log(3000)),
                     labels = c("300", "550", "1000", "1700", "3000"))
