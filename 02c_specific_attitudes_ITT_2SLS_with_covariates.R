###libraries####

library(dplyr)
library(tidyr)
library(lattice)
library(lme4)
library(lmerTest)

##for table output
library(broom)
library(dplyr)
library(knitr)
library(kableExtra)
library(stargazer)
library(texreg)
library(modelsummary)
library(officer)
library(flextable)
##for ivr
library(ivreg)

##for plots
library(ggplot2)

options(scipen = 999)
options("modelsummary_factory_default" = "flextable")



##0. load data ####

load("./datasets/df1_final_ac.Rdata")
load("./datasets/df2_final_ac.Rdata")
load("./datasets/df3_final_ac.Rdata")



##1. recode variables####

df1$group_f <- as.factor(df1$group)
df2$group_f <- as.factor(df2$group)
df3$group_f <- as.factor(df3$group)


#3-level IV
##group_f is already this
df1$group_f



##1. Kindergrundsicherung####

##1.1. extract special attitude variables####



##subset df1 to only include variables with both spec_ and _diff
dfs <- df1[, grepl("spec_", colnames(df1)) & grepl("_diff", colnames(df1))]

##remove from dfs those that end in _2
dfs <- dfs[, !grepl("_2", colnames(dfs))]
dfs_abs <- abs(dfs)

##put _abs behind all colnames in dfs_abs
colnames(dfs_abs) <- paste0(colnames(dfs_abs), "_abs")

##bind dfs and dfs_abs
dfs <- cbind(dfs, dfs_abs)


dfs$group_f <- df1$group_f
dfs$url_int_visits_yn <- df1$url_int_visits_yn
dfs$age <- df1$age
dfs$gender_f <- df1$gender_f
dfs$edu_time <- df1$edu_time
dfs$search_internet_pre <- df1$search_internet_pre


table(dfs$spec_own_office_diff, dfs$group_f)
table(dfs$spec_own_office_diff_abs, dfs$group_f)

table(dfs$spec_treasury_cost_1_diff, dfs$group_f)
table(dfs$spec_treasury_cost_1_diff_abs, dfs$group_f)

colnames(dfs)

##1.2. models directed kindergrundsicherung####

sls_b1 <- ivreg(spec_payment_guarantee_1_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b1)

sls_b2 <- ivreg(spec_payment_income_1_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b2)

sls_b3 <- ivreg(spec_digital_application_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b3)

sls_b4 <- ivreg(spec_automatic_payment_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b4)

sls_b5 <- ivreg(spec_payment_establishment_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b5)

sls_b6 <- ivreg(spec_own_office_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b6)

sls_b7 <- ivreg(spec_treasury_cost_1_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b7)



# Combine the model outputs
models <- list(sls_b1, sls_b2, sls_b3, sls_b4, sls_b5, sls_b6,sls_b7)
names(models) <- rep(c("guaranteed KG", "incomebased KG", "app", "automatic pay", 
                       "all_families in D", "special office","state cost"))




m1 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("url_int_visits_yn" = "URL visits", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table3.docx"
                   ,title = 'Kindergrundsicherung: Effect of URL tracked visits on specific attitudes, directed (group 3 levels).'
)

m1



##1.3. models absolute change####

sls_b1 <- ivreg(spec_payment_guarantee_1_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b1)

sls_b2 <- ivreg(spec_payment_income_1_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b2)

sls_b3 <- ivreg(spec_digital_application_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b3)

sls_b4 <- ivreg(spec_automatic_payment_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b4)

sls_b5 <- ivreg(spec_payment_establishment_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b5)

sls_b6 <- ivreg(spec_own_office_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b6)

sls_b7 <- ivreg(spec_treasury_cost_1_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b7)



# Combine the model outputs
models <- list(sls_b1, sls_b2, sls_b3, sls_b4, sls_b5, sls_b6,sls_b7)
names(models) <- rep(c("guaranteed KG", "incomebased KG", "app", "automatic pay", 
                       "all_families in D", "special office","state cost"))



m2 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("url_int_visits_yn" = "URL visits", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table3.docx"
                   ,title = 'Kindergrundsicherung: Effect of URL tracked visits on specific attitudes, abs change (group 3 levels).'
)

m2


##2. Erneuerbare Energie####

##2.1. extract special attitude variables####


##subset df1 to only include variables with both spec_ and _diff
dfs <- df2[, grepl("spec_", colnames(df2)) & grepl("_diff", colnames(df2))]

##only use cols 1:7
dfs <- dfs[, 1:7]
dfs_abs <- abs(dfs)

##put _abs behind all colnames in dfs_abs
colnames(dfs_abs) <- paste0(colnames(dfs_abs), "_abs")

##bind dfs and dfs_abs
dfs <- cbind(dfs, dfs_abs)


dfs$group_f <- df2$group_f
dfs$url_int_visits_yn <- df2$url_int_visits_yn
dfs$age <- df2$age
dfs$gender_f <- df2$gender_f
dfs$edu_time <- df2$edu_time
dfs$search_internet_pre <- df2$search_internet_pre

colnames(dfs)

##2.2. models directed EEG####

sls_b1 <- ivreg(spec_renewable_energy_germany_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b1)

sls_b2 <- ivreg(spec_funding_solar_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b2)

sls_b3 <- ivreg(spec_funding_wind_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b3)

sls_b4 <- ivreg(spec_investment_state_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b4)

sls_b5 <- ivreg(spec_prohibition_oilgas_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b5)

sls_b6 <- ivreg(spec_area_wind_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b6)

sls_b7 <- ivreg(spec_area_solar_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b7)



# Combine the model outputs
models <- list(sls_b1, sls_b2, sls_b3, sls_b4, sls_b5, sls_b6,sls_b7)
names(models) <- rep(c("ren energy %", "solar funding", "wind funding", 
                       "investment state", "prohibit oilgas", "area wind","area solar"))




m3 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("url_int_visits_yn" = "URL visits", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table3.docx"
                   ,title = 'EEG: Effect of URL tracked visits on specific attitudes, directed (group 3 levels).'
)

m3



##2.3. models absolute change####
sls_b1 <- ivreg(spec_renewable_energy_germany_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b1)

sls_b2 <- ivreg(spec_funding_solar_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b2)

sls_b3 <- ivreg(spec_funding_wind_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b3)

sls_b4 <- ivreg(spec_investment_state_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b4)

sls_b5 <- ivreg(spec_prohibition_oilgas_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b5)

sls_b6 <- ivreg(spec_area_wind_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b6)

sls_b7 <- ivreg(spec_area_solar_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b7)



# Combine the model outputs
models <- list(sls_b1, sls_b2, sls_b3, sls_b4, sls_b5, sls_b6,sls_b7)
names(models) <- rep(c("ren energy %", "solar funding", "wind funding", 
                       "investment state", "prohibit oilgas", "area wind","area solar"))


m4 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("url_int_visits_yn" = "URL visits", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table3.docx"
                   ,title = 'EEG: Effect of URL tracked visits on specific attitudes, abs change (group 3 levels).'
)

m4


##3. Cannabis Legalisierung####

##3.1. extract special attitude variables####


##subset df1 to only include variables with both spec_ and _diff
dfs <- df3[, grepl("spec_", colnames(df3)) & grepl("_diff", colnames(df3))]

##
##only use cols 1:9
dfs <- dfs[, 1:9]
dfs_abs <- abs(dfs)

##put _abs behind all colnames in dfs_abs
colnames(dfs_abs) <- paste0(colnames(dfs_abs), "_abs")

##bind dfs and dfs_abs
dfs <- cbind(dfs, dfs_abs)


dfs$group_f <- df3$group_f
dfs$url_int_visits_yn <- df3$url_int_visits_yn
dfs$age <- df3$age
dfs$gender_f <- df3$gender_f
dfs$edu_time <- df3$edu_time
dfs$search_internet_pre <- df3$search_internet_pre

colnames(dfs)

##3.2. models directed Cannabis####

sls_b1 <- ivreg(spec_recreational_use_legalisation_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b1)

sls_b2 <- ivreg(spec_purchase_restricted_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b2)

sls_b3 <- ivreg(spec_purchase_limit_recrational_use_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b3)

sls_b4 <- ivreg(spec_cultivation_cannabis_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b4)

sls_b5 <- ivreg(spec_cultivation_germany_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b5)

sls_b6 <- ivreg(spec_control_cultivation_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b6)

sls_b7 <- ivreg(spec_staterevenue_legalisation_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b7)

sls_b8 <- ivreg(spec_consumption_private_appartments_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b8)

sls_b9 <- ivreg(spec_protection_children_diff ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b9)

# Combine the model outputs
models <- list(sls_b1, sls_b2, sls_b3, sls_b4, sls_b5, sls_b6,sls_b7,sls_b8,sls_b9)
names(models) <- rep(c("recr use", "restricted purchase", "limit", 
                       "cultivation", "private apartments", "child protect",
                       "import","control","revenue"))




m5 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("url_int_visits_yn" = "URL visits", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "rel_indiv_cannabis_consumed" = "Cannabis consumed",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table3.docx"
                   ,title = 'Cannabislegalisierung: Effect of URL tracked visits on specific attitudes directed (3 levels).'
)

m5



##3.3. models absolute change####
sls_b1 <- ivreg(spec_recreational_use_legalisation_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b1)

sls_b2 <- ivreg(spec_purchase_restricted_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b2)

sls_b3 <- ivreg(spec_purchase_limit_recrational_use_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b3)

sls_b4 <- ivreg(spec_cultivation_cannabis_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b4)

sls_b5 <- ivreg(spec_cultivation_germany_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b5)

sls_b6 <- ivreg(spec_control_cultivation_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b6)

sls_b7 <- ivreg(spec_staterevenue_legalisation_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b7)

sls_b8 <- ivreg(spec_consumption_private_appartments_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b8)

sls_b9 <- ivreg(spec_protection_children_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b9)


# Combine the model outputs
models <- list(sls_b1, sls_b2, sls_b3, sls_b4, sls_b5, sls_b6,sls_b7,sls_b8,sls_b9)
names(models) <- rep(c("recr use", "restricted purchase", "limit", 
                       "cultivation", "private apartments", "child protect",
                       "import","control","revenue"))



m6 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("url_int_visits_yn" = "URL visits", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "rel_indiv_cannabis_consumed" = "Cannabis consumed",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table3.docx"
                   ,title = 'Cannabislegalisierung: Effect of URL tracked visits on specific attitudes, abs change (3 levels).'
)

m6




###4. add to word####
m1 <- fontsize(m1, size=10, part="all")
m1 <- bold(m1, i=1)
m1


set_flextable_defaults(keep_with_next = F)
# Function to apply modifications and export to 
apply_modifications_and_export <- function(table, index, docx) {
  table <- fontsize(table, size = 10, part = "header")
  table <- bold(table, i = 1)
  docx <- body_add_flextable(docx, table, split=FALSE)|> 
    body_add_break()
  return(docx)
}

# Create a Word document
word_export <- read_docx()

# Iterate through each table
for (i in 1:6) {
  # Access the table object
  table_name <- paste0("m", i)
  table <- get(table_name)
  
  # Apply modifications and add to Word document
  word_export <- apply_modifications_and_export(table, i, word_export)
}

# Save the Word document
print(word_export, "IV_reg_tables_spec_attitudes.docx")


#5. ITT models####


##..1.1. IT effect####


lm_b1 <- lm(gen_pol_attitude_topic_diff_abs ~ intervention, data = df1)
summary(lm_b1)

lm_b2 <- lm(gen_pol_attitude_topic_diff_abs ~ intervention, data = df2)
summary(lm_b2)

lm_b3 <- lm(gen_pol_attitude_topic_diff_abs ~ intervention, data = df3)
summary(lm_b3)

lm_b1c <- lm(gen_pol_attitude_topic_diff ~ intervention, data = df1)
summary(lm_b1c)

lm_b2c <- lm(gen_pol_attitude_topic_diff ~ intervention, data = df2)
summary(lm_b2c)

lm_b3c <- lm(gen_pol_attitude_topic_diff ~ intervention, data = df3)
summary(lm_b3c)


# Combine the model outputs
models <- list(lm_b1, lm_b2, lm_b3, lm_b1c, lm_b2c, lm_b3c)
names(models) <- rep(c("Child support", "Energy transition", 
                       "Cannabis legalization", 
                       "Child support", "Energy transition", 
                       "Cannabis legalization"))




m9 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("url_int_visits_yn" = "URL visits",
                                    "search_int_visits_yn" = "URL searches")
                   ,gof_omit = 'Log.Lik.|Deviance|AIC|BIC'
                   ,standardize = "basic"
                   ,title = 'ITT effect of intervention on attitude change (factor), left (undirected), right (directed).'
)

m9



sls_b1 <- ivreg(spec_payment_guarantee_1_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b1)

sls_b2 <- ivreg(spec_payment_income_1_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b2)

sls_b3 <- ivreg(spec_digital_application_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b3)

sls_b4 <- ivreg(spec_automatic_payment_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b4)

sls_b5 <- ivreg(spec_payment_establishment_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b5)

sls_b6 <- ivreg(spec_own_office_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b6)

sls_b7 <- ivreg(spec_treasury_cost_1_diff_abs ~ url_int_visits_yn  + age + gender_f + edu_time + search_internet_pre| group_f  + age + gender_f + edu_time+ search_internet_pre, data = dfs)
summary(sls_b7)



# Combine the model outputs
models <- list(sls_b1, sls_b2, sls_b3, sls_b4, sls_b5, sls_b6,sls_b7)
names(models) <- rep(c("guaranteed KG", "incomebased KG", "app", "automatic pay", 
                       "all_families in D", "special office","state cost"))



m2 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("url_int_visits_yn" = "URL visits", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table3.docx"
                   ,title = 'Kindergrundsicherung: Effect of URL tracked visits on specific attitudes, abs change (group 3 levels).'
)

m2