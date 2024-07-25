###libraries####

library(dplyr)
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

options(scipen = 999)
options("modelsummary_factory_default" = "flextable")


##0.0 load data ####

load("./datasets/df1_final_ac.Rdata")
load("./datasets/df2_final_ac.Rdata")
load("./datasets/df3_final_ac.Rdata")


##0.1. recode variables####


df1$gen_pol_attitude_topic_diff_abs <- abs(df1$gen_pol_attitude_topic_diff)
df2$gen_pol_attitude_topic_diff_abs <- abs(df2$gen_pol_attitude_topic_diff)
df3$gen_pol_attitude_topic_diff_abs <- abs(df3$gen_pol_attitude_topic_diff)


df1$intervention <- df1$group_f
df2$intervention <- df2$group_f
df3$intervention <- df3$group_f

##create wave participation var

df1$prev_wave <- 0
df2$prev_wave <- ifelse(df2$pid %in% df1$pid, 1, 0)
df3$prev_wave <- ifelse(df3$pid %in% df2$pid, 1, ifelse(df3$pid %in% df1$pid, 1, 0))

table(df2$prev_wave)
table(df3$prev_wave)


# binary IVs control vs search
df1 <- df1 %>%
  mutate(z_verbal = ifelse(group_f == "control", 0, 
                           ifelse(group_f == "verbal", 1, NA)))
df2 <- df2 %>%
  mutate(z_verbal = ifelse(group_f == "control", 0, 
                           ifelse(group_f == "verbal", 1, NA)))
df3 <- df3 %>%
  mutate(z_verbal = ifelse(group_f == "control", 0, 
                           ifelse(group_f == "verbal", 1, NA)))
table(df1$group_f)
table(df1$z_verbal)

# binary IVs incentive
df1 <- df1 %>%
  mutate(z_money = ifelse(group_f == "control", 0, 
                           ifelse(group_f == "verbal", NA, 1)))
df2 <- df2 %>%
  mutate(z_money = ifelse(group_f == "control", 0, 
                           ifelse(group_f == "verbal", NA, 1)))
df3 <- df3 %>%
  mutate(z_money = ifelse(group_f == "control", 0, 
                           ifelse(group_f == "verbal", NA, 1)))
table(df1$z_money)



##_______________####

##1. Paper analyses####


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





##_______________####
##2. 3-level IV  instrument####

###............####
####2.1. attitude likert####
####..2.1.1. URL visits####
df1$sear

sls_b1 <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn | group_f, data = df1)
summary(sls_b1)

sls_b2 <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn | group_f, data = df2)
summary(sls_b2)

sls_b3 <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn | group_f, data = df3)
summary(sls_b3)

sls_b1c <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df1)
summary(sls_b1c)

sls_b2c <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df2)
summary(sls_b2c)

sls_b3c <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df3)
summary(sls_b3c)


# Combine the model outputs
models <- list(sls_b1, sls_b2, sls_b3, sls_b1c, sls_b2c, sls_b3c)
names(models) <- rep(c("Child support", "Energy transition", "Cannabis legalization", "Child support", "Energy transition", "Cannabis legalization"))




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
                   ,title = 'Effect of URL tracked visits on attitude change (3 levels).'
)
m1

####..2.1.2. searches####

sls_sr_b1 <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn | group_f, data = df1)
summary(sls_sr_b1)

sls_sr_b2 <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn | group_f, data = df2)
summary(sls_sr_b2)

sls_sr_b3 <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn | group_f, data = df3)
summary(sls_sr_b3)

sls_sr_b1c <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df1)
summary(sls_sr_b1c)

sls_sr_b2c <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df2)
summary(sls_sr_b2c)

sls_sr_b3c <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df3)
summary(sls_sr_b3c)

# Combine the model outputs
models <- list(sls_sr_b1, sls_sr_b2, sls_sr_b3, sls_sr_b1c, sls_sr_b2c, sls_sr_b3c)
names(models) <- rep(c("Child support", "Energy transition", "Cannabis legalization", "Child support", "Energy transition", "Cannabis legalization"))




m2 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("search_int_visits_yn" = "URL searches", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table6.docx"
                   ,title = 'Effect of searches on attitude change (3 levels).'
)





##_______________####
##3. verbal instrument####

##........######
####3.1. attitude####
####..3.1.1. URL visits####

sls_v1 <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn | z_verbal, data = df1)
summary(sls_v1)

sls_v2 <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn | z_verbal, data = df2)
summary(sls_v2)

sls_v3 <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn | z_verbal, data = df3)
summary(sls_v3)

sls_v1c <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave | z_verbal + age + gender_f + edu_time+ prev_wave, data = df1)
summary(sls_v1c)

sls_v2c <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave | z_verbal + age + gender_f + edu_time + prev_wave, data = df2)
summary(sls_v2c)

sls_v3c <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave | z_verbal + age + gender_f + edu_time + prev_wave, data = df3)
summary(sls_v3c)

# Combine the model outputs
models <- list(sls_v1, sls_v2, sls_v3, sls_v1c, sls_v2c, sls_v3c)
names(models) <- rep(c("Child support", "Energy transition", "Cannabis legalization", "Child support", "Energy transition", "Cannabis legalization"))

##to check which statistics are available: gm <- transform(modelsummary::gof_map, omit = FALSE)

###print model

m3 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                  ,metrics = "all"
                  ,stars = TRUE
                  ,statistic = c("conf.int")
                  ,coef_omit = "Intercept"
                  ,coef_rename = c("url_int_visits_yn" = "URL visits", 
                                   "gender_fnot female" = "gender - male",
                                   "edu_time" = "years education",
                                   "prev_wave" = "multiple waves")
                  #,gof_omit = 'DF|Deviance|AIC|BIC'
                  ,gof_map = c("weak.instruments","weak.instruments.p",
                               "wu.hausman", "wu.hausman.p",
                               "rmse",
                               "nobs")
                  ,standardize = "basic"
                  #, output = "./IV_reg_tables/table1.docx"
                  ,title = 'Effect of URL tracked visits on attitude change (verbal encouragement).'
)

####..3.1.2. searches####

sls_sr_b1 <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn | z_verbal, data = df1)
summary(sls_sr_b1)

sls_sr_b2 <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn | z_verbal, data = df2)
summary(sls_sr_b2)

sls_sr_b3 <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn | z_verbal, data = df3)
summary(sls_sr_b3)

sls_sr_b1c <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave | z_verbal + age + gender_f + edu_time + prev_wave, data = df1)
summary(sls_sr_b1c)

sls_sr_b2c <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave | z_verbal + age + gender_f + edu_time + prev_wave, data = df2)
summary(sls_sr_b2c)

sls_sr_b3c <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave | z_verbal + age + gender_f + edu_time + prev_wave, data = df3)
summary(sls_sr_b3c)

# Combine the model outputs
models <- list(sls_sr_b1, sls_sr_b2, sls_sr_b3, sls_sr_b1c, sls_sr_b2c, sls_sr_b3c)
names(models) <- rep(c("Child support", "Energy transition", "Cannabis legalization", "Child support", "Energy transition", "Cannabis legalization"))


m4 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("search_int_visits_yn" = "URL searches", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table6.docx"
                   ,title = 'Effect of searches on attitude change (verbal encouragement).'
)



##_______________####
##4. money instrument####

##........######
####4.1. attitude####

####..4.1.1. URL visits####

sls_m1 <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn | z_money, data = df1)
summary(sls_m1)

sls_m2 <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn | z_money, data = df2)
summary(sls_m2)

sls_m3 <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn | z_money, data = df3)
summary(sls_m3)

sls_m1c <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave | z_money + age + gender_f + edu_time + prev_wave, data = df1)
summary(sls_m1c)

sls_m2c <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave | z_money + age + gender_f + edu_time + prev_wave, data = df2)
summary(sls_m2c)

sls_m3c <- ivreg(gen_pol_attitude_topic_diff_abs ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave | z_money + age + gender_f + edu_time + prev_wave, data = df3)
summary(sls_m3c)

# Combine the model outputs
models <- list(sls_m1, sls_m2, sls_m3, sls_m1c, sls_m2c, sls_m3c)
names(models) <- rep(c("Child support", "Energy transition", "Cannabis legalization", "Child support", "Energy transition", "Cannabis legalization"))



m5 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("url_int_visits_yn" = "URL visits", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table2.docx"
                   ,title = 'Effect of URL tracked visits on attitude change (money encouragement).'
)

####..4.1.2. self-reported####


sls_sr_m1 <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn | z_money, data = df1)
summary(sls_sr_m1)

sls_sr_m2 <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn | z_money, data = df2)
summary(sls_sr_m2)

sls_sr_m3 <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn | z_money, data = df3)
summary(sls_sr_m3)

sls_sr_m1c <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave | z_money + age + gender_f + edu_time + prev_wave, data = df1)
summary(sls_sr_m1c)

sls_sr_m2c <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave | z_money + age + gender_f + edu_time + prev_wave, data = df2)
summary(sls_sr_m2c)

sls_sr_m3c <- ivreg(gen_pol_attitude_topic_diff_abs ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave | z_money + age + gender_f + edu_time + prev_wave, data = df3)
summary(sls_sr_m3c)

# Combine the model outputs
models <- list(sls_sr_m1, sls_sr_m2, sls_sr_m3, sls_sr_m1c, sls_sr_m2c, sls_sr_m3c)
names(models) <- rep(c("Child support", "Energy transition", "Cannabis legalization", "Child support", "Energy transition", "Cannabis legalization"))



m6 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("search_int_visits_yn" = "URL searches",  
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table5.docx"
                   ,title = 'Effect of searches on attitude change (money encouragement).'
)







###............####
####5. directed attitude likert####
###........####
####..5.1. URL visits####

slsd_b1 <- ivreg(gen_pol_attitude_topic_diff ~ url_int_visits_yn | group_f, data = df1)
summary(slsd_b1)

slsd_b2 <- ivreg(gen_pol_attitude_topic_diff ~ url_int_visits_yn | group_f, data = df2)
summary(slsd_b2)

slsd_b3 <- ivreg(gen_pol_attitude_topic_diff ~ url_int_visits_yn | group_f, data = df3)
summary(slsd_b3)

slsd_b1c <- ivreg(gen_pol_attitude_topic_diff ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df1)
summary(slsd_b1c)

slsd_b2c <- ivreg(gen_pol_attitude_topic_diff ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df2)
summary(slsd_b2c)

slsd_b3c <- ivreg(gen_pol_attitude_topic_diff ~ url_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df3)
summary(slsd_b3c)


# Combine the model outputs
models <- list(slsd_b1, slsd_b2, slsd_b3, slsd_b1c, slsd_b2c, slsd_b3c)
names(models) <- rep(c("Child support", "Energy transition", "Cannabis legalization", "Child support", "Energy transition", "Cannabis legalization"))




m7 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
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
                   ,title = 'Effect of URL tracked visits on attitude (directed) (3 levels).'
)


####..5.2. searches####

slsd_sr_b1 <- ivreg(gen_pol_attitude_topic_diff ~ search_int_visits_yn | group_f, data = df1)
summary(slsd_sr_b1)

slsd_sr_b2 <- ivreg(gen_pol_attitude_topic_diff ~ search_int_visits_yn | group_f, data = df2)
summary(slsd_sr_b2)

slsd_sr_b3 <- ivreg(gen_pol_attitude_topic_diff ~ search_int_visits_yn | group_f, data = df3)
summary(slsd_sr_b3)

slsd_sr_b1c <- ivreg(gen_pol_attitude_topic_diff ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df1)
summary(slsd_sr_b1c)

slsd_sr_b2c <- ivreg(gen_pol_attitude_topic_diff ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df2)
summary(slsd_sr_b2c)

slsd_sr_b3c <- ivreg(gen_pol_attitude_topic_diff ~ search_int_visits_yn + age + gender_f + edu_time + prev_wave| group_f + age + gender_f + edu_time + prev_wave, data = df3)
summary(slsd_sr_b3c)

# Combine the model outputs
models <- list(slsd_sr_b1, slsd_sr_b2, slsd_sr_b3, slsd_sr_b1c, slsd_sr_b2c, slsd_sr_b3c)
names(models) <- rep(c("Child support", "Energy transition", "Cannabis legalization", "Child support", "Energy transition", "Cannabis legalization"))




m8 <- modelsummary(models,estimate = c("{estimate}{stars} p={p.value}")
                   ,metrics = "all"
                   ,stars = TRUE
                   ,statistic = c("conf.int")
                   ,coef_omit = "Intercept"
                   ,coef_rename = c("search_int_visits_yn" = "URL searches", 
                                    "gender_fnot female" = "gender - male",
                                    "edu_time" = "years of education",
                                    "prev_wave" = "multiple waves")
                   #,gof_omit = 'DF|Deviance|AIC|BIC'
                   ,gof_map = c("weak.instruments","weak.instruments.p",
                                "wu.hausman", "wu.hausman.p",
                                "rmse",
                                "nobs")
                   ,standardize = "basic"
                   #, output = "./IV_reg_tables/table6.docx"
                   ,title = 'Effect of searches on attitude (directed) (3 levels).'
)



##_______________####

###6. add to word####
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
for (i in 1:9) {
  # Access the table object
  table_name <- paste0("m", i)
  table <- get(table_name)
  
  # Apply modifications and add to Word document
  word_export <- apply_modifications_and_export(table, i, word_export)
}

# Save the Word document
print(word_export, "IV_reg_gen_tables.docx")
