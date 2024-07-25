###libraries####

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
library(expss)
library(compareGroups)


options(scipen = 999)
options("modelsummary_factory_default" = "flextable")

##0. load data ####

load("./datasets/df1_final_ac.Rdata")
load("./datasets/df2_final_ac.Rdata")
load("./datasets/df3_final_ac.Rdata")


##1. recode variables####

df1$wave <- "df1"
df2$wave <- "df2"
df3$wave <- "df3"

df1$group_f <- as.factor(df1$group)
df2$group_f <- as.factor(df2$group)
df3$group_f <- as.factor(df3$group)


df1$gen_pol_attitude_topic_diff_abs <- abs(df1$gen_pol_attitude_topic_diff)
df2$gen_pol_attitude_topic_diff_abs <- abs(df2$gen_pol_attitude_topic_diff)
df3$gen_pol_attitude_topic_diff_abs <- abs(df3$gen_pol_attitude_topic_diff)

table(df1$group_f)
table(df2$group_f)
table(df3$group_f)


##replace 0 with NA 
df1$url_int_visits_NA <- ifelse(df1$url_int_visits == 0, NA, df1$url_int_visits)
df2$url_int_visits_NA <- ifelse(df2$url_int_visits == 0, NA, df2$url_int_visits)
df3$url_int_visits_NA <- ifelse(df3$url_int_visits == 0, NA, df3$url_int_visits)
df1$search_int_visits_NA <- ifelse(df1$search_int_visits == 0, NA, df1$search_int_visits)
df2$search_int_visits_NA <- ifelse(df2$search_int_visits == 0, NA, df2$search_int_visits)
df3$search_int_visits_NA <- ifelse(df3$search_int_visits == 0, NA, df3$search_int_visits)
df1$url_int_time_NA <- ifelse(df1$url_int_time == 0, NA, df1$url_int_time)
df2$url_int_time_NA <- ifelse(df2$url_int_time == 0, NA, df2$url_int_time)
df3$url_int_time_NA <- ifelse(df3$url_int_time == 0, NA, df3$url_int_time)
df1$search_int_time_NA <- ifelse(df1$search_int_time == 0, NA, df1$search_int_time)
df2$search_int_time_NA <- ifelse(df2$search_int_time == 0, NA, df2$search_int_time)
df3$search_int_time_NA <- ifelse(df3$search_int_time == 0, NA, df3$search_int_time)

##create wave participation var

df1$prev_wave <- 0
df2$prev_wave <- ifelse(df2$pid %in% df1$pid, 1, 0)
df3$prev_wave <- ifelse(df3$pid %in% df2$pid, 1, ifelse(df3$pid %in% df1$pid, 1, 0))

table(df2$prev_wave)
table(df3$prev_wave)


# binary IVs control vs search
df1 <- df1 %>%
  mutate(z_verbal = ifelse(group_f == "1control", 0, 
                           ifelse(group_f == "2search", 1, NA)))
df2 <- df2 %>%
  mutate(z_verbal = ifelse(group_f == "1control", 0, 
                           ifelse(group_f == "2search", 1, NA)))
df3 <- df3 %>%
  mutate(z_verbal = ifelse(group_f == "1control", 0, 
                           ifelse(group_f == "2search", 1, NA)))

# binary IVs incentive
df1 <- df1 %>%
  mutate(z_money = ifelse(group_f == "1control", 0, 
                          ifelse(group_f == "2search", NA, 1)))
df2 <- df2 %>%
  mutate(z_money = ifelse(group_f == "1control", 0, 
                          ifelse(group_f == "2search", NA, 1)))
df3 <- df3 %>%
  mutate(z_money = ifelse(group_f == "1control", 0, 
                          ifelse(group_f == "2search", NA, 1)))


#1. main survey items

###get only age, gender education and pid
dfm1 <- subset(df1, select=c(
                             "url_int_visits_yn", "url_int_visits","url_int_visits_NA",
                             "search_int_visits_yn","search_int_visits","search_int_visits_NA",
                             "url_int_time_NA","search_int_time_NA",
                             "gen_pol_attitude_topic_pre", "gen_pol_attitude_topic_post", 
                             "gen_pol_attitude_topic_diff_abs", "gen_pol_attitude_topic_diff", 
                             # "spec_payment_guarantee_1_diff",
                             # "spec_payment_income_1_diff",
                             # "spec_digital_application_diff",
                             # "spec_automatic_payment_diff",
                             # "spec_payment_establishment_diff",
                             # "spec_own_office_diff",
                             # "spec_treasury_cost_1_diff",
                             "group","wave"))

dfm2 <- subset(df2, select=c("url_int_visits_yn", "url_int_visits","url_int_visits_NA",
                             "search_int_visits_yn","search_int_visits","search_int_visits_NA",
                             "url_int_time_NA","search_int_time_NA",
                             "gen_pol_attitude_topic_pre", "gen_pol_attitude_topic_post", 
                             "gen_pol_attitude_topic_diff_abs", "gen_pol_attitude_topic_diff", 
                             # "spec_renewable_energy_germany_diff",
                             # "spec_funding_solar_diff",
                             # "spec_funding_wind_diff",
                             # "spec_investment_state_diff",
                             # "spec_prohibition_oilgas_diff",
                             # "spec_area_wind_diff",
                             # "spec_area_solar_diff",
                             "group","wave"))
  
dfm3 <- subset(df3, select=c("url_int_visits_yn", "url_int_visits","url_int_visits_NA",
                             "search_int_visits_yn","search_int_visits","search_int_visits_NA",
                             "url_int_time_NA","search_int_time_NA",
                             "gen_pol_attitude_topic_pre", "gen_pol_attitude_topic_post", 
                             "gen_pol_attitude_topic_diff_abs", "gen_pol_attitude_topic_diff", 
                             # "spec_recreational_use_legalisation_diff",
                             # "spec_purchase_restricted_diff",
                             # "spec_purchase_limit_recrational_use_diff",
                             # "spec_cultivation_cannabis_diff",
                             # "spec_consumption_private_appartments_diff",
                             # "spec_protection_children_diff",
                             # "spec_cultivation_germany_diff",
                             # "spec_control_cultivation_diff",
                             # "spec_staterevenue_legalisation_diff",
                             "group","wave"))

##merge three dfms
dfm <- rbind(dfm1, dfm2, dfm3)

##change values in dfm$group
dfm$group <- ifelse(dfm$group == "1control", "control", 
                    ifelse(dfm$group == "2search", "verbal","monetary" ))
##keep order of group
dfm$group <- factor(dfm$group, levels = c("control", "verbal", "monetary"))

##redo some variables
dfm$url_int_visits_yn <- ifelse(dfm$url_int_visits_yn == 1, "yes", "no")
dfm$search_int_visits_yn <- ifelse(dfm$search_int_visits_yn == 1, "yes", "no")

dfm = apply_labels(dfm,
                  url_int_visits_yn = "Participants with policy-related URL visits",
                  search_int_visits_yn = "Participants with policy-related searches",
                  search_int_visits_NA = "Avg policy-related searches n(searches)>0",
                  url_int_visits_NA = "Avg policy-related URL visits n(visits)>0",
                  url_int_time_NA = "AVg time spent on policy-related URL visits (sec)",
                  search_int_time_NA = "Avt time spent on policy-related searches (sec)"
                  )


#2. descriptives for all waves together####

variables <- names(dfm)[!names(dfm) %in% c("group", "wave")]
formula <- as.formula(paste("group ~", paste(variables, collapse = " + ")))
# Use compareGroups to generate the summary
res <- compareGroups(formula, data = dfm)
# Get the summary table
sum_table <- createTable(res, show.all = TRUE, digits = 2, type=2, hide.no = "no")


KG <- createTable(update(res,subset=wave=='df1'), type=1, , hide.no = "no",show.p.overall = FALSE)
EEG <- createTable(update(res,subset=wave=='df2'), type=1, hide.no = "no" ,show.p.overall = FALSE)
CL <- createTable(update(res,subset=wave=='df3'), type=1, hide.no = "no" ,show.p.overall = FALSE)
sum_table <- cbind("Child support"=KG,"Energy transition"=EEG,"Cannabis legalisation"=CL)


sum_table

export2word(sum_table, file='allwaves_DV.docx')


#4. each wave individually####

variables <- names(dfm1)[!names(dfm1) %in% c("group", "wave")]
formula <- as.formula(paste("group ~", paste(variables, collapse = " + ")))
# Use compareGroups to generate the summary
res <- compareGroups(formula, data = dfm1)
# Get the summary table
st1 <- createTable(res, show.all = TRUE, digits = 2, type=1)
st1

variables <- names(dfm2)[!names(dfm2) %in% c("group", "wave")]
formula <- as.formula(paste("group ~", paste(variables, collapse = " + ")))
# Use compareGroups to generate the summary
res <- compareGroups(formula, data = dfm2)
# Get the summary table
st2 <- createTable(res, show.all = TRUE, digits = 2, type=1)
st2

variables <- names(dfm3)[!names(dfm3) %in% c("group", "wave")]
formula <- as.formula(paste("group ~", paste(variables, collapse = " + ")))
# Use compareGroups to generate the summary
res <- compareGroups(formula, data = dfm3)
# Get the summary table
st3 <- createTable(res, show.all = TRUE, digits = 2, type=1)
st3

#4. individual with the special variables
export2word(st1, file='st1_DV.docx')
export2word(st2, file='st2_DV.docx')
export2word(st3, file='st3_DV.docx')





