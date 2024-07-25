

#0. libraries####

library(ggplot2)
library(forcats)
library(car)
library(dplyr)
library(lattice)
library(reshape2)
library(psych)
library(mosaic)
library(purrr)
library(rlang)
library(tidyr)
library(writexl)
library(compareGroups)
library(expss)


#1. load data####


load("./datasets/df1_final_ac.Rdata")
load("./datasets/df2_final_ac.Rdata")
load("./datasets/df3_final_ac.Rdata")

#2. merge and select vars####

##2.1. select and recode vars####

df1$wave <- "df1"
df2$wave <- "df2"
df3$wave <- "df3"

table(df1$income_f)

##reduce income to 2000 and under, 2000-5000 and > 5000
df1$income <- ifelse(df1$income_f < 2000, "<2000", ifelse(df1$income_f >= 2000 & df1$income_f < 5000, "2000-5000", ">5000"))
df2$income <- ifelse(df2$income_f < 2000, "<2000", ifelse(df2$income_f >= 2000 & df2$income_f < 5000, "2000-5000", ">5000"))
df3$income <- ifelse(df3$income_f < 2000, "<2000", ifelse(df3$income_f >= 2000 & df3$income_f < 5000, "2000-5000", ">5000"))

##sort in order
df1$income <- factor(df1$income, levels = c("<2000", "2000-5000", ">5000"))
df2$income <- factor(df2$income, levels = c("<2000", "2000-5000", ">5000"))
df3$income <- factor(df3$income, levels = c("<2000", "2000-5000", ">5000"))

##apply label to df1 income
df1 = apply_labels(df1,
                   income = "Income")
df2 = apply_labels(df2,
                   income = "Income")
df3 = apply_labels(df3,
                   income = "Income")

##make election_voted yes or no
df1$election_voted <- ifelse(df1$election_voted == 1, "yes", "no")
df2$election_voted <- ifelse(df2$election_voted == 1, "yes", "no")
df3$election_voted <- ifelse(df3$election_voted == 1, "yes", "no")



##2.2. merge all three dfs####

###get only age, gender education and pid
dfm1 <- subset(df1, select=c("age", "gender_f", "edu_time", "edu_s","income",
                             "pol_attitude","pol_interest_general", "polknow_total","election_voted",
                             "internet_use_skill","newsuse_online","activity_visits",
                             "pid", "group","wave"))
dfm2 <- subset(df2, select=c("age", "gender_f", "edu_time", "edu_s","income",
                             "pol_attitude","pol_interest_general", "polknow_total","election_voted",
                             "internet_use_skill","newsuse_online","activity_visits",
                             "pid", "group","wave"))
dfm3 <- subset(df3, select=c("age", "gender_f", "edu_time", "edu_s","income",
                             "pol_attitude","pol_interest_general", "polknow_total","election_voted",
                             "internet_use_skill","newsuse_online","activity_visits",
                             "pid", "group","wave"))



#merge
df <- rbind(dfm1, dfm2, dfm3)
#remove duplicate pids
#df <- df[!duplicated(df$pid),]

rm(dfm1, dfm2, dfm3)

##labels

df$group <- ifelse(df$group == "1control", "control", 
                   ifelse(df$group == "2search", "verbal",
                          "monetary"))
df$edu_s <- ifelse(df$edu_s == "01_lower", "elementary", 
                   ifelse(df$edu_s == "02_middle", "middle school",
                          ifelse(df$edu_s == "03_high", "high school", "university")))

df$gender_f <- ifelse(df$gender_f == "not female", "men", "women")

df = apply_labels(df,
                  edu_s = "Education level",
                  gender_f = "Gender",
                  election_voted = "Voted in last election")

###total of 751 individuals if remove pid duplicates
dfr <- df[!duplicated(df$pid),]


###.............####
#3. all waves####


##extract all variables from df, excluding group, wave and pid and edu_f
variables <- names(dfr)[!names(dfr) %in% c("group", "wave","pid", "edu_f")]
variables

formula <- as.formula(paste("group ~", paste(variables, collapse = " + ")))

# Use compareGroups to generate the summary
res <- compareGroups(formula, data = dfr)

# Get the summary table
st <- createTable(res, show.all = TRUE, digits = 1, type=1,
                             hide = c(gender_f="men", election_voted = "yes"))

st_numbers <- createTable(res, show.all = TRUE, digits = 1, type=2,
                  #hide = c(gender_f="not female", election_voted = "yes")
                  )

print(st_numbers)
# Print the summary table
print(st)

export2word(st, file='descriptives_demos.docx')


###.............####

#4. each wave individually####

variables <- names(dfr)[!names(dfr) %in% c("group", "wave","pid", "edu_f")]


formula <- as.formula(paste("group ~", paste(variables, collapse = " + ")))

# Use compareGroups to generate the summary
res <- compareGroups(formula, data = df)

# Get the summary table
st <- createTable(res, show.all = TRUE, digits = 1, type=1,
                  hide = c(gender_f="men", election_voted = "yes"))


#4. individual with the special variables

st1 <- createTable(update(res,subset=wave=='df1'), digits = 1, type=1)
st2 <- createTable(update(res,subset=wave=='df2'), digits = 1, type=1)
st3 <- createTable(update(res,subset=wave=='df3'), digits = 1, type=1)

st1
st2
st3

export2word(st1, file='st1.docx')
export2word(st2, file='st2.docx')
export2word(st3, file='st3.docx')



#5. age gender graph####

###pyramid comparing it with the population of Germany

###grouping age and gender and creating the counts

##count gender
df_count <- dfr %>%
  group_by(gender_f) %>%
  summarise(count = n())

#age 

favstats(~age, data = dfr)

dfr$age_cat <- cut(dfr$age, breaks = c(0, 25, 40, 60, 100), labels = c("0-25", "26-40", "41-60", "61-100"))

###calculate the distribution of age, gender and group in percentages BY GENDER (could also be group)
df_count <- dfr %>%
  group_by(edu_s, gender_f, age_cat) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(df %>% group_by(gender_f) %>% summarise(total = n()), by = "gender_f") %>%
  mutate(percent = (count / total) * 100)


##change color to pastel

ggplot(df_count, aes(x = age_cat, y = count, fill = edu_s)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~gender_f, ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Age Category", y = "Count", fill = "Group")+
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 1) +
  scale_fill_manual(values = c("elementary" = "#f0f0f0", "middle school" = "lightgrey", 
                               "high school" = "grey", "university" = "darkgrey"))

##reorder edu_s to be elementary, middle, high and uni

df_count$edu_s <- factor(df_count$edu_s, levels = c("elementary", "middle school", "high school", "university"))

ggplot(df_count, aes(x = age_cat, y = count, fill = edu_s)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~gender_f, ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Age Category", y = "Count", fill = "Group")+
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = 1) +
  scale_fill_manual(values = c("elementary" = "#f0f0f0", "middle school" = "lightgrey", 
                               "high school" = "grey", "university" = "darkgrey"))


