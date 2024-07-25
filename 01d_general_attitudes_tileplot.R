library(patchwork)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(forcats)



##0. load data ####

load("./datasets/df1_final_ac.Rdata")
load("./datasets/df2_final_ac.Rdata")
load("./datasets/df3_final_ac.Rdata")



#1. recode variables####

df1$group_f <- as.factor(df1$group)
df2$group_f <- as.factor(df2$group)
df3$group_f <- as.factor(df3$group)


df1$gen_pol_attitude_topic_diff_abs <- abs(df1$gen_pol_attitude_topic_diff)
df2$gen_pol_attitude_topic_diff_abs <- abs(df2$gen_pol_attitude_topic_diff)
df3$gen_pol_attitude_topic_diff_abs <- abs(df3$gen_pol_attitude_topic_diff)

table(df1$group_f)

df1 <- df1 %>%
  mutate(z = ifelse(group_f == "1control", 0, 1))
df1$z

##subset url_int_visits_yn == 1 for 2search and 3incentive only


##4. heat map####

##crate a new df with gen_pol_attitude_topic_pre, gen_pol_attitude_topic_post and group_f from df1 df2 adn df3
df1m <- df1 %>%
  select(gen_pol_attitude_topic_pre, gen_pol_attitude_topic_post, url_int_visits_yn,group_f)
df2m <- df2 %>%
  select(gen_pol_attitude_topic_pre, gen_pol_attitude_topic_post, url_int_visits_yn,group_f)
df3m <- df3 %>%
  select(gen_pol_attitude_topic_pre, gen_pol_attitude_topic_post, url_int_visits_yn,group_f)

df1m <- df1m %>%
  select(gen_pol_attitude_topic_pre, gen_pol_attitude_topic_post, url_int_visits_yn, group_f) %>%
  filter((group_f == "2search" & url_int_visits_yn == 1) |
           (group_f == "3incentive" & url_int_visits_yn == 1) |
           (group_f == "1control" & url_int_visits_yn == 0))

df2m <- df2m %>%
  select(gen_pol_attitude_topic_pre, gen_pol_attitude_topic_post, url_int_visits_yn, group_f) %>%
  filter((group_f == "2search" & url_int_visits_yn == 1) |
           (group_f == "3incentive" & url_int_visits_yn == 1) |
           (group_f == "1control" & url_int_visits_yn == 0))

df3m <- df3m %>%
  select(gen_pol_attitude_topic_pre, gen_pol_attitude_topic_post, url_int_visits_yn, group_f) %>%
  filter((group_f == "2search" & url_int_visits_yn == 1) |
           (group_f == "3incentive" & url_int_visits_yn == 1) |
           (group_f == "1control" & url_int_visits_yn == 0))

# Combine the dataframes
df1m$experiment <- "Child support"
df2m$experiment <- "Energy transition"
df3m$experiment <- "Cannabis legalisation"

#merged_df <- rbind(df1m, df2m, df3m)

merged_df <- rbind(df1m, df3m)

merged_df$experiment <- as.factor(merged_df$experiment)
##keep order of variables
#merged_df$experiment <- factor(merged_df$experiment, levels = c("Child support", "Energy transition", "Cannabis legalisation"))
merged_df$experiment <- factor(merged_df$experiment, levels = c("Child support", "Cannabis legalisation"))

##NEW####

###for participants without online visits (untreated); with online visits (treated)

merged_df$url_int_visits_yn <- factor(merged_df$url_int_visits_yn, labels = c("Untreated in control", "Treated in treatment groups"))


full_df <- merged_df %>%
  mutate(
    gen_pol_attitude_topic_pre = as.integer(as_factor(gen_pol_attitude_topic_pre)),
    gen_pol_attitude_topic_post = as.integer(as_factor(gen_pol_attitude_topic_post))
  )


# Prepare the data for proportion calculation
data_summary <- full_df %>%
  group_by(url_int_visits_yn, experiment, gen_pol_attitude_topic_pre, gen_pol_attitude_topic_post) %>%
  summarise(count = n()) %>%
  group_by(experiment, url_int_visits_yn) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Add a column to differentiate upper and lower triangles and to identify the center line
data_summary <- data_summary %>%
  mutate(
    grp = ifelse(gen_pol_attitude_topic_pre < gen_pol_attitude_topic_post, "changed to positive", 
                 ifelse(gen_pol_attitude_topic_pre > gen_pol_attitude_topic_post, "changed to negative", "no change")),
    alpha = ifelse(gen_pol_attitude_topic_pre == gen_pol_attitude_topic_post, .09, proportion)
  )



##plot


ggplot(data_summary, aes(x = gen_pol_attitude_topic_pre, y = gen_pol_attitude_topic_post, 
                         fill = grp, alpha = alpha)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("changed to negative" = "#DC5656", "changed to positive" = "#4688DE", "no change" = "lightgrey")) +
  scale_alpha_continuous(range = c(0.3, 1.3)) +
  scale_x_continuous(breaks = unique(data_summary$gen_pol_attitude_topic_pre)) +
  scale_y_continuous(breaks = unique(data_summary$gen_pol_attitude_topic_post)) +
  geom_text(aes(label = sub("^0", "", sprintf("%.2f", proportion))), color = "black", size = 3) +
  facet_wrap(experiment ~ fct_inorder(url_int_visits_yn)) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",    # Move legend to the bottom
    legend.direction = "horizontal" # Arrange legend items horizontally
  ) +
  labs(
    title = "Direction of attitude changes from before to after intervention",
    x = "Attitude before intervention",
    y = "Attitude after intervention"
  )









