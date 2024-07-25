
options(scipen = 999)
library(ggplot2)



##0. load data ####

load("./datasets/df1_final_ac.Rdata")
load("./datasets/df2_final_ac.Rdata")
load("./datasets/df3_final_ac.Rdata")



df$group <- factor(df$group, labels = c("Control", "Verbal", "Monetary"))
df$wave <- factor(df$wave, labels = c("Wave 1: Child support", "Wave 2: Energy transition", "Wave 3: Cannabis legalisation"))


##colors: ['#ff7f0e', '#1f77b4', '#2ca02c']

###1. plots####

##1.1. ggplot of attitude change####


ggplot(df, aes(x = group, y = gen_pol_attitude_topic_change_diff, color = wave, shape = as.factor(group))) +
  geom_point(stat = "summary", fun = "mean", position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = "", y = "Mean attitude change", title = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c('#ff7f0e', '#1f77b4', '#2ca02c')) +
  facet_wrap(~ wave, nrow = 1, scales = "free_x") +
  theme(panel.spacing = unit(1, "lines")) + # Adjusts the spacing between panels
  scale_shape_manual(values = c(16, 17, 18, 19))+
  theme(
    panel.spacing = unit(1, "lines"), # Increase spacing between panels
    strip.text = element_text(size = 12, face = "bold"), # Customize facet labels
    panel.border = element_rect(color = "black", fill = NA), # Add panel borders
    strip.background = element_rect(fill = "grey90", color = "black") # Customize strip background
  )


