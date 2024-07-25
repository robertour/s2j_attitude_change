###libraries####

library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ivreg)
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


# spec_payment_guarantee_1: Ein fester Betrag Kindergrundsicherung pro Monat pro Kind sollte jeder Familie gewährt werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
#   spec_payment_income_1: Zusätzlich zu einem fixen Betrag Kindergrundsicherung für alle Empfänger, sollte eine Zuzahlung je nach Einkommen der Eltern gewährt werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
#   spec_digital_application: Die Beantragung der Kindergrundsicherung sollte über eine digitale Online-Platform abgewickelt werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
#   spec_automatic_payment: Der feste Kindergrundsicherungsbetrag sollte automatisch und ohne Antrag nach Geburt oder beim Zuzug nach Deutschland an Familien ausgezahlt werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
#   spec_payment_establishment: Kindergrundsicherung sollte allen Familien in Deutschland ausgezahlt werden, unabhängig von Ihrem Niederlassungsstatus. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
#   spec_own_office: Es sollte ein eigenes Amt für die Abwicklung der Kindergrundsicherung geschaffen werden, z.B. eine Kindergrundsicherungsstelle. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
#   spec_treasury_cost_1: Zusätzliche Kosten für die Staatskasse von mehreren Milliarden Euro sind für die Kindergrundsicherung akzeptabel. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
#   

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




##2. Erneuerbare Energie####

##2.1. extract special attitude variables####

# q35r1: Deutschlands Energie sollte hauptsächlich aus erneuerbaren Energien produziert werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# q35r2: Photovoltaik (Solarstrom) Anlagen müssen jetzt sehr stark gefördert werden, damit viele neue Anlagen gebaut werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# q35r3: Windenergie Anlagen müssen jetzt sehr stark gefördert werden, damit viele neue Anlagen gebaut werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# q35r4: Der Staatshaushalt sollte private Wind- und Solar-Anlagen durch Subventionen und Steuerentlastungen mitfinanzieren. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# q35r5: Das Einbauen neuer Öl- und Gas-Heizungen sollte ab 2024 verboten werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# q35r6: Anteile der deutschen Landesfläche sollten für Windenergie zur Verfügung stehen. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# q35r7: Anteile der deutschen Landesfläche sollten für Solarparks zur Verfügung stehen. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:


##subset df1 to only include variables with both spec_ and _diff
dfee <- df2[, grepl("spec_", colnames(df2)) & grepl("_diff", colnames(df2))]

##only use cols 1:7
dfee <- dfee[, 1:7]
dfee_abs <- abs(dfee)

##put _abs behind all colnames in dfee_abs
colnames(dfee_abs) <- paste0(colnames(dfee_abs), "_abs")

##bind dfee and dfee_abs
dfee <- cbind(dfee, dfee_abs)


dfee$group_f <- df2$group_f
dfee$url_int_visits_yn <- df2$url_int_visits_yn
dfee$age <- df2$age
dfee$gender_f <- df2$gender_f
dfee$edu_time <- df2$edu_time
dfee$search_internet_pre <- df2$search_internet_pre

colnames(dfee)



##3. Cannabis Legalisierung####

##3.1. extract special attitude variables####

# spec_recreational_use_legalisation: Freizeitkonsum von Cannabis sollte in Form von Vereinigungen legalisiert werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# spec_purchase_restricted: Der Kauf von Cannabis sollte auf lizenzierte Fachhändler oder Apotheken beschränkt sein. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# spec_purchase_limit_recrational_use: Ein Kauflimit für Cannabis zum Freizeitkonsum sollte gesetzlich vorgeschrieben sein. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# spec_cultivation_cannabis: Individuen sollte es erlaubt sein, eine kleine Anzahl an Cannabis Pflanzen selbst anzubauen. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# spec_consumption_private_appartments: Cannabis-Konsum sollte auf Privaträume beschränkt sein und nicht in der Öffentlichkeit stattfinden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# spec_protection_children: Orte, an denen sich Kinder und Jugendliche aufhalten, sollten besonders vor Cannabis-Konsum geschützt werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# spec_cultivation_germany: Cannabis, der in Deutschland genutzt wird, sollte in Deutschland angebaut werden. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
# spec_staterevenue_legalisation: Es ist wichtig, dass der Staat Einnahmen durch die Cannabis-Legalisierung hat, z.B. durch Einsparungen bei Polizei und Justiz oder die Cannabis-Steuer. - Bitte geben Sie Ihre persönliche Einstellung zu folgenden Aussagen an:
#   

##subset df1 to only include variables with both spec_ and _diff
dfc <- df3[, grepl("spec_", colnames(df3)) & grepl("_diff", colnames(df3))]

##
##only use cols 1:9
dfc <- dfc[, 1:9]
dfc_abs <- abs(dfc)

##put _abs behind all colnames in dfc_abs
colnames(dfc_abs) <- paste0(colnames(dfc_abs), "_abs")

##bind dfc and dfc_abs
dfc <- cbind(dfc, dfc_abs)


dfc$group_f <- df3$group_f
dfc$url_int_visits_yn <- df3$url_int_visits_yn
dfc$age <- df3$age
dfc$gender_f <- df3$gender_f
dfc$edu_time <- df3$edu_time
dfc$search_internet_pre <- df3$search_internet_pre

###4. graphs####
# Load necessary libraries

# Function to extract coefficients and standard errors from models
extract_coef_se <- function(models) {
  lapply(models, function(model) {
    coef_summary <- summary(model)$coefficients
    coef <- coef_summary["url_int_visits_yn", "Estimate"]
    se <- coef_summary["url_int_visits_yn", "Std. Error"]
    return(c(coef, se))
  })
}

# Function to combine results into a single data frame
combine_results <- function(coef_list, model_names, type, category) {
  data.frame(
    Model = model_names,
    Estimate = sapply(coef_list, `[`, 1),
    StdError = sapply(coef_list, `[`, 2),
    Type = type,
    Category = category
  )
}


#### Define and run models for original differences####
models_original <- list(
  sls_b1 = ivreg(spec_payment_guarantee_1_diff ~ url_int_visits_yn | group_f, data = dfs),
  sls_b2 = ivreg(spec_payment_income_1_diff ~ url_int_visits_yn | group_f, data = dfs),
  sls_b3 = ivreg(spec_digital_application_diff ~ url_int_visits_yn | group_f, data = dfs),
  sls_b4 = ivreg(spec_automatic_payment_diff ~ url_int_visits_yn | group_f, data = dfs),
  sls_b5 = ivreg(spec_payment_establishment_diff ~ url_int_visits_yn | group_f, data = dfs),
  sls_b6 = ivreg(spec_own_office_diff ~ url_int_visits_yn | group_f, data = dfs),
  sls_b7 = ivreg(spec_treasury_cost_1_diff ~ url_int_visits_yn | group_f, data = dfs)
)
coef_list_original <- extract_coef_se(models_original)
model_names_original <- c("guaranteed KG", "incomebased KG", 
                          "app", "automatic pay", 
                          "all_families in D", "special office", "state cost")
results_df_original <- combine_results(coef_list_original, model_names_original, "directed change", "child support policy issues")

# Define and run models for absolute differences
models_abs <- list(
  sls_b1 = ivreg(spec_payment_guarantee_1_diff_abs ~ url_int_visits_yn | group_f, data = dfs),
  sls_b2 = ivreg(spec_payment_income_1_diff_abs ~ url_int_visits_yn | group_f, data = dfs),
  sls_b3 = ivreg(spec_digital_application_diff_abs ~ url_int_visits_yn | group_f, data = dfs),
  sls_b4 = ivreg(spec_automatic_payment_diff_abs ~ url_int_visits_yn | group_f, data = dfs),
  sls_b5 = ivreg(spec_payment_establishment_diff_abs ~ url_int_visits_yn | group_f, data = dfs),
  sls_b6 = ivreg(spec_own_office_diff_abs ~ url_int_visits_yn | group_f, data = dfs),
  sls_b7 = ivreg(spec_treasury_cost_1_diff_abs ~ url_int_visits_yn | group_f, data = dfs)
)
coef_list_abs <- extract_coef_se(models_abs)
results_df_abs <- combine_results(coef_list_abs, model_names_original, "absolute change", "child support policy issues")

# Define and run models for energy differences
models_energy <- list(
  sls_b1 = ivreg(spec_renewable_energy_germany_diff ~ url_int_visits_yn | group_f, data = dfee),
  sls_b2 = ivreg(spec_funding_solar_diff ~ url_int_visits_yn | group_f, data = dfee),
  sls_b3 = ivreg(spec_funding_wind_diff ~ url_int_visits_yn | group_f, data = dfee),
  sls_b4 = ivreg(spec_investment_state_diff ~ url_int_visits_yn | group_f, data = dfee),
  sls_b5 = ivreg(spec_prohibition_oilgas_diff ~ url_int_visits_yn | group_f, data = dfee),
  sls_b6 = ivreg(spec_area_wind_diff ~ url_int_visits_yn | group_f, data = dfee),
  sls_b7 = ivreg(spec_area_solar_diff ~ url_int_visits_yn | group_f, data = dfee)
)
coef_list_energy <- extract_coef_se(models_energy)
model_names_energy <- c("ren energy %", "solar funding", 
                        "wind funding", "investment state", 
                        "prohibit oilgas", "area wind", "area solar")
results_df_energy <- combine_results(coef_list_energy, model_names_energy, "directed change", "energy transition policy issues")

# Define and run models for absolute energy differences
models_energy_abs <- list(
  sls_b1 = ivreg(spec_renewable_energy_germany_diff_abs ~ url_int_visits_yn | group_f, data = dfee),
  sls_b2 = ivreg(spec_funding_solar_diff_abs ~ url_int_visits_yn | group_f, data = dfee),
  sls_b3 = ivreg(spec_funding_wind_diff_abs ~ url_int_visits_yn | group_f, data = dfee),
  sls_b4 = ivreg(spec_investment_state_diff_abs ~ url_int_visits_yn | group_f, data = dfee),
  sls_b5 = ivreg(spec_prohibition_oilgas_diff_abs ~ url_int_visits_yn | group_f, data = dfee),
  sls_b6 = ivreg(spec_area_wind_diff_abs ~ url_int_visits_yn | group_f, data = dfee),
  sls_b7 = ivreg(spec_area_solar_diff_abs ~ url_int_visits_yn | group_f, data = dfee)
)
coef_list_energy_abs <- extract_coef_se(models_energy_abs)
results_df_energy_abs <- combine_results(coef_list_energy_abs, model_names_energy, "absolute change", "energy transition policy issues")

# Define and run models for cannabis differences
models_cannabis <- list(
  sls_b1 = ivreg(spec_recreational_use_legalisation_diff ~ url_int_visits_yn | group_f, data = dfc),
  sls_b2 = ivreg(spec_purchase_restricted_diff ~ url_int_visits_yn | group_f, data = dfc),
  sls_b3 = ivreg(spec_purchase_limit_recrational_use_diff ~ url_int_visits_yn | group_f, data = dfc),
  sls_b4 = ivreg(spec_cultivation_cannabis_diff ~ url_int_visits_yn | group_f, data = dfc),
  sls_b5 = ivreg(spec_cultivation_germany_diff ~ url_int_visits_yn | group_f, data = dfc),
  sls_b6 = ivreg(spec_control_cultivation_diff ~ url_int_visits_yn | group_f, data = dfc),
  sls_b7 = ivreg(spec_staterevenue_legalisation_diff ~ url_int_visits_yn | group_f, data = dfc),
  sls_b8 = ivreg(spec_consumption_private_appartments_diff ~ url_int_visits_yn | group_f, data = dfc),
  sls_b9 = ivreg(spec_protection_children_diff ~ url_int_visits_yn | group_f, data = dfc)
)
coef_list_cannabis <- extract_coef_se(models_cannabis)
model_names_cannabis <- c("recr use", "restricted purchase", 
                          "limit", "cultivation", "import","control",
                          "revenue","private apartments", "child protect")

results_df_cannabis <- combine_results(coef_list_cannabis, model_names_cannabis, "directed change", "cannabis legalisation policy issues")

# Define and run models for absolute cannabis differences
models_cannabis_abs <- list(
  sls_b1 = ivreg(spec_recreational_use_legalisation_diff_abs ~ url_int_visits_yn | group_f, data = dfc),
  sls_b2 = ivreg(spec_purchase_restricted_diff_abs ~ url_int_visits_yn | group_f, data = dfc),
  sls_b3 = ivreg(spec_purchase_limit_recrational_use_diff_abs ~ url_int_visits_yn | group_f, data = dfc),
  sls_b4 = ivreg(spec_cultivation_cannabis_diff_abs ~ url_int_visits_yn | group_f, data = dfc),
  sls_b5 = ivreg(spec_cultivation_germany_diff_abs ~ url_int_visits_yn | group_f, data = dfc),
  sls_b6 = ivreg(spec_control_cultivation_diff_abs ~ url_int_visits_yn | group_f, data = dfc),
  sls_b7 = ivreg(spec_staterevenue_legalisation_diff_abs ~ url_int_visits_yn | group_f, data = dfc),
  sls_b8 = ivreg(spec_consumption_private_appartments_diff_abs ~ url_int_visits_yn | group_f, data = dfc),
  sls_b9 = ivreg(spec_protection_children_diff_abs ~ url_int_visits_yn | group_f, data = dfc)
)
coef_list_cannabis_abs <- extract_coef_se(models_cannabis_abs)
results_df_cannabis_abs <- combine_results(coef_list_cannabis_abs, model_names_cannabis, "absolute change", "cannabis legalisation policy issues")

# Combine all results into a single data frame
results_df <- rbind(results_df_original, results_df_abs, results_df_energy, results_df_energy_abs, results_df_cannabis, results_df_cannabis_abs)


# Add confidence intervals
results_df$LowerCI <- results_df$Estimate - 1.96 * results_df$StdError
results_df$UpperCI <- results_df$Estimate + 1.96 * results_df$StdError

# Plot the estimates with facet wrap
ggplot(results_df, aes(x = reorder(Model, Estimate), y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  facet_grid(Category ~ Type, scales = "free_y", space = "free_y") +
  labs(title = "Effect of url_int_visits_yn on Different Dependent Variables",
       x = "Model",
       y = "Estimate (with 95% CI)") +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines")) +
  coord_flip()


##extract coefficients####

# Function to extract coefficients, standard errors, and p-values from models
extract_coef_se_p <- function(models) {
  lapply(models, function(model) {
    coef_summary <- summary(model)$coefficients
    coef <- coef_summary["url_int_visits_yn", "Estimate"]
    se <- coef_summary["url_int_visits_yn", "Std. Error"]
    p_value <- coef_summary["url_int_visits_yn", "Pr(>|t|)"]
    return(c(coef, se, p_value))
  })
}

# Function to combine results into a single data frame
combine_results <- function(coef_list, model_names, type, category) {
  data <- do.call(rbind, lapply(1:length(coef_list), function(i) {
    coef_se_p <- coef_list[[i]]
    data.frame(
      Model = model_names[i],
      Estimate = coef_se_p[1],
      StdError = coef_se_p[2],
      PValue = coef_se_p[3],
      Type = type,
      Category = category
    )
  }))
  return(data)
}

# Extract coefficients, standard errors, and p-values for all sets of models
coef_list_original <- extract_coef_se_p(models_original)
coef_list_abs <- extract_coef_se_p(models_abs)
coef_list_energy <- extract_coef_se_p(models_energy)
coef_list_energy_abs <- extract_coef_se_p(models_energy_abs)
coef_list_cannabis <- extract_coef_se_p(models_cannabis)
coef_list_cannabis_abs <- extract_coef_se_p(models_cannabis_abs)

# Combine all results into data frames
results_df_original <- combine_results(coef_list_original, model_names_original, "directed change", "child support policy issues")
results_df_abs <- combine_results(coef_list_abs, model_names_original, "absolute change", "child support policy issues")
results_df_energy <- combine_results(coef_list_energy, model_names_energy, "directed change", "energy transition policy issues")
results_df_energy_abs <- combine_results(coef_list_energy_abs, model_names_energy, "absolute change", "energy transition policy issues")
results_df_cannabis <- combine_results(coef_list_cannabis, model_names_cannabis, "directed change", "cannabis legalisation policy issues")
results_df_cannabis_abs <- combine_results(coef_list_cannabis_abs, model_names_cannabis, "absolute change", "cannabis legalisation policy issues")

# Combine all results into a single data frame
results_df <- rbind(results_df_original, results_df_abs, results_df_energy, results_df_energy_abs, results_df_cannabis, results_df_cannabis_abs)

# Add confidence intervals
results_df$LowerCI <- results_df$Estimate - 1.96 * results_df$StdError
results_df$UpperCI <- results_df$Estimate + 1.96 * results_df$StdError

# Add significance annotations
results_df$Significance <- ifelse(results_df$PValue < 0.01, "< .01",
                                  ifelse(results_df$PValue < 0.05, "< .05",
                                         ifelse(results_df$PValue < 0.007, "< .007 (Bonf.)", "")))

#rename values of results_df$Model
results_df$Model
# [1] "guaranteed KG"       "incomebased KG"      "app"                 "automatic pay"       "all_families in D"   "special office"     
# [7] "state cost"          "guaranteed KG"       "incomebased KG"      "app"                 "automatic pay"       "all_families in D"  
# [13] "special office"      "state cost"          "ren energy %"        "solar funding"       "wind funding"        "investment state"   
# [19] "prohibit oilgas"     "area wind"           "area solar"          "ren energy %"        "solar funding"       "wind funding"       
# [25] "investment state"    "prohibit oilgas"     "area wind"           "area solar"          "recr use"            "restricted purchase"
# [31] "limit"               "cultivation"         "private apartments"  "child protect"       "import"              "control"            
# [37] "revenue"             "recr use"            "restricted purchase" "limit"               "cultivation"         "private apartments" 
# [43] "child protect"       "import"              "control"             "revenue"            


##rename model names
results_df <- results_df %>%
  mutate(Model = case_when(
    Model == "guaranteed KG" ~ "guaranteed KG",
    Model == "incomebased KG" ~ "incomebased KG",
    Model == "app" ~ "app",
    Model == "automatic pay" ~ "automatic pay",
    Model == "all_families in D" ~ "all_families in D",
    Model == "special office" ~ "special office",
    Model == "state cost" ~ "state cost",
    Model == "ren energy %" ~ "ren energy %",
    Model == "solar funding" ~ "solar funding",
    Model == "wind funding" ~ "wind funding",
    Model == "investment state" ~ "investment state",
    Model == "prohibit oilgas" ~ "prohibit oilgas",
    Model == "area wind" ~ "area wind",
    Model == "area solar" ~ "area solar",
    Model == "recr use" ~ "recr use",
    Model == "restricted purchase" ~ "restricted purchase",
    Model == "limit" ~ "limit",
    Model == "cultivation" ~ "cultivation",
    Model == "private apartments" ~ "private apartments",
    Model == "child protect" ~ "child protect",
    Model == "import" ~ "import",
    Model == "control" ~ "control",
    Model == "revenue" ~ "revenue",
    TRUE ~ Model
  ))

# spec_payment_guarantee_1_diff = "Δ Guaranteed payment",
# spec_payment_income_1_diff = "Δ Income-based payment",
# spec_digital_application_diff = "Δ Dedicated app",
# spec_automatic_payment_diff = "Δ Automatic payment",
# spec_payment_establishment_diff = "Δ Residence status independent",
# spec_own_office_diff = "Δ Dedicated office",
# spec_treasury_cost_1_diff = "Δ Additional cost to state",
# spec_renewable_energy_germany_diff = "Δ Renewable Energy in grid",
# spec_funding_solar_diff = "Δ Solar/PV systems funding",
# spec_funding_wind_diff = "Δ Wind turbine funding",
# spec_investment_state_diff = "Δ State subsidies",
# spec_prohibition_oilgas_diff = "Δ Oil/gas heater ban",
# spec_area_wind_diff = "Δ Public land area for wind parks",
# spec_area_solar_diff = "Δ Public land area for solar parks",
# spec_recreational_use_legalisation_diff = "Δ Legalization via associations",
# spec_purchase_restricted_diff = "Δ Licensed sellers only",
# spec_purchase_limit_recrational_use_diff = "Δ Purchase limit",
# spec_cultivation_cannabis_diff = "Δ Cultivation limit",
# spec_consumption_private_appartments_diff = "Δ Consumption in private only",
# spec_protection_children_diff = "Δ Child protection measures",
# spec_cultivation_germany_diff = "Δ Local production",
# spec_control_cultivation_diff = "Δ State controlled production",
# spec_staterevenue_legalisation_diff = "Δ State Revenue",

#rename model names based on these names
results_df <- results_df %>%
  mutate(Model = case_when(
    Model == "guaranteed KG" ~ "Δ Guaranteed payment",
    Model == "incomebased KG" ~ "Δ Income-based payment",
    Model == "app" ~ "Δ Dedicated app",
    Model == "automatic pay" ~ "Δ Automatic payment",
    Model == "all_families in D" ~ "Δ Residence status independent",
    Model == "special office" ~ "Δ Dedicated office",
    Model == "state cost" ~ "Δ Additional cost to state",
    Model == "ren energy %" ~ "Δ Renewable energy in grid",
    Model == "solar funding" ~ "Δ Solar/PV systems funding",
    Model == "wind funding" ~ "Δ Wind turbine funding",
    Model == "investment state" ~ "Δ State subsidies",
    Model == "prohibit oilgas" ~ "Δ Oil/gas heater ban",
    Model == "area wind" ~ "Δ Public land area for wind parks",
    Model == "area solar" ~ "Δ Public land area for solar parks",
    Model == "recr use" ~ "Δ Legalization via associations",
    Model == "restricted purchase" ~ "Δ Licensed sellers only",
    Model == "limit" ~ "Δ Purchase limit",
    Model == "cultivation" ~ "Δ Cultivation limit",
    Model == "private apartments" ~ "Δ Consumption in private only",
    Model == "child protect" ~ "Δ Child protection measures",
    Model == "import" ~ "Δ Local production",
    Model == "control" ~ "Δ State controlled production",
    Model == "revenue" ~ "Δ State revenue",
    TRUE ~ Model
  ))

# Define custom colors for the categories
custom_colors <- c('#ff7f0e', '#1f77b4', '#2ca02c')

##create ggplot
ggplot(results_df, aes(x = reorder(Model, -Estimate), y = Estimate, color = Category)) +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  geom_text(aes(label = Significance), hjust = -0.7, vjust = -0.5, size=3, color='red') +
  geom_text(aes(label = round(Estimate, 2)), vjust = -0.5,size = 3, color = "black") + # Add estimate numbers over the dots
  facet_grid(fct_inorder(Category) ~ Type, scales = "free_y", space = "free_y") +
  scale_color_manual(values = custom_colors) +
  labs(title = "Effect of online visits on attitudes towards specific policy issues",
       x = "",
       y = "Estimate (with 95% CI)") +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines")) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Add horizontal line at zero
  theme(
    panel.spacing = unit(1, "lines"), # Increase spacing between panels
    strip.text = element_text(size = 10, face = "bold"), # Customize facet labels
    panel.border = element_rect(color = "black", fill = NA), # Add panel borders
    strip.background = element_rect(fill = "grey90", color = "black"), # Customize strip background
    legend.position = "none" # Remove legend
  )


