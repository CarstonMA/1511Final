library(broom.mixed)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggeffects)
library(lme4)
library(splines)
library(dplyr)
library(kableExtra)
library(readr)

df_race_env <- read.csv("88101_HI.csv")
df <- read_csv("Results.csv")
df_combined <- df %>%
  left_join(df_race_env %>% select(Race, mean_pm25, HeatIndex_C), by = "Race")


#Interaction model with automatic age spline
model_age_interaction <- lmer(
  Finish ~ mean_pm25 * Gender * ns(Age, df = 4) + 
    ns(HeatIndex_C, df = 5) + 
    (1 | Race),
  data = df_combined
)

#Interaction model with predefined age brackets
model_age_bracket <- lmer(
  Finish ~ mean_pm25 * Gender * `Age Bracket` +
    ns(HeatIndex_C, df = 5) +
    (1 | Race),
  data = df_combined
)

# Tidy the model
tidy_model <- broom.mixed::tidy(model_age_bracket, effects = "fixed", conf.int = TRUE)


pm_terms <- tidy_model %>%
  filter(grepl("mean_pm25:", term)) %>%
  mutate(
    gender = case_when(
      grepl("GenderM", term) ~ "Male",
      grepl("GenderX", term) ~ "X",
      grepl("GenderU", term) ~ "U",
      TRUE ~ "Female"
    ),
    age_bracket = case_when(
      grepl("40-44", term) ~ "40-44",
      grepl("45-49", term) ~ "45-49",
      grepl("50-54", term) ~ "50-54",
      grepl("55-59", term) ~ "55-59",
      grepl("60-64", term) ~ "60-64",
      grepl("65-69", term) ~ "65-69",
      grepl("70-74", term) ~ "70-74",
      grepl("75-79", term) ~ "75-79",
      grepl("80 and Over", term) ~ "80+",
      grepl("Under 35", term) ~ "Under 35",
      grepl("Unknown", term) ~ "Unknown",
      TRUE ~ "35-39"  # Reference group
    )
  )


ref_row <- tidy_model %>%
  filter(term == "mean_pm25") %>%
  mutate(gender = "Female", age_bracket = "35-39")


pm_plot_data <- bind_rows(pm_terms, ref_row) %>%
  mutate(
    age_bracket = fct_relevel(age_bracket,
                              "Under 35", "35-39", "40-44", "45-49", "50-54", "55-59",
                              "60-64", "65-69", "70-74", "75-79", "80+", "Unknown"
    )
  )

pm_plot_data_filtered <- pm_plot_data %>%
  filter(
    gender %in% c("Male", "Female"),
    age_bracket != "Unknown"
  )

#Estimated effect by age and gender
ggplot(pm_plot_data_filtered, aes(x = age_bracket, y = estimate, color = gender, group = gender)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Estimated Effect of PM2.5 on Finish Time by Age and Gender",
    y = "Seconds added per 1 µg/m³ PM2.5",
    x = "Age Bracket",
    color = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tidy_model <- tidy(model_age_bracket, effects = "fixed", conf.int = TRUE)

#Summary of fixed effects
kable(tidy_model, format = "html", caption = "Summary of Fixed Effects in Age Bracket Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)


pred <- ggpredict(model_age_bracket, terms = c("mean_pm25", "Gender", "Age Bracket"))

pred$facet <- fct_relevel(pred$facet,
                          "Under 35", "35-39", "40-44", "45-49", "50-54", "55-59",
                          "60-64", "65-69", "70-74", "75-79", "80 and Over"
)

pred_filtered <- pred %>%
  filter(group %in% c("M", "F"), !facet %in% c("Unknown"))

#Predicted finish times
ggplot(pred_filtered, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  facet_wrap(~facet, scales = "free_y") +
  labs(
    title = "Predicted Finish Time by PM2.5, Age Bracket, and Gender",
    x = "PM2.5 (µg/m³)",
    y = "Predicted Finish Time (seconds)",
    color = "Gender",
    fill = "Gender"
  ) +
  theme_minimal()

#Residuals vs. fitted
plot(model_age_bracket, which = 1)

#QQ Plot
qqnorm(resid(model_age_bracket, type = "pearson"))
qqline(resid(model_age_bracket, type = "pearson"), col = "red")

#Histogram of residuals
hist(resid(model_age_bracket, type = "pearson"), breaks = 100, main = "Histogram of Residuals")
