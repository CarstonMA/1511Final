library(lme4)
library(splines)
library(dplyr)
library(kableExtra)

# for me setwd("C:/Users/audr2/Documents/PHP1511/Final_Project")

### LINEAR MIXED MODELS FOR MALES
# Read race-level data (includes PM2.5 and HeatIndex)
df_race_env <- read.csv("88101_HI.csv")
df <- read_csv("Results.csv")

# filter = male
df_male_all <- df %>%
  filter(Gender == "M")

# Merge with runner-level data
df_model_male <- df_male_all %>%
  left_join(df_race_env %>% select(Race, mean_pm25, HeatIndex_C), by = "Race")

model_male <- lmer(
  Finish ~ mean_pm25 + ns(HeatIndex_C, df = 5) + (1 | Race),
  data = df_model_male
)
#summary(model_male)

#### SAME FOR FEMALES

# Read race-level data (includes PM2.5 and HeatIndex)
df_race_env <- read.csv("88101_HI.csv")
df <- read_csv("Results.csv")

# filter = female
df_female_all <- df %>%
  filter(Gender == "F")

# Merge with runner-level data
df_model_female <- df_female_all %>%
  left_join(df_race_env %>% select(Race, mean_pm25, HeatIndex_C), by = "Race")

# Fit the model for female marathoners
model_female <- lmer(
  Finish ~ mean_pm25 + ns(HeatIndex_C, df = 5) + (1 | Race),
  data = df_model_female
)
# Summarize the model for female marathoners
#summary(model_female)

# Extract the fixed effects and their estimates
fixed_effects_male <- summary(model_male)$coefficients
fixed_effects_female <- summary(model_female)$coefficients


### SLOWEST PART OF THE CODE
# Get confidence intervals for the coefficients (profile method is robust for mixed models)
# Keep only the confidence intervals for fixed effects (not random effects)
conf_intervals_male <- confint(model_male, method = "profile")[names(fixef(model_male)), ]
conf_intervals_female <- confint(model_female, method = "profile")[names(fixef(model_female)), ]

library(tibble)

# Round numeric values
results_table_male_clean <- tibble(
  Term = rownames(fixed_effects_male),
  Estimate = round(fixed_effects_male[, "Estimate"], 3),
  `Std. Error` = round(fixed_effects_male[, "Std. Error"], 3),
  `t value` = round(fixed_effects_male[, "t value"], 3),
  `2.5% CI` = round(conf_intervals_male[rownames(fixed_effects_male), 1], 3),
  `97.5% CI` = round(conf_intervals_male[rownames(fixed_effects_male), 2], 3)
)

results_table_female_clean <- tibble(
  Term = rownames(fixed_effects_female),
  Estimate = round(fixed_effects_female[, "Estimate"], 3),
  `Std. Error` = round(fixed_effects_female[, "Std. Error"], 3),
  `t value` = round(fixed_effects_female[, "t value"], 3),
  `2.5% CI` = round(conf_intervals_female[rownames(fixed_effects_female), 1], 3),
  `97.5% CI` = round(conf_intervals_female[rownames(fixed_effects_female), 2], 3)
)

# Create and save clean tables
save_kable(
  kable(results_table_male_clean, format = "html", caption = "Linear Mixed Model Results for Male Runners") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")),
  "results_male_table.html"
)

save_kable(
  kable(results_table_female_clean, format = "html", caption = "Linear Mixed Model Results for Female Runners") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")),
  "results_female_table.html"
)

