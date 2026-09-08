# 05_covariates_prep.R
# Prepare and binarize baseline covariate predictors for future modeling

library(dplyr)
library(tidyr)

# 1. Load Data
demo_data <- readRDS("demographics_longitudinal_clean.rds")

# 2. Extract and Recode Predictors
covariates <- demo_data %>%
  select(
    egoid,
    gender_1,
    ethnicity_1,
    religcateg_1,
    pincome_1,
    momed_1,
    daded_1,
    occupationmom_1,
    occupationdad_1,
    political_1, # Political ideology scale
    happy_1      # Baseline happiness
  ) %>%
  mutate(
    # Gender: Women (1) vs Others (0)
    is_woman = case_when(
      gender_1 == "Female" ~ 1,
      gender_1 == "Male" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Race: White (1) vs Other (0)
    is_white = case_when(
      ethnicity_1 == "White/Caucasian" ~ 1,
      is.na(ethnicity_1) | ethnicity_1 == "" ~ NA_real_,
      TRUE ~ 0
    ),
    
    # Religion: Catholic (1) vs Other (0)
    is_catholic = case_when(
      religcateg_1 == "Roman Catholic" ~ 1,
      is.na(religcateg_1) | religcateg_1 == "" ~ NA_real_,
      TRUE ~ 0
    ),
    
    # Political Ideology: 0=Not Sure, 1=Extremely Liberal ... 7=Extremely Conservative
    # Let's map it to a numeric scale (removing Not Sure)
    pol_conservatism = case_when(
      political_1 == "Not sure" | is.na(political_1) ~ NA_real_,
      political_1 == "Extremely liberal" ~ 1,
      political_1 == "Liberal" ~ 2,
      political_1 == "Slightly liberal" ~ 3,
      political_1 == "Moderate" ~ 4,
      political_1 == "Slightly conservative" ~ 5,
      political_1 == "Conservative" ~ 6,
      political_1 == "Extremely conservative" ~ 7,
      TRUE ~ as.numeric(as.character(political_1)) 
    ),
    
    # Happiness: 0=Not Sure, 1=Not so happy, 2=Pretty happy, 3=Happy, 4=Very happy
    happiness_num = case_when(
      happy_1 == "Not sure" | is.na(happy_1) ~ NA_real_,
      happy_1 == "Not so happy" ~ 1,
      happy_1 == "Pretty happy" ~ 2,
      happy_1 == "Happy" ~ 3,
      happy_1 == "Very happy" ~ 4,
      TRUE ~ as.numeric(as.character(happy_1))
    )
  )

# Verify recoding
cat("--- Gender Recoding ---\n")
print(table(covariates$gender_1, covariates$is_woman, useNA = "ifany"))

cat("\n--- Race Recoding ---\n")
print(table(covariates$ethnicity_1, covariates$is_white, useNA = "ifany"))

cat("\n--- Religion Recoding ---\n")
print(table(covariates$religcateg_1, covariates$is_catholic, useNA = "ifany"))

# Save the covariates dataset
saveRDS(covariates, "Cache/covariates_clean.rds")
cat("\nCovariates cleaned and saved to 'Cache/covariates_clean.rds'.\n")
