library(dplyr)
library(mice)

covariates <- readRDS("Cache/covariates_clean_extended.rds")
fit_arts <- readRDS("Cache/brms_3class_fixed_events.rds")
cult_long <- fit_arts$data

ego_classes <- cult_long %>% distinct(egoid)

analysis_df <- ego_classes %>%
  left_join(covariates, by = "egoid") %>%
  mutate(
    income_num = case_when(
      pincome_1 == "less than 10k" ~ 5,
      pincome_1 == "10000-14999" ~ 12.5,
      pincome_1 == "15000-19999" ~ 17.5,
      pincome_1 == "20000-24999" ~ 22.5,
      pincome_1 == "25000-29999" ~ 27.5,
      pincome_1 == "30000-39999" ~ 35,
      pincome_1 == "40000-49999" ~ 45,
      pincome_1 == "50000-59999" ~ 55,
      pincome_1 == "60000-74999" ~ 67.5,
      pincome_1 == "75000-99999" ~ 87.5,
      pincome_1 == "100000-149999" ~ 125,
      pincome_1 == "150000-199999" ~ 175,
      pincome_1 == "200000-249999" ~ 225,
      pincome_1 == "250000 or more" ~ 300,
      TRUE ~ NA_real_
    ),
    momed_years = case_when(
      momed_1 == "Junior High/middle school or less" ~ 8,
      momed_1 == "Some high school" ~ 10,
      momed_1 == "High school Graduate" ~ 12,
      momed_1 == "Postsecondary school other than college" ~ 13,
      momed_1 == "Some college" ~ 14,
      momed_1 == "College Degree" ~ 16,
      momed_1 == "Some graduate school" ~ 17,
      momed_1 == "Graduate degree" ~ 18,
      TRUE ~ NA_real_
    ),
    daded_years = case_when(
      daded_1 == "Junior High/middle school or less" ~ 8,
      daded_1 == "Some high school" ~ 10,
      daded_1 == "High school Graduate" ~ 12,
      daded_1 == "Postsecondary school other than college" ~ 13,
      daded_1 == "Some college" ~ 14,
      daded_1 == "College Degree" ~ 16,
      daded_1 == "Some graduate school" ~ 17,
      daded_1 == "Graduate degree" ~ 18,
      TRUE ~ NA_real_
    ),
    parent_ed_years = pmax(momed_years, daded_years, na.rm = TRUE)
  )

# Use mice to impute missing values
imp_data <- analysis_df %>%
  select(egoid, is_woman, is_white, is_catholic, income_num, parent_ed_years, high_hs_grade, aims_advanced_degree, is_stem_major, hometown)

set.seed(42)
imputed <- mice(imp_data, m=1, maxit=50, method='pmm', seed=500, printFlag=FALSE)
complete_data <- complete(imputed, 1)
saveRDS(complete_data, "Cache/covariates_imputed.rds")
cat("Successfully imputed missing values and saved to Cache/covariates_imputed.rds\n")
