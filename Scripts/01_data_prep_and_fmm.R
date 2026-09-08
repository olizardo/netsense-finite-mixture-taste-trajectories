# 01_data_prep_and_fmm.R
# Setting up cultural taste change trajectories using Finite Mixture Models
# Project: NetSense Taste Trajectories

library(dplyr)
library(tidyr)
library(stringr)

# 1. Load Data
demo_data <- readRDS("demographics_longitudinal_clean.rds")

# 2. Select Relevant Variables
# We'll focus on music preferences across waves 1 to 6
music_cols <- grep("egoid|^musicpref[0-9]+_[1-6]$", names(demo_data), value = TRUE)
music_data <- demo_data %>% select(all_of(music_cols))

# 3. Reshape to Long Format for Trajectory Modeling
# The names are like musicpref1_1, where the first number is the genre ID and the second is the wave
music_long <- music_data %>%
  pivot_longer(
    cols = starts_with("musicpref"),
    names_to = c("genre", "wave"),
    names_pattern = "musicpref([0-9]+)_([1-6])",
    values_to = "preference"
  ) %>%
  mutate(
    wave = as.numeric(wave),
    genre = as.numeric(genre),
    # Convert 'Yes' / 'No' to 1 / 0, treating empty strings as NA
    pref_binary = case_when(
      preference == "Yes" ~ 1,
      preference == "No" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Let's pivot wider again to have one column per genre for the FMM
music_trajectories <- music_long %>%
  select(-preference) %>%
  pivot_wider(names_from = genre, values_from = pref_binary, names_prefix = "genre_")

# 4. Preparing for Finite Mixture Modeling (FMM)
# We use `flexmix` to fit a finite mixture of longitudinal regressions.
# flexmix allows us to find latent classes (trajectories) of taste change over time.

cat("Data is prepped. We have", n_distinct(music_trajectories$egoid), "egos across up to 6 waves.\n")

# To fit a flexmix model, we need the `flexmix` package:
library(flexmix)

# Example FMM (Latent Class Growth Analysis) for genre 1 over time:
# We specify a binomial family since pref_binary is 0/1.
# This fits a model with 2 latent classes (k=2) representing different trajectories.

# Since flexmix does not inherently handle longitudinal grouping in its basic formula
# the same way as lcmm, we can treat the repeated measures as independent within class 
# or use FLXMRglm.

set.seed(123)
# Subset to rows with non-NA values for genre 1 for this example
music_subset <- music_trajectories %>% filter(!is.na(genre_1))

if (nrow(music_subset) > 0) {
  model_k2 <- flexmix(cbind(genre_1, 1 - genre_1) ~ wave, 
                      data = music_subset, 
                      k = 2, 
                      model = FLXMRglm(family = "binomial"))
  
  print(model_k2)
  print(summary(model_k2))
}
