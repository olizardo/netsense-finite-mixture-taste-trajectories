# 08_prep_music_books_trajectories.R
# Prepping longitudinal data for Music and Books GMMs

library(dplyr)
library(tidyr)

# 1. Load Data
demo_data <- readRDS("demographics_longitudinal_clean.rds")

# 2. Extract Music Preferences Data (Long Format)
# Variables are named musicpref1_1 through musicpref22_6
music_cols <- grep("^egoid|^musicpref[0-9]+_[1-6]$", names(demo_data), value = TRUE)

music_long <- demo_data %>% 
  select(all_of(music_cols)) %>%
  pivot_longer(
    cols = starts_with("musicpref"),
    names_to = c("genre", "wave"),
    names_pattern = "musicpref([0-9]+)_([1-6])",
    values_to = "preference"
  ) %>%
  mutate(
    wave = as.numeric(wave),
    genre = as.factor(genre), # Treat as factor for crossed random effects
    # Convert Yes=1, No=0
    pref_binary = case_when(
      preference == "Yes" ~ 1,
      preference == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    # Center time for MCMC convergence
    time = wave - 1
  ) %>%
  filter(!is.na(pref_binary))

# Create mapping dictionary based on NetSense Codebook
music_map <- c(
  "1" = "Big band",
  "2" = "Bluegrass",
  "3" = "Blues or R&B",
  "4" = "Broadway/Show tunes",
  "5" = "Choral or glee club",
  "6" = "Classic rock/Oldies",
  "7" = "Classical/Chamber",
  "8" = "Country",
  "9" = "Dance music",
  "10" = "Ethnic/National",
  "11" = "Folk music",
  "12" = "Hymns/Gospel",
  "13" = "Jazz",
  "14" = "Latin/Spanish/Salsa",
  "15" = "Mood/Easy listening",
  "16" = "New age",
  "17" = "Opera",
  "18" = "Operetta/Musicals",
  "19" = "Parade/Marching band",
  "20" = "Rap/Hip-hop",
  "21" = "Reggae",
  "22" = "Rock/Heavy metal"
)

music_long <- music_long %>%
  mutate(genre_label = music_map[as.character(genre)])

# 3. Extract Book Preferences Data (Long Format)
# Variables are named typebookread1_1 through typebookread9_6
book_cols <- grep("^egoid|^typebookread[0-9]+_[1-6]$", names(demo_data), value = TRUE)

book_long <- demo_data %>% 
  select(all_of(book_cols)) %>%
  pivot_longer(
    cols = starts_with("typebookread"),
    names_to = c("book_type", "wave"),
    names_pattern = "typebookread([0-9]+)_([1-6])",
    values_to = "preference"
  ) %>%
  mutate(
    wave = as.numeric(wave),
    book_type = as.factor(book_type), # Treat as factor for crossed random effects
    # Convert Yes=1, No=0
    pref_binary = case_when(
      preference == "Yes" ~ 1,
      preference == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    # Center time for MCMC convergence
    time = wave - 1
  ) %>%
  filter(!is.na(pref_binary))

book_map <- c(
  "1" = "Mysteries",
  "2" = "Thrillers",
  "3" = "Romance",
  "4" = "Science fiction/Fantasy",
  "5" = "Other fiction",
  "6" = "Health/Fitness/Self-improvement",
  "7" = "History/Political",
  "8" = "Biographies/Memoirs",
  "9" = "Other non-fiction"
)

book_long <- book_long %>%
  mutate(book_label = book_map[as.character(book_type)])

# 4. Save to Cache
saveRDS(music_long, "Cache/music_long_clean.rds")
saveRDS(book_long, "Cache/books_long_clean.rds")

cat("Music and Books data reshaped to long format and saved to Cache/\n")
cat("Music rows:", nrow(music_long), "| Distinct Egos:", n_distinct(music_long$egoid), "\n")
cat("Books rows:", nrow(book_long), "| Distinct Egos:", n_distinct(book_long$egoid), "\n")
