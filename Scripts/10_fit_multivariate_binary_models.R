#' Fit Multivariate Binary Trajectory Models for Arts, Books, and Music
#'
#' Implements multivariate binary mixture models using natural cubic splines (`ns(time, df = 2)`):
#' 1. Arts & Cultural Events: All 12 binary events across Waves 1 to 4.
#' 2. Book Reading Types: All 9 binary book genres across Waves 1 to 6.
#' 3. Music Genre Preferences: Top 10 musical genres across Waves 1 to 6.
#' Includes endogenous multinomial concomitants (`FLXPmultinom`).
#'
#' @author Omar Lizardo & AI Assistant
#' @date 2026-09-08

suppressPackageStartupMessages({
  library(flexmix)
  library(dplyr)
  library(tidyr)
  library(splines)
  library(nnet)
})

cat("====================================================================\n")
cat("Starting Complete Multivariate Binary Trajectory Modeling Pipeline  \n")
cat("====================================================================\n")

# 1. Load Data
demo_data <- readRDS("demographics_longitudinal_clean.rds")
music_long <- readRDS("Cache/music_long_clean.rds")
books_long <- readRDS("Cache/books_long_clean.rds")
covs <- readRDS("Cache/covariates_imputed.rds")

if (!dir.exists("Cache/summaries")) dir.create("Cache/summaries", recursive = TRUE)
if (!dir.exists("Plots")) dir.create("Plots")

form_full <- ~ is_woman + is_white + is_catholic + income_num + parent_ed_years + high_hs_grade + aims_advanced_degree + is_stem_major + hometown

var_clean_map <- c(
  "(Intercept)" = "Constant",
  "is_woman" = "Woman (ref: Man)",
  "is_white" = "White (ref: Non-White)",
  "is_catholic" = "Catholic (ref: Non-Catholic)",
  "income_num" = "Parent Income ($1,000s)",
  "parent_ed_years" = "Parent Education (Years)",
  "high_hs_grade" = "High School GPA (A/A-)",
  "aims_advanced_degree" = "Aspires to Advanced Degree",
  "is_stem_major" = "STEM Major (ref: Non-STEM)",
  "hometown" = "Hometown Urbanicity"
)

# Helper function to extract robust multinom concomitants
extract_concom <- function(mod, df_comp, domain_name, ref_level = "1") {
  df_ego <- df_comp %>%
    mutate(clust = factor(clusters(mod))) %>%
    group_by(egoid) %>%
    summarize(clust = names(sort(table(clust), decreasing = TRUE)[1]), .groups = "drop") %>%
    mutate(clust = relevel(factor(clust), ref = ref_level)) %>%
    inner_join(covs, by = "egoid")
    
  m_mnl <- multinom(
    clust ~ is_woman + is_white + is_catholic + income_num + parent_ed_years + 
      high_hs_grade + aims_advanced_degree + is_stem_major + hometown,
    data = df_ego, trace = FALSE
  )
  
  coef_mat <- summary(m_mnl)$coefficients
  se_mat <- summary(m_mnl)$standard.errors
  z_mat <- coef_mat / se_mat
  p_mat <- (1 - pnorm(abs(z_mat))) * 2
  
  res_list <- list()
  for (cls in rownames(coef_mat)) {
    for (v in colnames(coef_mat)) {
      est <- coef_mat[cls, v]
      se <- se_mat[cls, v]
      z <- z_mat[cls, v]
      p <- p_mat[cls, v]
      sig <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else ""
      
      res_list[[paste(cls, v, sep="_")]] <- data.frame(
        Domain = domain_name,
        Comparison = paste("Class", cls, "vs. Class", ref_level),
        Variable = var_clean_map[v],
        Estimate = est,
        Std_Error = se,
        Odds_Ratio = exp(est),
        Z = z,
        P_Value = p,
        Sig = sig,
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(res_list)
}

# =============================================================================
# A. ARTS & CULTURAL EVENTS (Revised 9 Items, Waves 1 to 4)
# =============================================================================
cat("\n--> [1/3] Estimating Arts Multivariate Binary Model (9 Events)...\n")

event_clean_labels <- c(
  "art_classical_opera"   = "Classical Concert or Opera",
  "art_rock_folk_country" = "Rock, Pop, Folk, or Country",
  "art_ballet_dance"       = "Ballet or Modern Dance",
  "art_jazz_blues"         = "Jazz or Blues Performance",
  "art_musical_theater"    = "Musical Stage Play",
  "art_stage_play"         = "Stage Play (Non-Musical)",
  "art_comedy_club"        = "Comedy Club",
  "art_art_museum"         = "Art Museum or Gallery",
  "art_cinema"             = "Cinema or Movie Theater"
)
art_cols <- names(event_clean_labels)

cult_cols <- grep("^egoid|^culturalevents[0-9]+_[1-6]$", names(demo_data), value = TRUE)
df_arts_comp <- demo_data %>% 
  dplyr::select(all_of(cult_cols)) %>%
  pivot_longer(
    cols = starts_with("culturalevents"),
    names_to = c("event_type", "wave"),
    names_pattern = "culturalevents([0-9]+)_([1-6])",
    values_to = "preference"
  ) %>%
  mutate(
    wave = as.numeric(wave),
    time = wave - 1,
    pref_binary = case_when(preference == "Yes" ~ 1, preference == "No" ~ 0, TRUE ~ NA_real_),
    event_clean = paste0("event_", event_type)
  ) %>%
  filter(!is.na(pref_binary)) %>%
  dplyr::select(egoid, wave, time, event_clean, pref_binary) %>%
  pivot_wider(names_from = event_clean, values_from = pref_binary) %>%
  filter(complete.cases(.)) %>%
  mutate(
    art_classical_opera   = as.numeric(event_2 == 1 | event_4 == 1),
    art_rock_folk_country = as.numeric(event_1 == 1 | event_3 == 1),
    art_ballet_dance       = event_5,
    art_jazz_blues         = event_6,
    art_musical_theater    = event_7,
    art_stage_play         = event_8,
    art_comedy_club        = event_9,
    art_art_museum         = event_10,
    art_cinema             = event_12
  ) %>%
  dplyr::select(
    egoid, wave, time,
    all_of(art_cols)
  ) %>%
  inner_join(covs, by = "egoid") %>%
  arrange(egoid, time)

specs_arts <- lapply(art_cols, function(col) {
  FLXMRglm(as.formula(paste0("cbind(", col, ", 1 - ", col, ") ~ ns(time, df = 2)")), family = "binomial")
})

set.seed(2026)
mod_arts_final <- flexmix(
  as.formula(paste0("cbind(", paste(art_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")),
  data = df_arts_comp, k = 3, model = specs_arts,
  concomitant = FLXPmultinom(form_full),
  control = list(iter.max = 300, minprior = 0.04)
)

# Trajectory generation: smooth line + wave point markers
t_smooth_arts <- seq(0, 3, length.out = 80)
t_points_arts <- 0:3
basis_arts <- ns(df_arts_comp$time, df = 2)
eval_smooth_arts <- predict(basis_arts, t_smooth_arts)
eval_points_arts <- predict(basis_arts, t_points_arts)

params_arts <- parameters(mod_arts_final)
arts_labels_map <- c(
  "1" = "Omnivores",
  "2" = "Traditionalists",
  "3" = "Minimalists"
)

# Build smooth lines
arts_smooth_list <- list()
for (ev_idx in 1:length(art_cols)) {
  col <- art_cols[ev_idx]
  ev_label <- event_clean_labels[col]
  for (c_idx in 1:3) {
    b <- params_arts[[ev_idx]][, c_idx]
    eta <- b[1] + b[2] * eval_smooth_arts[, 1] + b[3] * eval_smooth_arts[, 2]
    arts_smooth_list[[paste(ev_idx, c_idx, sep="_")]] <- data.frame(
      Event = ev_label,
      Class = arts_labels_map[as.character(c_idx)],
      time = t_smooth_arts,
      prob = plogis(eta)
    )
  }
}
df_arts_smooth <- bind_rows(arts_smooth_list)

# Build discrete wave points
arts_points_list <- list()
for (ev_idx in 1:length(art_cols)) {
  col <- art_cols[ev_idx]
  ev_label <- event_clean_labels[col]
  for (c_idx in 1:3) {
    b <- params_arts[[ev_idx]][, c_idx]
    eta <- b[1] + b[2] * eval_points_arts[, 1] + b[3] * eval_points_arts[, 2]
    arts_points_list[[paste(ev_idx, c_idx, sep="_")]] <- data.frame(
      Event = ev_label,
      Class = arts_labels_map[as.character(c_idx)],
      time = t_points_arts,
      wave = t_points_arts + 1,
      prob = plogis(eta)
    )
  }
}
df_arts_points <- bind_rows(arts_points_list)

saveRDS(list(smooth = df_arts_smooth, points = df_arts_points), "Cache/summaries/arts_trajectories.rds")
cat("   Saved: Cache/summaries/arts_trajectories.rds\n")

# =============================================================================
# B. BOOK READING TYPES (All 9 Items, Waves 1 to 6)
# =============================================================================
cat("\n--> [2/3] Estimating Books Multivariate Binary Model (9 Book Types)...\n")

book_clean_labels <- c(
  "book_1" = "Mysteries",
  "book_2" = "Thrillers",
  "book_3" = "Romance",
  "book_4" = "Science Fiction / Fantasy",
  "book_5" = "Other Fiction",
  "book_6" = "Health / Self-Improvement",
  "book_7" = "History / Politics",
  "book_8" = "Biographies / Memoirs",
  "book_9" = "Other Non-Fiction"
)

df_books_comp <- books_long %>%
  mutate(book_clean = paste0("book_", book_type), time = wave - 1) %>%
  select(egoid, wave, time, book_clean, pref_binary) %>%
  pivot_wider(names_from = book_clean, values_from = pref_binary) %>%
  filter(complete.cases(.)) %>%
  inner_join(covs, by = "egoid") %>%
  arrange(egoid, time)

book_cols <- grep("^book_", names(df_books_comp), value = TRUE)
specs_books <- lapply(book_cols, function(col) {
  FLXMRglm(as.formula(paste0("cbind(", col, ", 1 - ", col, ") ~ ns(time, df = 2)")), family = "binomial")
})

set.seed(2026)
mod_books_final <- flexmix(
  as.formula(paste0("cbind(", paste(book_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")),
  data = df_books_comp, k = 3, model = specs_books,
  concomitant = FLXPmultinom(form_full),
  control = list(iter.max = 300, minprior = 0.04)
)

t_smooth_books <- seq(0, 5, length.out = 80)
t_points_books <- 0:5
basis_books <- ns(df_books_comp$time, df = 2)
eval_smooth_books <- predict(basis_books, t_smooth_books)
eval_points_books <- predict(basis_books, t_points_books)

params_books <- parameters(mod_books_final)

# Strictly unique class naming based on verified component profiles:
# Comp 1: High Non-Fiction & History
# Comp 2: High Sci-Fi & Thrillers
# Comp 3: Low / Selective Readers
books_labels_map <- c(
  "1" = "Nonfictionists",
  "2" = "Fictionists",
  "3" = "Minimalists"
)

books_smooth_list <- list()
for (bk_idx in 1:9) {
  col <- book_cols[bk_idx]
  bk_label <- book_clean_labels[col]
  for (c_idx in 1:3) {
    b <- params_books[[bk_idx]][, c_idx]
    eta <- b[1] + b[2] * eval_smooth_books[, 1] + b[3] * eval_smooth_books[, 2]
    books_smooth_list[[paste(bk_idx, c_idx, sep="_")]] <- data.frame(
      Book_Type = bk_label,
      Class = books_labels_map[as.character(c_idx)],
      time = t_smooth_books,
      prob = plogis(eta)
    )
  }
}
df_books_smooth <- bind_rows(books_smooth_list)

books_points_list <- list()
for (bk_idx in 1:9) {
  col <- book_cols[bk_idx]
  bk_label <- book_clean_labels[col]
  for (c_idx in 1:3) {
    b <- params_books[[bk_idx]][, c_idx]
    eta <- b[1] + b[2] * eval_points_books[, 1] + b[3] * eval_points_books[, 2]
    books_points_list[[paste(bk_idx, c_idx, sep="_")]] <- data.frame(
      Book_Type = bk_label,
      Class = books_labels_map[as.character(c_idx)],
      time = t_points_books,
      wave = t_points_books + 1,
      prob = plogis(eta)
    )
  }
}
df_books_points <- bind_rows(books_points_list)

saveRDS(list(smooth = df_books_smooth, points = df_books_points), "Cache/summaries/books_trajectories.rds")
cat("   Saved: Cache/summaries/books_trajectories.rds\n")

# =============================================================================
# C. MUSIC GENRE PREFERENCES (Top 10 Genres, Waves 1 to 6)
# =============================================================================
cat("\n--> [3/3] Estimating Music Multivariate Binary Model (Top 10 Genres)...\n")

genre_clean_labels <- c(
  "rap_hip_hop" = "Rap / Hip-Hop",
  "classic_rock_oldies" = "Classic Rock / Oldies",
  "dance_music" = "Dance Music",
  "rock_heavy_metal" = "Rock / Heavy Metal",
  "country" = "Country",
  "broadway_show_tunes" = "Broadway / Show Tunes",
  "classical_chamber" = "Classical / Chamber",
  "mood_easy_listening" = "Mood / Easy Listening",
  "folk_music" = "Folk Music",
  "jazz" = "Jazz"
)

top10_genres_order <- c("Rap/Hip-hop", "Classic rock/Oldies", "Dance music", "Rock/Heavy metal", 
                        "Country", "Broadway/Show tunes", "Classical/Chamber", "Mood/Easy listening", 
                        "Folk music", "Jazz")

df_music_wide <- music_long %>%
  filter(genre_label %in% top10_genres_order, !is.na(pref_binary)) %>%
  mutate(genre_clean = gsub("[^a-zA-Z0-9]", "_", tolower(genre_label)), time = wave - 1) %>%
  select(egoid, wave, time, genre_clean, pref_binary) %>%
  pivot_wider(names_from = genre_clean, values_from = pref_binary) %>%
  filter(complete.cases(.)) %>%
  inner_join(covs, by = "egoid") %>%
  arrange(egoid, time)

music_cols <- grep("^(rap|classic|dance|rock|country|broadway|classical|mood|folk|jazz)", names(df_music_wide), value = TRUE)
specs_music <- lapply(music_cols, function(col) {
  FLXMRglm(as.formula(paste0("cbind(", col, ", 1 - ", col, ") ~ ns(time, df = 2)")), family = "binomial")
})

set.seed(2026)
mod_music_final <- flexmix(
  as.formula(paste0("cbind(", paste(music_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")),
  data = df_music_wide, k = 3, model = specs_music,
  concomitant = FLXPmultinom(form_full),
  control = list(iter.max = 300, minprior = 0.04)
)

t_smooth_music <- seq(0, 5, length.out = 80)
t_points_music <- 0:5
basis_music <- ns(df_music_wide$time, df = 2)
eval_smooth_music <- predict(basis_music, t_smooth_music)
eval_points_music <- predict(basis_music, t_points_music)

params_music <- parameters(mod_music_final)

music_labels_map <- c(
  "1" = "Omnivores",
  "2" = "Rockers",
  "3" = "Mainstreamers"
)

music_smooth_list <- list()
for (m_idx in 1:10) {
  col <- music_cols[m_idx]
  m_label <- genre_clean_labels[col]
  for (c_idx in 1:3) {
    b <- params_music[[m_idx]][, c_idx]
    eta <- b[1] + b[2] * eval_smooth_music[, 1] + b[3] * eval_smooth_music[, 2]
    music_smooth_list[[paste(m_idx, c_idx, sep="_")]] <- data.frame(
      Genre = m_label,
      Class = music_labels_map[as.character(c_idx)],
      time = t_smooth_music,
      prob = plogis(eta)
    )
  }
}
df_music_smooth <- bind_rows(music_smooth_list)

music_points_list <- list()
for (m_idx in 1:10) {
  col <- music_cols[m_idx]
  m_label <- genre_clean_labels[col]
  for (c_idx in 1:3) {
    b <- params_music[[m_idx]][, c_idx]
    eta <- b[1] + b[2] * eval_points_music[, 1] + b[3] * eval_points_music[, 2]
    music_points_list[[paste(m_idx, c_idx, sep="_")]] <- data.frame(
      Genre = m_label,
      Class = music_labels_map[as.character(c_idx)],
      time = t_points_music,
      wave = t_points_music + 1,
      prob = plogis(eta)
    )
  }
}
df_music_points <- bind_rows(music_points_list)

saveRDS(list(smooth = df_music_smooth, points = df_music_points), "Cache/summaries/music_trajectories.rds")
cat("   Saved: Cache/summaries/music_trajectories.rds\n")

# =============================================================================
# D. CONCOMITANT MODEL EXTRACTION (ALL 3 DOMAINS)
# =============================================================================
cat("\n--> Extracting multinomial concomitant models across all three domains...\n")

df_concom_arts <- extract_concom(mod_arts_final, df_arts_comp, "Arts & Cultural Events", "1")
df_concom_books <- extract_concom(mod_books_final, df_books_comp, "Book Reading Types", "1")
df_concom_music <- extract_concom(mod_music_final, df_music_wide, "Music Genre Preferences", "1")

df_concom_all <- bind_rows(df_concom_arts, df_concom_books, df_concom_music)
write.csv(df_concom_all, "Cache/summaries/all_domains_concomitants.csv", row.names = FALSE)
cat("   Saved: Cache/summaries/all_domains_concomitants.csv\n")

cat("\n====================================================================\n")
cat("Pipeline Finished! Trajectories and Concomitant Summaries Serialized.\n")
cat("====================================================================\n")
