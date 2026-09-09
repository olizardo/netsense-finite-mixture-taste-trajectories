#' Fit Multivariate Binary Trajectory Models for Books and Music (K = 4)
#'
#' Implements multivariate binary mixture models using natural cubic splines (`ns(time, df = 2)`):
#' 1. Book Reading Types: All 9 binary book genres across Waves 1 to 6 (K = 4).
#' 2. Music Genre Preferences: Top 10 musical genres across Waves 1 to 6 (K = 4).
#' Includes endogenous multinomial concomitants (`FLXPmultinom`).
#'
#' @author Omar Lizardo & AI Assistant
#' @date 2026-09-09

suppressPackageStartupMessages({
  library(flexmix)
  library(dplyr)
  library(tidyr)
  library(splines)
  library(nnet)
})

cat("====================================================================\n")
cat("Starting Multivariate Binary Trajectory Modeling Pipeline (K = 4)    \n")
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

# =============================================================================
# A. BOOK READING TYPES (All 9 Items, Waves 1 to 6, K = 4)
# =============================================================================
cat("\n--> [1/2] Estimating Books Multivariate Binary Model (9 Book Types, K = 4)...\n")

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
  dplyr::select(egoid, wave, time, book_clean, pref_binary) %>%
  pivot_wider(names_from = book_clean, values_from = pref_binary) %>%
  filter(complete.cases(.)) %>%
  inner_join(covs, by = "egoid") %>%
  arrange(egoid, time)

book_cols <- grep("^book_", names(df_books_comp), value = TRUE)
specs_books <- lapply(book_cols, function(col) {
  FLXMRglm(as.formula(paste0("cbind(", col, ", 1 - ", col, ") ~ ns(time, df = 2)")), family = "binomial")
})

set.seed(2026)
mod_books_final4 <- flexmix(
  as.formula(paste0("cbind(", paste(book_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")),
  data = df_books_comp, k = 4, model = specs_books,
  concomitant = FLXPmultinom(form_full),
  control = list(iter.max = 300, minprior = 0.02)
)

saveRDS(mod_books_final4, "Cache/mod_books_k4_fit.rds")

t_smooth_books <- seq(0, 5, length.out = 80)
t_points_books <- 0:5
basis_books <- ns(df_books_comp$time, df = 2)
eval_smooth_books <- predict(basis_books, t_smooth_books)
eval_points_books <- predict(basis_books, t_points_books)

params_books <- parameters(mod_books_final4)

# Class profiles:
# Comp 1 (n=97, 48.3%): Genre Specialists (Sci-Fi, Thrillers, Mysteries, History)
# Comp 2 (n=40, 19.9%): Romance Readers (High Romance & Other Fiction)
# Comp 3 (n=32, 15.9%): Nonfictionists (High History, Biography, Non-Fiction)
# Comp 4 (n=32, 15.9%): Omnivorous Fictionists (High Across All Fiction Categories)
books_labels_map <- c(
  "1" = "Genre Specialists",
  "2" = "Romance Readers",
  "3" = "Nonfictionists",
  "4" = "Omnivorous Fictionists"
)

books_smooth_list <- list()
for (bk_idx in 1:9) {
  col <- book_cols[bk_idx]
  bk_label <- book_clean_labels[col]
  for (c_idx in 1:4) {
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
  for (c_idx in 1:4) {
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
cat("   Saved: Cache/summaries/books_trajectories.rds (K = 4)\n")

# =============================================================================
# B. MUSIC GENRE PREFERENCES (Top 10 Genres, Waves 1 to 6, K = 4)
# =============================================================================
cat("\n--> [2/2] Estimating Music Multivariate Binary Model (Top 10 Genres, K = 4)...\n")

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
  dplyr::select(egoid, wave, time, genre_clean, pref_binary) %>%
  pivot_wider(names_from = genre_clean, values_from = pref_binary) %>%
  filter(complete.cases(.)) %>%
  inner_join(covs, by = "egoid") %>%
  arrange(egoid, time)

music_cols <- grep("^(rap|classic|dance|rock|country|broadway|classical|mood|folk|jazz)", names(df_music_wide), value = TRUE)
specs_music <- lapply(music_cols, function(col) {
  FLXMRglm(as.formula(paste0("cbind(", col, ", 1 - ", col, ") ~ ns(time, df = 2)")), family = "binomial")
})

set.seed(2026)
mod_music_final4 <- flexmix(
  as.formula(paste0("cbind(", paste(music_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")),
  data = df_music_wide, k = 4, model = specs_music,
  concomitant = FLXPmultinom(form_full),
  control = list(iter.max = 300, minprior = 0.02)
)

saveRDS(mod_music_final4, "Cache/mod_music_k4_fit.rds")

t_smooth_music <- seq(0, 5, length.out = 80)
t_points_music <- 0:5
basis_music <- ns(df_music_wide$time, df = 2)
eval_smooth_music <- predict(basis_music, t_smooth_music)
eval_points_music <- predict(basis_music, t_points_music)

params_music <- parameters(mod_music_final4)

# Class profiles:
# Comp 1 (n=40, 19.9%): Classic Rockers (Classic Rock, Rock, Classical, Broadway, Jazz)
# Comp 2 (n=60, 29.9%): Mainstreamers (Rap, Dance, Country; no Rock)
# Comp 3 (n=48, 23.9%): Contemporary Rockers (Rap, Rock, Classic Rock, Dance; no Broadway/Classical)
# Comp 4 (n=53, 26.4%): Omnivores (High Across All 10 Genres)
music_labels_map <- c(
  "1" = "Classic Rockers",
  "2" = "Mainstreamers",
  "3" = "Contemporary Rockers",
  "4" = "Omnivores"
)

music_smooth_list <- list()
for (m_idx in 1:10) {
  col <- music_cols[m_idx]
  m_label <- genre_clean_labels[col]
  for (c_idx in 1:4) {
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
  for (c_idx in 1:4) {
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
cat("   Saved: Cache/summaries/music_trajectories.rds (K = 4)\n")

# Helper function to extract robust multinom concomitants for K = 4
extract_concom4 <- function(mod, df_comp, domain_name, labels_map, ref_level = "Omnivores") {
  df_ego <- df_comp %>%
    mutate(clust = factor(clusters(mod))) %>%
    group_by(egoid) %>%
    summarize(clust = names(sort(table(clust), decreasing = TRUE)[1]), .groups = "drop") %>%
    mutate(class_label = labels_map[as.character(clust)]) %>%
    mutate(class_label = relevel(factor(class_label), ref = ref_level)) %>%
    inner_join(covs, by = "egoid")
    
  m_mnl <- multinom(
    class_label ~ is_woman + is_white + is_catholic + income_num + parent_ed_years + 
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
        Comparison = paste(cls, "vs.", ref_level),
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

concom_books4 <- extract_concom4(mod_books_final4, df_books_comp, "Books", books_labels_map, ref_level = "Genre Specialists")
concom_music4 <- extract_concom4(mod_music_final4, df_music_wide, "Music", music_labels_map, ref_level = "Omnivores")

saveRDS(list(books = concom_books4, music = concom_music4), "Cache/summaries/multinomial_concomitants_k4.rds")
cat("   Saved: Cache/summaries/multinomial_concomitants_k4.rds\n")

cat("\nModeling Pipeline Complete!\n")
