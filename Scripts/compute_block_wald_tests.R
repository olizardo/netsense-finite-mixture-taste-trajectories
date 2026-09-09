#' Compute Block-Level Predictive Power (Wald / LRT Statistics) for Latent Class Assignment
#'
#' Evaluates the overall predictive power of theoretical blocks of variables:
#' Block 1: Demographic Identity (Gender, Race, Religion)
#' Block 2: Family Socioeconomic Status (Income, Education)
#' Block 3: Pre-Collegiate Scholastic Capital (GPA, Degree Aspirations)
#' Block 4: Collegiate Context (STEM Major, Hometown)
#' Block 1+2: All Sociodemographics
#' Full Multivariable Model (All Blocks)
#'
#' Computes Likelihood Ratio Chi-Square, Wald Chi-Square, degrees of freedom, p-values,
#' AIC, and BIC across Arts Events, Book Types, and Music Genres.

suppressPackageStartupMessages({
  library(flexmix)
  library(dplyr)
  library(tidyr)
  library(splines)
  library(nnet)
})

covs <- readRDS("Cache/covariates_imputed.rds")
demo_data <- readRDS("demographics_longitudinal_clean.rds")
music_long <- readRDS("Cache/music_long_clean.rds")
books_long <- readRDS("Cache/books_long_clean.rds")

# Helper function to extract student ego-level modal class assignment
get_ego_classes <- function(mod, df_data) {
  df_data %>%
    mutate(clust = factor(clusters(mod))) %>%
    group_by(egoid) %>%
    summarize(class = factor(names(sort(table(clust), decreasing = TRUE)[1])), .groups = "drop") %>%
    inner_join(covs, by = "egoid")
}

test_blocks <- function(df_ego, domain_name) {
  m_null <- multinom(class ~ 1, data = df_ego, trace = FALSE)
  m_full <- multinom(class ~ is_woman + is_white + is_catholic + income_num + parent_ed_years + 
                       high_hs_grade + aims_advanced_degree + is_stem_major + hometown, 
                     data = df_ego, trace = FALSE)
  
  m_demo <- multinom(class ~ is_woman + is_white + is_catholic, data = df_ego, trace = FALSE)
  m_ses  <- multinom(class ~ income_num + parent_ed_years, data = df_ego, trace = FALSE)
  m_acad <- multinom(class ~ high_hs_grade + aims_advanced_degree, data = df_ego, trace = FALSE)
  m_coll <- multinom(class ~ is_stem_major + hometown, data = df_ego, trace = FALSE)
  m_all_demo <- multinom(class ~ is_woman + is_white + is_catholic + income_num + parent_ed_years, data = df_ego, trace = FALSE)
  
  models <- list(
    "Block 1: Demographic Identity (Gender, Race, Religion)" = m_demo,
    "Block 2: Family Socioeconomic Status (Income, Education)" = m_ses,
    "Block 3: Pre-Collegiate Scholastic Capital (GPA, Degree Aspirations)" = m_acad,
    "Block 4: Collegiate Context (STEM Major, Hometown Urbanicity)" = m_coll,
    "Block 1+2: Combined Sociodemographics (Demographics + SES)" = m_all_demo,
    "Full Multivariable Model (All Blocks Combined)" = m_full
  )
  
  res <- list()
  for (nm in names(models)) {
    m <- models[[nm]]
    lrt <- 2 * (logLik(m)[1] - logLik(m_null)[1])
    df_diff <- length(coef(m)) - length(coef(m_null))
    p_val <- 1 - pchisq(lrt, df_diff)
    
    # Also compute Wald Chi-Square: b^T * V^-1 * b
    b <- as.vector(coef(m))
    v <- vcov(m)
    # Exclude intercept elements if testing predictors only
    int_idx <- grep("(Intercept)", names(b))
    if (length(int_idx) > 0) {
      b_pred <- b[-int_idx]
      v_pred <- v[-int_idx, -int_idx]
    } else {
      b_pred <- b
      v_pred <- v
    }
    wald_stat <- tryCatch(
      as.numeric(t(b_pred) %*% solve(v_pred) %*% b_pred),
      error = function(e) as.numeric(t(b_pred) %*% MASS::ginv(v_pred) %*% b_pred)
    )
    wald_p <- 1 - pchisq(wald_stat, length(b_pred))
    
    res[[nm]] <- data.frame(
      Domain = domain_name,
      Block = nm,
      LRT_Chi2 = lrt,
      Wald_Chi2 = wald_stat,
      df = df_diff,
      p_val = p_val,
      AIC = AIC(m),
      BIC = BIC(m),
      stringsAsFactors = FALSE
    )
  }
  bind_rows(res)
}

# 1. Arts Data (Revised 9 Items: Classical & Opera combined; Rock/Pop & Folk/Country combined; Science Museum removed)
cult_cols <- grep("^egoid|^culturalevents[0-9]+_[1-6]$", names(demo_data), value = TRUE)
df_arts_comp <- demo_data %>% 
  dplyr::select(all_of(cult_cols)) %>%
  pivot_longer(cols = starts_with("culturalevents"), names_to = c("event_type", "wave"), names_pattern = "culturalevents([0-9]+)_([1-6])", values_to = "preference") %>%
  mutate(wave = as.numeric(wave), time = wave - 1, pref_binary = case_when(preference == "Yes" ~ 1, preference == "No" ~ 0, TRUE ~ NA_real_), event_clean = paste0("event_", event_type)) %>%
  filter(!is.na(pref_binary)) %>% 
  dplyr::select(egoid, wave, time, event_clean, pref_binary) %>%
  pivot_wider(names_from = event_clean, values_from = pref_binary) %>% 
  filter(complete.cases(.)) %>%
  mutate(
    art_classical_opera = as.numeric(event_2 == 1 | event_4 == 1),
    art_rock_folk_country = as.numeric(event_1 == 1 | event_3 == 1),
    art_ballet_dance = event_5,
    art_jazz_blues = event_6,
    art_musical_theater = event_7,
    art_stage_play = event_8,
    art_comedy_club = event_9,
    art_art_museum = event_10,
    art_cinema = event_12
  ) %>%
  dplyr::select(
    egoid, wave, time,
    art_classical_opera,
    art_rock_folk_country,
    art_ballet_dance,
    art_jazz_blues,
    art_musical_theater,
    art_stage_play,
    art_comedy_club,
    art_art_museum,
    art_cinema
  ) %>%
  arrange(egoid, time)

art_cols <- c(
  "art_classical_opera",
  "art_rock_folk_country",
  "art_ballet_dance",
  "art_jazz_blues",
  "art_musical_theater",
  "art_stage_play",
  "art_comedy_club",
  "art_art_museum",
  "art_cinema"
)
specs_arts <- lapply(art_cols, function(col) FLXMRglm(as.formula(paste0("cbind(", col, ", 1 - ", col, ") ~ ns(time, df = 2)")), family = "binomial"))
set.seed(2026)
m_arts <- flexmix(as.formula(paste0("cbind(", paste(art_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")), data = df_arts_comp, k = 3, model = specs_arts, control = list(iter.max = 250, minprior = 0.04))
df_arts_ego <- get_ego_classes(m_arts, df_arts_comp)

# 2. Books Data
df_books_comp <- books_long %>% 
  mutate(book_clean = paste0("book_", book_type), time = wave - 1) %>% 
  select(egoid, wave, time, book_clean, pref_binary) %>% 
  pivot_wider(names_from = book_clean, values_from = pref_binary) %>% 
  filter(complete.cases(.)) %>% 
  arrange(egoid, time)

book_cols <- grep("^book_", names(df_books_comp), value = TRUE)
specs_books <- lapply(book_cols, function(col) FLXMRglm(as.formula(paste0("cbind(", col, ", 1 - ", col, ") ~ ns(time, df = 2)")), family = "binomial"))
set.seed(2026)
m_books <- flexmix(as.formula(paste0("cbind(", paste(book_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")), data = df_books_comp, k = 3, model = specs_books, control = list(iter.max = 200, minprior = 0.04))
df_books_ego <- get_ego_classes(m_books, df_books_comp)

# 3. Music Data
top10_genres_order <- c("Rap/Hip-hop", "Classic rock/Oldies", "Dance music", "Rock/Heavy metal", "Country", "Broadway/Show tunes", "Classical/Chamber", "Mood/Easy listening", "Folk music", "Jazz")
df_music_wide <- music_long %>% 
  filter(genre_label %in% top10_genres_order, !is.na(pref_binary)) %>% 
  mutate(genre_clean = gsub("[^a-zA-Z0-9]", "_", tolower(genre_label)), time = wave - 1) %>% 
  select(egoid, wave, time, genre_clean, pref_binary) %>% 
  pivot_wider(names_from = genre_clean, values_from = pref_binary) %>% 
  filter(complete.cases(.)) %>% 
  arrange(egoid, time)

music_cols <- grep("^(rap|classic|dance|rock|country|broadway|classical|mood|folk|jazz)", names(df_music_wide), value = TRUE)
specs_music <- lapply(music_cols, function(col) FLXMRglm(as.formula(paste0("cbind(", col, ", 1 - ", col, ") ~ ns(time, df = 2)")), family = "binomial"))
set.seed(2026)
m_music <- flexmix(as.formula(paste0("cbind(", paste(music_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")), data = df_music_wide, k = 3, model = specs_music, control = list(iter.max = 200, minprior = 0.04))
df_music_ego <- get_ego_classes(m_music, df_music_wide)

# Run block comparisons
t_arts <- test_blocks(df_arts_ego, "Arts & Cultural Events")
t_books <- test_blocks(df_books_ego, "Book Reading Types")
t_music <- test_blocks(df_music_ego, "Music Genre Preferences")

all_blocks <- bind_rows(t_arts, t_books, t_music)
write.csv(all_blocks, "Cache/summaries/table_block_predictive_power.csv", row.names = FALSE)

# Generate APA Markdown Table
md_blocks <- c(
  "| Domain & Predictor Block | Wald $\\chi^2$ | LRT $\\chi^2$ | $df$ | $p$-value | AIC | BIC |",
  "|:---|:---:|:---:|:---:|:---:|:---:|:---:|"
)

for (dom in unique(all_blocks$Domain)) {
  md_blocks <- c(md_blocks, sprintf("| **%s** | | | | | | |", dom))
  sub_df <- all_blocks %>% filter(Domain == dom)
  for (i in 1:nrow(sub_df)) {
    r <- sub_df[i, ]
    p_str <- if (r$p_val < 0.001) "< .001" else sprintf("%.3f", r$p_val)
    md_blocks <- c(md_blocks, sprintf(
      "| %s | %.2f | %.2f | %d | %s | %.1f | %.1f |",
      r$Block, r$Wald_Chi2, r$LRT_Chi2, r$df, p_str, r$AIC, r$BIC
    ))
  }
}
md_blocks <- c(
  md_blocks,
  "| | | | | | | |",
  "*Note:* Likelihood Ratio Tests (LRT $\\chi^2$) and Wald $\\chi^2$ statistics evaluate the joint predictive capacity of each variable block relative to an empty baseline model ($N = 201$ complete panel). Degrees of freedom ($df$) reflect $2 \\times$ the number of predictors corresponding to contrasts against Class 1."
)

writeLines(md_blocks, "Cache/summaries/table_block_predictive_power.md")
cat("Saved: Cache/summaries/table_block_predictive_power.md and .csv\n")
