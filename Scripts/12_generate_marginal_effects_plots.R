#' Generate Model-Implied Marginal Class Probability Plots (K = 4)
#'
#' Computes model-implied marginal predicted class probabilities and 95% simulation
#' confidence intervals for statistically significant predictor blocks from Table 3
#' across Leisure Book Reading Types (9 items, K = 4) and Musical Genre Preferences (10 genres, K = 4).
#'
#' @author Omar Lizardo & AI Assistant
#' @date 2026-09-09

suppressPackageStartupMessages({
  library(flexmix)
  library(splines)
  library(nnet)
  library(MASS)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid)
})

cat("====================================================================\n")
cat("Generating Model-Implied Marginal Class Probability Plots (K = 4)   \n")
cat("====================================================================\n")

# 1. Load Data
music_long <- readRDS("Cache/music_long_clean.rds")
books_long <- readRDS("Cache/books_long_clean.rds")
covs       <- readRDS("Cache/covariates_imputed.rds")

if (!dir.exists("Plots")) dir.create("Plots")
if (!dir.exists("Cache/summaries")) dir.create("Cache/summaries", recursive = TRUE)

form_full <- ~ is_woman + is_white + is_catholic + income_num + parent_ed_years + high_hs_grade + aims_advanced_degree + is_stem_major + hometown

# -----------------------------------------------------------------------------
# A. PREPARE ESTIMATION DATASETS
# -----------------------------------------------------------------------------

# Books
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

# Music
top10_genres_order <- c("Rap/Hip-hop", "Classic rock/Oldies", "Dance music", "Rock/Heavy metal", "Country", "Broadway/Show tunes", "Classical/Chamber", "Mood/Easy listening", "Folk music", "Jazz")
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

# -----------------------------------------------------------------------------
# B. FIT CONCOMITANT MULTINOMIAL MODELS (K = 4)
# -----------------------------------------------------------------------------

books_labels_map <- c(
  "1" = "Nonfictionists",
  "2" = "Romance Readers",
  "3" = "Genre Specialists",
  "4" = "Omnivorous Fictionists"
)

music_labels_map <- c(
  "1" = "Classic Rockers",
  "2" = "Mainstreamers",
  "3" = "Contemporary Rockers",
  "4" = "Omnivores"
)

fit_concom_model4 <- function(mod, df_comp, labels_map, ref_level) {
  df_ego <- df_comp %>%
    mutate(clust = factor(clusters(mod))) %>%
    group_by(egoid) %>%
    summarize(clust = names(sort(table(clust), decreasing = TRUE)[1]), .groups = "drop") %>%
    mutate(class_name = labels_map[as.character(clust)]) %>%
    mutate(class_name = relevel(factor(class_name), ref = ref_level)) %>%
    inner_join(covs, by = "egoid")
    
  m_mnl <- multinom(
    class_name ~ is_woman + is_white + is_catholic + income_num + parent_ed_years + 
      high_hs_grade + aims_advanced_degree + is_stem_major + hometown,
    data = df_ego, trace = FALSE
  )
  return(list(model = m_mnl, data = df_ego))
}

mod_books4 <- readRDS("Cache/mod_books_k4_fit.rds")
res_books <- fit_concom_model4(mod_books4, df_books_comp, books_labels_map, ref_level = "Genre Specialists")

mod_music4 <- readRDS("Cache/mod_music_k4_fit.rds")
res_music <- fit_concom_model4(mod_music4, df_music_wide, music_labels_map, ref_level = "Omnivores")

# -----------------------------------------------------------------------------
# C. SIMULATION FUNCTION FOR MARGINAL CLASS PROBABILITIES
# -----------------------------------------------------------------------------
simulate_marginal_probs <- function(model, newdata, n_draws = 2000, class_names = NULL) {
  coef_mat <- coef(model)
  K_minus_1 <- nrow(coef_mat)
  P <- ncol(coef_mat)
  
  mu_vec <- as.vector(t(coef_mat))
  Sigma_mat <- vcov(model)
  
  set.seed(2026)
  beta_draws <- MASS::mvrnorm(n_draws, mu = mu_vec, Sigma = Sigma_mat)
  
  X_mat <- model.matrix(formula(model)[-2], data = newdata)
  n_obs <- nrow(X_mat)
  K <- K_minus_1 + 1
  prob_draws <- array(0, dim = c(n_obs, K, n_draws))
  
  for (d in 1:n_draws) {
    eta_mat <- matrix(0, nrow = n_obs, ncol = K)
    for (k in 2:K) {
      idx_start <- (k - 2) * P + 1
      idx_end   <- (k - 1) * P
      b_k <- beta_draws[d, idx_start:idx_end]
      eta_mat[, k] <- as.vector(X_mat %*% b_k)
    }
    max_eta <- apply(eta_mat, 1, max)
    exp_eta <- exp(eta_mat - max_eta)
    sum_exp <- rowSums(exp_eta)
    for (k in 1:K) {
      prob_draws[, k, d] <- exp_eta[, k] / sum_exp
    }
  }
  
  if (is.null(class_names)) class_names <- paste0("Class ", 1:K)
  
  res_list <- list()
  for (k in 1:K) {
    res_list[[class_names[k]]] <- tibble(
      Class  = class_names[k],
      Mean   = apply(prob_draws[, k, ], 1, mean),
      Median = apply(prob_draws[, k, ], 1, median),
      Low    = apply(prob_draws[, k, ], 1, quantile, probs = 0.025),
      High   = apply(prob_draws[, k, ], 1, quantile, probs = 0.975)
    )
  }
  bind_rows(res_list)
}

# -----------------------------------------------------------------------------
# D. GENERATE PREDICTIONS FOR SIGNIFICANT BLOCKS
# -----------------------------------------------------------------------------
cat("--> Generating predictions for statistically significant predictor blocks...\n")

df_covs_base <- res_books$data %>%
  summarize(
    is_woman = 0,
    is_white = 1,
    is_catholic = 1,
    income_num = mean(income_num),
    parent_ed_years = mean(parent_ed_years),
    high_hs_grade = 1,
    aims_advanced_degree = 1,
    is_stem_major = 0,
    hometown = mean(hometown)
  )

# Books Predictors: Gender, Religion, Major
grid_gender_books <- bind_rows(
  df_covs_base %>% mutate(is_woman = 0, Condition = "Men"),
  df_covs_base %>% mutate(is_woman = 1, Condition = "Women")
)
grid_relig_books <- bind_rows(
  df_covs_base %>% mutate(is_catholic = 1, Condition = "Roman Catholic"),
  df_covs_base %>% mutate(is_catholic = 0, Condition = "Non-Catholic")
)
grid_stem_books <- bind_rows(
  df_covs_base %>% mutate(is_stem_major = 0, Condition = "Non-STEM Major"),
  df_covs_base %>% mutate(is_stem_major = 1, Condition = "STEM Major")
)

book_classes <- c("Genre Specialists", "Romance Readers", "Nonfictionists", "Omnivorous Fictionists")

ci_gender_books <- simulate_marginal_probs(res_books$model, grid_gender_books, class_names = book_classes) %>%
  mutate(Condition = rep(grid_gender_books$Condition, 4), Predictor = "Gender Identity")

ci_relig_books <- simulate_marginal_probs(res_books$model, grid_relig_books, class_names = book_classes) %>%
  mutate(Condition = rep(grid_relig_books$Condition, 4), Predictor = "Religious Identity")

ci_stem_books <- simulate_marginal_probs(res_books$model, grid_stem_books, class_names = book_classes) %>%
  mutate(Condition = rep(grid_stem_books$Condition, 4), Predictor = "Undergraduate Major")

df_plot_books <- bind_rows(ci_gender_books, ci_relig_books, ci_stem_books) %>%
  mutate(
    Domain = "Leisure Book Reading Types",
    Class = factor(Class, levels = book_classes),
    Condition = factor(Condition, levels = rev(c("Men", "Women", "Roman Catholic", "Non-Catholic", "Non-STEM Major", "STEM Major")))
  )

# Music Predictors: High School GPA, Gender, Major
grid_gpa_music <- bind_rows(
  df_covs_base %>% mutate(high_hs_grade = 0, Condition = "B+ or Lower GPA"),
  df_covs_base %>% mutate(high_hs_grade = 1, Condition = "Mostly A/A- GPA")
)
grid_gender_music <- bind_rows(
  df_covs_base %>% mutate(is_woman = 0, Condition = "Men"),
  df_covs_base %>% mutate(is_woman = 1, Condition = "Women")
)
grid_stem_music <- bind_rows(
  df_covs_base %>% mutate(is_stem_major = 0, Condition = "Non-STEM Major"),
  df_covs_base %>% mutate(is_stem_major = 1, Condition = "STEM Major")
)

music_classes <- c("Omnivores", "Classic Rockers", "Contemporary Rockers", "Mainstreamers")

ci_gpa_music <- simulate_marginal_probs(res_music$model, grid_gpa_music, class_names = music_classes) %>%
  mutate(Condition = rep(grid_gpa_music$Condition, 4), Predictor = "High School Grades")

ci_gender_music <- simulate_marginal_probs(res_music$model, grid_gender_music, class_names = music_classes) %>%
  mutate(Condition = rep(grid_gender_music$Condition, 4), Predictor = "Gender Identity")

ci_stem_music <- simulate_marginal_probs(res_music$model, grid_stem_music, class_names = music_classes) %>%
  mutate(Condition = rep(grid_stem_music$Condition, 4), Predictor = "Undergraduate Major")

df_plot_music <- bind_rows(ci_gpa_music, ci_gender_music, ci_stem_music) %>%
  mutate(
    Domain = "Music Genre Preferences",
    Class = factor(Class, levels = music_classes),
    Condition = factor(Condition, levels = rev(c("B+ or Lower GPA", "Mostly A/A- GPA", "Men", "Women", "Non-STEM Major", "STEM Major")))
  )

# Save intermediate tabular summaries
saveRDS(list(books = df_plot_books, music = df_plot_music), "Cache/summaries/marginal_effects_summary.rds")

# -----------------------------------------------------------------------------
# E. VISUALIZATION (PALETTES & THEMES)
# -----------------------------------------------------------------------------
cat("--> Generating publication-grade figures (K = 4)...\n")

PALETTE_BOOKS4 <- c(
  "Genre Specialists"      = "#0072B2",
  "Romance Readers"        = "#CC79A7",
  "Nonfictionists"         = "#009E73",
  "Omnivorous Fictionists" = "#D55E00"
)

PALETTE_MUSIC4 <- c(
  "Omnivores"            = "#0072B2",
  "Classic Rockers"      = "#D55E00",
  "Contemporary Rockers" = "#E69F00",
  "Mainstreamers"        = "#009E73"
)

theme_facet_pub <- function(base_size = 9.5) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, hjust = 0.5, margin = margin(b = 3)),
      plot.subtitle = element_text(size = 8, hjust = 0.5, color = "grey35", margin = margin(b = 5)),
      strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
      strip.background = element_rect(fill = "grey95", color = NA),
      axis.title.x = element_text(size = 8.5, margin = margin(t = 4)),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 8, color = "black"),
      axis.text.x = element_text(size = 7.5),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      plot.margin = margin(t = 4, r = 6, b = 4, l = 6)
    )
}

# Reference lines: overall probability of falling in each class
ref_books <- df_plot_books %>%
  distinct(Class) %>%
  left_join(
    res_books$data %>% count(class_name) %>% mutate(ref_prob = n / sum(n)),
    by = c("Class" = "class_name")
  )

ref_music <- df_plot_music %>%
  distinct(Class) %>%
  left_join(
    res_music$data %>% count(class_name) %>% mutate(ref_prob = n / sum(n)),
    by = c("Class" = "class_name")
  )

# 1. Books Plot (4 panels, free x-axis, class-specific reference line)
p_books <- ggplot(df_plot_books, aes(x = Mean, y = Condition, color = Class)) +
  geom_vline(data = ref_books, aes(xintercept = ref_prob), linetype = "dashed", color = "grey55", linewidth = 0.45) +
  geom_pointrange(aes(xmin = pmax(0, Low), xmax = pmin(1, High)), size = 0.4, linewidth = 0.7) +
  facet_wrap(~ Class, ncol = 4, scales = "free_x") +
  scale_color_manual(values = PALETTE_BOOKS4) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.06, 0.08))
  ) +
  labs(
    title = "Panel A: Leisure Book Reading Types (K = 4, N = 201)",
    subtitle = "Adjusted predicted class probabilities with 95% simulation CIs across Gender, Religion, and Academic Major",
    x = NULL
  ) +
  theme_facet_pub()

# 2. Music Plot (4 panels, free x-axis, class-specific reference line)
p_music <- ggplot(df_plot_music, aes(x = Mean, y = Condition, color = Class)) +
  geom_vline(data = ref_music, aes(xintercept = ref_prob), linetype = "dashed", color = "grey55", linewidth = 0.45) +
  geom_pointrange(aes(xmin = pmax(0, Low), xmax = pmin(1, High)), size = 0.4, linewidth = 0.7) +
  facet_wrap(~ Class, ncol = 4, scales = "free_x") +
  scale_color_manual(values = PALETTE_MUSIC4) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.06, 0.08))
  ) +
  labs(
    title = "Panel B: Musical Genre Preferences (K = 4, N = 201)",
    subtitle = "Adjusted predicted class probabilities with 95% simulation CIs across High School GPA, Gender, and Academic Major",
    x = "Model-Implied Predicted Class Probability"
  ) +
  theme_facet_pub()

# Save Unified Compound Figure (as both fig4 and fig5 for backwards compatibility)
png("Plots/fig4_marginal_effects_books_music.png", width = 6.5, height = 6.0, units = "in", res = 300)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1, heights = grid::unit(c(1, 1), "null"))))
print(p_books, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_music, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()

png("Plots/fig5_marginal_effects_books_music.png", width = 6.5, height = 6.0, units = "in", res = 300)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1, heights = grid::unit(c(1, 1), "null"))))
print(p_books, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_music, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()

cat("Figure saved successfully: Plots/fig4_marginal_effects_books_music.png and Plots/fig5_marginal_effects_books_music.png\n")
