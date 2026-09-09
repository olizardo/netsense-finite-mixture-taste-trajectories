#' Generate Model-Implied Marginal Class Probability Plots
#'
#' Computes model-implied marginal predicted class probabilities and 95% simulation
#' confidence intervals for statistically significant predictor blocks from Table 3
#' across all three cultural taste domains (Arts [9 items], Books [9 items], and Music [10 genres]).
#' Follows the visualization architecture from predicting-degree-trajectories-NetHealth.
#'
#' @author Omar Lizardo & AI Assistant
#' @date 2026-09-08

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
cat("Generating Model-Implied Marginal Class Probability Plots            \n")
cat("====================================================================\n")

# 1. Load Data
demo_data  <- readRDS("demographics_longitudinal_clean.rds")
music_long <- readRDS("Cache/music_long_clean.rds")
books_long <- readRDS("Cache/books_long_clean.rds")
covs       <- readRDS("Cache/covariates_imputed.rds")

if (!dir.exists("Plots")) dir.create("Plots")
if (!dir.exists("Cache/summaries")) dir.create("Cache/summaries", recursive = TRUE)

form_full <- ~ is_woman + is_white + is_catholic + income_num + parent_ed_years + high_hs_grade + aims_advanced_degree + is_stem_major + hometown

# -----------------------------------------------------------------------------
# A. PREPARE ESTIMATION DATASETS
# -----------------------------------------------------------------------------

# Arts (Revised 9 Items)
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
  inner_join(covs, by = "egoid") %>%
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
specs_arts <- lapply(art_cols, function(col) {
  FLXMRglm(as.formula(paste0("cbind(", col, ", 1 - ", col, ") ~ ns(time, df = 2)")), family = "binomial")
})

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
# B. FIT FLEXMIX MODELS WITH CONCOMITANTS AND EXTRACT MNL
# -----------------------------------------------------------------------------
cat("--> Fitting models and extracting multinomial concomitants...\n")

fit_concom_model <- function(mod, df_comp, ref_level = "1") {
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
  return(list(model = m_mnl, data = df_ego))
}

set.seed(2026)
mod_arts <- flexmix(
  as.formula(paste0("cbind(", paste(art_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")),
  data = df_arts_comp, k = 3, model = specs_arts,
  concomitant = FLXPmultinom(form_full),
  control = list(iter.max = 300, minprior = 0.04)
)
res_arts <- fit_concom_model(mod_arts, df_arts_comp)

set.seed(2026)
mod_books <- flexmix(
  as.formula(paste0("cbind(", paste(book_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")),
  data = df_books_comp, k = 3, model = specs_books,
  concomitant = FLXPmultinom(form_full),
  control = list(iter.max = 300, minprior = 0.04)
)
res_books <- fit_concom_model(mod_books, df_books_comp)

set.seed(2026)
mod_music <- flexmix(
  as.formula(paste0("cbind(", paste(music_cols, collapse=", "), ") ~ ns(time, df = 2) | egoid")),
  data = df_music_wide, k = 3, model = specs_music,
  concomitant = FLXPmultinom(form_full),
  control = list(iter.max = 300, minprior = 0.04)
)
res_music <- fit_concom_model(mod_music, df_music_wide)

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
# D. GENERATE PREDICTIONS FOR SIGNIFICANT BLOCKS (TABLE 3)
# -----------------------------------------------------------------------------
cat("--> Generating predictions for statistically significant predictor blocks...\n")

df_covs_base <- res_arts$data %>%
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

# 1. Arts Predictions: Gender Identity & Degree Aspirations
grid_gender_arts <- bind_rows(
  df_covs_base %>% mutate(is_woman = 0, Condition = "Men"),
  df_covs_base %>% mutate(is_woman = 1, Condition = "Women")
)
ci_gender_arts <- simulate_marginal_probs(res_arts$model, grid_gender_arts, class_names = c("Omnivores", "Traditionalists", "Minimalists")) %>%
  mutate(Condition = rep(grid_gender_arts$Condition, 3), Predictor = "Gender Identity")

grid_deg_arts <- bind_rows(
  df_covs_base %>% mutate(aims_advanced_degree = 0, Condition = "BA Only"),
  df_covs_base %>% mutate(aims_advanced_degree = 1, Condition = "Post-BA Aspirations")
)
ci_deg_arts <- simulate_marginal_probs(res_arts$model, grid_deg_arts, class_names = c("Omnivores", "Traditionalists", "Minimalists")) %>%
  mutate(Condition = rep(grid_deg_arts$Condition, 3), Predictor = "Degree Aspirations")

df_plot_arts <- bind_rows(ci_gender_arts, ci_deg_arts) %>%
  mutate(
    Domain = "Arts & Cultural Events",
    Class = factor(Class, levels = c("Omnivores", "Traditionalists", "Minimalists")),
    Condition = factor(Condition, levels = rev(c("Men", "Women", "BA Only", "Post-BA Aspirations")))
  )

# 2. Books Predictions: Gender, Religion, and Collegiate Major
grid_gender_books <- bind_rows(
  df_covs_base %>% mutate(is_woman = 0, Condition = "Men"),
  df_covs_base %>% mutate(is_woman = 1, Condition = "Women")
)
ci_gender_books <- simulate_marginal_probs(res_books$model, grid_gender_books, class_names = c("Nonfictionists", "Fictionists", "Minimalists")) %>%
  mutate(Condition = rep(grid_gender_books$Condition, 3), Predictor = "Gender Identity")

grid_rel_books <- bind_rows(
  df_covs_base %>% mutate(is_catholic = 0, Condition = "Non-Catholic"),
  df_covs_base %>% mutate(is_catholic = 1, Condition = "Roman Catholic")
)
ci_rel_books <- simulate_marginal_probs(res_books$model, grid_rel_books, class_names = c("Nonfictionists", "Fictionists", "Minimalists")) %>%
  mutate(Condition = rep(grid_rel_books$Condition, 3), Predictor = "Religion")

grid_stem_books <- bind_rows(
  df_covs_base %>% mutate(is_stem_major = 0, Condition = "Non-STEM Major"),
  df_covs_base %>% mutate(is_stem_major = 1, Condition = "STEM Major")
)
ci_stem_books <- simulate_marginal_probs(res_books$model, grid_stem_books, class_names = c("Nonfictionists", "Fictionists", "Minimalists")) %>%
  mutate(Condition = rep(grid_stem_books$Condition, 3), Predictor = "Undergraduate Major")

df_plot_books <- bind_rows(ci_gender_books, ci_rel_books, ci_stem_books) %>%
  mutate(
    Domain = "Book Reading Types",
    Class = factor(Class, levels = c("Nonfictionists", "Fictionists", "Minimalists")),
    Condition = factor(Condition, levels = rev(c("Men", "Women", "Non-Catholic", "Roman Catholic", "Non-STEM Major", "STEM Major")))
  )

# 3. Music Predictions: High School GPA, Gender, and Collegiate Major
grid_gpa_music <- bind_rows(
  df_covs_base %>% mutate(high_hs_grade = 0, Condition = "B+ or Lower GPA"),
  df_covs_base %>% mutate(high_hs_grade = 1, Condition = "Mostly A/A- GPA")
)
ci_gpa_music <- simulate_marginal_probs(res_music$model, grid_gpa_music, class_names = c("Omnivores", "Rockers", "Mainstreamers")) %>%
  mutate(Condition = rep(grid_gpa_music$Condition, 3), Predictor = "High School Grades")

grid_gender_music <- bind_rows(
  df_covs_base %>% mutate(is_woman = 0, Condition = "Men"),
  df_covs_base %>% mutate(is_woman = 1, Condition = "Women")
)
ci_gender_music <- simulate_marginal_probs(res_music$model, grid_gender_music, class_names = c("Omnivores", "Rockers", "Mainstreamers")) %>%
  mutate(Condition = rep(grid_gender_music$Condition, 3), Predictor = "Gender Identity")

grid_stem_music <- bind_rows(
  df_covs_base %>% mutate(is_stem_major = 0, Condition = "Non-STEM Major"),
  df_covs_base %>% mutate(is_stem_major = 1, Condition = "STEM Major")
)
ci_stem_music <- simulate_marginal_probs(res_music$model, grid_stem_music, class_names = c("Omnivores", "Rockers", "Mainstreamers")) %>%
  mutate(Condition = rep(grid_stem_music$Condition, 3), Predictor = "Undergraduate Major")

df_plot_music <- bind_rows(ci_gpa_music, ci_gender_music, ci_stem_music) %>%
  mutate(
    Domain = "Music Genre Preferences",
    Class = factor(Class, levels = c("Omnivores", "Rockers", "Mainstreamers")),
    Condition = factor(Condition, levels = rev(c("B+ or Lower GPA", "Mostly A/A- GPA", "Men", "Women", "Non-STEM Major", "STEM Major")))
  )

# Save intermediate tabular summaries
saveRDS(list(arts = df_plot_arts, books = df_plot_books, music = df_plot_music), "Cache/summaries/marginal_effects_summary.rds")
cat("   Saved: Cache/summaries/marginal_effects_summary.rds\n")

# -----------------------------------------------------------------------------
# E. VISUALIZATION (PALETTES & THEMES)
# -----------------------------------------------------------------------------
cat("--> Generating publication-grade figures...\n")

PALETTE_ARTS  <- c("Omnivores" = "#0072B2", "Traditionalists" = "#D55E00", "Minimalists" = "#56B4E9")
PALETTE_BOOKS <- c("Nonfictionists" = "#009E73", "Fictionists" = "#0072B2", "Minimalists" = "#D55E00")
PALETTE_MUSIC <- c("Omnivores" = "#0072B2", "Rockers" = "#D55E00", "Mainstreamers" = "#E69F00")

theme_facet_pub <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5, margin = margin(b = 3)),
      plot.subtitle = element_text(size = 8.5, hjust = 0.5, color = "grey35", margin = margin(b = 6)),
      strip.text = element_text(face = "bold", size = 9.5, lineheight = 1.1),
      strip.background = element_rect(fill = "grey95", color = NA),
      axis.title.x = element_text(size = 9.5, margin = margin(t = 5)),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 8.5, color = "black"),
      axis.text.x = element_text(size = 8),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(0.7, "lines"),
      plot.margin = margin(t = 6, r = 8, b = 4, l = 8)
    )
}

# 1. Arts Plot
p_arts <- ggplot(df_plot_arts, aes(x = Mean, y = Condition, color = Class)) +
  geom_vline(xintercept = 1/3, linetype = "dashed", color = "grey65", linewidth = 0.4) +
  geom_pointrange(aes(xmin = pmax(0, Low), xmax = pmin(1, High)), size = 0.45, linewidth = 0.75) +
  facet_wrap(~ Class, ncol = 3) +
  scale_color_manual(values = PALETTE_ARTS) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Panel A: Public Arts & Cultural Events (N = 199)",
    subtitle = "Adjusted predicted class probabilities with 95% simulation CIs across Gender and Degree Aspirations",
    x = NULL
  ) +
  theme_facet_pub()

# 2. Books Plot
p_books <- ggplot(df_plot_books, aes(x = Mean, y = Condition, color = Class)) +
  geom_vline(xintercept = 1/3, linetype = "dashed", color = "grey65", linewidth = 0.4) +
  geom_pointrange(aes(xmin = pmax(0, Low), xmax = pmin(1, High)), size = 0.45, linewidth = 0.75) +
  facet_wrap(~ Class, ncol = 3) +
  scale_color_manual(values = PALETTE_BOOKS) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Panel B: Leisure Book Reading Types (N = 201)",
    subtitle = "Adjusted predicted class probabilities with 95% simulation CIs across Gender, Religion, and Academic Major",
    x = NULL
  ) +
  theme_facet_pub()

# 3. Music Plot
p_music <- ggplot(df_plot_music, aes(x = Mean, y = Condition, color = Class)) +
  geom_vline(xintercept = 1/3, linetype = "dashed", color = "grey65", linewidth = 0.4) +
  geom_pointrange(aes(xmin = pmax(0, Low), xmax = pmin(1, High)), size = 0.45, linewidth = 0.75) +
  facet_wrap(~ Class, ncol = 3) +
  scale_color_manual(values = PALETTE_MUSIC) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Panel C: Musical Genre Preferences (N = 201)",
    subtitle = "Adjusted predicted class probabilities with 95% simulation CIs across High School GPA, Gender, and Academic Major",
    x = "Model-Implied Predicted Class Probability"
  ) +
  theme_facet_pub()

# Save Standalone Plots
ggsave("Plots/fig4_marginal_arts.png", p_arts, width = 6.5, height = 2.8, dpi = 300)
ggsave("Plots/fig5_marginal_books.png", p_books, width = 6.5, height = 3.4, dpi = 300)
ggsave("Plots/fig6_marginal_music.png", p_music, width = 6.5, height = 3.4, dpi = 300)

# Save Unified Compound Figure 4
png("Plots/fig4_marginal_effects_all_domains.png", width = 6.5, height = 7.1, units = "in", res = 300)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(3, 1, heights = grid::unit(c(2.0, 2.55, 2.55), "null"))))
print(p_arts, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_books, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_music, vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 1))
dev.off()

cat("All plots generated successfully:\n")
cat(" - Plots/fig4_marginal_effects_all_domains.png (Unified compound figure)\n")
cat(" - Plots/fig4_marginal_arts.png\n")
cat(" - Plots/fig5_marginal_books.png\n")
cat(" - Plots/fig6_marginal_music.png\n")
cat("====================================================================\n")
