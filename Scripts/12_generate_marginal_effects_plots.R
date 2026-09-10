#' Generate Model-Implied Marginal Class Probability Plots (K = 4)
#'
#' Computes model-implied marginal predicted class probabilities and 95% simulation
#' confidence intervals for the top-ranked predictors by LRT Chi-Square across
#' Leisure Book Reading Types (9 items, K = 4) and Musical Genre Preferences (10 genres, K = 4).
#'
#' @author Omar Lizardo & AI Assistant
#' @date 2026-09-09

suppressPackageStartupMessages({
  library(flexmix)
  library(nnet)
  library(MASS)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid)
  library(gridExtra)
})

cat("====================================================================\n")
cat("Generating Model-Implied Marginal Class Probability Plots (K = 4)   \n")
cat("====================================================================\n")

# 1. Load Data
covs <- readRDS("Cache/covariates_imputed.rds")
books_long <- readRDS("Cache/books_long_clean.rds")
music_long <- readRDS("Cache/music_long_clean.rds")
mod_books <- readRDS("Cache/mod_books_k4_fit.rds")
mod_music <- readRDS("Cache/mod_music_k4_fit.rds")

# Reconstruct ego datasets with modal assignments
df_books_comp <- books_long %>%
  mutate(book_clean = paste0("book_", book_type), time = wave - 1) %>%
  dplyr::select(egoid, wave, time, book_clean, pref_binary) %>%
  pivot_wider(names_from = book_clean, values_from = pref_binary) %>%
  filter(complete.cases(.)) %>%
  inner_join(covs, by = "egoid") %>%
  arrange(egoid, time)

post_b <- as.data.frame(posterior(mod_books))
post_b$egoid <- df_books_comp$egoid
df_ego_b <- post_b %>%
  group_by(egoid) %>%
  summarise(across(V1:V4, first), .groups = "drop") %>%
  mutate(class_num = max.col(cbind(V1, V2, V3, V4)),
         class = factor(class_num, levels = 1:4,
                        labels = c("Genre Specialists", "Romance Readers", "Nonfictionists", "Fictionists"))) %>%
  inner_join(covs, by = "egoid")

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

post_m <- as.data.frame(posterior(mod_music))
post_m$egoid <- df_music_wide$egoid
df_ego_m <- post_m %>%
  group_by(egoid) %>%
  summarise(across(V1:V4, first), .groups = "drop") %>%
  mutate(class_num = max.col(cbind(V1, V2, V3, V4)),
         class = factor(class_num, levels = 1:4,
                        labels = c("Classic Rockers", "Mainstreamers", "Modern Rockers", "Omnivores"))) %>%
  inner_join(covs, by = "egoid")

# Reference categories
df_ego_b$class_ref <- relevel(df_ego_b$class, ref = "Genre Specialists")
df_ego_m$class_ref <- relevel(df_ego_m$class, ref = "Omnivores")

# Fit models with Hess = TRUE
fmla_full <- class_ref ~ is_woman + is_white + is_catholic + income_num + parent_ed_years + 
                         high_hs_grade + aims_advanced_degree + hs_high_ap + 
                         hs_catholic + hs_single_sex + 
                         cult_events_sum + prior_books_read + large_music_collection + 
                         is_stem_major + hometown

m_full_b <- multinom(fmla_full, data = df_ego_b, decay = 0.05, trace = FALSE, Hess = TRUE)
m_full_m <- multinom(fmla_full, data = df_ego_m, decay = 0.05, trace = FALSE, Hess = TRUE)

saveRDS(list(books = m_full_b, music = m_full_m), "Cache/summaries/multinomial_concomitants_k4.rds")

# Helper function to construct design matrix
build_X <- function(model, newdata) {
  cols <- colnames(coef(model))
  X <- matrix(0, nrow = nrow(newdata), ncol = length(cols))
  colnames(X) <- cols
  X[, "(Intercept)"] <- 1
  for (cn in cols[-1]) {
    if (cn %in% names(newdata)) {
      X[, cn] <- newdata[[cn]]
    }
  }
  X
}

# Simulation function
simulate_probs <- function(model, newdata, n_draws = 2000) {
  coef_mat <- coef(model)
  K_minus_1 <- nrow(coef_mat)
  P <- ncol(coef_mat)
  
  mu_vec <- as.vector(t(coef_mat))
  Sigma_mat <- vcov(model)
  
  set.seed(2026)
  beta_draws <- MASS::mvrnorm(n_draws, mu = mu_vec, Sigma = Sigma_mat)
  
  X_mat <- build_X(model, newdata)
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
  
  actual_class_names <- model$lev
  
  res_list <- list()
  for (k in 1:K) {
    cls_k <- actual_class_names[k]
    res_list[[cls_k]] <- tibble(
      Class  = cls_k,
      Mean   = apply(prob_draws[, k, ], 1, mean),
      Median = apply(prob_draws[, k, ], 1, median),
      Low    = apply(prob_draws[, k, ], 1, quantile, probs = 0.025),
      High   = apply(prob_draws[, k, ], 1, quantile, probs = 0.975)
    )
  }
  bind_rows(res_list)
}

# Reference baseline covariate values (sample means)
covs_base <- covs %>% summarize(across(-egoid, mean))

# -----------------------------------------------------------------------------
# TOP-RANKED PREDICTORS: BOOKS
# 1. Gender Identity (Chi2 = 97.66)
# 2. Catholic High School (Chi2 = 13.66)
# 3. High School GPA (Chi2 = 9.84)
# 4. Prior Reading Volume (Chi2 = 10.20)
# -----------------------------------------------------------------------------
grid_b_gender <- bind_rows(covs_base %>% mutate(is_woman = 0, Condition = "Men"),
                           covs_base %>% mutate(is_woman = 1, Condition = "Women"))
grid_b_hs     <- bind_rows(covs_base %>% mutate(hs_catholic = 0, Condition = "Public/Prep High School"),
                           covs_base %>% mutate(hs_catholic = 1, Condition = "Catholic High School"))
grid_b_gpa    <- bind_rows(covs_base %>% mutate(high_hs_grade = 0, Condition = "B+ or Lower GPA"),
                           covs_base %>% mutate(high_hs_grade = 1, Condition = "Mostly A/A- GPA"))
grid_b_books  <- bind_rows(covs_base %>% mutate(prior_books_read = 3, Condition = "Low Prior Reading (3)"),
                           covs_base %>% mutate(prior_books_read = 15, Condition = "High Prior Reading (15)"))

ci_b_gender <- simulate_probs(m_full_b, grid_b_gender) %>% mutate(Condition = rep(grid_b_gender$Condition, 4), Predictor = "Gender Identity")
ci_b_hs     <- simulate_probs(m_full_b, grid_b_hs)     %>% mutate(Condition = rep(grid_b_hs$Condition, 4), Predictor = "Secondary School Type")
ci_b_gpa    <- simulate_probs(m_full_b, grid_b_gpa)    %>% mutate(Condition = rep(grid_b_gpa$Condition, 4), Predictor = "High School GPA")
ci_b_books  <- simulate_probs(m_full_b, grid_b_books)  %>% mutate(Condition = rep(grid_b_books$Condition, 4), Predictor = "Prior Reading Volume")

df_plot_books <- bind_rows(ci_b_gender, ci_b_hs, ci_b_gpa, ci_b_books) %>%
  mutate(
    Domain = "Leisure Book Reading Types",
    Class = factor(Class, levels = c("Genre Specialists", "Romance Readers", "Nonfictionists", "Fictionists")),
    Condition = factor(Condition, levels = rev(c(
      "Men", "Women",
      "Public/Prep High School", "Catholic High School",
      "B+ or Lower GPA", "Mostly A/A- GPA",
      "Low Prior Reading (3)", "High Prior Reading (15)"
    )))
  )

# -----------------------------------------------------------------------------
# TOP-RANKED PREDICTORS: MUSIC
# 1. Prior Arts Attendance (Chi2 = 29.63)
# 2. High AP Course Load (Chi2 = 18.07)
# 3. High School GPA (Chi2 = 10.18)
# 4. Gender Identity (Chi2 = 39.31)
# -----------------------------------------------------------------------------
grid_m_arts   <- bind_rows(covs_base %>% mutate(cult_events_sum = 2, Condition = "Low Prior Arts (2)"),
                           covs_base %>% mutate(cult_events_sum = 7, Condition = "High Prior Arts (7)"))
grid_m_ap     <- bind_rows(covs_base %>% mutate(hs_high_ap = 0, Condition = "Standard AP (< 5)"),
                           covs_base %>% mutate(hs_high_ap = 1, Condition = "High AP Load (>= 5)"))
grid_m_gpa    <- bind_rows(covs_base %>% mutate(high_hs_grade = 0, Condition = "B+ or Lower GPA"),
                           covs_base %>% mutate(high_hs_grade = 1, Condition = "Mostly A/A- GPA"))
grid_m_gender <- bind_rows(covs_base %>% mutate(is_woman = 0, Condition = "Men"),
                           covs_base %>% mutate(is_woman = 1, Condition = "Women"))

ci_m_arts   <- simulate_probs(m_full_m, grid_m_arts)   %>% mutate(Condition = rep(grid_m_arts$Condition, 4), Predictor = "Prior Arts Attendance")
ci_m_ap     <- simulate_probs(m_full_m, grid_m_ap)     %>% mutate(Condition = rep(grid_m_ap$Condition, 4), Predictor = "AP Course Load")
ci_m_gpa    <- simulate_probs(m_full_m, grid_m_gpa)    %>% mutate(Condition = rep(grid_m_gpa$Condition, 4), Predictor = "High School GPA")
ci_m_gender <- simulate_probs(m_full_m, grid_m_gender) %>% mutate(Condition = rep(grid_m_gender$Condition, 4), Predictor = "Gender Identity")

df_plot_music <- bind_rows(ci_m_arts, ci_m_ap, ci_m_gpa, ci_m_gender) %>%
  mutate(
    Domain = "Music Genre Preferences",
    Class = factor(Class, levels = c("Omnivores", "Classic Rockers", "Modern Rockers", "Mainstreamers")),
    Condition = factor(Condition, levels = rev(c(
      "Low Prior Arts (2)", "High Prior Arts (7)",
      "Standard AP (< 5)", "High AP Load (>= 5)",
      "B+ or Lower GPA", "Mostly A/A- GPA",
      "Men", "Women"
    )))
  )

saveRDS(list(books = df_plot_books, music = df_plot_music), "Cache/summaries/marginal_effects_summary.rds")

# -----------------------------------------------------------------------------
# PLOTTING
# -----------------------------------------------------------------------------
PALETTE_BOOKS4 <- c(
  "Genre Specialists" = "#0072B2",
  "Romance Readers"   = "#CC79A7",
  "Nonfictionists"    = "#009E73",
  "Fictionists"       = "#D55E00"
)

PALETTE_MUSIC4 <- c(
  "Omnivores"       = "#0072B2",
  "Classic Rockers" = "#D55E00",
  "Modern Rockers"  = "#E69F00",
  "Mainstreamers"   = "#009E73"
)

theme_facet_pub <- function(base_size = 9.0) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 10.0, hjust = 0.5, margin = margin(b = 3)),
      plot.subtitle = element_text(size = 7.8, hjust = 0.5, color = "grey35", margin = margin(b = 4)),
      strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
      strip.background = element_rect(fill = "grey95", color = NA),
      axis.title.x = element_text(size = 8.0, margin = margin(t = 4)),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 7.5, color = "black"),
      axis.text.x = element_text(size = 7.0),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(0.45, "lines"),
      plot.margin = margin(t = 3, r = 5, b = 3, l = 5)
    )
}

# Empirical class prevalence reference values
ref_books <- data.frame(
  Class = factor(c("Genre Specialists", "Romance Readers", "Nonfictionists", "Fictionists")),
  ref_prob = c(75/201, 47/201, 33/201, 46/201)
)

ref_music <- data.frame(
  Class = factor(c("Omnivores", "Classic Rockers", "Modern Rockers", "Mainstreamers")),
  ref_prob = c(49/201, 37/201, 53/201, 62/201)
)

# 1. Books Plot
p_books <- ggplot(df_plot_books, aes(x = Mean, y = Condition, color = Class)) +
  geom_vline(data = ref_books, aes(xintercept = ref_prob), linetype = "dashed", color = "grey55", linewidth = 0.45) +
  geom_segment(aes(x = pmax(0, Low), xend = pmin(1, High), y = Condition, yend = Condition), linewidth = 1.3, alpha = 0.15) +
  geom_point(aes(x = Mean), size = 2.8, alpha = 1.0) +
  facet_wrap(~ Class, ncol = 4, scales = "free_x") +
  scale_color_manual(values = PALETTE_BOOKS4) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.06, 0.08))
  ) +
  labs(
    title = "Panel A: Leisure Book Reading Types (K = 4, N = 201)",
    subtitle = "Top-ranked predictors: Gender Identity, Catholic High School, High School GPA, and Prior Reading Volume",
    x = NULL
  ) +
  theme_facet_pub()

# 2. Music Plot
p_music <- ggplot(df_plot_music, aes(x = Mean, y = Condition, color = Class)) +
  geom_vline(data = ref_music, aes(xintercept = ref_prob), linetype = "dashed", color = "grey55", linewidth = 0.45) +
  geom_segment(aes(x = pmax(0, Low), xend = pmin(1, High), y = Condition, yend = Condition), linewidth = 1.3, alpha = 0.15) +
  geom_point(aes(x = Mean), size = 2.8, alpha = 1.0) +
  facet_wrap(~ Class, ncol = 4, scales = "free_x") +
  scale_color_manual(values = PALETTE_MUSIC4) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.06, 0.08))
  ) +
  labs(
    title = "Panel B: Musical Genre Preferences (K = 4, N = 201)",
    subtitle = "Top-ranked predictors: Prior Arts Attendance, High AP Load, High School GPA, and Gender Identity",
    x = "Model-Implied Predicted Trajectory Class Probability"
  ) +
  theme_facet_pub()

# Combine Vertically
p_combined <- grid.arrange(
  p_books,
  p_music,
  ncol = 1,
  heights = c(1, 1)
)

ggsave("Plots/fig5_marginal_effects_books_music.png", plot = p_combined, width = 6.5, height = 8.2, dpi = 300)
cat("Successfully generated and saved Plots/fig5_marginal_effects_books_music.png (6.5 x 8.2 in, 300 DPI)\n")
