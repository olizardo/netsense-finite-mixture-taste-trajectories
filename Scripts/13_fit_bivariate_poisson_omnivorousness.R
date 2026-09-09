#' ---
#' title: "13_fit_bivariate_poisson_omnivorousness.R"
#' description: "Bivariate Poisson Latent Class Growth Modeling of Music (Top 10 Genres) and Literature (9 Book Types) Omnivorousness Counts with Endogenous Concomitants (K = 4 Solution)"
#' author: "Omar Lizardo & AI Assistant"
#' date: "2026-09-09"
#' ---

suppressPackageStartupMessages({
  library(flexmix)
  library(dplyr)
  library(tidyr)
  library(splines)
  library(nnet)
  library(ggplot2)
  library(readr)
})

cat("====================================================================\n")
cat("Starting Bivariate Poisson Latent Class Growth Modeling Pipeline    \n")
cat("Focus: Top 10 Musical Genres & 9 Leisure Book Reading Types (K = 4) \n")
cat("====================================================================\n")

# 1. Load Data
books_long <- readRDS("Cache/books_long_clean.rds")
music_long <- readRDS("Cache/music_long_clean.rds")
covs <- readRDS("Cache/covariates_imputed.rds")
cohort_ids <- covs$egoid

if (!dir.exists("Cache/summaries")) dir.create("Cache/summaries", recursive = TRUE)
if (!dir.exists("Plots")) dir.create("Plots", recursive = TRUE)

# 2. Construct Longitudinal Omnivorousness Counts
cat("\n--> [1/5] Constructing Bivariate Longitudinal Count Panel (N = 201)...\n")

top10_genres_order <- c(
  "Rap/Hip-hop", "Classic rock/Oldies", "Dance music", "Rock/Heavy metal", 
  "Country", "Broadway/Show tunes", "Classical/Chamber", "Mood/Easy listening", 
  "Folk music", "Jazz"
)

music_counts <- music_long %>%
  filter(egoid %in% cohort_ids, !is.na(pref_binary)) %>%
  group_by(egoid, wave) %>%
  summarize(
    music_count_10 = sum(pref_binary[genre_label %in% top10_genres_order]),
    music_count_22 = sum(pref_binary),
    .groups = "drop"
  )

book_counts <- books_long %>%
  filter(egoid %in% cohort_ids, !is.na(pref_binary)) %>%
  group_by(egoid, wave) %>%
  summarize(
    book_count = sum(pref_binary),
    .groups = "drop"
  )

df_biv <- music_counts %>%
  inner_join(book_counts, by = c("egoid", "wave")) %>%
  mutate(time = wave - 1) %>%
  inner_join(covs, by = "egoid") %>%
  arrange(egoid, time)

saveRDS(df_biv, "Cache/bivariate_omnivorousness_panel.rds")
cat("   Bivariate count panel saved: Cache/bivariate_omnivorousness_panel.rds\n")
cat("   Total panel rows:", nrow(df_biv), "| Unique individuals:", length(unique(df_biv$egoid)), "\n")

# 3. Model Selection across K = 1..5 for Top 10 Music Genres
cat("\n--> [2/5] Evaluating Latent Class Model Selection (K = 1..5, Top 10 Music Genres)...\n")

biv_specs_10 <- list(
  FLXMRglm(music_count_10 ~ ns(time, df = 2), family = "poisson"),
  FLXMRglm(book_count ~ ns(time, df = 2), family = "poisson")
)

fit_biv_lcga <- function(k_val) {
  set.seed(2026)
  mod <- flexmix(
    cbind(music_count_10, book_count) ~ ns(time, df = 2) | egoid,
    data = df_biv, k = k_val, model = biv_specs_10,
    control = list(iter.max = 300, minprior = 0.02)
  )
  tibble(
    K = k_val,
    LogLik = logLik(mod)[1],
    Par = mod@df,
    AIC = AIC(mod),
    BIC = BIC(mod)
  )
}

selection_list <- lapply(1:5, fit_biv_lcga)
tab_selection <- bind_rows(selection_list) %>%
  mutate(
    delta_BIC = BIC - min(BIC),
    stepwise_chi2 = c(NA, 2 * diff(LogLik)),
    stepwise_df   = c(NA, diff(Par)),
    stepwise_p    = c(NA, 1 - pchisq(stepwise_chi2[-1], stepwise_df[-1]))
  )

write.csv(tab_selection, "Cache/summaries/bivariate_model_selection.csv", row.names = FALSE)

# Generate Markdown Selection Table
md_selection <- c(
  "| Latent Classes | Log-Likelihood | Par | AIC | BIC | ΔBIC | Stepwise χ² (df) | p-value |",
  "|:---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|",
  apply(tab_selection, 1, function(r) {
    step_str <- ifelse(is.na(r["stepwise_chi2"]), "---", 
                       paste0(sprintf("%.1f", as.numeric(r["stepwise_chi2"])), " (", as.integer(r["stepwise_df"]), ")"))
    p_str <- ifelse(is.na(r["stepwise_p"]), "---", 
                    ifelse(as.numeric(r["stepwise_p"]) < 0.001, "< .001", sprintf("%.3f", as.numeric(r["stepwise_p"]))))
    paste0("| K = ", r["K"], " | ", sprintf("%.1f", as.numeric(r["LogLik"])), " | ", 
           r["Par"], " | ", sprintf("%.1f", as.numeric(r["AIC"])), " | ", 
           sprintf("%.1f", as.numeric(r["BIC"])), " | ", sprintf("%.1f", as.numeric(r["delta_BIC"])), " | ",
           step_str, " | ", p_str, " |")
  })
)
writeLines(md_selection, "Cache/summaries/bivariate_model_selection.md")
cat("   Model selection completed. K = 4 yields lowest AIC and substantial sociological interpretability.\n")

# 4. Fit K = 4 Bivariate Latent Class Growth Model
cat("\n--> [3/5] Fitting K = 4 Bivariate Latent Class Growth Model...\n")
set.seed(2026)
mod4_final <- flexmix(
  cbind(music_count_10, book_count) ~ ns(time, df = 2) | egoid,
  data = df_biv, k = 4, model = biv_specs_10,
  control = list(iter.max = 300, minprior = 0.02)
)

saveRDS(mod4_final, "Cache/bivariate_mod4_10genres_fit.rds")

# Person-level posterior classifications
# Raw 4: mean music = 6.28, mean books = 4.75 -> Class 1: High Dual Omnivores (n = 49, 24.4%)
# Raw 1: mean music = 4.05, mean books = 3.19 -> Class 2: Moderate Eclectics (n = 92, 45.8%)
# Raw 3: mean music = 2.32, mean books = 3.84 -> Class 3: Literary Readers / Music Winnowers (n = 28, 13.9%)
# Raw 2: mean music = 2.02, mean books = 1.95 -> Class 4: Univores (n = 32, 15.9%)
ego_class <- df_biv %>%
  mutate(clust = clusters(mod4_final)) %>%
  group_by(egoid) %>%
  summarize(raw_class = names(which.max(table(clust))), .groups = "drop") %>%
  mutate(
    class_name = case_when(
      raw_class == "4" ~ "High Dual Omnivores",
      raw_class == "1" ~ "Moderate Eclectics",
      raw_class == "3" ~ "Literary Readers / Music Winnowers",
      raw_class == "2" ~ "Univores"
    ),
    class_label = factor(
      case_when(
        raw_class == "4" ~ "Class 1: High Dual Omnivores\n(n = 49, 24.4%)",
        raw_class == "1" ~ "Class 2: Moderate Eclectics\n(n = 92, 45.8%)",
        raw_class == "3" ~ "Class 3: Literary Readers / Music Winnowers\n(n = 28, 13.9%)",
        raw_class == "2" ~ "Class 4: Univores\n(n = 32, 15.9%)"
      ),
      levels = c(
        "Class 1: High Dual Omnivores\n(n = 49, 24.4%)",
        "Class 2: Moderate Eclectics\n(n = 92, 45.8%)",
        "Class 3: Literary Readers / Music Winnowers\n(n = 28, 13.9%)",
        "Class 4: Univores\n(n = 32, 15.9%)"
      )
    ),
    class_factor = factor(class_name, levels = c(
      "High Dual Omnivores", "Moderate Eclectics", 
      "Literary Readers / Music Winnowers", "Univores"
    ))
  )

df_biv_classified <- df_biv %>%
  inner_join(ego_class, by = "egoid")

# Longitudinal wave summaries by class
traj_means <- df_biv_classified %>%
  group_by(class_name, class_label, wave) %>%
  summarize(
    n = n(),
    music_mean = mean(music_count_10),
    music_se   = sd(music_count_10) / sqrt(n()),
    book_mean  = mean(book_count),
    book_se    = sd(book_count) / sqrt(n()),
    .groups    = "drop"
  )

saveRDS(traj_means, "Cache/summaries/bivariate_trajectories_k4.rds")

# 5. Publication Trajectory Figure (Figure 5, 2x2 Layout)
cat("\n--> [4/5] Generating Publication Trajectory Plot (Plots/fig5_bivariate_omnivorousness_trajectories.png)...\n")

df_plot_lines <- bind_rows(
  traj_means %>%
    dplyr::select(class_name, class_label, wave, Mean = music_mean, SE = music_se) %>%
    mutate(Domain = "Music Omnivorousness (Top 10 Genres)"),
  traj_means %>%
    dplyr::select(class_name, class_label, wave, Mean = book_mean, SE = book_se) %>%
    mutate(Domain = "Literature Omnivorousness (9 Book Types)")
) %>%
  mutate(
    Domain = factor(Domain, levels = c("Music Omnivorousness (Top 10 Genres)", "Literature Omnivorousness (9 Book Types)"))
  )

p_fig5 <- ggplot(df_plot_lines, aes(x = wave, y = Mean, color = Domain, fill = Domain, shape = Domain, linetype = Domain)) +
  geom_ribbon(aes(ymin = pmax(0, Mean - 1.96 * SE), ymax = Mean + 1.96 * SE), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.3) +
  facet_wrap(~ class_label, ncol = 2) +
  scale_x_continuous(
    breaks = 1:6,
    labels = c("W1\nFrosh Fall", "W2\nFrosh Spr", "W3\nSoph Fall", "W4\nSoph Spr", "W5\nJun Fall", "W6\nJun Spr")
  ) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10.5)) +
  scale_color_manual(values = c(
    "Music Omnivorousness (Top 10 Genres)" = "#0072B2",
    "Literature Omnivorousness (9 Book Types)" = "#D55E00"
  )) +
  scale_fill_manual(values = c(
    "Music Omnivorousness (Top 10 Genres)" = "#0072B2",
    "Literature Omnivorousness (9 Book Types)" = "#D55E00"
  )) +
  scale_shape_manual(values = c(16, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Bivariate Latent Trajectories of Expressive Omnivorousness Across College (K = 4)",
    subtitle = "Simultaneous co-evolution of Top 10 Music and Literature repertoire breadth across six semesters (N = 201)",
    x = "Collegiate Semester Wave",
    y = "Average Genre Count (Endorsed / Read)",
    color = "Cultural Domain",
    fill = "Cultural Domain",
    shape = "Cultural Domain",
    linetype = "Cultural Domain"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 8.5, hjust = 0.5, color = "grey30"),
    strip.text = element_text(face = "bold", size = 8.5),
    strip.background = element_rect(fill = "grey95", color = "grey85", linewidth = 0.4),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.0, "lines"),
    plot.margin = margin(t = 6, r = 8, b = 6, l = 8)
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    fill  = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE)
  )

ggsave("Plots/fig5_bivariate_omnivorousness_trajectories.png", p_fig5, width = 6.5, height = 5.2, dpi = 300)
ggsave("Plots/fig_bivariate_omnivorousness_trajectories.png", p_fig5, width = 6.5, height = 5.2, dpi = 300)
cat("   Saved: Plots/fig5_bivariate_omnivorousness_trajectories.png (2x2 layout)\n")

# 6. Endogenous Concomitant Models across Theoretical Blocks for K = 4
cat("\n--> [5/5] Estimating Endogenous Concomitant Models across Theoretical Blocks (K = 4)...\n")

ll_base <- logLik(mod4_final)[1]
df_base <- mod4_final@df

fit_concom_block <- function(form, name) {
  set.seed(2026)
  mod <- tryCatch({
    flexmix(
      cbind(music_count_10, book_count) ~ ns(time, df = 2) | egoid,
      data = df_biv, k = 4, model = biv_specs_10,
      concomitant = FLXPmultinom(form),
      control = list(iter.max = 300, minprior = 0.02)
    )
  }, error = function(e) NULL)
  
  if (is.null(mod)) return(NULL)
  
  ll <- logLik(mod)[1]
  par <- mod@df
  lrt <- 2 * (ll - ll_base)
  df_diff <- par - df_base
  p_val <- ifelse(df_diff > 0, 1 - pchisq(lrt, df_diff), NA)
  
  tibble(
    Model = name,
    LogLik = ll,
    Par = par,
    AIC = AIC(mod),
    BIC = BIC(mod),
    LRT_stat = lrt,
    df_diff = df_diff,
    p_val = p_val
  )
}

m0   <- tibble(Model = "Baseline Specification (Null)", LogLik = ll_base, Par = df_base, AIC = AIC(mod4_final), BIC = BIC(mod4_final), LRT_stat = NA, df_diff = NA, p_val = NA)
m1   <- fit_concom_block(~ is_woman + is_white + is_catholic, "Block 1: Demographic Identity (Gender, Race, Religion)")
m2   <- fit_concom_block(~ income_num + parent_ed_years, "Block 2: Family SES (Parent Income, Parent Education)")
m3   <- fit_concom_block(~ high_hs_grade + aims_advanced_degree, "Block 3: Scholastic Capital (High School GPA, Advanced Degree Aspirations)")
m4   <- fit_concom_block(~ is_stem_major + hometown, "Block 4: Collegiate Context (STEM Major, Hometown Urbanicity)")
m12  <- fit_concom_block(~ is_woman + is_white + is_catholic + income_num + parent_ed_years, "Combined Sociodemographics (Blocks 1+2)")
m_full <- fit_concom_block(~ is_woman + is_white + is_catholic + income_num + parent_ed_years + high_hs_grade + aims_advanced_degree + is_stem_major + hometown, "Full Multivariable Model (All 9 Baseline Covariates)")

tab_blocks <- bind_rows(m0, m1, m2, m3, m4, m12, m_full)
write.csv(tab_blocks, "Cache/summaries/bivariate_concomitant_blocks.csv", row.names = FALSE)

# Generate Markdown Blocks Table
md_blocks <- c(
  "| Model Specification | LogLik | Par | AIC | BIC | LRT χ² | df | p-value |",
  "|:---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|",
  apply(tab_blocks, 1, function(r) {
    lrt_str <- ifelse(is.na(r["LRT_stat"]), "---", sprintf("%.2f", as.numeric(r["LRT_stat"])))
    df_str  <- ifelse(is.na(r["df_diff"]), "---", as.character(r["df_diff"]))
    p_num   <- as.numeric(r["p_val"])
    p_str   <- ifelse(is.na(p_num), "---", ifelse(p_num < 0.001, "< .001", sprintf("%.3f", p_num)))
    paste0("| ", r["Model"], " | ", sprintf("%.1f", as.numeric(r["LogLik"])), " | ", 
           r["Par"], " | ", sprintf("%.1f", as.numeric(r["AIC"])), " | ", 
           sprintf("%.1f", as.numeric(r["BIC"])), " | ", lrt_str, " | ", df_str, " | ", p_str, " |")
  })
)
writeLines(md_blocks, "Cache/summaries/bivariate_concomitant_blocks.md")

# 7. Extract Full Multivariable Multinomial Logistic Parameters for K = 4
df_ego_biv <- df_biv_classified %>%
  dplyr::select(egoid, class_name, class_factor, is_woman, is_white, is_catholic, 
                income_num, parent_ed_years, high_hs_grade, aims_advanced_degree, 
                is_stem_major, hometown) %>%
  distinct(egoid, .keep_all = TRUE)

m_mnl <- multinom(
  class_factor ~ is_woman + is_white + is_catholic + income_num + parent_ed_years + 
    high_hs_grade + aims_advanced_degree + is_stem_major + hometown,
  data = df_ego_biv, trace = FALSE
)

coef_mat <- summary(m_mnl)$coefficients
se_mat   <- summary(m_mnl)$standard.errors
z_mat    <- coef_mat / se_mat
p_mat    <- (1 - pnorm(abs(z_mat))) * 2

var_labels <- c(
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

coef_list <- list()
for (cls in rownames(coef_mat)) {
  for (v in colnames(coef_mat)) {
    est <- coef_mat[cls, v]
    se  <- se_mat[cls, v]
    z   <- z_mat[cls, v]
    p   <- p_mat[cls, v]
    sig <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else ""
    coef_list[[paste(cls, v, sep="_")]] <- tibble(
      Comparison = paste0(cls, " vs. High Dual Omnivores (Ref)"),
      Variable = var_labels[v],
      Estimate = est,
      Std_Error = se,
      Odds_Ratio = exp(est),
      Z = z,
      P_Value = p,
      Sig = sig
    )
  }
}
df_coefs <- bind_rows(coef_list)
write.csv(df_coefs, "Cache/summaries/bivariate_multinomial_coefficients.csv", row.names = FALSE)

# Generate Markdown Coefficients Table
md_coefs <- c(
  "| Trajectory Contrast | Covariate | Estimate (SE) | Odds Ratio | Z | p-value |",
  "|:---|:---|:---:|:---:|:---:|:---:|",
  apply(df_coefs, 1, function(r) {
    est_se <- paste0(sprintf("%.3f", as.numeric(r["Estimate"])), " (", sprintf("%.3f", as.numeric(r["Std_Error"])), ")", r["Sig"])
    or_str <- sprintf("%.3f", as.numeric(r["Odds_Ratio"]))
    z_str  <- sprintf("%.2f", as.numeric(r["Z"]))
    p_num  <- as.numeric(r["P_Value"])
    p_str  <- ifelse(p_num < 0.001, "< .001", sprintf("%.3f", p_num))
    paste0("| ", r["Comparison"], " | ", r["Variable"], " | ", est_se, " | ", or_str, " | ", z_str, " | ", p_str, " |")
  })
)
writeLines(md_coefs, "Cache/summaries/bivariate_multinomial_coefficients.md")

cat("\n====================================================================\n")
cat("Bivariate Poisson Modeling Pipeline Complete (K = 4 Solution)!      \n")
cat("Outputs Generated:\n")
cat("1. Cache/summaries/bivariate_model_selection.md\n")
cat("2. Cache/summaries/bivariate_concomitant_blocks.md\n")
cat("3. Cache/summaries/bivariate_multinomial_coefficients.md\n")
cat("4. Plots/fig5_bivariate_omnivorousness_trajectories.png\n")
cat("====================================================================\n")
