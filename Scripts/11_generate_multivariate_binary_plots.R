#' Generate Publication Plots for Multivariate Binary Trajectory Models
#'
#' Generates publication figures strictly adhering to global guidelines:
#' 6.5-inch width, 300 DPI, modernized `linewidth` aesthetic, discrete wave markers,
#' Okabe-Ito palettes with matching line colors and point shapes.
#'
#' Produces:
#' 1. Plots/fig1_arts_12_events_by_class.png (12 panels with wave markers)
#' 2. Plots/fig2_books_9_items_by_class.png (9 panels with wave markers, 3 distinct classes)
#' 3. Plots/fig3_music_10_genres_by_class.png (10 panels with wave markers)
#' 4. Plots/fig4_multivariate_concomitant_odds_ratios.png (Odds ratio forest plot)
#'
#' @author Omar Lizardo & AI Assistant
#' @date 2026-09-08

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

cat("--> Loading trajectory datasets...\n")
arts_data <- readRDS("Cache/summaries/arts_trajectories.rds")
books_data <- readRDS("Cache/summaries/books_trajectories.rds")
music_data <- readRDS("Cache/summaries/music_trajectories.rds")
df_concom_all <- read.csv("Cache/summaries/all_domains_concomitants.csv", stringsAsFactors = FALSE)

theme_publication <- function(base_size = 9.5) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "sans", color = "#222222"),
      plot.title = element_text(face = "bold", size = rel(1.05), hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "gray30", size = rel(0.85), margin = margin(b = 8)),
      plot.caption = element_text(color = "gray40", size = rel(0.70), hjust = 0, margin = margin(t = 8)),
      axis.title = element_text(face = "bold", size = rel(0.82)),
      axis.text = element_text(size = rel(0.75)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.35),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.75)),
      legend.box.margin = margin(t = -4, b = 2),
      panel.spacing = unit(0.7, "lines"),
      strip.text = element_text(face = "bold", size = rel(0.80))
    )
}

# -----------------------------------------------------------------------------
# Figure 1: All 12 Arts Events by Class (with Wave Markers)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 1: Arts Events with Wave Markers...\n")

PALETTE_ARTS <- c(
  "Class 1: Omnivorous Cultural Enthusiasts" = "#0072B2",   # Deep Blue
  "Class 2: Museum & Performing Arts Regulars" = "#E69F00", # Orange
  "Class 3: Selective / Low Attendance" = "#56B4E9"        # Sky Blue
)

SHAPES_3CLASS <- c(
  "Class 1: Omnivorous Cultural Enthusiasts" = 16,   # Circle
  "Class 2: Museum & Performing Arts Regulars" = 17, # Triangle
  "Class 3: Selective / Low Attendance" = 15        # Square
)

p_arts <- ggplot() +
  geom_line(data = arts_data$smooth, aes(x = time, y = prob, color = Class), linewidth = 0.95) +
  geom_point(data = arts_data$points, aes(x = time, y = prob, color = Class, shape = Class), size = 1.9) +
  facet_wrap(~ Event, ncol = 3, scales = "free_y") +
  scale_x_continuous(
    breaks = 0:3,
    labels = c("W1", "W2", "W3", "W4")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.0)
  ) +
  scale_color_manual(values = PALETTE_ARTS) +
  scale_shape_manual(values = SHAPES_3CLASS) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(
    title = "Latent Trajectories Across 12 Arts & Cultural Events",
    subtitle = "Predicted participation probabilities with wave markers from 3-class multivariate model",
    x = "Collegiate Semester (W1: Fall Frosh to W4: Spr Soph)",
    y = "Predicted Attendance Probability"
  ) +
  theme_publication()

ggsave("Plots/fig1_arts_12_events_by_class.png", p_arts, width = 6.5, height = 8.0, dpi = 300)
cat("   Saved: Plots/fig1_arts_12_events_by_class.png\n")

# -----------------------------------------------------------------------------
# Figure 2: All 9 Book Types by Class (with Wave Markers)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 2: Book Reading Types with Wave Markers...\n")

PALETTE_BOOKS <- c(
  "Class 1: Non-Fiction, History & Biography" = "#009E73",         # Bluish Green
  "Class 2: Popular Genre Fiction (Sci-Fi, Thrillers)" = "#0072B2", # Deep Blue
  "Class 3: Low / Selective Leisure Readers" = "#E69F00"           # Orange
)

SHAPES_BOOKS <- c(
  "Class 1: Non-Fiction, History & Biography" = 16,
  "Class 2: Popular Genre Fiction (Sci-Fi, Thrillers)" = 17,
  "Class 3: Low / Selective Leisure Readers" = 15
)

p_books <- ggplot() +
  geom_line(data = books_data$smooth, aes(x = time, y = prob, color = Class), linewidth = 0.95) +
  geom_point(data = books_data$points, aes(x = time, y = prob, color = Class, shape = Class), size = 1.9) +
  facet_wrap(~ Book_Type, ncol = 3, scales = "free_y") +
  scale_x_continuous(
    breaks = 0:5,
    labels = paste0("W", 1:6)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.0)
  ) +
  scale_color_manual(values = PALETTE_BOOKS) +
  scale_shape_manual(values = SHAPES_BOOKS) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(
    title = "Latent Trajectories Across 9 Book Reading Types",
    subtitle = "Predicted reading probabilities with wave markers from 3-class multivariate model",
    x = "Collegiate Survey Wave (W1: Fall Frosh to W6: Spr Junior)",
    y = "Predicted Reading Probability"
  ) +
  theme_publication()

ggsave("Plots/fig2_books_9_items_by_class.png", p_books, width = 6.5, height = 6.8, dpi = 300)
cat("   Saved: Plots/fig2_books_9_items_by_class.png\n")

# -----------------------------------------------------------------------------
# Figure 3: Top 10 Music Genres by Class (with Wave Markers)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 3: Music Genres with Wave Markers...\n")

PALETTE_MUSIC <- c(
  "Class 1: High Musical Omnivores" = "#0072B2",                  # Deep Blue
  "Class 2: Rock & Heavy Metal Aficionados" = "#D55E00",          # Vermillion
  "Class 3: Mainstream Hits (Rap, Dance, Country)" = "#E69F00"     # Orange
)

SHAPES_MUSIC <- c(
  "Class 1: High Musical Omnivores" = 16,
  "Class 2: Rock & Heavy Metal Aficionados" = 17,
  "Class 3: Mainstream Hits (Rap, Dance, Country)" = 15
)

p_music <- ggplot() +
  geom_line(data = music_data$smooth, aes(x = time, y = prob, color = Class), linewidth = 0.95) +
  geom_point(data = music_data$points, aes(x = time, y = prob, color = Class, shape = Class), size = 1.9) +
  facet_wrap(~ Genre, ncol = 3, scales = "free_y") +
  scale_x_continuous(
    breaks = 0:5,
    labels = paste0("W", 1:6)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.0)
  ) +
  scale_color_manual(values = PALETTE_MUSIC) +
  scale_shape_manual(values = SHAPES_MUSIC) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(
    title = "Latent Trajectories Across Top 10 Music Genres",
    subtitle = "Predicted preference probabilities with wave markers from 3-class multivariate model",
    x = "Collegiate Survey Wave (W1: Fall Frosh to W6: Spr Junior)",
    y = "Predicted Preference Probability"
  ) +
  theme_publication()

ggsave("Plots/fig3_music_10_genres_by_class.png", p_music, width = 6.5, height = 7.4, dpi = 300)
cat("   Saved: Plots/fig3_music_10_genres_by_class.png\n")

# -----------------------------------------------------------------------------
# Figure 4: Multivariable Concomitant Odds Ratio Forest Plot
# -----------------------------------------------------------------------------
cat("--> Generating Figure 4: Concomitant Predictors Forest Plot...\n")

df_forest <- df_concom_all %>%
  filter(Variable != "Constant") %>%
  mutate(
    Low_CI = exp(Estimate - 1.96 * Std_Error),
    High_CI = exp(Estimate + 1.96 * Std_Error),
    Credible = ifelse(P_Value < 0.05, "Statistically Credible (p < .05)", "Spanning Null (p >= .05)"),
    Variable = factor(Variable, levels = rev(c(
      "Woman (ref: Man)", "White (ref: Non-White)", "Catholic (ref: Non-Catholic)",
      "Parent Income ($1,000s)", "Parent Education (Years)", "High School GPA (A/A-)",
      "Aspires to Advanced Degree", "STEM Major (ref: Non-STEM)", "Hometown Urbanicity"
    ))),
    Domain_Clean = case_when(
      Domain == "Arts & Cultural Events" ~ "Arts Events (12 Items)",
      Domain == "Book Reading Types" ~ "Book Types (9 Items)",
      Domain == "Music Genre Preferences" ~ "Music Genres (10 Items)"
    ),
    Panel_Label = paste0(Domain_Clean, "\n", Comparison)
  )

p_forest <- ggplot(df_forest, aes(x = Odds_Ratio, y = Variable, color = Credible)) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_errorbar(aes(xmin = Low_CI, xmax = High_CI), width = 0.25, linewidth = 0.55, orientation = "y") +
  geom_point(size = 2.0) +
  facet_wrap(~ Panel_Label, ncol = 3) +
  scale_x_log10(
    breaks = c(0.05, 0.2, 0.5, 1.0, 2.0, 5.0, 20.0),
    labels = c("0.05", "0.2", "0.5", "1.0", "2.0", "5.0", "20.0")
  ) +
  scale_color_manual(
    values = c(
      "Statistically Credible (p < .05)" = "#D55E00", # Vermillion
      "Spanning Null (p >= .05)" = "gray60"           # Grayscale
    )
  ) +
  guides(color = guide_legend(nrow = 1)) +
  labs(
    title = "Predictors of Latent Trajectory Class Membership Across Domains",
    subtitle = "Odds Ratios from full multivariable endogenous concomitant models (Reference: Class 1)",
    x = "Odds Ratio (Log Scale)",
    y = ""
  ) +
  theme_publication() +
  theme(
    strip.text = element_text(face = "bold", size = rel(0.70)),
    axis.text.y = element_text(size = rel(0.78)),
    legend.position = "bottom"
  )

ggsave("Plots/fig4_multivariate_concomitant_odds_ratios.png", p_forest, width = 6.5, height = 7.0, dpi = 300)
cat("   Saved: Plots/fig4_multivariate_concomitant_odds_ratios.png\n")

# -----------------------------------------------------------------------------
# Figure 5: Activity Time Trend Shifts Within Latent Classes
# -----------------------------------------------------------------------------
cat("--> Generating Figure 5: Activity Time Trend Shifts Within Classes...\n")

df_arts_chg <- arts_data$points %>%
  filter(wave %in% c(1, 4)) %>%
  select(Activity = Event, Class, wave, prob) %>%
  tidyr::pivot_wider(names_from = wave, values_from = prob, names_prefix = "W") %>%
  mutate(Delta = (W4 - W1) * 100, Domain = "Arts Events (Waves 1-4)")

df_books_chg <- books_data$points %>%
  filter(wave %in% c(1, 6)) %>%
  select(Activity = Book_Type, Class, wave, prob) %>%
  tidyr::pivot_wider(names_from = wave, values_from = prob, names_prefix = "W") %>%
  mutate(Delta = (W6 - W1) * 100, Domain = "Book Types (Waves 1-6)")

df_music_chg <- music_data$points %>%
  filter(wave %in% c(1, 6)) %>%
  select(Activity = Genre, Class, wave, prob) %>%
  tidyr::pivot_wider(names_from = wave, values_from = prob, names_prefix = "W") %>%
  mutate(Delta = (W6 - W1) * 100, Domain = "Music Genres (Waves 1-6)")

all_chg <- bind_rows(df_arts_chg, df_books_chg, df_music_chg) %>%
  mutate(
    Class_Clean = case_when(
      grepl("Class 1", Class) ~ "Class 1: High Omnivores / Conservers",
      grepl("Class 2", Class) ~ "Class 2: Specialized / Traditionalists",
      grepl("Class 3", Class) ~ "Class 3: Low / Selective Consumers"
    ),
    Direction = ifelse(Delta >= 0, "Expansion (Increase)", "Contraction (Decrease)"),
    Activity = factor(Activity)
  )

act_order <- all_chg %>%
  group_by(Activity) %>%
  summarize(mean_d = mean(Delta), .groups = "drop") %>%
  arrange(mean_d) %>%
  pull(Activity)

all_chg$Activity <- factor(all_chg$Activity, levels = act_order)

theme_pub_shift <- theme_minimal(base_size = 9) +
  theme(
    text = element_text(family = "sans", color = "#222222"),
    plot.title = element_text(face = "bold", size = rel(1.05), hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(color = "gray30", size = rel(0.85), margin = margin(b = 8)),
    axis.title = element_text(face = "bold", size = rel(0.82)),
    axis.text.y = element_text(size = rel(0.72)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.35),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing = unit(0.8, "lines"),
    strip.text = element_text(face = "bold", size = rel(0.78))
  )

p_chg <- ggplot(all_chg, aes(x = Delta, y = Activity, color = Direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = Delta, y = Activity, yend = Activity), linewidth = 0.6) +
  geom_point(size = 1.9) +
  facet_grid(Domain ~ Class_Clean, scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("Expansion (Increase)" = "#0072B2", "Contraction (Decrease)" = "#D55E00")) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x), " pp")) +
  labs(
    title = "Net Trajectory Shifts by Activity Within Latent Classes",
    subtitle = "Percentage point change from baseline matriculation (Wave 1) to final survey wave",
    x = "Net Percentage Point Shift (Wave End - Wave 1)",
    y = ""
  ) +
  theme_pub_shift

ggsave("Plots/fig5_activity_time_trend_shifts.png", p_chg, width = 6.5, height = 8.8, dpi = 300)
cat("   Saved: Plots/fig5_activity_time_trend_shifts.png\n")

cat("\nAll publication plots successfully generated!\n")
