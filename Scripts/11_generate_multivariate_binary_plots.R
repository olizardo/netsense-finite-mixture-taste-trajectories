#' Generate Publication Plots for Multivariate Binary Trajectory Models
#'
#' Generates publication figures strictly adhering to global guidelines:
#' 6.5-inch width, 300 DPI, modernized `linewidth` aesthetic, discrete wave markers,
#' Okabe-Ito palettes with matching line colors and point shapes, and punchy single-word labels.
#'
#' Produces:
#' 1. Plots/fig1_books_9_items_by_class.png (9 panels with wave markers, 3x3 grid)
#' 2. Plots/fig2_music_10_genres_by_class.png (10 panels with wave markers)
#' 3. Plots/fig3_activity_time_trend_shifts.png (Net trajectory shifts within classes)
#'
#' @author Omar Lizardo & AI Assistant
#' @date 2026-09-08

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

cat("--> Loading trajectory datasets...\n")
books_data <- readRDS("Cache/summaries/books_trajectories.rds")
music_data <- readRDS("Cache/summaries/music_trajectories.rds")

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
      legend.text = element_text(size = rel(0.80), face = "bold"),
      legend.box.margin = margin(t = -4, b = 2),
      panel.spacing = unit(0.7, "lines"),
      strip.text = element_text(face = "bold", size = rel(0.80))
    )
}

# -----------------------------------------------------------------------------
# Figure 1: All 9 Book Types by Class (with Wave Markers)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 1: Book Reading Types with Wave Markers...\n")

PALETTE_BOOKS <- c(
  "Nonfictionists" = "#009E73", # Bluish Green
  "Fictionists"    = "#0072B2", # Deep Blue
  "Minimalists"    = "#D55E00"  # Vermillion
)

SHAPES_BOOKS <- c(
  "Nonfictionists" = 16,
  "Fictionists"    = 17,
  "Minimalists"    = 15
)

p_books <- ggplot() +
  geom_line(data = books_data$smooth, aes(x = time, y = prob, color = Class), linewidth = 0.95) +
  geom_point(data = books_data$points, aes(x = time, y = prob, color = Class, shape = Class), size = 2.0) +
  facet_wrap(~ Book_Type, ncol = 3, scales = "free_y") +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("W1", "W2", "W3", "W4", "W5", "W6")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.0)
  ) +
  scale_color_manual(values = PALETTE_BOOKS) +
  scale_shape_manual(values = SHAPES_BOOKS) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  labs(
    title = "Latent Trajectories Across 9 Book Reading Types",
    subtitle = "Predicted reading probabilities with wave markers from 3-class multivariate model",
    x = "Collegiate Semester (W1: Fall Frosh to W6: Spr Junior)",
    y = "Predicted Reading Probability"
  ) +
  theme_publication()

ggsave("Plots/fig1_books_9_items_by_class.png", p_books, width = 6.5, height = 6.8, dpi = 300)
cat("   Saved: Plots/fig1_books_9_items_by_class.png\n")

# -----------------------------------------------------------------------------
# Figure 2: Top 10 Music Genres by Class (with Wave Markers)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 2: Music Genre Preferences with Wave Markers...\n")

PALETTE_MUSIC <- c(
  "Omnivores"     = "#0072B2", # Deep Blue
  "Rockers"       = "#D55E00", # Vermillion
  "Mainstreamers" = "#E69F00"  # Orange
)

SHAPES_MUSIC <- c(
  "Omnivores"     = 16,
  "Rockers"       = 17,
  "Mainstreamers" = 15
)

p_music <- ggplot() +
  geom_line(data = music_data$smooth, aes(x = time, y = prob, color = Class), linewidth = 0.95) +
  geom_point(data = music_data$points, aes(x = time, y = prob, color = Class, shape = Class), size = 2.0) +
  facet_wrap(~ Genre, ncol = 3, scales = "free_y") +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("W1", "W2", "W3", "W4", "W5", "W6")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.0)
  ) +
  scale_color_manual(values = PALETTE_MUSIC) +
  scale_shape_manual(values = SHAPES_MUSIC) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  labs(
    title = "Latent Trajectories Across Top 10 Music Genres",
    subtitle = "Predicted preference probabilities with wave markers from 3-class multivariate model",
    x = "Collegiate Semester (W1: Fall Frosh to W6: Spr Junior)",
    y = "Predicted Preference Probability"
  ) +
  theme_publication()

ggsave("Plots/fig2_music_10_genres_by_class.png", p_music, width = 6.5, height = 7.5, dpi = 300)
cat("   Saved: Plots/fig2_music_10_genres_by_class.png\n")

# -----------------------------------------------------------------------------
# Figure 3: Activity Time Trend Shifts Within Latent Classes
# -----------------------------------------------------------------------------
cat("--> Generating Figure 3: Activity Time Trend Shifts Within Classes...\n")

df_books_chg <- books_data$points %>%
  filter(wave %in% c(1, 6)) %>%
  dplyr::select(Activity = Book_Type, Class, wave, prob) %>%
  tidyr::pivot_wider(names_from = wave, values_from = prob, names_prefix = "W") %>%
  mutate(
    Delta = (W6 - W1) * 100, 
    Domain = "Book Reading Types (Waves 1--6)"
  )

df_music_chg <- music_data$points %>%
  filter(wave %in% c(1, 6)) %>%
  dplyr::select(Activity = Genre, Class, wave, prob) %>%
  tidyr::pivot_wider(names_from = wave, values_from = prob, names_prefix = "W") %>%
  mutate(
    Delta = (W6 - W1) * 100, 
    Domain = "Music Genre Preferences (Waves 1--6)"
  )

all_chg <- bind_rows(df_books_chg, df_music_chg) %>%
  mutate(
    Domain = factor(Domain, levels = c("Book Reading Types (Waves 1--6)", "Music Genre Preferences (Waves 1--6)")),
    Class_Clean = factor(case_when(
      Class %in% c("Omnivores", "Nonfictionists") ~ "Class 1\n(Omnivores / Nonfictionists)",
      Class %in% c("Rockers", "Fictionists")       ~ "Class 2\n(Rockers / Fictionists)",
      Class %in% c("Mainstreamers", "Minimalists") ~ "Class 3\n(Mainstreamers / Minimalists)"
    ), levels = c(
      "Class 1\n(Omnivores / Nonfictionists)",
      "Class 2\n(Rockers / Fictionists)",
      "Class 3\n(Mainstreamers / Minimalists)"
    )),
    Direction = ifelse(Delta >= 0, "Expansion (+)", "Contraction (-)"),
    Activity = factor(Activity)
  )

book_acts <- all_chg %>% filter(grepl("Book", Domain)) %>% group_by(Activity) %>% summarize(m = mean(Delta)) %>% arrange(m) %>% pull(Activity)
music_acts <- all_chg %>% filter(grepl("Music", Domain)) %>% group_by(Activity) %>% summarize(m = mean(Delta)) %>% arrange(m) %>% pull(Activity)
all_chg$Activity <- factor(all_chg$Activity, levels = c(book_acts, music_acts))

theme_pub_shift <- theme_minimal(base_size = 9.5) +
  theme(
    text = element_text(family = "sans", color = "#222222"),
    plot.title = element_text(face = "bold", size = rel(1.05), hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(color = "gray30", size = rel(0.85), margin = margin(b = 8)),
    axis.title.x = element_text(face = "bold", size = rel(0.82), margin = margin(t = 6)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = rel(0.78), color = "black"),
    axis.text.x = element_text(size = rel(0.75)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.35),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = rel(0.80), face = "bold"),
    panel.spacing = unit(0.7, "lines"),
    strip.text = element_text(face = "bold", size = rel(0.80)),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

p_chg <- ggplot(all_chg, aes(x = Delta, y = Activity, color = Direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = Delta, y = Activity, yend = Activity), linewidth = 0.75) +
  geom_point(size = 2.2) +
  facet_grid(Domain ~ Class_Clean, scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("Expansion (+)" = "#0072B2", "Contraction (-)" = "#D55E00")) +
  scale_x_continuous(
    labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x), " pp"),
    breaks = seq(-30, 30, 10)
  ) +
  labs(
    title = "Net Trajectory Shifts by Cultural Item Within Latent Classes",
    subtitle = "Percentage point shift (Wave 6 - Wave 1) across Book Reading Types and Music Genres",
    x = "Net Percentage Point Shift (Wave 6 - Wave 1)"
  ) +
  theme_pub_shift

ggsave("Plots/fig3_activity_time_trend_shifts.png", p_chg, width = 6.5, height = 6.2, dpi = 300)
cat("   Saved: Plots/fig3_activity_time_trend_shifts.png\n")

cat("\n====================================================================\n")
cat("Publication Figures 1, 2, and 3 Generated Successfully!\n")
cat("====================================================================\n")
