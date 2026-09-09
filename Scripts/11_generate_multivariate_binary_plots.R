#' Generate Publication Plots for Multivariate Binary Trajectory Models
#'
#' Generates publication figures strictly adhering to global guidelines:
#' 6.5-inch width, 300 DPI, modernized `linewidth` aesthetic, discrete wave markers,
#' Okabe-Ito palettes with matching line colors and point shapes, and punchy single-word labels.
#'
#' Produces:
#' 1. Plots/fig1_arts_9_events_by_class.png (9 panels with wave markers, 3x3 grid)
#' 2. Plots/fig2_books_9_items_by_class.png (9 panels with wave markers, 3x3 grid)
#' 3. Plots/fig3_music_10_genres_by_class.png (10 panels with wave markers)
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
# Figure 1: Revised 9 Arts Events by Class (with Wave Markers)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 1: Arts Events (9 Items, 3x3 Grid) with Wave Markers...\n")

PALETTE_ARTS <- c(
  "Omnivores"      = "#0072B2", # Deep Blue
  "Traditionalists" = "#D55E00", # Vermillion
  "Minimalists"    = "#56B4E9"  # Sky Blue
)

SHAPES_ARTS <- c(
  "Omnivores"      = 16, # Circle
  "Traditionalists" = 17, # Triangle
  "Minimalists"    = 15  # Square
)

p_arts <- ggplot() +
  geom_line(data = arts_data$smooth, aes(x = time, y = prob, color = Class), linewidth = 0.95) +
  geom_point(data = arts_data$points, aes(x = time, y = prob, color = Class, shape = Class), size = 2.0) +
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
  scale_shape_manual(values = SHAPES_ARTS) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  labs(
    title = "Latent Trajectories Across 9 Public Arts & Cultural Events",
    subtitle = "Predicted participation probabilities with wave markers from 3-class multivariate model",
    x = "Collegiate Semester (W1: Fall Frosh to W4: Spr Soph)",
    y = "Predicted Attendance Probability"
  ) +
  theme_publication()

ggsave("Plots/fig1_arts_9_events_by_class.png", p_arts, width = 6.5, height = 6.8, dpi = 300)
# Also save as legacy name for safety
file.copy("Plots/fig1_arts_9_events_by_class.png", "Plots/fig1_arts_12_events_by_class.png", overwrite = TRUE)
cat("   Saved: Plots/fig1_arts_9_events_by_class.png\n")

# -----------------------------------------------------------------------------
# Figure 2: All 9 Book Types by Class (with Wave Markers)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 2: Book Reading Types with Wave Markers...\n")

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

ggsave("Plots/fig2_books_9_items_by_class.png", p_books, width = 6.5, height = 6.8, dpi = 300)
cat("   Saved: Plots/fig2_books_9_items_by_class.png\n")

# -----------------------------------------------------------------------------
# Figure 3: Top 10 Music Genres by Class (with Wave Markers)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 3: Music Genre Preferences with Wave Markers...\n")

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

ggsave("Plots/fig3_music_10_genres_by_class.png", p_music, width = 6.5, height = 7.5, dpi = 300)
cat("   Saved: Plots/fig3_music_10_genres_by_class.png\n")

cat("\n====================================================================\n")
cat("Publication Figures 1, 2, and 3 Generated Successfully!\n")
cat("====================================================================\n")
