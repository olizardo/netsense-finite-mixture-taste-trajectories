#' Generate Publication Plots for Multivariate Binary Trajectory Models (K = 4)
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
#' @date 2026-09-09

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

cat("--> Loading trajectory datasets (K = 4)...\n")
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
      legend.text = element_text(size = rel(0.78), face = "bold"),
      legend.box.margin = margin(t = -4, b = 2),
      panel.spacing = unit(0.7, "lines"),
      strip.text = element_text(face = "bold", size = rel(0.80))
    )
}

# -----------------------------------------------------------------------------
# Figure 1: All 9 Book Types by Class (with Wave Markers, K = 4)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 1: Book Reading Types with Wave Markers (K = 4)...\n")

PALETTE_BOOKS4 <- c(
  "Genre Specialists" = "#0072B2", # Deep Blue
  "Romance Readers"   = "#CC79A7", # Reddish Purple
  "Nonfictionists"    = "#009E73", # Bluish Green
  "Fictionists"       = "#D55E00"  # Vermillion
)

SHAPES_BOOKS4 <- c(
  "Genre Specialists" = 16, # Circle
  "Romance Readers"   = 18, # Diamond
  "Nonfictionists"    = 17, # Triangle
  "Fictionists"       = 15  # Square
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
  scale_color_manual(values = PALETTE_BOOKS4) +
  scale_shape_manual(values = SHAPES_BOOKS4) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(
    title = "Latent Trajectories Across 9 Book Reading Types (K = 4)",
    subtitle = "Predicted reading probabilities with wave markers from 4-class multivariate binary model",
    x = "Collegiate Semester (W1: Fall Frosh to W6: Spr Junior)",
    y = "Predicted Reading Probability"
  ) +
  theme_publication()

ggsave("Plots/fig1_books_9_items_by_class.png", p_books, width = 6.5, height = 7.0, dpi = 300)
cat("   Saved: Plots/fig1_books_9_items_by_class.png\n")

# -----------------------------------------------------------------------------
# Figure 2: Top 10 Music Genres by Class (with Wave Markers, K = 4)
# -----------------------------------------------------------------------------
cat("--> Generating Figure 2: Music Genre Preferences with Wave Markers (K = 4)...\n")

PALETTE_MUSIC4 <- c(
  "Omnivores"       = "#0072B2", # Deep Blue
  "Classic Rockers" = "#D55E00", # Vermillion
  "Modern Rockers"  = "#E69F00", # Orange
  "Mainstreamers"   = "#009E73"  # Bluish Green
)

SHAPES_MUSIC4 <- c(
  "Omnivores"       = 16, # Circle
  "Classic Rockers" = 17, # Triangle
  "Modern Rockers"  = 15, # Square
  "Mainstreamers"   = 18  # Diamond
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
  scale_color_manual(values = PALETTE_MUSIC4) +
  scale_shape_manual(values = SHAPES_MUSIC4) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(
    title = "Latent Trajectories Across Top 10 Music Genres (K = 4)",
    subtitle = "Predicted preference probabilities with wave markers from 4-class multivariate binary model",
    x = "Collegiate Semester (W1: Fall Frosh to W6: Spr Junior)",
    y = "Predicted Preference Probability"
  ) +
  theme_publication()

ggsave("Plots/fig2_music_10_genres_by_class.png", p_music, width = 6.5, height = 7.5, dpi = 300)
cat("   Saved: Plots/fig2_music_10_genres_by_class.png\n")

# -----------------------------------------------------------------------------
# Figure 3 & 4: Trajectory Shifts Split by Domain (Books & Music, K = 4)
# -----------------------------------------------------------------------------
cat("--> Generating Figures 3 & 4: Trajectory Shifts Split by Domain (K = 4)...\n")

df_books_chg <- books_data$points %>%
  filter(wave %in% c(1, 6)) %>%
  dplyr::select(Activity = Book_Type, Class, wave, prob) %>%
  tidyr::pivot_wider(names_from = wave, values_from = prob, names_prefix = "W") %>%
  mutate(
    Delta = (W6 - W1) * 100, 
    Direction = ifelse(Delta >= 0, "Expansion (+)", "Contraction (-)")
  )

df_music_chg <- music_data$points %>%
  filter(wave %in% c(1, 6)) %>%
  dplyr::select(Activity = Genre, Class, wave, prob) %>%
  tidyr::pivot_wider(names_from = wave, values_from = prob, names_prefix = "W") %>%
  mutate(
    Delta = (W6 - W1) * 100, 
    Direction = ifelse(Delta >= 0, "Expansion (+)", "Contraction (-)")
  )

# Order items by mean shift across classes for clean display
book_order <- df_books_chg %>%
  group_by(Activity) %>%
  summarize(m = mean(Delta), .groups = "drop") %>%
  arrange(m) %>%
  pull(Activity)

df_books_chg$Activity <- factor(df_books_chg$Activity, levels = book_order)
df_books_chg$Class <- factor(df_books_chg$Class, levels = c("Genre Specialists", "Romance Readers", "Nonfictionists", "Fictionists"))

music_order <- df_music_chg %>%
  group_by(Activity) %>%
  summarize(m = mean(Delta), .groups = "drop") %>%
  arrange(m) %>%
  pull(Activity)

df_music_chg$Activity <- factor(df_music_chg$Activity, levels = music_order)
df_music_chg$Class <- factor(df_music_chg$Class, levels = c("Omnivores", "Classic Rockers", "Modern Rockers", "Mainstreamers"))

theme_pub_shift_split <- theme_minimal(base_size = 9.5) +
  theme(
    text = element_text(family = "sans", color = "#222222"),
    plot.title = element_text(face = "bold", size = rel(1.05), hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(color = "gray30", size = rel(0.85), margin = margin(b = 8)),
    axis.title.x = element_text(face = "bold", size = rel(0.82), margin = margin(t = 6)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = rel(0.80), color = "black"),
    axis.text.x = element_text(size = rel(0.75)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.35),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = rel(0.80), face = "bold"),
    legend.margin = margin(t = -2, b = 2),
    panel.spacing = unit(0.6, "lines"),
    strip.text = element_text(face = "bold", size = rel(0.82)),
    strip.background = element_rect(fill = "grey95", color = NA),
    plot.margin = margin(t = 6, r = 6, b = 6, l = 6)
  )

# Figure 3: Books Shifts
p_shift_books <- ggplot(df_books_chg, aes(x = Delta, y = Activity, color = Direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = Delta, y = Activity, yend = Activity), linewidth = 0.75) +
  geom_point(size = 2.0) +
  facet_wrap(~ Class, ncol = 4) +
  scale_color_manual(values = c("Expansion (+)" = "#0072B2", "Contraction (-)" = "#D55E00")) +
  scale_x_continuous(
    labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x), " pp"),
    breaks = seq(-40, 40, 20),
    limits = c(-45, 45)
  ) +
  labs(
    title = "Net Trajectory Shifts in Book Reading Types (K = 4)",
    subtitle = "Percentage point shift (Wave 6 - Wave 1) by reading type across latent classes",
    x = "Net Percentage Point Shift (Wave 6 - Wave 1)"
  ) +
  theme_pub_shift_split

ggsave("Plots/fig3_books_trajectory_shifts.png", p_shift_books, width = 6.5, height = 4.4, dpi = 300)
cat("   Saved: Plots/fig3_books_trajectory_shifts.png\n")

# Figure 4: Music Shifts
p_shift_music <- ggplot(df_music_chg, aes(x = Delta, y = Activity, color = Direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = Delta, y = Activity, yend = Activity), linewidth = 0.75) +
  geom_point(size = 2.0) +
  facet_wrap(~ Class, ncol = 4) +
  scale_color_manual(values = c("Expansion (+)" = "#0072B2", "Contraction (-)" = "#D55E00")) +
  scale_x_continuous(
    labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x), " pp"),
    breaks = seq(-40, 40, 20),
    limits = c(-45, 45)
  ) +
  labs(
    title = "Net Trajectory Shifts in Music Genre Preferences (K = 4)",
    subtitle = "Percentage point shift (Wave 6 - Wave 1) by musical genre across latent classes",
    x = "Net Percentage Point Shift (Wave 6 - Wave 1)"
  ) +
  theme_pub_shift_split

ggsave("Plots/fig4_music_trajectory_shifts.png", p_shift_music, width = 6.5, height = 4.6, dpi = 300)
cat("   Saved: Plots/fig4_music_trajectory_shifts.png\n")

# Legacy / compound shift plot
all_chg <- bind_rows(
  df_books_chg %>% mutate(Domain = "Book Reading Types (Waves 1--6)"),
  df_music_chg %>% mutate(Domain = "Music Genre Preferences (Waves 1--6)")
) %>%
  mutate(
    Domain = factor(Domain, levels = c("Book Reading Types (Waves 1--6)", "Music Genre Preferences (Waves 1--6)")),
    Class_Clean = factor(case_when(
      Class %in% c("Genre Specialists", "Omnivores")            ~ "Class 1",
      Class %in% c("Romance Readers", "Classic Rockers")        ~ "Class 2",
      Class %in% c("Nonfictionists", "Contemporary Rockers")    ~ "Class 3",
      Class %in% c("Omnivorous Fictionists", "Mainstreamers")   ~ "Class 4"
    )),
    Activity = factor(Activity, levels = c(book_order, music_order))
  )

p_chg <- ggplot(all_chg, aes(x = Delta, y = Activity, color = Direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = Delta, y = Activity, yend = Activity), linewidth = 0.75) +
  geom_point(size = 2.0) +
  facet_grid(Domain ~ Class, scales = "free", space = "free_y") +
  scale_color_manual(values = c("Expansion (+)" = "#0072B2", "Contraction (-)" = "#D55E00")) +
  scale_x_continuous(
    labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x), " pp"),
    breaks = seq(-40, 40, 20)
  ) +
  labs(
    title = "Net Trajectory Shifts by Cultural Item Within Latent Classes (K = 4)",
    subtitle = "Percentage point shift (Wave 6 - Wave 1) across Book Reading Types and Music Genres",
    x = "Net Percentage Point Shift (Wave 6 - Wave 1)"
  ) +
  theme_pub_shift_split

ggsave("Plots/fig3_activity_time_trend_shifts.png", p_chg, width = 6.5, height = 6.2, dpi = 300)
cat("   Saved: Plots/fig3_activity_time_trend_shifts.png (Fallback)\n")

cat("\n====================================================================\n")
cat("Plot Generation Complete!\n")
