# =========================
# Edinburgh Plotting Style
# =========================

library(ggplot2)
library(gt)
library(dplyr)
library(rlang)

source("scripts/utils/name_cleaning.R")
source("scripts/utils/model_utils.R")

# ---- Colour palette ----
EDINBURGH_DARK   <- "#041E42"
EDINBURGH_BLUE   <- "#005EB8"
EDINBURGH_LIGHT  <- "#A7C6ED"
EDINBURGH_ACCENT <- "#E94B3C"

# =========================
# Colour scales
# =========================

scale_colour_edinburgh <- function() {
  scale_colour_manual(values = c(
    EDINBURGH_BLUE,
    EDINBURGH_ACCENT
  ))
}

scale_fill_edinburgh <- function() {
  scale_fill_manual(values = c(
    EDINBURGH_BLUE,
    EDINBURGH_ACCENT
  ))
}

# =========================
# Label helpers
# =========================

extract_var_name <- function(mapping) {
  rlang::as_name(rlang::get_expr(mapping))
}

apply_edinburgh_labels <- function(p) {
  # Always apply if mapping exists (don't rely on label checks)
  if (!is.null(p$mapping$x)) {
    x_var <- extract_var_name(p$mapping$x)
    p <- p + labs(x = clean_labels_edinburgh(x_var))
  }
  if (!is.null(p$mapping$y)) {
    y_var <- extract_var_name(p$mapping$y)
    p <- p + labs(y = clean_labels_edinburgh(y_var))
  } else {
    # Handle default stats like histogram count
    p <- p + labs(y = "Count")
  }
  return(p)
}

# =========================
# Theme
# =========================

theme_edinburgh <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14,
        colour = EDINBURGH_DARK,
        hjust = 0
      ),
      plot.subtitle = element_text(
        size = 11,
        colour = "grey30"
      ),
      axis.title = element_text(
        face = "bold",
        colour = EDINBURGH_DARK
      ),
      axis.text = element_text(colour = "grey20"),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      legend.position = "top",
      legend.title = element_blank(),
      plot.caption = element_text(
        size = 9,
        colour = "grey40",
        hjust = 0
      )
    )
}

set_edinburgh_theme <- function() {
  theme_set(theme_edinburgh())
}

# =========================
# Custom geoms
# =========================

geom_histogram_edinburgh <- function(...) {
  geom_histogram(
    fill = EDINBURGH_BLUE,
    colour = "white",
    ...
  )
}

geom_point_edinburgh <- function(...) {
  geom_point(
    colour = EDINBURGH_BLUE,
    alpha = 0.4,
    ...
  )
}

geom_smooth_edinburgh <- function(se = TRUE, ...) {
  geom_smooth(
    method = "lm",
    colour = EDINBURGH_ACCENT,
    se = se,
    ...
  )
}

# =========================
# QQ Plot (Edinburgh Style)
# =========================

qq_plot_edinburgh <- function(residuals,
                              title = "Normal Q-Q Plot of Residuals") {
  df <- data.frame(residuals = residuals)
  p <- ggplot(df, aes(sample = residuals)) +
    stat_qq(
      colour = EDINBURGH_BLUE,
      alpha = 0.6
    ) +
    stat_qq_line(
      colour = EDINBURGH_ACCENT,
      linewidth = 1
    ) +
    labs(
      title = title,
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )
  return(p)
}

# =========================
# Model Diagnostics Panel
# =========================

library(patchwork)

diagnostics_panel_edinburgh <- function(model) {
  df <- data.frame(
    fitted = fitted(model),
    residuals = resid(model),
    std_resid = rstandard(model),
    leverage = hatvalues(model)
  )
  # 1. Residuals vs Fitted
  p1 <- ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point_edinburgh() +
    geom_hline(yintercept = 0, linetype = "dashed", colour = EDINBURGH_ACCENT) +
    labs(title = "Residuals vs Fitted")
  p1 <- apply_edinburgh_labels(p1) + theme_edinburgh()
  # 2. Q-Q plot
  p2 <- ggplot(df, aes(sample = residuals)) +
    stat_qq(colour = EDINBURGH_BLUE, alpha = 0.6) +
    stat_qq_line(colour = EDINBURGH_ACCENT) +
    labs(title = "Normal Q-Q Plot",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_edinburgh()
  # 3. Scale-location
  # p3 <- ggplot(df, aes(x = fitted, y = sqrt(abs(std_resid)))) +
  #   geom_point_edinburgh() +
  #   labs(title = "Scale-Location",
  #        y = "√|Standardised Residuals|") +
  #   theme_edinburgh()
  # # 4. Residuals vs leverage
  # p4 <- ggplot(df, aes(x = leverage, y = std_resid)) +
  #   geom_point_edinburgh() +
  #   geom_hline(yintercept = 0, linetype = "dashed", colour = EDINBURGH_ACCENT) +
  #   labs(title = "Residuals vs Leverage") +
  #   theme_edinburgh()
  # # Combine
  # (p1 + p2) / (p3 + p4)
  p1 + p2
}

# =========================
# Plot saving
# =========================

save_plot <- function(plot, filename, folder = "outputs", width = 6, height = 4) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  ggsave(
    filename = file.path(folder, filename),
    plot = plot,
    width = width,
    height = height
  )
}

# =========================
# Table styling
# =========================

table_edinburgh <- function(df, title = NULL, clean_names = TRUE, digits = 3) {
  # Apply significant figures
  df <- df %>%
    mutate(across(where(is.numeric), ~ signif(.x, digits)))
  # Clean column names
  if (clean_names) {
    new_names <- colnames(df)
    # Only clean "raw" variable names (no spaces, %, or punctuation)
    to_clean <- !grepl("[ %\\-\\.]", new_names)
    new_names[to_clean] <- clean_names_edinburgh(new_names[to_clean])
    colnames(df) <- new_names
  }
  df %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title, "**"))
    ) %>%
    tab_style(
      style = cell_text(color = "white", weight = "bold"),
      locations = cells_title(groups = "title")
    ) %>%
    tab_options(
      heading.background.color = EDINBURGH_DARK,
      table.border.top.color = EDINBURGH_DARK,
      table.border.bottom.color = EDINBURGH_DARK,
      column_labels.font.weight = "bold",
      table.font.size = 12
    ) %>%
    cols_align(
      align = "center",
      everything()
    )
}

# =========================
# Table saving
# =========================

save_table <- function(gt_table, filename, folder = "outputs") {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  gtsave(
    data = gt_table,
    filename = file.path(folder, filename)
  )
}