# Eurostat ggplot2 Theme
# Based on the visual aesthetics of Eurostat statistical charts


# Main theme function
nfsa_theme_eurostat <- function(base_size = 11,
                           base_family = "sans",
                           grid = TRUE,
                           axis = TRUE) {

  library(ggplot2)

  # Define Eurostat color palette
  eurostat_colors <- c(
    blue = "#003399",      # Primary Eurostat blue
    yellow = "#FFB800",    # Eurostat yellow
    lightblue = "#5694CA", # Light blue for secondary data
    gray = "#707070",      # Gray for text
    lightgray = "#E5E5E5"  # Light gray for gridlines
  )

  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Plot background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),

      # Grid lines - light gray, subtle
      panel.grid.major = if(grid) {
        element_line(color = "#E5E5E5", linewidth = 0.3)
      } else {
        element_blank()
      },
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(), # Typically only horizontal gridlines

      # Axes
      axis.line = if(axis) {
        element_line(color = "#707070", linewidth = 0.5)
      } else {
        element_blank()
      },
      axis.line.x = element_line(color = "#707070", linewidth = 0.5),
      axis.line.y = element_blank(), # No y-axis line typically
      axis.ticks = element_line(color = "#707070", linewidth = 0.5),
      axis.ticks.y = element_blank(),
      axis.text = element_text(color = "#707070", size = rel(0.9)),
      axis.title = element_text(color = "#333333", size = rel(1.0),
                                face = "plain"),
      axis.title.y = element_text(angle = 90, margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),

      # Plot title and subtitle
      plot.title = element_text(
        color = "#003399",
        size = rel(1.3),
        face = "bold",
        hjust = 0,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        color = "#707070",
        size = rel(1.0),
        hjust = 0,
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        color = "#707070",
        size = rel(0.8),
        hjust = 0,
        margin = margin(t = 12)
      ),

      # Legend
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      legend.text = element_text(color = "#707070", size = rel(0.9)),
      legend.title = element_text(color = "#333333", size = rel(1.0),
                                  face = "plain"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box.spacing = unit(0.5, "cm"),

      # Faceting
      strip.background = element_rect(fill = "#F5F5F5", color = NA),
      strip.text = element_text(color = "#003399", size = rel(1.0),
                               face = "bold", margin = margin(5, 5, 5, 5)),

      # Margins
      plot.margin = margin(15, 15, 15, 15)
    )
}

# Alternative theme with more traditional Eurostat styling
nfsa_theme_eurostat_classic <- function(base_size = 11, base_family = "sans") {
  theme_eurostat(base_size = base_size, base_family = base_family) +
    theme(
      panel.border = element_rect(color = "#CCCCCC", fill = NA, linewidth = 0.5),
      axis.line = element_blank(),
      legend.position = "right"
    )
}

# Color scale functions for Eurostat palette
nfsa_scale_color_eurostat <- function(...) {
  scale_color_manual(
    values = c("#003399", "#FFB800", "#5694CA", "#9DC3E6",
               "#A6CE39", "#F47B20", "#C00000"),
    ...
  )
}

nfsa_scale_fill_eurostat <- function(...) {
  scale_fill_manual(
    values = c("#003399", "#FFB800", "#5694CA", "#9DC3E6",
               "#A6CE39", "#F47B20", "#C00000"),
    ...
  )
}

# Continuous color scales
nfsa_scale_color_eurostat_c <- function(...) {
  scale_color_gradient(
    low = "#E5E5E5",
    high = "#003399",
    ...
  )
}

nfsa_scale_fill_eurostat_c <- function(...) {
  scale_fill_gradient(
    low = "#E5E5E5",
    high = "#003399",
    ...
  )
}


  # Example 1: Line chart (like household saving rate)
  df_line <- data.frame(
    quarter = seq(as.Date("2020-01-01"), as.Date("2025-10-01"), by = "quarter"),
    EA = runif(24, 12, 16),
    EU = runif(24, 11, 15)
  )

  p1 <- ggplot(df_line, aes(x = quarter)) +
    geom_line(aes(y = EA, color = "Euro area"), linewidth = 1) +
    geom_line(aes(y = EU, color = "EU"), linewidth = 1) +
    scale_color_eurostat() +
    labs(
      title = "Household saving rate",
      subtitle = "Seasonally adjusted data, %",
      x = NULL,
      y = "%",
      color = NULL,
      caption = "Source: Eurostat"
    ) +
    theme_eurostat()

  print(p1)

  # Example 2: Bar chart
  df_bar <- data.frame(
    category = c("Income", "Consumption", "Saving", "Investment"),
    value = c(100, 85, 15, 9),
    change = c(0.7, 1.0, -0.3, -0.7)
  )

  p2 <- ggplot(df_bar, aes(x = category, y = change, fill = category)) +
    geom_col() +
    scale_fill_eurostat() +
    labs(
      title = "Quarterly changes in household accounts",
      subtitle = "Q3 2025, %",
      x = NULL,
      y = "Change (%)",
      caption = "Source: Eurostat"
    ) +
    theme_eurostat() +
    theme(legend.position = "none")

  print(p2)
