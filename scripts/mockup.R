#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Creating viz mockup for customer presentation
#
# Scatter plots for threshold violations (PM2-5. PM10, NO2)
#
# https://github.com/INWT/fairq-viz/issues/2
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
library(tidyverse)

# A function factory for getting integer y-axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

max_days_month <- 13
months <-
  c("Mai 22", "April 22", "März 22", "Februar 22", "Januar 22")
months <- factor(months, levels = rev(months))
matters <- factor(c("PM2.5", "PM10", "NO2"),
                  levels = c("PM2.5", "PM10", "NO2"))
days <- runif(length(months) * length(matters),
              min = 0,
              max = max_days_month) %>%
  floor()
mockup_cols <- c("PM2.5" = "#673ab7",
                 "PM10" = "#9c27b0",
                 "NO2" = "#2196f3")

fairq_mockup_months <- tibble(
  month = rep(months, length(matters)),
  matter = rep(matters, each = length(months)),
  `Anzahl Überschreitungstage` = days
)
fairq_mockup_months


plot_threshold_month <- ggplot(fairq_mockup_months,
                               aes(y = `Anzahl Überschreitungstage`,
                                   x = month,
                                   col = matter)) +
  geom_point(size = 5,
             alpha = 0.7) +
  ylim(c(0, max_days_month)) +
  scale_y_continuous(breaks = integer_breaks()) +
  scale_color_manual(values = mockup_cols) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(family = "OpenSans")
  )
plot_threshold_month

fairq_mockup_year <- tibble(
  year = factor(c(rep("2020", 3), rep("2021", 3))),
  matter = rep(matters, 2),
  `Anzahl Überschreitungstage` = c(1, 2, 31, 8, 10, 24)
)

plot_threshold_year <- ggplot(fairq_mockup_year,
                              aes(y = `Anzahl Überschreitungstage`,
                                  x = year,
                                  col = matter)) +
  geom_point(size = 5,
             alpha = 0.7) +
  # ylim(c(0, 365)) +
  scale_color_manual(values = mockup_cols) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(family = "OpenSans")
  ) +
  geom_hline(yintercept = 4,
             linetype = "dotted")
plot_threshold_year
