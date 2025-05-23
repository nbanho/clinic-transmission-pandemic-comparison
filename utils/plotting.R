require(patchwork)
require(ggpattern)
require(gridExtra)
require(GGally)
require(Cairo)
require(gg.layers)
require(wesanderson) # https://github.com/karthik/wesanderson
require(paletteer)
require(ggsignif)

smart.round <- function(x) {
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

# ggplot theme for Nature style
text_size <- 8
update_geom_defaults("text", list(size = text_size))

theme_nature <- function() {
  theme_classic(base_size = text_size, base_family = "sans") %+replace%
    theme(
      axis.text = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      plot.title = element_text(size = text_size + 2, face = "bold", hjust = 0.5, margin = ggplot2::margin(0, 0, 5, 0))
    )
}

theme_custom <- function() {
  theme_minimal() %+replace%
    theme(
      text = element_text(size = text_size),
      axis.text = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      plot.title = element_text(size = text_size + 2, face = "bold", hjust = 0, margin = ggplot2::margin(0, 0, 5, 0)),
      strip.text = element_text(size = text_size),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      axis.ticks = element_line(),
      legend.text = element_text(size = 8)
    )
}

theme_bw2 <- function() {
  theme_bw(base_size = text_size, base_family = "sans") %+replace%
    theme(
      axis.text = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      plot.title = element_text(size = text_size + 2, face = "bold", hjust = 0, margin = ggplot2::margin(0, 0, 5, 0))
    )
}

# Correlation plot
# Helpful code:
# https://stackoverflow.com/questions/44961437/how-to-include-density-coloring-in-pairwise-correlation-scatter-plot
# https://stackoverflow.com/questions/37889222/change-colors-in-ggpairs-now-that-params-is-deprecated?answertab=votes#tab-top
contours <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    stat_density2d() +
    scale_x_continuous(breaks = seq(-.6, 0, .2)) +
    scale_y_continuous(breaks = seq(-.6, 0, .2))
  p
}

# Plot for correlation
contours <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    stat_density2d() #+
  # scale_x_continuous(breaks = seq(-.8,.4,.4)) +
  # scale_y_continuous(breaks = seq(-.8,.4,.4))
  p
}
hex <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_hex() #+
  # scale_x_continuous(breaks = seq(-.8,.4,.4)) +
  # scale_y_continuous(breaks = seq(-.8,.4,.4))
  p
}
cor_fun <- function(data, mapping, method = "pearson", ndp = 2, sz = 5, stars = TRUE, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)

  corr <- cor.test(x, y, method = method)
  est <- corr$estimate
  # lb.size <- sz* abs(est)

  if (stars) {
    stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1))]
    lbl <- paste0(round(est, ndp), stars)
  } else {
    lbl <- round(est, ndp)
  }

  ggplot(data = data, mapping = mapping) +
    annotate("text", x = mean(x, na.rm = TRUE), y = mean(y, na.rm = TRUE), label = lbl, size = sz, ...) +
    theme(panel.grid = element_blank())
}
hist_fun <- function(data, mapping) {
  p <- ggplot(data, mapping) +
    geom_histogram(bins = 20) #+
  # scale_x_continuous(limits = c(-1, 1), breaks = seq(-.8, .4, .4))
}

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


save_plot <- function(pl, pdf_file = NULL, eps_file = NULL, tikz_file = NULL, w = 8, h = 4) {
  print(pl)
  if (!is.null(pdf_file)) {
    ggsave(pdf_file, width = w / cm(1), height = h / cm(1))
  }
  if (!is.null(tikz_file)) {
    tikz(tikz_file, width = w / cm(1), height = h / cm(1))
    print(pl)
    dev.off()
  }
  if (!is.null(eps_file)) {
    cairo_ps(filename = eps_file, width = w / cm(1), height = h / cm(1))
  }
}
