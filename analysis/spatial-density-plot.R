# libraries
library(tidyverse)
library(lubridate)
library(scales)
library(parallel)
library(png)
library(terra)
library(sf)
library(grid)
library(ggpubr)
source("utils/plotting.R")

# load tracking data
tracking_files_19 <- list.files(
  "data-raw/2019/tracking/annotated/",
  full.names = TRUE
)
tracking_data_19 <- do.call(
  rbind, parallel::mclapply(tracking_files_19, readRDS, mc.cores = 6)
) %>%
  mutate(date = as.Date(date_time)) %>%
  filter(
    is_waitingroom,
    between(hour(date_time), 8, 15)
  )

tracking_files_21 <- list.files(
  "data-raw/2021/tracking/annotated/",
  full.names = TRUE
)
tracking_data_21 <- do.call(
  rbind, parallel::mclapply(tracking_files_21, readRDS, mc.cores = 6)
) %>%
  rename(date_time = time) %>%
  mutate(date = as.Date(date_time)) %>%
  filter(
    is_waitingroom,
    between(hour(date_time), 8, 15)
  )

# subset data
complete_dates <- readRDS("data-clean/combined-data-wr.rds") %>%
  mutate(year = factor(year, levels = c(2019, 2021))) %>%
  filter(across(starts_with("pt"), ~ !is.na(.)))
tracking_data <- rbind(
  dplyr::select(tracking_data_19, date_time, obs_id, x, y),
  dplyr::select(tracking_data_21, date_time, obs_id, x, y)
) %>%
  mutate(
    year = lubridate::year(date_time),
    date = as.Date(date_time),
    daytime = factor(
      ifelse(hour(date_time) < 12, "Morning", "Afternoon"),
      levels = c("Morning", "Afternoon")
    )
  ) %>%
  filter(between(hour(date_time), 8, 15)) %>%
  filter(date %in% complete_dates$date)

# total person times to normalize spatial density
tt <- tracking_data %>%
  group_by(year, daytime) %>%
  summarise(tt = n_distinct(date) * 4) %>%
  ungroup()

# background image
clinic_img <- png::readPNG(
  "data-raw/building/clinic_waiting-room.png"
)

# plot
plot_spatial <- function(pl, text_descr = 8) {
  entrance_lab <- textGrob(
    label = "Entrance",
    x = 0.2, y = 0.21,
    gp = gpar(fontsize = text_descr), just = c("left")
  )
  wr_lab <- textGrob(
    label = "Waiting room",
    x = 0.05, y = 0.58,
    gp = gpar(fontsize = text_descr), just = c("left")
  )
  re_lab <- textGrob(
    label = "Registration",
    x = 0.9, y = 0.4,
    gp = gpar(fontsize = text_descr), just = c("left"), rot = 90
  )

  pl +
    ggpubr::background_image(clinic_img) +
    annotation_custom(
      entrance_lab,
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf
    ) +
    annotation_custom(
      wr_lab,
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf
    ) +
    annotation_custom(
      re_lab,
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf
    ) +
    scale_x_continuous(
      limits = c(min(tracking_data$x), max(tracking_data$x)),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_y_continuous(
      limits = c(min(tracking_data$y), max(tracking_data$y)),
      expand = expansion(mult = c(0.48, 0.12))
    ) +
    scale_fill_gradientn(
      name = "Tracks per hour",
      colours = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
      trans = "log1p",
      limits = c(0, 1000),
      breaks = c(0, 10, 100, 1000)
    ) +
    theme_custom() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(2, "cm"),
      text = element_text(size = text_descr),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(
        fill = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")[1]
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_colorbar(title.vjust = .8))
}

pl_19_morn <- plot_spatial(
  ggplot(
    data = filter(tracking_data, year == 2019, daytime == "Morning"),
    mapping = aes(x = x, y = y)
  ) +
    geom_bin2d(
      aes(
        fill = ..count.. /
          tt$tt[tt$year == 2019 & tt$daytime == "Morning"]
      ),
      binwidth = 200
    )
) +
  ggtitle("Year: 2019 (Morning)")
pl_19_noon <- plot_spatial(
  ggplot(
    data = filter(tracking_data, year == 2019, daytime == "Afternoon"),
    mapping = aes(x = x, y = y)
  ) +
    geom_bin2d(
      aes(
        fill = ..count.. /
          tt$tt[tt$year == 2019 & tt$daytime == "Afternoon"]
      ),
      binwidth = 200
    )
) +
  ggtitle("Year: 2019 (Afternoon)")
pl_21_morn <- plot_spatial(
  ggplot(
    data = filter(tracking_data, year == 2021, daytime == "Morning"),
    mapping = aes(x = x, y = y)
  ) +
    geom_bin2d(
      aes(
        fill = ..count.. / tt$tt[tt$year == 2021 & tt$daytime == "Morning"]
      ),
      binwidth = 200
    )
) +
  ggtitle("Year: 2021 (Morning)")
pl_21_noon <- plot_spatial(
  ggplot(
    data = filter(tracking_data, year == 2021, daytime == "Afternoon"),
    mapping = aes(x = x, y = y)
  ) +
    geom_bin2d(
      aes(
        fill = ..count.. / tt$tt[tt$year == 2021 & tt$daytime == "Afternoon"]
      ),
      binwidth = 200
    )
) +
  ggtitle("Year: 2021 (Afternoon)")

spatial_facet_pl <- pl_19_morn + pl_21_morn + pl_19_noon + pl_21_noon +
  plot_layout(nrow = 2, guides = "collect") &
  theme(legend.position = "bottom")

save_plot(
  spatial_facet_pl,
  pdf_file = "results/spatial-density-plot.png",
  w = 16, h = 16
)
