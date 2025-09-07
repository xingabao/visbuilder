# F3176

paired_dot_plot <- function(
    plot.dat,
    col.x,
    col.y,
    col.group,
    col.highlight = NULL,

    col.x.level = NULL,
    highlights = '',

    line.width.small = 0.5,
    line.width.large = 1,
    point.size.small = 1.5,
    point.size.large = 3,

    label.offset = 0.05,
    label.accuracy = 0.1,
    label.size = 11,
    label.seed = NA,
    label.colors = c("#092044", "#C33C2E", "grey60", "#BF9D09"),

    axis.text.x.color = '#000000',
    axis.text.x.size = 14,
    axis.text.x.face = 'bold',
    axis.text.x.family = 'serif',
    axis.text.x.angle = 0,
    axis.text.x.hjust = 0.5,
    axis.text.x.vjust = 0.5,
    axis.text.x.lineheight = 1,
    axis.text.x.margin.t = 0,
    axis.text.x.margin.r = 0,
    axis.text.x.margin.b = 0.15,
    axis.text.x.margin.l = 0,

    panel.grid.major.x.color = "#ECEEF2",
    panel.grid.major.x.linewidth = 5,
    panel.grid.major.x.linetype = 1,
    panel.grid.major.x.lineend = "square",

    legend = FALSE,
    legend.border = FALSE,
    legend.position = 'right',
    legend.position.inside.x = 0.5,
    legend.position.inside.y = 0.5,
    legend.direction = 'vertical',
    legend.key.width = 0.25,
    legend.key.height = 0.25,
    #
    legend.title = NA,
    legend.title.color = '#000000',
    legend.title.size = 14,
    legend.title.face = 'bold',
    legend.title.family = 'serif',
    legend.title.angle = 0,
    legend.title.hjust = 0,
    legend.title.vjust = 0.5,
    legend.title.lineheight = 1,
    legend.title.margin.t = 0,
    legend.title.margin.r = 0,
    legend.title.margin.b = 0.05,
    legend.title.margin.l = 0,
    #
    legend.text.color = '#000000',
    legend.text.size = 11,
    legend.text.face = 'plain',
    legend.text.family = 'serif',
    legend.text.angle = 0,
    legend.text.hjust = 0.5,
    legend.text.vjust = 0.5,
    legend.text.lineheight = 1,
    legend.text.margin.t = 0,
    legend.text.margin.r = 0,
    legend.text.margin.b = 0.05,
    legend.text.margin.l = 0
) {

  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(ggplot2)))
  suppressWarnings(suppressMessages(library(ggrepel)))

  if (is.null(col.highlight)) {
    if (length(highlights) == 0) {
      plot.dat$highlight <- 'default'
    } else {
      plot.dat <- plot.dat %>% mutate(
        highlight = case_when(
          Entity %in% highlights  ~ Entity,
          TRUE ~ 'default'
        )
      )
    }
  }

  g.n <- length(unique(unlist(plot.dat[, col.x])))

  data_non_highlight <- plot.dat %>% filter(!highlight %in% highlights)
  data_highlight <- plot.dat %>% filter(highlight %in% highlights)

  plot.dat <- plot.dat %>%
    mutate(col.group.xab = .data[[!!sym(col.group)]]) %>%
    mutate(col.y.xab = .data[[!!sym(col.y)]]) %>%
    mutate(label = glue::glue("{col.group.xab} ({scales::number(col.y.xab, accuracy = label.accuracy)})")) %>%
    select(-col.group.xab, -col.y.xab)

  if (is.null(col.x.level) & is.null(col.highlight)) {
    gg <- plot.dat %>% ggplot(aes(factor(!!sym(col.x)), !!sym(col.y), group = !!sym(col.group)))
  } else if (is.null(col.x.level) & !is.null(col.highlight)) {
    gg <- plot.dat %>% ggplot(aes(factor(!!sym(col.x)), !!sym(col.y), group = !!sym(col.group), color = !!sym(col.highlight)))
  } else if (!is.null(col.x.level) & is.null(col.highlight)) {
    gg <- plot.dat %>% ggplot(aes(factor(!!sym(col.x), levels = col.x.level), !!sym(col.y), group = !!sym(col.group)))
  } else {
    gg <- plot.dat %>% ggplot(aes(factor(!!sym(col.x), levels = col.x.level), !!sym(col.y), group = !!sym(col.group), color = !!sym(col.highlight)))
  }
  gg <- gg + geom_line(data = data_non_highlight, size = line.width.small) +
    geom_line(data = data_highlight, size = line.width.large) +
    geom_point(data = data_non_highlight, size = point.size.small) +
    geom_point(data = data_highlight, size = point.size.large)

  if (is.null(col.highlight)) {
    gg <- gg +  ggrepel::geom_text_repel(
      data = . %>% filter(highlight %in% highlights),
      aes(x = ifelse(!!sym(col.x) == min(!!sym(col.x)), 1 - label.offset, g.n + label.offset), label = label, hjust = ifelse(!!sym(col.x) == min(!!sym(col.x)), 1, 0)),
      size = label.size / 3.88,
      nudge_x = 0,
      direction = "y",
      seed = label.seed,
      segment.size = 0, color = label.colors[1]
    )
  }  else {
    gg <- gg + ggrepel::geom_text_repel(
      data = . %>% filter(highlight %in% highlights),
      aes(x = ifelse(!!sym(col.x) == min(!!sym(col.x)), 1 - label.offset, g.n + label.offset), label = label, hjust = ifelse(!!sym(col.x) == min(!!sym(col.x)), 1, 0)),
      size = label.size / 3.88,
      nudge_x = 0,
      direction = "y",
      seed = label.seed,
      segment.size = 0
    )
  }
  gg <- gg +
    scale_x_discrete(position = "top") +
    scale_size_identity() +
    coord_cartesian(clip = "off") +
    scale_color_manual(values = label.colors)

  gg <- gg + theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = panel.grid.major.x.color, linewidth = panel.grid.major.x.linewidth, linetype = panel.grid.major.x.linetype, lineend = panel.grid.major.x.lineend),
      axis.title = element_blank(),
      axis.text.x.top = element_text(color = axis.text.x.color, face = axis.text.x.face, family = axis.text.x.family, angle = axis.text.x.angle, hjust = axis.text.x.hjust, vjust = axis.text.x.vjust, size = axis.text.x.size, lineheight = axis.text.x.lineheight, margin = margin(t = axis.text.x.margin.t, r = axis.text.x.margin.r, b = axis.text.x.margin.b, l = axis.text.x.margin.l, unit = 'in')),
      axis.text.y = element_blank()
    )

  if (!is.null(legend.title)) {
    if (!is.na(legend.title) & tolower(legend.title) != 'none') { gg <- gg + labs(color = legend.title) }
  }

  if (legend) {
    if (legend.border) gg <- gg + theme(legend.background = ggfun::element_roundrect(color = "#636363", linetype = 1))
    gg <- gg + theme(
      legend.position = legend.position,
      legend.position.inside = c(legend.position.inside.x, legend.position.inside.y),
      legend.direction = legend.direction,
      legend.key.width = unit(legend.key.width, units = 'in'),
      legend.key.height = unit(legend.key.height, units = 'in'),
      legend.title = element_text(color = legend.title.color, face = legend.title.face, family = legend.title.family, angle = legend.title.angle, hjust = legend.title.hjust, vjust = legend.title.vjust, size = legend.title.size, lineheight = legend.title.lineheight, margin = margin(t = legend.title.margin.t, r = legend.title.margin.r, b = legend.title.margin.b, l = legend.title.margin.l, unit = 'in')),
      legend.text = element_text(color = legend.text.color, face = legend.text.face, family = legend.text.family, angle = legend.text.angle, hjust = legend.text.hjust, vjust = legend.text.vjust, size = legend.text.size, lineheight = legend.text.lineheight, margin = margin(t = legend.text.margin.t, r = legend.text.margin.r, b = legend.text.margin.b, l = legend.text.margin.l, unit = 'in'))
    )
  } else {
    gg <- gg + guides(col = "none")
  }

  gg
}

plot.dat. <- read.csv("E:/BaiduSyncdisk/006.vis-Gallery/F/F3176/data/F3176.csv")
paired_dot_plot(
  plot.dat = plot.dat.,
  col.x = 'Year',
  col.y = 'Hours',
  col.group = 'Entity',
  # col.highlight = 'highlight',
  # highlights = c('decrease', 'increase', 'same'),
  highlights = plot.dat.$Entity, # c('Argentina', 'Austria', 'Luxembourg'),
  # label.colors = 'red',
  col.x.level = c(2017, 1970),
  line.width.small = 0.5,
  line.width.large = 1,
  point.size.small = 1.5,
  point.size.large = 3,

  label.offset = 0.05,
  label.accuracy = 0.1,
  label.size = 11,
  label.seed = 1234175,
  legend = TRUE,
  legend.title = 'SADA'
) + theme(plot.margin = margin(t = 0.15, b = 0.15, l = 0.15, r = 0.15, unit = 'in'))
