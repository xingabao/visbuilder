#' Function Title
#'
#' Brief description of the function.
#'
#' @param param1 Description of the first parameter.
#' @param param2 Description of the second parameter.
#' @return Description of the return value.
#'
#' @examples
barplot.horizontal.filled <- function(dat) {
  gg <- ggplot() + geom_segment(
    data = dat,
    aes(x = name, xend = name, y = -2, yend = 3),
    color = "grey80",
    size = 0.25) +
    geom_hline(yintercept = -1, color = 'grey80', size = 0.5) +
    geom_hline(yintercept = -0, color = 'grey80', size = 0.5) +
    geom_hline(yintercept = 1, color = 'grey80', size = 0.5) +
    geom_hline(yintercept = 2, color = 'grey80', size = 0.5) +
    geom_rect(aes(xmin = 0.5, xmax = 8.5, ymin = -3.2, ymax = -2), fill = "blue", color = NA, alpha = 0.3) +
    geom_rect(aes(xmin = 8.5, xmax = 16.5, ymin = -3.2, ymax = -2), fill = "green", color = NA, alpha = 0.3) +
    geom_rect(aes(xmin = 16.5, xmax = 24.5, ymin = -3.2, ymax = -2), fill = "orange", color = NA, alpha = 0.3) +
    geom_rect(aes(xmin = 24.5, xmax = 32.5, ymin = -3.2, ymax = -2), fill = "red", color = NA, alpha = 0.3) +
    geom_col(data = dat, aes(x = name, y = mpg_z, fill = mpg_grp), show.legend = TRUE) +
    scale_y_continuous(limits = c(-3.2, 3), expand = c(0, 0), breaks = c(-2, -1, 0, 1, 2, 3)) +
    scale_fill_manual(values = c("low" = "#75A4C9", "high" = "#9D69B1")) +
    annotate(
      geom = "text",
      x = dat$name,
      y = -2.05,
      label = dat$name,
      angle = 0,
      hjust = 1.0,
      vjust = 0.5,
      size = 3.5
    ) + coord_flip() +
    geom_segment(
      data = dat,
      aes(x = name, xend = name, y = -2.03, yend = -2),
      color = "#000000",
      size = 0.5
    ) +
    labs(
      x = 'Name',
      y = 'MPG z-score',
      fill = 'MPG Score'
    ) +
    geom_hline(yintercept = -2, color = "#000000", size = 0.5) +
    geom_vline(xintercept = 32.6, color = "#000000", size = 1) +
    geom_vline(xintercept = 0.4, color = "#000000", size = 1) +
    theme(
      axis.text.x = element_text(size = 12, color = '#000000'),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(color = '#000000', size = 14),
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.position = 'top',
      legend.title = element_text(color = '#000000', size = 14),
      legend.text = element_text(color = '#000000', size = 14),
      legend.margin = margin(t = 5, b = -5)
    )

  return(gg)
}
