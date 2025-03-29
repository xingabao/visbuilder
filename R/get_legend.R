#' Retrieve the legend of a plot
#'
#' This function extracts just the legend from a ggplot
#'
#' @param ggplot A ggplot or gtable from which to retrieve the legend
#' @return A gtable object holding just the legend or \code{NULL} if there is no legend.
#' @examples
#' library(grid)
#' library(ggplot2)
#'
#' p <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
#'   geom_point(size = 3) +
#'   theme_minimal()
#'
#' legend <- get_legend(p)
#'
#' grid.newpage()
#' grid.draw(legend)
#' @export
get_legend <- function(plot) {
  g <- ggplotGrob(plot)
  legend_index <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
  legend <- g$grobs[[legend_index]] + theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank()
  )
  return(legend)
}
