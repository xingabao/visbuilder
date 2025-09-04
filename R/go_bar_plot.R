#' Generate a GO Enrichment Bar Plot
#'
#' This function creates a bar plot for GO enrichment analysis results. It visualizes
#' the significance of GO terms across three categories: Biological Process (BP),
#' Cellular Component (CC), and Molecular Function (MF). Bars are colored by category,
#' and additional information such as gene counts and gene IDs can be displayed.
#'
#' @param GO.dat A data frame containing GO enrichment results. It must include
#'   columns for description, ontology group, p-values, gene IDs, and counts.
#' @param col.description Character. Name of the column containing GO term descriptions.
#' @param col.group Character. Name of the column containing ontology groups (e.g., BP, CC, MF).
#' @param col.pval Character. Name of the column containing adjusted p-values.
#' @param col.gene Character. Name of the column containing gene IDs.
#' @param col.count Character. Name of the column containing gene counts.
#' @param ontology.col Character vector. Colors to represent BP, CC, and MF categories.
#' @param pvalueFilter Numeric. Threshold for filtering significant GO terms (default: 0.05).
#' @param show.n Integer. Maximum number of terms to display for each ontology group.
#' @param new.line Integer. Maximum number of genes displayed per line in the gene ID list.
#'
#' @param axis.text.x.color Character. Color of the x-axis text.
#' @param axis.text.x.size Numeric. Font size of the x-axis text.
#' @param axis.text.x.face Character. Font face of the x-axis text (e.g., bold, italic).
#' @param axis.text.x.family Character. Font family of the x-axis text.
#' @param axis.text.x.angle Numeric. Rotation angle of the x-axis text.
#' @param axis.text.x.hjust Numeric. Horizontal justification of the x-axis text.
#' @param axis.text.x.vjust Numeric. Vertical justification of the x-axis text.
#' @param axis.text.x.lineheight Numeric. Line height of the x-axis text.
#' @param axis.text.x.margin.t Numeric. Top margin around the x-axis text.
#' @param axis.text.x.margin.r Numeric. Right margin around the x-axis text.
#' @param axis.text.x.margin.b Numeric. Bottom margin around the x-axis text.
#' @param axis.text.x.margin.l Numeric. Left margin around the x-axis text.
#'
#' @param axis.text.y.limit Integer. Character limit for wrapping text in y-axis labels.
#' @param axis.text.y.color Character. Color of the y-axis text.
#' @param axis.text.y.size Numeric. Font size of the y-axis text.
#' @param axis.text.y.face Character. Font face of the y-axis text.
#' @param axis.text.y.family Character. Font family of the y-axis text.
#' @param axis.text.y.angle Numeric. Rotation angle of the y-axis text.
#' @param axis.text.y.hjust Numeric. Horizontal justification of the y-axis text.
#' @param axis.text.y.vjust Numeric. Vertical justification of the y-axis text.
#' @param axis.text.y.lineheight Numeric. Line height of the y-axis text.
#' @param axis.text.y.margin.t Numeric. Top margin around the y-axis text.
#' @param axis.text.y.margin.r Numeric. Right margin around the y-axis text.
#' @param axis.text.y.margin.b Numeric. Bottom margin around the y-axis text.
#' @param axis.text.y.margin.l Numeric. Left margin around the y-axis text.
#'
#' @param axis.title.x.color Character. Color of the x-axis title.
#' @param axis.title.x.size Numeric. Font size of the x-axis title.
#' @param axis.title.x.face Character. Font face of the x-axis title.
#' @param axis.title.x.family Character. Font family of the x-axis title.
#' @param axis.title.x.angle Numeric. Rotation angle of the x-axis title.
#' @param axis.title.x.hjust Numeric. Horizontal justification of the x-axis title.
#' @param axis.title.x.vjust Numeric. Vertical justification of the x-axis title.
#' @param axis.title.x.lineheight Numeric. Line height of the x-axis title.
#' @param axis.title.x.margin.t Numeric. Top margin around the x-axis title.
#' @param axis.title.x.margin.r Numeric. Right margin around the x-axis title.
#' @param axis.title.x.margin.b Numeric. Bottom margin around the x-axis title.
#' @param axis.title.x.margin.l Numeric. Left margin around the x-axis title.
#'
#' @param strip.text.color Character. Color of the facet strip text.
#' @param strip.text.size Numeric. Font size of the facet strip text.
#' @param strip.text.face Character. Font face of the facet strip text.
#' @param strip.text.family Character. Font family of the facet strip text.
#' @param strip.text.angle Numeric. Rotation angle of the facet strip text.
#' @param strip.text.hjust Numeric. Horizontal justification of the facet strip text.
#' @param strip.text.vjust Numeric. Vertical justification of the facet strip text.
#' @param strip.text.lineheight Numeric. Line height of the facet strip text.
#' @param strip.text.margin.t Numeric. Top margin around the facet strip text.
#' @param strip.text.margin.r Numeric. Right margin around the facet strip text.
#' @param strip.text.margin.b Numeric. Bottom margin around the facet strip text.
#' @param strip.text.margin.l Numeric. Left margin around the facet strip text.
#'
#' @param count.color Character. Color of the gene count text.
#' @param count.size Numeric. Font size of the gene count text.
#' @param count.face Character. Font face of the gene count text.
#' @param count.family Character. Font family of the gene count text.
#' @param count.angle Numeric. Rotation angle of the gene count text.
#' @param count.hjust Numeric. Horizontal justification of the gene count text.
#' @param count.vjust Numeric. Vertical justification of the gene count text.
#' @param count.lineheight Numeric. Line height of the gene count text.
#'
#' @param label.color Character. Color of the gene ID labels.
#' @param label.size Numeric. Font size of the gene ID labels.
#' @param label.face Character. Font face of the gene ID labels.
#' @param label.family Character. Font family of the gene ID labels.
#' @param label.angle Numeric. Rotation angle of the gene ID labels.
#' @param label.hjust Numeric. Horizontal justification of the gene ID labels.
#' @param label.vjust Numeric. Vertical justification of the gene ID labels.
#' @param label.lineheight Numeric. Line height of the gene ID labels.
#'
#' @return A bar plot showing GO enrichment results, facetted by ontology groups.
#' @export
#'
#' @examples
#' # Example usage
#' go_bar_plot(GO.dat = my_GO_data)
go_bar_plot <- function(
    GO.dat,
    col.description = 'Description',
    col.group = 'ONTOLOGY',
    col.pval = 'p.adjust',
    col.gene = 'geneID',
    col.count = 'Count',
    ontology.col = c("#3B4992", "#EE0000", "#008B45"),
    pvalueFilter = 0.05,
    show.n = 5,
    new.line = 12,

    axis.text.x.color = '#000000',
    axis.text.x.size = 11,
    axis.text.x.face = 'bold',
    axis.text.x.family = 'serif',
    axis.text.x.angle = 0,
    axis.text.x.hjust = 0.5,
    axis.text.x.vjust = 1,
    axis.text.x.lineheight = 1,
    axis.text.x.margin.t = 0.05,
    axis.text.x.margin.r = 0,
    axis.text.x.margin.b = 0,
    axis.text.x.margin.l = 0,

    axis.text.y.limit = 40,
    axis.text.y.color = NULL,
    axis.text.y.size = 11,
    axis.text.y.face = 'bold',
    axis.text.y.family = 'serif',
    axis.text.y.angle = 0,
    axis.text.y.hjust = 1,
    axis.text.y.vjust = 1,
    axis.text.y.lineheight = 1,
    axis.text.y.margin.t = 0,
    axis.text.y.margin.r = 0,
    axis.text.y.margin.b = 0,
    axis.text.y.margin.l = 0,

    axis.title.x.text = '-log10(P)',
    axis.title.x.color = '#000000',
    axis.title.x.size = 14,
    axis.title.x.face = 'bold',
    axis.title.x.family = 'serif',
    axis.title.x.angle = 0,
    axis.title.x.hjust = 0.5,
    axis.title.x.vjust = 1,
    axis.title.x.lineheight = 1,
    axis.title.x.margin.t = 0.05,
    axis.title.x.margin.r = 0,
    axis.title.x.margin.b = 0,
    axis.title.x.margin.l = 0,

    strip.text.color = '#000000',
    strip.text.size = 11,
    strip.text.face = 'bold',
    strip.text.family = 'serif',
    strip.text.angle = 0,
    strip.text.hjust = 0.5,
    strip.text.vjust = 0.5,
    strip.text.lineheight = 1,
    strip.text.margin.t = 0,
    strip.text.margin.r = 0.05,
    strip.text.margin.b = 0,
    strip.text.margin.l = 0.05,

    count.color = '#EE0000',
    count.size = 11,
    count.face = 'plain',
    count.family = 'serif',
    count.angle = 0,
    count.hjust = 0.15,
    count.vjust = -0.25,
    count.lineheight = 1,

    label.color = '#000000',
    label.size = 11,
    label.face = 'italic',
    label.family = 'serif',
    label.angle = 0,
    label.hjust = 0,
    label.vjust = 0.5,
    label.lineheight = 1
) {

  suppressWarnings(suppressMessages(library(grid)))
  suppressWarnings(suppressMessages(library(glue)))
  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(tidyr)))
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(ggtext)))
  suppressWarnings(suppressMessages(library(ggplot2)))

  data <- GO.dat[order(GO.dat[[col.pval]]), ]
  datasig <- data[data[[col.pval]] < pvalueFilter, , drop = FALSE]

  datasig[[col.description]] <- str_wrap(datasig[[col.description]], axis.text.y.limit)
  datasig[[col.description]] <- gsub("\n", "<br>", datasig[[col.description]])

  BP <- datasig[datasig[[col.group]] == "BP", , drop = FALSE]
  CC <- datasig[datasig[[col.group]] == "CC", , drop = FALSE]
  MF <- datasig[datasig[[col.group]] == "MF", , drop = FALSE]

  BP <- na.omit(head(BP, show.n))
  CC <- na.omit(head(CC, show.n))
  MF <- na.omit(head(MF, show.n))

  datasig <- rbind.data.frame(BP, CC, MF)
  datasig <- datasig %>% mutate(`-log10(P)` = -log10(!!sym(col.pval)))
  GO. <- data.frame()
  columns <- names(datasig)

  for (i in 1:nrow(datasig)) {
    current_row <- datasig[i, ]
    nr <- ceiling(current_row[[col.count]]/new.line)
    if (current_row[[col.count]] > new.line) {
      new_row <- as.data.frame(matrix(NA, nrow = nr, ncol = length(columns)))
      new_row[, which(columns == col.group)] <- rep(current_row[[col.group]], nr)
      new_row[, which(columns == "-log10(P)")] <- rep(0, nr)
    } else {
      new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = length(columns)))
      new_row[, which(columns == col.group)] <- rep(current_row[[col.group]], 1)
      new_row[, which(columns == "-log10(P)")] <- rep(0, 1)
    }
    colnames(new_row) <- columns
    GO. <- rbind(GO., current_row, new_row)
  }

  replace_every_nth_slash <- function(text) {
    parts <- stringr::str_split(text, "/")[[1]]
    n <- length(parts)
    if (n <= new.line) return(text)
    for (i in seq(new.line + 1, n, by = new.line)) {
      parts[i] <- paste0("\n", parts[i])
    }
    return(paste(parts, collapse = "/"))
  }

  GO. <- GO. %>% mutate('{col.gene}' := sapply(!!sym(col.gene), replace_every_nth_slash))

  palette.map <- setNames(
    ontology.col[1:length(unique(datasig[[col.group]]))],
    unique(datasig[[col.group]])
  )

  if (is.null(axis.text.y.color)) {
    axis.text.y.color.s = NA
    axis.text.y.color = NA
  } else {
    axis.text.y.color.s = axis.text.y.color
  }

  GO. <- GO. %>%
    ungroup() %>%
    mutate(Color = palette.map[!!sym(col.group)]) %>%
    mutate(id = as.character(row_number())) %>%
    mutate(desc = .data[[col.description]]) %>%
    mutate(
      '{col.description}' := case_when(
        !is.na(axis.text.y.color) ~ if_else(
          is.na(!!sym(col.description)),
          glue("<span style='color:#00000000'>{id}</span>"),
          glue("<span style='color:{axis.text.y.color}'>{desc}</span>")
        ),
        TRUE ~ if_else(
          is.na(!!sym(col.description)),
          glue("<span style='color:#00000000'>{id}</span>"),
          glue("<span style='color:{Color}'>{desc}</span>")
        )
      )
    ) %>%
    dplyr::select(-desc) %>%
    mutate('{col.description}' := factor(!!sym(col.description), levels = rev(!!sym(col.description))))

  GO.$id <- factor(GO.$id, levels = GO.$id %>% rev())

  GO.[[col.group]] <- factor(GO.[[col.group]])
  max_count = max(GO.$`-log10(P)`, na.rm = TRUE)
  max_axis = floor(max_count * 1.2)
  max_break = floor(max_axis/5)
  if (max_break == 0) { max_break = 1 }

  GO. <- GO. %>%
    mutate(
      n = ceiling(!!sym(col.count) / new.line),
      ce = ceiling(!!sym(col.count) / new.line) %% 2 == 0,
      nudge_y = -0.5 * n - 0.5
    )

  # Draw plot
  gg <-
    ggplot(GO., aes(`-log10(P)`, !!sym(col.description))) +
    geom_bar(aes(fill = !!sym(col.group)), stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = palette.map) +
    geom_text(data = GO. %>% drop_na(), aes(label = !!sym(col.count)), size = count.size / 3.88, hjust = count.vjust, vjust = count.hjust, colour = count.color, angle = count.angle, family = count.family, fontface = count.face, lineheight = count.lineheight) +
    geom_text(data = GO. %>% dplyr::filter(!is.na(geneID)), aes(x = 0.01, label = !!sym(col.gene)), vjust = label.vjust, hjust = label.hjust, size = label.size / 3.88, nudge_y = GO. %>% dplyr::filter(!is.na(geneID)) %>% pull(nudge_y), show.legend = FALSE, colour = label.color, angle = label.angle, family = label.family, fontface = label.face, lineheight = label.lineheight) +
    facet_grid(as.formula(paste(col.group, "~ .")), scales = "free_y", space = "free_y", switch = "y") +
    labs(x = axis.title.x.text, y = "") +
    scale_x_continuous(
      breaks = seq(0, max_axis, max_break),
      limits = c(0, max_axis),
      expand = c(0.001, 0)
    ) +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "#EEEEEE", color = "transparent"),
      strip.text = element_text(color = strip.text.color, face = strip.text.face, family = strip.text.family, angle = strip.text.angle, hjust = strip.text.hjust, vjust = strip.text.vjust, size = strip.text.size, lineheight = strip.text.lineheight, margin = margin(t = strip.text.margin.t, r = strip.text.margin.r, b = strip.text.margin.b, l = strip.text.margin.l, unit = 'in')),
      axis.ticks.y = element_blank(),
      axis.text.y = element_markdown(face = axis.text.y.face, family = axis.text.y.family, angle = axis.text.y.angle, hjust = axis.text.y.hjust, vjust = axis.text.y.vjust, size = axis.text.y.size, lineheight = axis.text.y.lineheight, margin = margin(t = axis.text.y.margin.t, r = axis.text.y.margin.r, b = axis.text.y.margin.b, l = axis.text.y.margin.l, unit = 'in')),
      axis.text.x = element_markdown(color = axis.text.x.color, face = axis.text.x.face, family = axis.text.x.family, angle = axis.text.x.angle, hjust = axis.text.x.hjust, vjust = axis.text.x.vjust, size = axis.text.x.size, lineheight = axis.text.x.lineheight, margin = margin(t = axis.text.x.margin.t, r = axis.text.x.margin.r, b = axis.text.x.margin.b, l = axis.text.x.margin.l, unit = 'in')),
      axis.title.x = element_text(color = axis.title.x.color, face = axis.title.x.face, family = axis.title.x.family, angle = axis.title.x.angle, hjust = axis.title.x.hjust, vjust = axis.title.x.vjust, size = axis.title.x.size, lineheight = axis.title.x.lineheight, margin = margin(t =axis.title.x.margin.t, r = axis.title.x.margin.r, b = axis.title.x.margin.b, l = axis.title.x.margin.l, unit = 'in')),
      axis.line.x.bottom = element_line(color = "black"),
      panel.spacing = unit(0.05, "lines"),
      panel.background = element_rect(fill = "white", color = "transparent"),
      panel.grid.major.x = element_blank(),
    )

  hh <- ggplot_gtable(ggplot_build(gg))
  stripr <- which(grepl("strip-l", hh$layout$name))
  k <- 1
  for (i in stripr) {
    j <- which(grepl("rect", hh$grobs[[i]]$grobs[[1]]$childrenOrder))
    hh$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- paste0(
      substr(palette.map[k], 1, 7),
      "7F"
    )
    k <- k + 1
  }

  grid.draw(hh)
}

# go_bar_plot(
#   GO.dat = visbuilderBase::GO,
#   col.description = 'Description',
#   col.group = 'ONTOLOGY',
#   col.pval = 'p.adjust',
#   col.gene = 'geneID',
#   col.count = 'Count',
#   ontology.col = c("#3B4992", "#EE0000", "#008B45"),
#   pvalueFilter = 0.05,
#   show.n = 5,
#   new.line = 7,
#
#   axis.text.x.color = '#000000',
#   axis.text.x.size = 11,
#   axis.text.x.face = 'bold',
#   axis.text.x.family = 'serif',
#   axis.text.x.angle = 0,
#   axis.text.x.hjust = 0.5,
#   axis.text.x.vjust = 1,
#   axis.text.x.lineheight = 1,
#   axis.text.x.margin.t = 0.05,
#   axis.text.x.margin.r = 0,
#   axis.text.x.margin.b = 0,
#   axis.text.x.margin.l = 0,
#
#   axis.text.y.limit = 40,
#   axis.text.y.color = '#000000',
#   axis.text.y.size = 11,
#   axis.text.y.face = 'bold',
#   axis.text.y.family = 'serif',
#   axis.text.y.angle = 0,
#   axis.text.y.hjust = 1,
#   axis.text.y.vjust = 1,
#   axis.text.y.lineheight = 1,
#   axis.text.y.margin.t = 0,
#   axis.text.y.margin.r = 0,
#   axis.text.y.margin.b = 0,
#   axis.text.y.margin.l = 0,
#
#   axis.title.x.color = '#000000',
#   axis.title.x.size = 14,
#   axis.title.x.face = 'bold',
#   axis.title.x.family = 'serif',
#   axis.title.x.angle = 0,
#   axis.title.x.hjust = 0.5,
#   axis.title.x.vjust = 1,
#   axis.title.x.lineheight = 1,
#   axis.title.x.margin.t = 0.05,
#   axis.title.x.margin.r = 0,
#   axis.title.x.margin.b = 0,
#   axis.title.x.margin.l = 0,
#
#   strip.text.color = '#000000',
#   strip.text.size = 11,
#   strip.text.face = 'bold',
#   strip.text.family = 'serif',
#   strip.text.angle = 0,
#   strip.text.hjust = 0.5,
#   strip.text.vjust = 0.5,
#   strip.text.margin.t = 0,
#   strip.text.margin.r = 0.05,
#   strip.text.margin.b = 0,
#   strip.text.margin.l = 0.05,
#
#   count.color = '#EE0000',
#   count.size = 11,
#   count.face = 'plain',
#   count.family = 'serif',
#   count.angle = 0,
#   count.hjust = 0.15,
#   count.vjust = -0.25,
#   count.lineheight = 1,
#
#   label.color = '#000000',
#   label.size = 11,
#   label.face = 'italic',
#   label.family = 'serif',
#   label.angle = 0,
#   label.hjust = 0,
#   label.vjust = 0.5,
#   label.lineheight = 1
# )

