#' Multi-panel met data time series
#'
#' Aggregates to pixel-width resolution using min/median/max per bin,
#' rendering a ribbon (min-max range) with median line. Ensures extreme
#' values are preserved even at high compression ratios.
#'
#' @param met Data frame with DateTime and met variable columns
#' @param vars Variable names to plot (NULL = auto-detect)
#' @param col_datetime Column name for datetime
#' @param title Plot title
#' @param xlim X-axis range
#' @param fixed_x Share x-axis across panels (default TRUE)
#' @param n_bins Pixel-width bins for aggregation (default 2000)
#' @param ncol Layout columns (default 1)
#' @param colour Line/ribbon colour (default "#377EB8")
#' @param ribbon_alpha Ribbon transparency (default 0.3)
#' @return A patchwork plot object
#' @export
lp_met <- function(met, vars = NULL,
                   col_datetime = "DateTime",
                   title = NULL, xlim = NULL, fixed_x = TRUE,
                   n_bins = 2000, ncol = 1,
                   colour = "#377EB8", ribbon_alpha = 0.3) {

  defaults <- lp_defaults("met")

  if (is.null(vars)) {
    vars <- intersect(defaults$var, names(met))
    vars <- vars[vapply(vars, function(v) sum(!is.na(met[[v]])) > 10, logical(1))]
  }

  if (is.null(xlim) && fixed_x) {
    xlim <- range(met[[col_datetime]], na.rm = TRUE)
  }

  plots <- lapply(vars, function(v) {
    d <- met[!is.na(met[[v]]), c(col_datetime, v)]
    d <- d[d[[v]] > -9000 & d[[v]] < 9000, ]
    if (nrow(d) == 0) return(NULL)

    agg <- .aggregate_to_pixels(d[[col_datetime]], d[[v]],
                                 n_bins = n_bins, xlim = xlim)
    if (nrow(agg) == 0) return(NULL)

    p <- ggplot2::ggplot() +
      .agg_layers(agg, colour = colour, ribbon_alpha = ribbon_alpha) +
      ggplot2::labs(x = NULL, y = .var_label(v)) +
      .lp_theme()

    if (!is.null(xlim)) p <- p + ggplot2::coord_cartesian(xlim = xlim)
    p
  })

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(ggplot2::ggplot())

  p <- patchwork::wrap_plots(plots, ncol = ncol)
  if (!is.null(title)) p <- p + patchwork::plot_annotation(title = title)
  p
}


#' Met data with raw/QC comparison
#'
#' Raw data rendered as grey ribbon, QC data as coloured ribbon + line.
#'
#' @param met_qc QC'd met data frame
#' @param met_raw Raw met data frame
#' @param vars,col_datetime,title,xlim,fixed_x,n_bins,ncol See [lp_met()]
#' @param raw_colour,raw_alpha Aesthetics for raw ribbon
#' @param qc_colour Colour for QC ribbon + line
#' @return A patchwork plot object
#' @export
lp_met_qc <- function(met_qc, met_raw, vars = NULL,
                       col_datetime = "DateTime",
                       title = NULL, xlim = NULL, fixed_x = TRUE,
                       n_bins = 2000, ncol = 1,
                       raw_colour = "grey70", raw_alpha = 0.2,
                       qc_colour = "#377EB8") {

  defaults <- lp_defaults("met")

  if (is.null(vars)) {
    vars <- intersect(defaults$var, names(met_qc))
    vars <- vars[vapply(vars, function(v) sum(!is.na(met_qc[[v]])) > 10, logical(1))]
  }

  if (is.null(xlim) && fixed_x) {
    xlim <- range(c(met_qc[[col_datetime]], met_raw[[col_datetime]]), na.rm = TRUE)
  }

  plots <- lapply(vars, function(v) {
    # QC
    d_qc <- met_qc[!is.na(met_qc[[v]]), c(col_datetime, v)]
    d_qc <- d_qc[d_qc[[v]] > -9000 & d_qc[[v]] < 9000, ]
    if (nrow(d_qc) == 0) return(NULL)
    agg_qc <- .aggregate_to_pixels(d_qc[[col_datetime]], d_qc[[v]],
                                    n_bins = n_bins, xlim = xlim)

    # Raw
    has_raw <- v %in% names(met_raw)
    if (has_raw) {
      d_raw <- met_raw[!is.na(met_raw[[v]]), c(col_datetime, v)]
      d_raw <- d_raw[d_raw[[v]] > -9000 & d_raw[[v]] < 9000, ]
      if (nrow(d_raw) > 0) {
        agg_raw <- .aggregate_to_pixels(d_raw[[col_datetime]], d_raw[[v]],
                                         n_bins = n_bins, xlim = xlim)
      } else {
        has_raw <- FALSE
      }
    }

    p <- ggplot2::ggplot()
    if (has_raw) {
      p <- p + .agg_layers(agg_raw, colour = raw_colour, ribbon_alpha = raw_alpha)
    }
    p <- p + .agg_layers(agg_qc, colour = qc_colour, ribbon_alpha = 0.3) +
      ggplot2::labs(x = NULL, y = .var_label(v)) +
      .lp_theme()

    if (!is.null(xlim)) p <- p + ggplot2::coord_cartesian(xlim = xlim)
    p
  })

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(ggplot2::ggplot())

  p <- patchwork::wrap_plots(plots, ncol = ncol)
  if (!is.null(title)) p <- p + patchwork::plot_annotation(title = title)
  p
}
