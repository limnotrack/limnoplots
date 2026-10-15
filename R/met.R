#' Multi-panel met data time series
#'
#' Standard panel plot for meteorological data: air temp, humidity,
#' wind, radiation, rainfall, pressure.
#'
#' @param met Data frame with DateTime and met variable columns
#' @param vars Variable names to plot (NULL = auto-detect)
#' @param col_datetime Column name for datetime
#' @param title Plot title
#' @param xlim X-axis range
#' @param fixed_x Share x-axis across panels (default TRUE)
#' @param ncol Layout columns (default 1)
#' @param line_colour Line colour (default "#377EB8")
#' @return A patchwork plot object
#' @export
lp_met <- function(met, vars = NULL,
                   col_datetime = "DateTime",
                   title = NULL, xlim = NULL, fixed_x = TRUE,
                   ncol = 1, line_colour = "#377EB8") {

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
    names(d) <- c("dt", "value")
    d <- d[d$value > -9000 & d$value < 9000, ]
    if (nrow(d) == 0) return(NULL)

    p <- ggplot2::ggplot(d, ggplot2::aes(x = dt, y = value)) +
      ggplot2::geom_line(colour = line_colour, linewidth = 0.3) +
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
#' @param met_qc QC'd met data frame
#' @param met_raw Raw met data frame
#' @param vars,col_datetime,title,xlim,fixed_x,ncol See [lp_met()]
#' @param raw_colour,raw_alpha Aesthetics for raw data
#' @param qc_colour Colour for QC data
#' @return A patchwork plot object
#' @export
lp_met_qc <- function(met_qc, met_raw, vars = NULL,
                       col_datetime = "DateTime",
                       title = NULL, xlim = NULL, fixed_x = TRUE,
                       ncol = 1,
                       raw_colour = "grey70", raw_alpha = 0.5,
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
    d_qc <- met_qc[!is.na(met_qc[[v]]), c(col_datetime, v)]
    names(d_qc) <- c("dt", "value")
    d_qc <- d_qc[d_qc$value > -9000 & d_qc$value < 9000, ]
    if (nrow(d_qc) == 0) return(NULL)

    has_raw <- v %in% names(met_raw)
    if (has_raw) {
      d_raw <- met_raw[!is.na(met_raw[[v]]), c(col_datetime, v)]
      names(d_raw) <- c("dt", "value")
      d_raw <- d_raw[d_raw$value > -9000 & d_raw$value < 9000, ]
      if (nrow(d_raw) == 0) has_raw <- FALSE
    }

    if (has_raw) {
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(data = d_raw, ggplot2::aes(x = dt, y = value),
                            colour = raw_colour, linewidth = 0.3, alpha = raw_alpha) +
        ggplot2::geom_line(data = d_qc, ggplot2::aes(x = dt, y = value),
                            colour = qc_colour, linewidth = 0.3)
    } else {
      p <- ggplot2::ggplot(d_qc, ggplot2::aes(x = dt, y = value)) +
        ggplot2::geom_line(colour = qc_colour, linewidth = 0.3)
    }

    p <- p + ggplot2::labs(x = NULL, y = .var_label(v)) + .lp_theme()
    if (!is.null(xlim)) p <- p + ggplot2::coord_cartesian(xlim = xlim)
    p
  })

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(ggplot2::ggplot())

  p <- patchwork::wrap_plots(plots, ncol = ncol)
  if (!is.null(title)) p <- p + patchwork::plot_annotation(title = title)
  p
}
