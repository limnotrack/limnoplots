#' Multi-panel dot plot of profile data coloured by depth
#'
#' The workhorse plot for profiler data. Each variable gets its own panel
#' with time on x, value on y, coloured by depth. Uses scattermore for
#' speed when available.
#'
#' @param pro Data frame of profile data with DateTime, DptSns, and
#'   variable columns.
#' @param vars Character vector of variable names to plot. If NULL,
#'   auto-detects from the data using [lp_defaults()].
#' @param col_datetime,col_depth Column names for datetime and depth.
#' @param title Plot title (default NULL = auto from data)
#' @param xlim Date/POSIXct range for x-axis (default NULL = auto)
#' @param fixed_x If TRUE, all panels share the same x-axis (default TRUE)
#' @param quantile_trim Quantile range for outlier trimming (default c(0.001, 0.999))
#' @param max_depth Maximum depth to include (default NULL = all)
#' @param point_size Point size (default 0.3)
#' @param alpha Point transparency (default 0.5)
#' @param ncol Number of columns in panel layout (default 1)
#' @return A patchwork plot object
#' @export
#'
#' @examples
#' \dontrun{
#' raw <- readRDS("data/raw/Okataina.rds")
#' profiler_dots(raw$pro, title = "Okataina — Raw")
#' }
profiler_dots <- function(pro, vars = NULL,
                    col_datetime = "DateTime", col_depth = "DptSns",
                    title = NULL, xlim = NULL, fixed_x = TRUE,
                    quantile_trim = c(0.001, 0.999),
                    max_depth = NULL, pointsize = 2, alpha = 0.6,
                    ncol = 1) {

  defaults <- lp_defaults("profile")

  # Filter depth
  if (!is.null(max_depth)) {
    pro <- pro[!is.na(pro[[col_depth]]) & pro[[col_depth]] <= max_depth, ]
  }
  pro <- pro[!is.na(pro[[col_depth]]) & pro[[col_depth]] > 0, ]

  # Auto-detect variables
  if (is.null(vars)) {
    vars <- intersect(defaults$var, names(pro))
    # Only keep vars with > 100 non-NA values
    vars <- vars[vapply(vars, function(v) sum(!is.na(pro[[v]])) > 100, logical(1))]
  }

  if (length(vars) == 0) {
    message("No plottable variables found")
    return(ggplot2::ggplot())
  }

  if (is.null(xlim) && fixed_x) {
    xlim <- range(pro[[col_datetime]], na.rm = TRUE)
  }

  plots <- lapply(vars, function(v) {
    d <- pro[!is.na(pro[[v]]), c(col_datetime, col_depth, v)]
    names(d) <- c("dt", "depth", "value")

    # Remove sentinels and extreme outliers
    d <- d[d$value > -9000 & d$value < 9000, ]
    if (nrow(d) == 0) return(NULL)

    mask <- .quantile_mask(d$value, quantile_trim[1], quantile_trim[2])
    d <- d[mask, ]
    if (nrow(d) == 0) return(NULL)

    p <- ggplot2::ggplot(d, ggplot2::aes(x = dt, y = value, colour = depth)) +
      .fast_points(ggplot2::aes(x = dt, y = value, colour = depth),
                   data = d, pointsize = pointsize, alpha = alpha) +
      .depth_colour() +
      ggplot2::labs(x = NULL, y = .var_label(v)) +
      .lp_theme()

    if (!is.null(xlim)) {
      p <- p + ggplot2::coord_cartesian(xlim = xlim)
    }

    p
  })

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(ggplot2::ggplot())

  p <- patchwork::wrap_plots(plots, ncol = ncol) +
    patchwork::plot_layout(guides = "collect")
  if (!is.null(title)) {
    p <- p + patchwork::plot_annotation(title = title)
  }
  p &  ggplot2::theme(legend.position = "right")
}


#' Multi-panel dot plot with raw/QC comparison
#'
#' QC data plotted in colour over raw data in grey. Instantly shows
#' what corrections were applied.
#'
#' @param pro_qc QC'd profile data frame
#' @param pro_raw Raw profile data frame
#' @param vars Variables to plot (NULL = auto-detect from QC data)
#' @param col_datetime,col_depth Column names
#' @param title Plot title
#' @param xlim,fixed_x X-axis control
#' @param quantile_trim Outlier trimming quantiles
#' @param max_depth Max depth filter
#' @param raw_colour Colour for raw data (default "grey80")
#' @param raw_alpha Alpha for raw data (default 0.3)
#' @param point_size,alpha Point aesthetics for QC data
#' @param ncol Layout columns
#' @return A patchwork plot object
#' @export
profiler_dots_qc <- function(pro_qc, pro_raw, vars = NULL,
                        col_datetime = "DateTime", col_depth = "DptSns",
                        title = NULL, xlim = NULL, fixed_x = TRUE,
                        quantile_trim = c(0.001, 0.999),
                        max_depth = NULL,
                        raw_colour = "grey80", raw_alpha = 0.3,
                        pointsize = 2, alpha = 0.6,
                        ncol = 1) {

  defaults <- lp_defaults("profile")

  if (!is.null(max_depth)) {
    pro_qc  <- pro_qc[!is.na(pro_qc[[col_depth]])  & pro_qc[[col_depth]]  <= max_depth & pro_qc[[col_depth]] > 0, ]
    pro_raw <- pro_raw[!is.na(pro_raw[[col_depth]]) & pro_raw[[col_depth]] <= max_depth & pro_raw[[col_depth]] > 0, ]
  } else {
    pro_qc  <- pro_qc[!is.na(pro_qc[[col_depth]])  & pro_qc[[col_depth]] > 0, ]
    pro_raw <- pro_raw[!is.na(pro_raw[[col_depth]]) & pro_raw[[col_depth]] > 0, ]
  }

  if (is.null(vars)) {
    vars <- intersect(defaults$var, names(pro_qc))
    vars <- vars[vapply(vars, function(v) sum(!is.na(pro_qc[[v]])) > 100, logical(1))]
  }

  if (is.null(xlim) && fixed_x) {
    xlim <- range(c(pro_qc[[col_datetime]], pro_raw[[col_datetime]]), na.rm = TRUE)
  }

  plots <- lapply(vars, function(v) {
    # QC data
    d_qc <- pro_qc[!is.na(pro_qc[[v]]), c(col_datetime, col_depth, v)]
    names(d_qc) <- c("dt", "depth", "value")
    d_qc <- d_qc[d_qc$value > -9000 & d_qc$value < 9000, ]
    if (nrow(d_qc) == 0) return(NULL)
    q <- stats::quantile(d_qc$value, quantile_trim, na.rm = TRUE)
    d_qc <- d_qc[d_qc$value >= q[1] & d_qc$value <= q[2], ]

    # Raw data (use same y-range as QC for comparability)
    raw_col <- v
    # Handle Cond→SpCond rename
    if (v == "SpCond" && !"SpCond" %in% names(pro_raw) && "Cond" %in% names(pro_raw)) {
      raw_col <- "Cond"
    }
    # Handle DOconc derived from DOpsat
    if (v == "DOconc" && !"DOconc" %in% names(pro_raw) && "DOpsat" %in% names(pro_raw)) {
      raw_col <- NULL  # skip raw underlay for derived vars
    }

    has_raw <- FALSE
    if (!is.null(raw_col) && raw_col %in% names(pro_raw)) {
      d_raw <- pro_raw[!is.na(pro_raw[[raw_col]]), c(col_datetime, col_depth, raw_col)]
      names(d_raw) <- c("dt", "depth", "value")
      d_raw <- d_raw[d_raw$value > -9000 & d_raw$value < 9000, ]
      d_raw <- d_raw[d_raw$value >= q[1] & d_raw$value <= q[2], ]
      if (nrow(d_raw) > 0) has_raw <- TRUE
    }

    if (has_raw) {
      p <- ggplot2::ggplot() +
        .fast_points(ggplot2::aes(x = dt, y = value), data = d_raw,
                     pointsize = max(1, pointsize - 1), alpha = raw_alpha, colour = raw_colour) +
        .fast_points(ggplot2::aes(x = dt, y = value, colour = depth), data = d_qc,
                     size = point_size, alpha = alpha)
    } else {
      p <- ggplot2::ggplot(d_qc, ggplot2::aes(x = dt, y = value, colour = depth)) +
        .fast_points(ggplot2::aes(x = dt, y = value, colour = depth),
                     data = d_qc, size = point_size, alpha = alpha)
    }

    p <- p + .depth_colour() +
      ggplot2::labs(x = NULL, y = .var_label(v)) +
      .lp_theme()

    if (!is.null(xlim)) p <- p + ggplot2::coord_cartesian(xlim = xlim)
    p
  })

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(ggplot2::ggplot())

  p <- patchwork::wrap_plots(plots, ncol = ncol) +
    patchwork::plot_layout(guides = "collect")
  if (!is.null(title)) p <- p + patchwork::plot_annotation(title = title)
  p & ggplot2::theme(legend.position = "right")
}
