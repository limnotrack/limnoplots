#' Fast scatter point layer
#'
#' Uses scattermore if available for speed, falls back to geom_point.
#' @keywords internal
.fast_points <- function(mapping, data, size = 0.3, alpha = 0.5, ...) {
  if (requireNamespace("scattermore", quietly = TRUE)) {
    scattermore::geom_scattermore(mapping = mapping, data = data,
                                  pointsize = size * 3, alpha = alpha, ...)
  } else {
    ggplot2::geom_point(mapping = mapping, data = data,
                         size = size, alpha = alpha, ...)
  }
}


#' Filter extreme outliers for readable plots
#' @param x Numeric vector
#' @param lo Lower quantile (default 0.001)
#' @param hi Upper quantile (default 0.999)
#' @return Logical mask: TRUE = within range
#' @keywords internal
.quantile_mask <- function(x, lo = 0.001, hi = 0.999) {
  q <- stats::quantile(x, c(lo, hi), na.rm = TRUE)
  !is.na(x) & x >= q[1] & x <= q[2]
}


#' Standard theme for limnoplots
#' @keywords internal
.lp_theme <- function(base_size = 9) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      legend.key.height = ggplot2::unit(0.4, "cm"),
      legend.key.width  = ggplot2::unit(0.8, "cm"),
      legend.position   = "right"
    )
}


#' Depth colour scale (viridis C, reversed)
#' @keywords internal
.depth_colour <- function(name = "Depth (m)") {
  ggplot2::scale_colour_viridis_c(option = "C", direction = -1, name = name)
}
