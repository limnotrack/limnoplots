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


#' Standard theme for limnoplots — grey panel background
#' @keywords internal
.lp_theme <- function(base_size = 9) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = "grey92", colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = "white", linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank(),
      legend.key.height = ggplot2::unit(0.4, "cm"),
      legend.key.width  = ggplot2::unit(0.8, "cm"),
      legend.position   = "right"
    )
}


#' Depth colour scale — reversed Spectral (warm shallow, cool deep)
#' @keywords internal
.depth_colour <- function(name = "Depth (m)") {
  spectral <- rev(c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B",
                     "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"))
  ggplot2::scale_colour_gradientn(colours = spectral, name = name)
}
