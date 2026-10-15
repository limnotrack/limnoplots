#' Fast scatter point layer
#'
#' Uses scattermore if available for speed, falls back to geom_point.
#' Pointsize is tuned for the depth-coloured dot plot aesthetic:
#' slightly larger than minimal, with moderate transparency so
#' overlapping depths blend nicely.
#'
#' @param mapping aes() mapping
#' @param data data frame
#' @param pointsize scattermore pointsize in pixels (default 2)
#' @param alpha transparency (default 0.6)
#' @param pixels scattermore pre-render resolution c(width, height).
#'   NULL = auto from device.
#' @param ... passed to geom
#' @keywords internal
.fast_points <- function(mapping, data, pointsize = 2, alpha = 0.6,
                         pixels = NULL, ...) {
  if (requireNamespace("scattermore", quietly = TRUE)) {
    args <- list(mapping = mapping, data = data,
                 pointsize = pointsize, alpha = alpha)
    if (!is.null(pixels)) args$pixels <- pixels
    do.call(scattermore::geom_scattermore, c(args, list(...)))
  } else {
    ggplot2::geom_point(mapping = mapping, data = data,
                         size = pointsize / 3, alpha = alpha, ...)
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


#' Standard theme for limnoplots — grey panel, no gridlines
#' @keywords internal
.lp_theme <- function(base_size = 9) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = "grey92", colour = NA),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      plot.title        = ggplot2::element_text(hjust = 0.5),
      legend.key.height = ggplot2::unit(0.4, "cm"),
      legend.key.width  = ggplot2::unit(0.8, "cm"),
      legend.position   = "right"
    )
}


#' Depth colour scale — limnoviz depthclrs palette
#' Warm olive/green for shallow, cool blue/purple for deep.
#' @keywords internal
.depth_colour <- function(name = "Depth (m)") {
  # From limnoviz::heatmap_palettes()$depthclrs
  depthclrs <- c("#CDC376FF", "#B0D48CFF", "#92D8A4FF", "#65C2A4FF",
                  "#52A8A3FF", "#488E9EFF", "#407498FF", "#3E5A92FF",
                  "#41407BFF", "#382D51FF", "#281A2CFF")
  ggplot2::scale_colour_gradientn(colours = depthclrs, name = name)
}
