#' Fast PNG save using ragg
#'
#' Wrapper around ggsave that uses ragg::agg_png for faster rendering.
#' Falls back to standard ggsave if ragg is not available.
#'
#' @param filename Output path
#' @param plot ggplot object
#' @param width Width in inches (default 14)
#' @param height Height in inches (default 14)
#' @param dpi Resolution (default 150)
#' @param ... Additional arguments passed to ggsave
#' @return Invisibly, the filename
#' @keywords internal
.fast_save <- function(filename, plot, width = 14, height = 14, dpi = 150, ...) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename, width = width, height = height, units = "in", res = dpi)
    print(plot)
    grDevices::dev.off()
  } else {
    ggplot2::ggsave(filename, plot, width = width, height = height, dpi = dpi, ...)
  }
  invisible(filename)
}
