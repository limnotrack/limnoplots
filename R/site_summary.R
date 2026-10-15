#' Generate standard plot set for a site
#'
#' One-call function that generates raw dot plot, QC dot plot, raw/QC
#' comparison, and met plots for a site, saving to a specified directory.
#'
#' @param raw Raw data list (with `pro`, `met` elements)
#' @param qc QC'd data list (with `pro`, `met` elements)
#' @param site Site name (used in titles and filenames)
#' @param out_dir Output directory for saved plots
#' @param vars_pro Profile variables to plot (NULL = auto)
#' @param vars_met Met variables to plot (NULL = auto)
#' @param max_depth Max depth filter for profile data
#' @param width,height,dpi Plot dimensions
#' @param show If TRUE, print plots to the device (default FALSE)
#' @return Invisibly, a list of ggplot objects
#' @export
lp_site_summary <- function(raw, qc = NULL, site = "Site",
                             out_dir = ".", vars_pro = NULL, vars_met = NULL,
                             max_depth = NULL,
                             width = 14, height = 14, dpi = 150,
                             show = FALSE) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  plots <- list()

  # --- Raw profile dots ---
  if ("pro" %in% names(raw) && nrow(raw$pro) > 0) {
    cat("  Plotting raw profiles...\n")
    p_raw <- profiler_dots(raw$pro, vars = vars_pro, max_depth = max_depth,
                      title = paste(site, "\u2014 Raw profiles"))
    if (show) print(p_raw)
    ggplot2::ggsave(file.path(out_dir, paste0(site, "_dots_raw.png")),
                     p_raw, width = width, height = height, dpi = dpi)
    plots$raw <- p_raw
  }

  # --- QC profile dots with raw underlay ---
  if (!is.null(qc) && "pro" %in% names(qc) && nrow(qc$pro) > 0) {
    cat("  Plotting QC profiles...\n")
    p_qc <- profiler_dots_qc(qc$pro, raw$pro, vars = vars_pro, max_depth = max_depth,
                         title = paste(site, "\u2014 QC profiles (raw in grey)"))
    if (show) print(p_qc)
    ggplot2::ggsave(file.path(out_dir, paste0(site, "_dots_qc.png")),
                     p_qc, width = width, height = height, dpi = dpi)
    plots$qc <- p_qc
  }

  # --- Raw met ---
  if ("met" %in% names(raw) && nrow(raw$met) > 0) {
    cat("  Plotting raw met...\n")
    p_met_raw <- lp_met(raw$met, vars = vars_met,
                         title = paste(site, "\u2014 Raw meteorology"))
    if (show) print(p_met_raw)
    ggplot2::ggsave(file.path(out_dir, paste0(site, "_met_raw.png")),
                     p_met_raw, width = width, height = 10, dpi = dpi)
    plots$met_raw <- p_met_raw
  }

  # --- QC met with raw underlay ---
  if (!is.null(qc) && "met" %in% names(qc) && nrow(qc$met) > 0) {
    cat("  Plotting QC met...\n")
    p_met_qc <- lp_met_qc(qc$met, raw$met, vars = vars_met,
                            title = paste(site, "\u2014 QC meteorology (raw in grey)"))
    if (show) print(p_met_qc)
    ggplot2::ggsave(file.path(out_dir, paste0(site, "_met_qc.png")),
                     p_met_qc, width = width, height = 10, dpi = dpi)
    plots$met_qc <- p_met_qc
  }

  cat("  Plots saved to:", out_dir, "\n")
  invisible(plots)
}
