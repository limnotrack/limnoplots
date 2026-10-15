#' Plot individual profiles (depth vs value)
#'
#' Shows selected profiles as depth-vs-value line/point plots, faceted
#' by profile datetime. Useful for checking individual profile shape,
#' sensor response, and QC corrections.
#'
#' @param pro Data frame of profile data
#' @param vars Variables to plot (default "TmpWtr")
#' @param col_datetime,col_depth Column names
#' @param n_profiles Number of profiles to show (default 12)
#' @param profile_id Optional integer vector of profile IDs. If NULL,
#'   profiles are spread evenly across the record.
#' @param colour_var Variable to colour by (default NULL = colour by var name)
#' @param ncol Facet columns (default 4)
#' @return A ggplot object
#' @export
lp_profile <- function(pro, vars = "TmpWtr",
                        col_datetime = "DateTime", col_depth = "DptSns",
                        n_profiles = 12, profile_id = NULL,
                        colour_var = NULL, ncol = 4) {

  # Detect profiles if needed
  if (is.null(profile_id)) {
    dt <- pro[[col_datetime]]
    dpt <- pro[[col_depth]]
    # Simple profile detection: large time gaps or depth resets
    dt_num <- as.numeric(dt)
    pid <- cumsum(c(1, diff(dt_num) > 3600 | diff(dpt) < -5))
  } else {
    pid <- profile_id
  }

  # Pick n_profiles evenly spread
  unique_pids <- unique(pid)
  if (length(unique_pids) > n_profiles) {
    idx <- round(seq(1, length(unique_pids), length.out = n_profiles))
    sel_pids <- unique_pids[idx]
  } else {
    sel_pids <- unique_pids
  }

  # Build plot data
  plot_data <- lapply(sel_pids, function(p) {
    mask <- pid == p
    d <- pro[mask, ]
    if (nrow(d) < 3) return(NULL)

    result <- lapply(vars, function(v) {
      if (!v %in% names(d) || all(is.na(d[[v]]))) return(NULL)
      data.frame(
        profile = format(d[[col_datetime]][1], "%Y-%m-%d %H:%M"),
        depth = d[[col_depth]],
        value = d[[v]],
        variable = v,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, result)
  })

  plot_data <- do.call(rbind, plot_data)
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    message("No plottable profiles")
    return(ggplot2::ggplot())
  }

  if (length(vars) == 1) {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, y = depth)) +
      ggplot2::geom_path(colour = "#377EB8", linewidth = 0.5) +
      ggplot2::geom_point(colour = "#377EB8", size = 0.8)
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, y = depth, colour = variable)) +
      ggplot2::geom_path(linewidth = 0.5) +
      ggplot2::geom_point(size = 0.8)
  }

  p + ggplot2::scale_y_reverse() +
    ggplot2::facet_wrap(~ profile, scales = "free_x", ncol = ncol) +
    ggplot2::labs(x = NULL, y = "Depth (m)", colour = NULL) +
    .lp_theme()
}
