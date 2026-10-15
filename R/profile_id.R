#' Add or detect profile ID column
#'
#' Ensures a profile data frame has a profile ID column. If one exists
#' (user-specified or auto-detected as `profile_id` or `pro_id`), it is
#' used. Otherwise, profiles are detected from depth resets and time gaps,
#' and an ID column is added in `yyyydddhhmm` format (year, day-of-year,
#' hour, minute based on profile start time).
#'
#' @param pro Data frame of profile data
#' @param col_profile Name of existing profile ID column, or NULL to auto-detect/create
#' @param col_datetime Name of datetime column (default "DateTime")
#' @param col_depth Name of depth column (default "DptSns")
#' @param gap_hours Time gap (hours) that starts a new profile (default 2)
#' @param reset_m Depth decrease (metres) that starts a new profile (default 5)
#' @return The input data frame with a `profile_id` column (character, yyyydddhhmm format).
#'   Also sets attribute `"profile_id_col"` to the column name used.
#' @export
lp_add_profile_id <- function(pro, col_profile = NULL,
                               col_datetime = "DateTime",
                               col_depth = "DptSns",
                               gap_hours = 2, reset_m = 5) {

  # Try to find existing column
  if (is.null(col_profile)) {
    candidates <- c("profile_id", "pro_id", ".pro_id")
    found <- intersect(candidates, names(pro))
    if (length(found) > 0) {
      col_profile <- found[1]
    }
  }

  if (!is.null(col_profile) && col_profile %in% names(pro)) {
    # Existing column found — standardise name
    if (col_profile != "profile_id") {
      pro$profile_id <- pro[[col_profile]]
    }
    attr(pro, "profile_id_col") <- col_profile
    return(pro)
  }

  # Detect profiles from depth resets and time gaps
  datetime <- pro[[col_datetime]]
  depth    <- pro[[col_depth]]

  n <- length(datetime)
  if (n == 0) {
    pro$profile_id <- character(0)
    return(pro)
  }

  dt_num <- as.numeric(datetime)
  gap_secs <- gap_hours * 3600

  # Detect profile boundaries
  is_new <- logical(n)
  is_new[1] <- TRUE
  for (i in 2:n) {
    time_gap <- is.na(dt_num[i]) || is.na(dt_num[i - 1]) ||
      (dt_num[i] - dt_num[i - 1]) > gap_secs
    depth_reset <- !is.na(depth[i]) && !is.na(depth[i - 1]) &&
      (depth[i] - depth[i - 1]) < -reset_m
    is_new[i] <- time_gap || depth_reset
  }

  # Assign IDs from profile start time
  pid_int <- cumsum(is_new)
  starts <- which(is_new)
  start_times <- datetime[starts]

  # Format: yyyydddhhmm
  id_labels <- format(start_times, "%Y%j%H%M")

  pro$profile_id <- id_labels[pid_int]
  attr(pro, "profile_id_col") <- "profile_id"

  pro
}


#' Get integer profile index from profile_id column
#'
#' Converts the character profile_id to a fast integer index for
#' grouping operations.
#'
#' @param profile_id Character vector of profile IDs
#' @return Integer vector (sequential profile number)
#' @keywords internal
.profile_int <- function(profile_id) {
  as.integer(factor(profile_id, levels = unique(profile_id)))
}
