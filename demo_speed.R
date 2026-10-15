## limnoplots speed demo
## Benchmarks render time for each plot type across multiple sites.
## Run interactively to see plots in the viewer.

library(ggplot2)
library(dplyr)
library(patchwork)

# Source package
pkg_dir <- "C:/Users/chris/Dropbox/limnotrack/software/limnoplots/R"
for (f in list.files(pkg_dir, full.names = TRUE)) source(f)

# Source QC helpers for running QC
qc_funs <- list.files("C:/Users/chris/Dropbox/limnotrack/dropbox/wqprofiler/scripts_qc/funs_qc",
                       pattern = "*.R", full.names = TRUE)
invisible(lapply(qc_funs, source))
tryCatch(source("C:/Users/chris/Dropbox/limnotrack/software/wqprofiler-data/fun/derived_vars.R"),
         error = function(e) NULL)

raw_base  <- "C:/Users/chris/Dropbox/limnotrack/dropbox/wqprofiler/data/raw"
full_base <- "C:/Users/chris/Dropbox/limnotrack/dropbox/wqprofiler/data/full"
qc_base   <- "C:/Users/chris/Dropbox/limnotrack/dropbox/wqprofiler/scripts_qc"
fig_dir   <- "C:/Users/chris/Dropbox/limnotrack/software/limnoplots/figures"
dir.create(fig_dir, showWarnings = FALSE)

Sys.setenv(tz = "Etc/GMT-12")

# Timing helper
bench <- function(label, expr) {
  t0 <- Sys.time()
  result <- force(expr)
  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 2)
  cat(sprintf("  %-40s %6.2f s\n", label, elapsed))
  invisible(result)
}


# =============================================================================
cat("\n================================================================\n")
cat("LIMNOPLOTS SPEED DEMO\n")
cat("================================================================\n\n")

sites <- c("Hayes", "Okataina", "Rotoiti")

for (site in sites) {

  cat(sprintf("\n========== %s ==========\n", toupper(site)))

  raw  <- readRDS(file.path(raw_base, paste0(site, ".rds")))
  full <- readRDS(file.path(full_base, paste0("full_", site, ".rds")))
  cat(sprintf("  Data: %s profile rows, %s met rows\n",
              format(nrow(raw$pro), big.mark = ","),
              format(nrow(raw$met), big.mark = ",")))

  # Add profile_id if not present
  raw$pro <- bench("lp_add_profile_id", lp_add_profile_id(raw$pro))

  # --- Raw dots (main benchmark) ---
  p <- bench("lp_dots (raw, all vars)", {
    lp_dots(raw$pro, title = paste(site, "— Raw"))
  })
  bench("  ggsave PNG 14x14 150dpi", {
    .fast_save(file.path(fig_dir, paste0(site, "_dots_raw.png")),
               p, width = 14, height = 14, dpi = 150)
  })

  # --- Raw met ---
  p_met <- bench("lp_met (raw)", {
    lp_met(raw$met, title = paste(site, "— Raw met"))
  })
  bench("  ggsave PNG 14x10 150dpi", {
    .fast_save(file.path(fig_dir, paste0(site, "_met_raw.png")),
               p_met, width = 14, height = 10, dpi = 150)
  })

  # --- QC ---
  qc_script <- file.path(qc_base, paste0("qc1_", site, ".R"))
  if (file.exists(qc_script)) {
    source(qc_script)
    qc_result <- bench("Run QC script", {
      tryCatch(qc1_site(raw, full$meta), error = function(e) {
        cat("    QC error:", conditionMessage(e), "\n")
        NULL
      })
    })

    if (!is.null(qc_result)) {
      # QC dots with raw underlay
      p_qc <- bench("lp_dots_qc (QC + raw underlay)", {
        lp_dots_qc(qc_result$pro, raw$pro,
                    title = paste(site, "— QC (raw in grey)"))
      })
      bench("  ggsave PNG 14x14 150dpi", {
        .fast_save(file.path(fig_dir, paste0(site, "_dots_qc.png")),
                   p_qc, width = 14, height = 14, dpi = 150)
      })

      # QC met with raw underlay
      if ("met" %in% names(qc_result) && nrow(qc_result$met) > 0) {
        p_met_qc <- bench("lp_met_qc (QC + raw)", {
          lp_met_qc(qc_result$met, raw$met,
                     title = paste(site, "— QC met (raw in grey)"))
        })
        bench("  ggsave PNG 14x10 150dpi", {
          .fast_save(file.path(fig_dir, paste0(site, "_met_qc.png")),
                   p_met_qc, width = 14, height = 10, dpi = 150)
        })
      }
    }
  }

  # --- Profile plots ---
  p_prof <- bench("lp_profile (16 profiles, TmpWtr)", {
    lp_profile(raw$pro, vars = "TmpWtr", n_profiles = 16)
  })
  bench("  ggsave PNG 14x10 150dpi", {
    .fast_save(file.path(fig_dir, paste0(site, "_profiles.png")),
               p_prof, width = 14, height = 10, dpi = 150)
  })
}


# =============================================================================
# Summary
# =============================================================================
cat("\n\n================================================================\n")
cat("SUMMARY\n")
cat("================================================================\n")
cat("Figures saved to:", fig_dir, "\n")
cat("\nKey functions:\n")
cat("  lp_dots()          — profile dot plot (depth-coloured)\n")
cat("  lp_dots_qc()       — QC with raw underlay\n")
cat("  lp_met()           — met time series\n")
cat("  lp_met_qc()        — QC met with raw underlay\n")
cat("  lp_profile()       — individual profiles\n")
cat("  lp_device_timeline() — device tracking\n")
cat("  lp_site_summary()  — all plots in one call\n")
cat("\nInstall: remotes::install_github('limnotrack/limnoplots')\n")
