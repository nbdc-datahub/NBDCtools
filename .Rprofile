source("renv/activate.R")

if (Sys.info()["sysname"] != "Darwin" && interactive()) {
  # use the PPM binary CRAN mirror on non-MacOS systems
  options(repos = c(PPM = "https://packagemanager.posit.co/cran/latest"))
}

if (Sys.getenv("INSIDE_DOCKER") == "true") {
  cat("Running inside Docker container\n")
  renv::settings$use.cache(FALSE)
  # add docker pre-installed packages to the library path to speed up package install
  .libPaths(c(.libPaths(), "/usr/local/lib/R/site-library"))
  # change addtional install path to current renv for cache
  Sys.setenv(RENV_PATHS_LIBRARY = file.path(getwd(), "renv/library"))

  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    renv::install("pkgdown")
  }
}

# {renv} aliases, make the shortcuts always start with "r"
if (requireNamespace("renv", quietly = TRUE)) {
  rstatus <- renv::status
  rinstall <- renv::install
  rremove <- renv::remove
  rsnapshot <- renv::snapshot
  rrestore <- renv::restore
}
