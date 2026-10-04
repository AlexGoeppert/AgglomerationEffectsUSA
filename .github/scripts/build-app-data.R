# Build the deployment cache from the unchanged Stata panels, one level at a time.
repo <- normalizePath(Sys.getenv("APP_REPO", unset = "."), mustWork = TRUE)
setwd(repo)
loader <- new.env(parent = globalenv())
functions <- c("read_app_data", "app_data_spec", "read_app_cache_manifest", "make_app_data_loader")
for (expr in parse("app.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      as.character(expr[[2]]) %in% functions) eval(expr, loader)
}
spec <- loader$app_data_spec()
stopifnot(all(file.exists(spec$sources)))
cache_dir <- file.path(repo, "app-data")
dir.create(cache_dir, showWarnings = FALSE)
manifest_path <- file.path(cache_dir, "manifest.rds")
# An interrupted build must not leave a cache that claims to be complete.
if (file.exists(manifest_path)) unlink(manifest_path)
manifest <- list(version = spec$version, years = spec$years, files = spec$files,
  rows = setNames(integer(length(spec$files)), spec$files), columns = list(),
  source_md5 = tools::md5sum(spec$sources))
for (level in c(spec$levels, "State")) {
  cat("Preparing app data:", level, "\n")
  data <- if (level == "State") haven::read_dta(spec$sources[[level]]) else
    loader$read_app_data(spec$sources[[level]], level)
  if (anyNA(data$year) || !all(data$year %in% spec$years) ||
      !identical(sort(unique(as.integer(data$year))), spec$years))
    stop(paste(level, "must contain exactly the modern years 2001 through 2022."))
  manifest$columns[[level]] <- names(data)
  years <- if (level == "State") NA_integer_ else spec$years
  for (year in years) {
    filename <- if (level == "State") "state.rds" else paste0(level, "-", year, ".rds")
    part <- if (level == "State") data else data[data$year == year, , drop = FALSE]
    saveRDS(part, file.path(cache_dir, filename), compress = "gzip", version = 3)
    restored <- readRDS(file.path(cache_dir, filename))
    # This includes column labels, classes, missing values, row order and precision.
    if (!identical(part, restored)) stop(paste("Cache round-trip changed the source data:", filename))
    manifest$rows[[filename]] <- nrow(part)
    rm(part, restored)
  }
  rm(data)
  gc(verbose = FALSE)
}
manifest$bytes <- setNames(as.numeric(file.info(file.path(cache_dir, spec$files))$size), spec$files)
saveRDS(manifest, manifest_path, version = 3)
invisible(loader$read_app_cache_manifest(cache_dir))
cat("Validated", length(spec$files), "deployment data files; original Stata panels are unchanged.\n")
