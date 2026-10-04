source("tests/setup.R")
sizes <- vapply(list(MSA = app$msa_data, County = app$county_data, State = app$state_data),
  function(data) as.numeric(object.size(data)) / 1024^2, numeric(1))
print(sizes)
jsonlite::write_json(list(megabytes = as.list(sizes), total_megabytes = sum(sizes),
  note = "One modern year per MSA/County level and the full small State panel. Process peak RSS is checked separately by check_runtime_memory.R."),
  file.path(work, "data_memory.json"), pretty = TRUE, auto_unbox = TRUE)
if (sum(sizes) > 100) stop("The app's active data exceed the 100 MiB object-size limit.")

# An incomplete deployment cache must fail instead of loading the large Stata panels.
partial <- tempfile("partial-app-data-")
dir.create(partial)
failure <- tryCatch(app$make_app_data_loader(partial), error = identity)
stopifnot(inherits(failure, "error"), grepl("incomplete", conditionMessage(failure), fixed = TRUE))
unlink(partial)
if (dir.exists("app-data")) {
  manifest <- app$read_app_cache_manifest("app-data")
  partial <- tempfile("partial-app-data-")
  dir.create(partial)
  saveRDS(manifest, file.path(partial, "manifest.rds"))
  failure <- tryCatch(app$make_app_data_loader(partial), error = identity)
  stopifnot(inherits(failure, "error"), grepl("missing", conditionMessage(failure), fixed = TRUE))
  unlink(file.path(partial, "manifest.rds"))
  unlink(partial)
}
cat("Active data and incomplete-cache checks passed.\n")
