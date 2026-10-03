options(repos = c(CRAN = "https://cloud.r-project.org"))
target <- Sys.getenv("R_LIBS_USER")
if (!nzchar(target)) stop("R_LIBS_USER must name the isolated deployment library.")
dir.create(target, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(target, .libPaths()))

# Bootstrap a fixed renv version, then restore every package from the lockfile.
renv_version <- "1.2.4"
if (!requireNamespace("renv", quietly = TRUE) ||
    as.character(packageVersion("renv")) != renv_version) {
  archive <- tempfile(fileext = ".tar.gz")
  urls <- paste0("https://cloud.r-project.org/src/contrib/",
    c("", "Archive/renv/"), "renv_", renv_version, ".tar.gz")
  downloaded <- FALSE
  for (url in urls) {
    downloaded <- tryCatch({
      download.file(url, archive, mode = "wb", quiet = TRUE)
      file.exists(archive) && file.info(archive)$size > 0
    }, error = function(e) FALSE)
    if (downloaded) break
  }
  if (!downloaded) stop("Could not download the pinned renv package.")
  install.packages(archive, repos = NULL, type = "source", lib = target)
}
renv::restore(lockfile = "renv.lock", library = target, prompt = FALSE)

lock <- renv::lockfile_read("renv.lock")
if (as.character(getRversion()) != lock$R$Version)
  stop("The deployment R version differs from renv.lock.")
actual <- vapply(names(lock$Packages), function(package)
  as.character(packageVersion(package, lib.loc = target)), character(1))
expected <- vapply(lock$Packages, `[[`, character(1), "Version")
if (!identical(unname(actual), unname(expected)))
  stop("The restored package versions differ from renv.lock.")
output <- Sys.getenv("APP_TEST_OUTPUT", unset = "/tmp/agglomeration-checks")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
write.csv(data.frame(package = names(actual), version = actual),
  file.path(output, "package_versions.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(output, "session_info.txt"))
cat("Restored and checked", length(actual), "locked packages.\n")
