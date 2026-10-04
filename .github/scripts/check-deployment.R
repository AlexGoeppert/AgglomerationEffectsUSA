options(repos = c(CRAN = "https://cloud.r-project.org"))
target <- Sys.getenv("R_LIBS_USER")
if (!nzchar(target)) stop("R_LIBS_USER must name the isolated deployment library.")
expected_paths <- normalizePath(c(target, .Library), mustWork = TRUE)
actual_paths <- normalizePath(.libPaths(), mustWork = TRUE)
output <- Sys.getenv("APP_TEST_OUTPUT", unset = "/tmp/agglomeration-checks")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
cat("Deployment library search paths:\n", paste(actual_paths, collapse = "\n"), "\n")
writeLines(actual_paths, file.path(output, "deployment_library_paths.txt"))
if (!identical(actual_paths, unique(expected_paths)))
  stop("A library outside the tested deployment environment is on the R search path.")

lock <- renv::lockfile_read("renv.lock")
expected <- vapply(lock$Packages, `[[`, character(1), "Version")
# Use the same DESCRIPTION lookup as rsconnect, including already loaded packages.
actual <- vapply(names(expected), function(package)
  utils::packageDescription(package, fields = "Version"), character(1))
locations <- vapply(names(expected), find.package, character(1))
versions <- data.frame(package = names(expected), expected = expected,
  version = actual, path = locations)
write.csv(versions, file.path(output, "deployment_package_versions.csv"), row.names = FALSE)
different <- is.na(actual) | actual != expected
if (any(different)) {
  print(versions[different, ], row.names = FALSE)
  stop("The effective deployment library differs from renv.lock.")
}

# This calls the public strict bundler without copying or loading the data files.
dependencies <- rsconnect::appDependencies(appDir = ".",
  appFiles = c("app.R", "renv.lock"), dependencyResolution = "strict")
stopifnot(setequal(dependencies$Package, names(expected)),
  all(dependencies$Version == expected[dependencies$Package]))
write.csv(dependencies, file.path(output, "deployment_dependencies.csv"), row.names = FALSE)
cat("Strict deployment dependency check passed for", nrow(dependencies), "packages.\n")
