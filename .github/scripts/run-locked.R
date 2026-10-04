arguments <- commandArgs(trailingOnly = TRUE)
target_library <- Sys.getenv("R_LIBS_USER")
if (!nzchar(target_library)) stop("R_LIBS_USER must name the isolated deployment library.")
dir.create(target_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(target_library, .Library), include.site = FALSE)
if (!identical(normalizePath(.libPaths(), mustWork = TRUE),
    unique(normalizePath(c(target_library, .Library), mustWork = TRUE))))
  stop("Could not isolate the deployment R library.")

if (length(arguments) == 2L && identical(arguments[[1]], "-e")) {
  eval(parse(text = arguments[[2]]), envir = globalenv())
} else if (length(arguments) == 1L && file.exists(arguments[[1]])) {
  sys.source(arguments[[1]], envir = globalenv(), keep.source = TRUE)
} else {
  stop("Supply one R script, or -e followed by one R expression.")
}
