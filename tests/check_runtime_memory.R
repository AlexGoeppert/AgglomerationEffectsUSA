# Run this in a fresh R process after build-app-data.R, using the production loader.
repo <- normalizePath(Sys.getenv("APP_REPO", unset = "."), mustWork = TRUE)
work <- Sys.getenv("APP_TEST_OUTPUT", unset = tempdir())
dir.create(work, recursive = TRUE, showWarnings = FALSE)
setwd(repo)
limit_mb <- as.numeric(Sys.getenv("APP_MEMORY_LIMIT_MB", unset = "750"))
stopifnot(is.finite(limit_mb), limit_mb > 0)

check_runtime <- function() {
  records <- list()
  models <- list()
  passed <- FALSE
  problem <- NULL
  process_memory <- function() {
    status <- "/proc/self/status"
    if (!file.exists(status)) stop("The production memory guard requires Linux /proc/self/status.")
    lines <- readLines(status, warn = FALSE)
    value <- function(field) {
      line <- grep(paste0("^", field, ":"), lines, value = TRUE)
      if (length(line) != 1L) stop(paste("Missing process memory field:", field))
      as.numeric(sub(paste0("^", field, ":\\s*([0-9]+).*$"), "\\1", line, perl = TRUE)) / 1024
    }
    c(rss_mb = value("VmRSS"), peak_rss_mb = value("VmHWM"))
  }
  record_memory <- function(stage) {
    memory <- process_memory()
    records[[length(records) + 1L]] <<- c(list(stage = stage), as.list(memory))
    cat(sprintf("Memory %s: RSS %.1f MiB; peak %.1f MiB\n", stage, memory[[1]], memory[[2]]))
    if (any(!is.finite(memory)) || memory[["peak_rss_mb"]] > limit_mb)
      stop(sprintf("Production peak RSS exceeds the %.0f MiB budget at %s.", limit_mb, stage))
  }
  on.exit({
    jsonlite::write_json(list(passed = passed, error = problem, limit_mb = limit_mb,
      memory = records, models = models), file.path(work, "runtime_memory.json"),
      pretty = TRUE, auto_unbox = TRUE, null = "null", digits = 12)
  }, add = TRUE)
  tryCatch({
    if (!dir.exists("app-data")) stop("Build the deployment cache before checking production memory.")
    record_memory("before startup")
    app <- new.env(parent = globalenv())
    # Unlike setup.R, this executes the real startup block and keeps its data alive.
    sys.source("app.R", envir = app)
    stopifnot(app$app_data_loader$cached, length(app$app_data_loader$cache_info()) == 0L,
      !exists("msa_data", envir = app, inherits = FALSE), !exists("county_data", envir = app, inherits = FALSE))
    for (generic in c("coef", "vcov", "nobs", "residuals", "fitted", "confint"))
      registerS3method(generic, "ch_model", app[[paste0(generic, ".ch_model")]], envir = asNamespace("stats"))
    fixest::setFixest_notes(FALSE)
    record_memory("production startup")
    defaults <- list(analysis_level = "MSA", year_modern = "2010", analysis_type = "IV", use_fe = TRUE,
      fe_type = "historical", sample_scope = "states_only", approach = "density", instrument_form = "levels",
      msa_density_measure = "average", msa_sectors = "1", msa_sample_year = "1790", msa_iv_year = "1840",
      msa_instrument_type = "overlap", msa_overlap_pct = "5", msa_controls = c("water_1820", "railroads_1840"),
      msa_schooling_adj = "3", msa_apply_college_adj = TRUE, msa_college_coeff = "0.75",
      msa_se_spec = "cluster_instrument", msa_mining_filter_active = FALSE, msa_mining_threshold = "0.01", msa_spatial_cutoff = "100",
      county_sectors = "1", county_sample_year = "1790", county_iv_year = "1840", county_instrument_type = "max_density_overlap",
      county_overlap_threshold = "5", county_controls = character(), county_schooling_adj = "0",
      county_apply_college_adj = FALSE, county_college_coeff = "0.75", county_se_spec = "cluster_instrument",
      county_mining_filter_active = FALSE, county_mining_threshold = "0.01", county_spatial_cutoff = "100",
      county_msa_restriction = FALSE, geo_controls = character(), water_controls = character(), water_year = "1820",
      map_size = 75, show_map = TRUE)
    exercise <- function(settings, expected_n = NULL, year_switch = FALSE) {
      shiny::testServer(app$server, {
        do.call(session$setInputs, c(modifyList(defaults, settings), list(run_analysis = 1)))
        session$flushReact()
        verify <- function(expected_n = NULL) {
          result <- shiny::isolate(analysis_output())
          if (!is.null(result$error)) stop(result$error)
          stopifnot(length(result$models) > 0L)
          for (model in result$models) {
            stopifnot(all(is.finite(coef(model))), all(is.finite(vcov(model))), nobs(model) > 0L)
            if (!is.null(expected_n)) stopifnot(nobs(model) == expected_n)
          }
          stopifnot(grepl("Observations", output$results_table$html, fixed = TRUE),
            nzchar(output$results_header), nzchar(output$analysis_details$html), nzchar(output$data_notes$html),
            nzchar(output$map_header), nzchar(output$map_ui$html))
          map <- shiny::isolate(map_output())
          if (!is.null(map$src) && file.exists(map$src)) stopifnot(nzchar(output$instrument_map_render$src))
          plot <- output$effect_plot
          stopifnot(is.list(plot), is.character(plot$src), nzchar(plot$src))
          csv <- read.csv(output$download_coefficients)
          html <- paste(readLines(output$download_results, warn = FALSE), collapse = "\n")
          stopifnot(nrow(csv) > 0L, all(c("geography", "year", "estimate") %in% names(csv)),
            all(csv$geography == result$analysis_level), all(csv$year == result$year_modern), grepl("<html", html, fixed = TRUE),
            grepl("data:image/png", html, fixed = TRUE))
          label <- paste(result$analysis_level, result$year_modern, result$sample_scope)
          models[[length(models) + 1L]] <<- list(case = label,
            observations = unname(vapply(result$models, nobs, numeric(1))),
            coefficients = lapply(result$models, function(m) as.list(coef(m))))
          record_memory(paste(label, "rendered and downloaded"))
          result
        }
        old_result <- verify(expected_n)
        if (year_switch) {
          level <- settings$analysis_level
          old_coefficients <- lapply(old_result$models, coef)
          # Simulate another request replacing the shared cache before this session reruns.
          invisible(app$get_app_year(level, 2020))
          stopifnot(app$app_data_loader$cache_info()[[level]]$year == 2020L,
            all(old_result$data_for_fs$year == 2010), identical(old_coefficients, lapply(old_result$models, coef)))
          session$setInputs(year_modern = "2020", run_analysis = 2)
          session$flushReact()
          new_result <- verify()
          stopifnot(all(new_result$data_for_fs$year == 2020), all(old_result$data_for_fs$year == 2010))
        }
      })
    }
    for (scope in c("states_only", "states_territories")) {
      exercise(list(analysis_level = "State", year_modern = "2020", state_sample_year = "1900", state_iv_year = "1900",
        state_instrument_type = "area_population", sample_scope = scope),
        expected_n = if (scope == "states_only") 42L else 43L)
    }
    for (level in c("MSA", "County")) exercise(list(analysis_level = level), year_switch = TRUE)
    info <- app$app_data_loader$cache_info()
    stopifnot(setequal(names(info), c("MSA", "County")),
      all(vapply(info, function(x) identical(x$year, 2020L), logical(1))))
    record_memory("all production cases completed")
    passed <- TRUE
  }, error = function(e) {
    problem <<- conditionMessage(e)
    stop(e)
  })
}
check_runtime()
cat("Production startup, estimates, rendering, downloads and peak RSS passed.\n")
