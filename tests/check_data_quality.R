# Source-specific notes should follow the selected historical data, not the estimator.
local({
  if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
  app <- new.env(parent = globalenv())
  for (expr in parse("app.R")) if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(as.character(expr[[2]]), "data_quality_notes")) eval(expr, app)
  methods <- list(MSA = c("overlap", "area_population"),
    County = c("max_density_overlap", "weighted_density_overlap", "area_population"),
    State = c("area_population", "historical_ch"))
  checked <- 0L
  for (level in names(methods)) for (method in methods[[level]])
    for (iv_year in c(1790, 1880, 1900)) for (sample_year in c(1790, 1900))
      for (density in c("average", "CH")) for (scope in c("states_only", "states_territories")) {
        details <- list(analysis_level = level, instrument_type = method, iv_year = iv_year,
          sample_year = sample_year, density_measure = density, sample_scope = scope)
        notes <- app$data_quality_notes(details)
        expected <- iv_year == 1900 || sample_year == 1900
        stopifnot(length(notes) == as.integer(expected))
        if (expected) stopifnot(grepl("Winchester, Virginia, remains unverified", notes, fixed = TRUE),
          grepl("0.280 km", notes, fixed = TRUE), grepl("provisional", notes, fixed = TRUE),
          !grepl("https?://", notes))
        details$instrument_type <- "county_population"
        stopifnot(length(app$data_quality_notes(details)) == 0L)
        checked <- checked + 1L
      }
  stopifnot(length(app$data_quality_notes(list())) == 0L,
    length(app$data_quality_notes(list(analysis_level = "Other", instrument_type = "overlap",
      iv_year = 1900, sample_year = 1900))) == 0L)
  cat("Historical boundary note checks passed:", checked, "spatial settings, with county matching excluded.\n")
})
