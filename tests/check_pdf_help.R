# Check the PDF help without loading data or estimating a model.
library(shiny)
library(glue)
library(shinycssloaders)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
options(encoding = "UTF-8")

local({
  app <- new.env(parent = globalenv())
  source("pdf_help.R", local = app, encoding = "UTF-8")
  parsed <- parse("app.R", encoding = "UTF-8")
  for (expr in parsed) {
    if (!is.call(expr) || !identical(expr[[1]], as.name("<-"))) next
    name <- as.character(expr[[2]])
    function_definition <- is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))
    if (function_definition || grepl("^(help_|historical_census_years$|geographic_notes$|area_population_omissions$)", name)) eval(expr, app)
  }
  source("state_ch_adjustment_interface.R", local = app, encoding = "UTF-8")
  ui <- Filter(function(expr) is.call(expr) && identical(expr[[1]], as.name("<-")) &&
    identical(expr[[2]], as.name("ui")), parsed)
  stopifnot(length(ui) == 1L)
  eval(ui[[1]], app)
  html <- htmltools::renderTags(app$ui)$html
  buttons <- regmatches(html, gregexpr('<button[^>]*class="[^"]*help-btn[^"]*"[^>]*>', html, perl = TRUE))[[1]]
  ids <- sub('.*id="([^"]+)".*', '\\1', buttons)
  stopifnot(length(ids) > 40L, !anyDuplicated(ids), setequal(ids, app$pdf_help_ids()),
    all(grepl('aria-label="Open', buttons, fixed = TRUE)),
    !grepl("input\\.[A-Za-z0-9_]*help[A-Za-z0-9_]* % 2 == 1", html), !grepl('class="help-text"', html, fixed = TRUE),
    grepl("pdf_help_server", paste(deparse(body(app$server)), collapse = " "), fixed = TRUE))

  manifest <- app$pdf_help_manifest()
  stopifnot(identical(manifest$id, sprintf("%02d", 1:19)), !anyDuplicated(manifest$file),
    all(nzchar(manifest$title)), !any(grepl("[/\\\\]", c(manifest$file, manifest$image))))
  for (i in seq_len(nrow(manifest))) {
    pdf <- file.path("www", "help", manifest$file[i])
    png <- file.path("www", "help", manifest$image[i])
    stopifnot(file.exists(pdf), file.exists(png), file.info(pdf)$size > 1000,
      identical(rawToChar(readBin(pdf, "raw", 5L)), "%PDF-"),
      identical(as.integer(readBin(png, "raw", 8L)), c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L)))
    viewer <- htmltools::renderTags(app$pdf_help_viewer(manifest$id[i], image = FALSE))$html
    preview <- htmltools::renderTags(app$pdf_help_viewer(manifest$id[i]))$html
    stopifnot(grepl(paste0('data="help/', manifest$file[i], '#view=FitH"'), viewer, fixed = TRUE),
      grepl('type="application/pdf"', viewer, fixed = TRUE),
      grepl(paste0('download="', manifest$file[i], '"'), viewer, fixed = TRUE),
      grepl(paste0('src="help/', manifest$image[i], '"'), preview, fixed = TRUE),
      !grepl("<object", preview, fixed = TRUE))
  }

  topics <- function(id, settings = list()) app$pdf_help_button_topics(id, settings)
  for (id in ids) stopifnot(length(topics(id)) >= 1L, all(topics(id) %in% manifest$id))
  for (level in c("msa", "county", "state")) {
    id <- paste0("help_", level, "_instrument_type")
    for (method in c("overlap", "max_density_overlap", "weighted_density_overlap", "county_population", "area_population", "historical_ch")) {
      expected <- switch(method, county_population = "05", area_population = "06", historical_ch = "11", "04")
      settings <- setNames(list(method), paste0(level, "_instrument_type"))
      stopifnot(identical(topics(id, settings)[1], expected))
    }
  }
  for (level in c("msa", "state")) {
    for (method in c("county_population", "area_population")) {
      settings <- setNames(list(method), paste0(level, "_county_instrument"))
      stopifnot(identical(topics(paste0(level, "_help_county_instrument"), settings)[1],
        if (method == "county_population") "05" else "06"))
    }
    for (mode in c("geography", "both")) {
      settings <- setNames(list("county_adjusted", mode), paste0(level, c("_ch_mode", "_adjustment_mode")))
      expected <- if (mode == "both") "13" else "12"
      stopifnot(identical(topics(paste0(level, "_help_ch_mode"), settings)[1], expected),
        identical(topics(paste0(level, "_help_adjustment_mode"), settings)[1], expected))
    }
    stopifnot(topics(paste0(level, "_help_ch_mode"), setNames(list("standard"), paste0(level, "_ch_mode")))[1] == "10")
  }
  for (id in c("help_water_controls", "msa_help_water_controls", "state_help_water_controls")) {
    field <- if (id == "help_water_controls") "water_controls" else sub("_help_", "_", id)
    stopifnot(identical(topics(id, setNames(list(c("distance", "portage")), field)), c("16", "15")),
      identical(topics(id, setNames(list(c("shoreline", "distance")), field)), c("15", "16")))
  }
  stopifnot(topics("help_msa_density_measure", list(msa_density_measure = "CH"))[1] == "10",
    topics("help_msa_density_measure", list(msa_density_measure = "average"))[1] == "01")
  stopifnot(inherits(try(topics("not_a_help_button"), silent = TRUE), "try-error"))

  # Exercise real observers: every click opens a modal, including after Close,
  # and current settings determine the first document shown.
  events <- list()
  sent <- list()
  test_server <- function(input, output, session) {
    session$sendModal <- function(type, message) events[[length(events) + 1L]] <<- list(type = type, message = message)
    session$sendInputMessage <- function(inputId, message) sent[[inputId]] <<- message
    app$pdf_help_server(input, output, session)
  }
  shiny::testServer(test_server, {
    session$setInputs(msa_instrument_type = "overlap", state_ch_mode = "county_adjusted",
      state_adjustment_mode = "geography", msa_ch_mode = "county_adjusted", msa_adjustment_mode = "geography")
    session$flushReact()
    assert_document <- function(id, image = TRUE) {
      displayed <- output$pdf_help_viewer$html
      file <- manifest$file[manifest$id == id]
      stopifnot(grepl(paste0('href="help/', file, '"'), displayed, fixed = TRUE))
      stopifnot(identical(grepl("<object", displayed, fixed = TRUE), !image))
    }
    for (id in ids) {
      before <- length(events)
      do.call(session$setInputs, setNames(list(1L), id))
      stopifnot(length(events) == before + 1L, tail(events, 1)[[1]]$type == "show")
      assert_document(app$pdf_help_button_topics(id, shiny::reactiveValuesToList(input))[1])
    }
    shiny::removeModal(session = session)
    stopifnot(tail(events, 1)[[1]]$type == "remove")
    session$setInputs(help_msa_instrument_type = 2L)
    assert_document("04")
    session$setInputs(msa_instrument_type = "county_population", help_msa_instrument_type = 3L)
    assert_document("05")
    session$setInputs(msa_instrument_type = "area_population", help_msa_instrument_type = 4L)
    assert_document("06")
    session$setInputs(state_instrument_type = "historical_ch", help_state_instrument_type = 2L)
    assert_document("11")
    session$setInputs(state_help_ch_mode = 2L)
    assert_document("12")
    session$setInputs(pdf_help_topic = "10", pdf_help_native = TRUE)
    assert_document("10", image = FALSE)
    session$setInputs(pdf_help_native = FALSE)
    assert_document("10")
    session$setInputs(pdf_help_native = TRUE)
    assert_document("10", image = FALSE)
    shiny::removeModal(session = session)
    session$setInputs(state_adjustment_mode = "both", state_help_ch_mode = 3L)
    assert_document("13")
    stopifnot(sent$pdf_help_topic$value == "13", identical(sent$pdf_help_native$value, FALSE))
    session$setInputs(msa_adjustment_mode = "both", msa_help_ch_mode = 2L)
    assert_document("13")
    session$setInputs(pdf_help_topic = "12")
    assert_document("12")
    session$setInputs(msa_help_ch_mode = 3L)
    assert_document("13")
    session$setInputs(water_controls = c("distance", "portage"), help_water_controls = 2L)
    assert_document("16")
    session$setInputs(pdf_help_topic = "15")
    assert_document("15")
    # A stale or unexpected browser value cannot load an unrelated document.
    session$setInputs(pdf_help_topic = "../outside")
    stopifnot(!grepl("outside", output$pdf_help_viewer$html, fixed = TRUE))
    stopifnot(input$msa_instrument_type == "area_population", input$msa_adjustment_mode == "both")
  })
  cat("PDF help checks passed:", length(ids), "buttons, 19 local documents, dynamic topics, related notes, repeated opens, image previews, and optional PDF viewer.\n")
})
