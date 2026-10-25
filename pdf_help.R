# One-page explanations served with the app. Help never changes model inputs.
pdf_help_manifest <- function() {
  stems <- c("units_outcomes", "estimation", "historical_sample", "density_overlap",
    "county_matching", "area_weighting", "instrument_scale", "fixed_effects",
    "schooling_college", "standard_ch", "historical_ch_instrument", "county_geography_ch",
    "human_capital_ch", "terrain_crops_climate", "water_transport", "distance_portage",
    "standard_errors", "reading_results", "interface_exports", "predicted_historical_ch")
  titles <- c("Geographic units, outcomes and sample filters", "OLS, IV and the first stage",
    "Historical years, territory and the sample", "Historical density and geographic overlap",
    "Glaeser and Gottlieb (2009): county matching", "Area-weighted historical population",
    "Historical instrument scale", "State fixed effects", "Schooling and college adjustments",
    "The standard Ciccone–Hall model", "The historical CH instrument", "County geography inside CH",
    "Schooling and college inside CH", "Terrain, crops and climate", "Water access and historical transport",
    "Distances and approximate portage access", "Standard errors", "Reading the regression results",
    "Running, checking and saving an analysis", "County predictions in the historical CH instrument")
  ids <- sprintf("%02d", seq_along(stems))
  files <- paste0(ids, "_", stems)
  data.frame(id = ids, title = titles, file = paste0(files, ".pdf"),
    image = paste0(files, ".png"), stringsAsFactors = FALSE)
}

pdf_help_bindings <- function() {
  bindings <- c(help_analysis_level = "01", help_year_modern = "01",
    help_approach = "02", help_analysis_type = "02", help_instrument_form = "07",
    help_use_fe = "08", help_fe_type = "08", help_sample_scope = "03",
    help_state_ch = "10", help_state_sample_year = "03", help_state_iv_year = "03",
    help_state_instrument_type = "instrument", help_state_hch_coverage = "11",
    help_state_hch_variant = "20", help_state_hch_density_basis = "20",
    help_state_hch_prediction_geo_controls = "20", help_state_hch_prediction_water_controls = "20",
    help_state_hch_prediction_transport_controls = "20", help_state_hch_prediction_water_year = "20", help_geo_controls = "14",
    help_water_controls = "water", help_water_year = "15", help_county_msa_restriction = "01",
    help_display_options = "19", help_reading_results = "18", help_downloads = "19")
  for (level in c("msa", "county")) {
    fields <- c(density_measure = "density", sectors = "01", sample_year = "03", iv_year = "03",
      instrument_type = "instrument", schooling_adj = "09", apply_college_adj = "09",
      college_coeff = "09", mining_filter_active = "01", mining_threshold = "01",
      se_spec = "17", spatial_cutoff = "17", controls = "15")
    names(fields) <- paste0("help_", level, "_", names(fields))
    bindings <- c(bindings, fields)
  }
  bindings <- c(bindings, help_msa_overlap_pct = "04", help_county_overlap_threshold = "04")
  for (level in c("msa", "state")) {
    fields <- c(hch_method = "11", hch_gmm_steps = "11")
    if (level == "msa") fields <- c(fields, hch_coverage = "11", hch_variant = "20", hch_density_basis = "20",
      hch_prediction_geo_controls = "20", hch_prediction_water_controls = "20",
      hch_prediction_transport_controls = "20", hch_prediction_water_year = "20")
    names(fields) <- paste0("help_", level, "_", names(fields))
    bindings <- c(bindings, fields)
  }
  for (level in c("msa", "state")) {
    fields <- c(ch_mode = "ch", adjustment_mode = "adjustment", county_instrument = "calibration",
      county_area = "12", geo_controls = "14", water_controls = "water",
      transport_controls = "15", water_year = "15")
    names(fields) <- paste0(level, "_help_", names(fields))
    bindings <- c(bindings, fields)
  }
  bindings
}

pdf_help_ids <- function() names(pdf_help_bindings())

# values can be an ordinary list or Shiny input. Resolve the current selection at click time.
pdf_help_button_topics <- function(help_id, values = list()) {
  binding <- unname(pdf_help_bindings()[help_id])
  if (length(binding) != 1L || is.na(binding)) stop("Unknown PDF help button: ", help_id)
  if (binding %in% pdf_help_manifest()$id) return(binding)
  control <- if (grepl("^help_", help_id)) sub("^help_", "", help_id) else sub("_help_", "_", help_id)
  level <- sub("_.*$", "", control)
  construction_topic <- function(construction) {
    if (identical(construction, "county_population")) "05"
    else if (identical(construction, "area_population")) "06"
    else if (identical(construction, "historical_ch")) "11"
    else "04"
  }
  if (binding %in% c("instrument", "calibration"))
    return(construction_topic(values[[control]]))
  if (binding == "density")
    return(if (identical(values[[control]], "CH")) c("10", "01") else c("01", "10"))
  if (binding %in% c("ch", "adjustment")) {
    mode <- values[[paste0(level, "_ch_mode")]]
    human_capital <- values[[paste0(level, "_adjustment_mode")]] %in% c("human_capital", "both")
    if (binding == "ch" && !identical(mode, "county_adjusted")) return(c("10", "12", "13"))
    return(if (isTRUE(human_capital)) c("13", "12", "10") else c("12", "13", "10"))
  }
  if (binding == "water") {
    selected <- values[[control]]
    distance_only <- length(selected) > 0L && all(selected %in% c("distance", "portage"))
    return(if (distance_only) c("16", "15") else c("15", "16"))
  }
  stop("Unresolved PDF help button: ", help_id)
}

pdf_help_button <- function(help_id, label = NULL) {
  if (!help_id %in% pdf_help_ids()) stop("Unknown PDF help button: ", help_id)
  description <- if (is.null(label)) "Open one-page explanation" else paste("Open explanation:", label)
  shiny::actionButton(help_id, NULL, icon = shiny::icon("question-circle"), class = "help-btn",
    title = description, `aria-label` = description)
}

pdf_help_viewer <- function(topic, image = TRUE) {
  manifest <- pdf_help_manifest()
  note <- manifest[manifest$id == topic, , drop = FALSE]
  if (nrow(note) != 1L) stop("Unknown PDF explanation: ", topic)
  pdf <- paste0("help/", note$file)
  png <- paste0("help/", note$image)
  shiny::tagList(
    shiny::tags$h4(note$title, class = "pdf-help-title"),
    shiny::div(class = "pdf-help-links",
      shiny::tags$a("Open PDF", href = pdf, target = "_blank", rel = "noopener"),
      shiny::tags$a("Download PDF", href = pdf, download = note$file)),
    if (isTRUE(image)) shiny::div(class = "pdf-help-image-frame",
      shiny::tags$img(src = png, alt = paste("One-page explanation:", note$title), class = "pdf-help-image"))
    else shiny::tags$object(data = paste0(pdf, "#view=FitH"), type = "application/pdf",
      class = "pdf-help-document", title = note$title, `aria-label` = note$title,
      shiny::tags$p("If the PDF does not display, clear Use PDF viewer or choose Open PDF."),
      shiny::tags$img(src = png, alt = paste("One-page explanation:", note$title), class = "pdf-help-image")))
}

pdf_help_server <- function(input, output, session) {
  topics <- shiny::reactiveVal(character())
  selected_topic <- shiny::reactiveVal(NULL)
  image_view <- shiny::reactiveVal(TRUE)
  for (help_id in pdf_help_ids()) local({
    button <- help_id
    shiny::observeEvent(input[[button]], {
      current <- pdf_help_button_topics(button, input)
      topics(current)
      selected_topic(current[1L])
      image_view(TRUE)
      manifest <- pdf_help_manifest()
      notes <- manifest[match(current, manifest$id), , drop = FALSE]
      shiny::showModal(shiny::modalDialog(
        title = "One-page explanation", size = "l", easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        shiny::div(class = "pdf-help-modal",
          if (length(current) > 1L) shiny::selectInput("pdf_help_topic", "Related explanations:",
            choices = stats::setNames(notes$id, notes$title), selected = current[1L]),
          shiny::checkboxInput("pdf_help_native", "Use PDF viewer", FALSE),
          shiny::uiOutput("pdf_help_viewer"))), session = session)
      # Reset reused modal inputs before any new browser update arrives.
      shiny::updateSelectInput(session, "pdf_help_topic", selected = current[1L])
      shiny::updateCheckboxInput(session, "pdf_help_native", value = FALSE)
    }, ignoreInit = TRUE)
  })
  shiny::observeEvent(input$pdf_help_topic, {
    if (input$pdf_help_topic %in% topics()) selected_topic(input$pdf_help_topic)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$pdf_help_native, {
    image_view(!isTRUE(input$pdf_help_native))
  }, ignoreInit = TRUE)
  output$pdf_help_viewer <- shiny::renderUI({
    current <- topics()
    shiny::req(length(current))
    selected <- selected_topic()
    if (length(current) == 1L || is.null(selected) || !selected %in% current) selected <- current[1L]
    pdf_help_viewer(selected, image_view())
  })
  invisible(topics)
}
