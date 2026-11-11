# State observations follow the historical fixed-effect groups in the county data.
historical_region_scope <- function(scope) {
  if (length(scope) != 1L || is.na(scope) ||
      !scope %in% c("states_only", "states_territories", "states_and_territories"))
    stop("Choose a historical state or territory scope.")
  if (scope == "states_only") "states_only" else "states_and_territories"
}

read_historical_region_input <- local({
  cache <- new.env(parent = emptyenv())
  function(path, key, required) {
    if (!file.exists(path)) stop(paste("Historical region input is missing:", path, "Run the State data build."))
    info <- file.info(path)
    signature <- paste(normalizePath(path, winslash = "/"), info$size, as.numeric(info$mtime))
    entry <- cache[[path]]
    if (is.null(entry) || !identical(entry$signature, signature)) {
      value <- as.data.frame(haven::read_dta(path))
      if (!all(unique(c(key, required)) %in% names(value)) || anyNA(value[key]) || anyDuplicated(value[key]))
        stop(paste("Historical region input has missing fields or duplicate keys:", path))
      cache[[path]] <- list(signature = signature, value = value)
    }
    cache[[path]]$value
  }
})

get_historical_state_membership <- function(sample_year, scope,
    path = "State_historical_membership.dta") {
  value <- read_historical_region_input(path, c("sample_year", "scope", "geofips"),
    c("region_id", "region_name", "modern_statefips"))
  value <- value[value$sample_year == as.integer(sample_year) &
    value$scope == historical_region_scope(scope), , drop = FALSE]
  if (!nrow(value) || anyNA(value[c("region_id", "region_name", "modern_statefips")]) ||
      any(!nzchar(value$region_id)) || any(!nzchar(value$region_name)))
    stop("Historical county membership is unavailable for the selected sample year and scope.")
  value[order(value$region_id, value$geofips), , drop = FALSE]
}

get_historical_state_counties <- function(year, path = "State_historical_county_components.dta") {
  value <- read_historical_region_input(path, c("year", "geofips"),
    c("modern_statefips", "nc", "ac", "gdp"))
  value <- value[value$year == as.integer(year), , drop = FALSE]
  if (!nrow(value)) stop("Modern county components are unavailable for the selected year.")
  value
}

historical_state_components <- function(year, sample_year, scope, membership = NULL) {
  if (is.null(membership)) membership <- get_historical_state_membership(sample_year, scope)
  counties <- get_historical_state_counties(year)
  row <- match(membership$geofips, counties$geofips)
  found <- !is.na(row)
  if (any(found & membership$modern_statefips != counties$modern_statefips[row], na.rm = TRUE))
    stop("Historical membership disagrees with the modern county state identifier.")
  parts <- membership
  parts$nc <- counties$nc[row]
  parts$ac <- counties$ac[row]
  parts$gdp <- counties$gdp[row]
  parts$ch_slot <- as.integer(ave(seq_len(nrow(parts)), parts$region_id, FUN = seq_along))
  parts
}

historical_state_wide <- function(data, parts) {
  if (anyDuplicated(data$region_id) || anyDuplicated(parts$geofips))
    stop("Historical CH needs unique regions and one membership per county unit.")
  parts <- parts[parts$region_id %in% data$region_id, , drop = FALSE]
  data$component_membership_available <- as.integer(data$region_id %in% parts$region_id)
  data$ch_complete[data$component_membership_available == 0L] <- 0L
  if (!nrow(parts)) {
    data$nc1 <- data$ac1 <- NA_real_
    return(data)
  }
  slots <- max(parts$ch_slot)
  employment <- matrix(0, nrow(data), slots)
  area <- matrix(1, nrow(data), slots)
  cells <- cbind(match(parts$region_id, data$region_id), parts$ch_slot)
  employment[cells] <- parts$nc
  area[cells] <- parts$ac
  colnames(employment) <- paste0("nc", seq_len(slots))
  colnames(area) <- paste0("ac", seq_len(slots))
  cbind(data, as.data.frame(employment), as.data.frame(area))
}

attach_historical_region_instruments <- function(data, sample_year, scope, years,
    path = "State_historical_instruments.dta") {
  values <- read_historical_region_input(path,
    c("sample_year", "scope", "historical_year", "region_id"),
    c("GGmatchedpop", "AWobs", "HCHobs", "HCHnumer", "HCHcover"))
  values <- values[values$sample_year == as.integer(sample_year) &
    values$scope == historical_region_scope(scope), , drop = FALSE]
  fields <- grep("^(GG|AW|HCH)", names(values), value = TRUE)
  for (year in unique(as.integer(years))) {
    selected <- values[values$historical_year == year, , drop = FALSE]
    row <- match(data$region_id, selected$region_id)
    for (field in fields) data[[paste0(field, "_", year)]] <- selected[[field]][row]
  }
  data
}

state_historical_year_inputs <- function(data, year, construction, coverage, hch = NULL) {
  field <- function(name) {
    column <- paste0(name, "_", year)
    if (!column %in% names(data)) stop(paste("Missing historical regional input:", column))
    data[[column]]
  }
  matched <- construction == "county_population"
  population <- field(if (matched) "GGmatchedpop" else "AWobs")
  known <- field("HCHcover")
  valid_area <- is.finite(known) & known >= coverage$minimum
  reason <- rep("", nrow(data))
  reason[!is.finite(known) | known < 0] <- "Known-population area is unavailable."
  below <- is.finite(known) & known >= 0 & !valid_area
  reason[below] <- sprintf("Known-population area %.1f%% is below %s.", 100 * known[below], coverage$label)
  missing_population <- !is.finite(population) | population <= 0
  reason[missing_population & !nzchar(reason)] <- if (matched)
    "No positive population can be assigned by county identity." else "No positive population can be allocated by area."
  usable <- known
  derivative <- NULL
  if (!is.null(hch)) {
    row <- match(data$region_id, hch$states$region_id)
    usable <- hch$states$coverage[row]
    derivative <- hch$states$index[row]
    invalid <- is.na(row) | is.na(hch$states$keep[row]) | !hch$states$keep[row] | !is.finite(derivative)
    below <- !nzchar(reason) & invalid & is.finite(usable) & usable < coverage$minimum
    reason[below] <- sprintf("Area with usable index inputs %.1f%% is below %s.", 100 * usable[below], coverage$label)
    reason[!nzchar(reason) & invalid] <- "The selected historical index lacks usable inputs."
  }
  inputs <- data.frame(region_id = data$region_id, region_name = data$region_name, historical_year = year,
    known_area_coverage = known, mapped_area_coverage = field("HCHmappedcover"),
    usable_index_coverage = if (is.null(hch)) NA_real_ else usable,
    observed_population = population,
    unknown_population_records = field(if (matched) "GGmiss" else "AWmiss"),
    source_gap_flag = field(if (matched) "GGgap" else "AWgap"),
    unallocated_source_records = field(if (matched) "GGunallocated" else "AWunallocated"),
    historical_inputs_usable = !nzchar(reason), historical_input_reason = reason)
  if (!is.null(hch)) inputs$derivative_index_at_theta_one <- derivative
  if (matched) {
    inputs$ambiguous_population_omitted <- field("GGexcludedpop")
    inputs$ambiguous_positive_records_omitted <- field("GGexcludedn")
    inputs$unmatched_population_area_allocated_to_region <- field("GGunmatchedawpop")
    inputs$potential_unmatched_population_without_geometry <- field("GGunlocatedpop")
  } else inputs$potential_omitted_population_without_geometry <- field("HCHomitted_pop")
  inputs
}

historical_state_coverage_table <- function(details) {
  rows <- details$historical_group_inputs
  if (is.null(rows) || !nrow(rows)) return("")
  rows <- rows[order(rows$region_name, rows$historical_year), , drop = FALSE]
  pct <- function(x) ifelse(is.finite(x), sprintf("%.1f%%", 100 * x), "Unavailable")
  displayed <- data.frame(Group = rows$region_name, Census = rows$historical_year,
    "Known area" = pct(rows$known_area_coverage), check.names = FALSE)
  if (identical(details$instrument_type, "historical_ch") &&
      !isTRUE(all.equal(rows$known_area_coverage, rows$usable_index_coverage, tolerance = 1e-10)))
    displayed[["Area with usable index inputs"]] <- pct(rows$usable_index_coverage)
  displayed[["Missing-population records"]] <- rows$unknown_population_records
  displayed[["Source gap"]] <- ifelse(rows$source_gap_flag > 0, "Yes", "")
  displayed[["Unallocated records"]] <- rows$unallocated_source_records
  if (all(is.na(rows$in_estimation))) {
    displayed[["Fit assessment"]] <- ifelse(!is.na(rows$assessed_for_fit) & rows$assessed_for_fit, "Assessed",
      ifelse(nzchar(rows$exclusion_reason), rows$exclusion_reason, "Assessment unavailable"))
  } else displayed[["Estimation sample"]] <- ifelse(rows$in_estimation, "Included", rows$exclusion_reason)
  tag <- htmltools::tags
  as.character(tag$details(tag$summary("Coverage and exclusions by historical group"),
    tag$div(style = "overflow-x:auto", tag$table(class = "table table-condensed",
      tag$thead(tag$tr(lapply(names(displayed), tag$th))),
      tag$tbody(lapply(seq_len(nrow(displayed)), function(i)
        tag$tr(lapply(displayed[i, , drop = FALSE], function(x) tag$td(as.character(x))))))))))
}

state_sample_counts_text <- function(details) {
  years <- paste(unique(c(details$sample_year, details$iv_year)), collapse = " and ")
  text <- sprintf("%d of %d historical groups passed the source and coverage requirements in %s.",
    details$historical_inputs_usable_n, details$historical_region_eligible_n, years)
  inputs <- details$historical_group_inputs
  if (any(!is.na(inputs$assessed_for_fit))) text <- paste(text, sprintf("%d groups entered the estimator.",
    length(unique(inputs$region_id[!is.na(inputs$assessed_for_fit) & inputs$assessed_for_fit]))))
  text
}

state_source_export <- function(details) {
  table <- details$historical_group_inputs
  table$instrument_construction <- details$instrument_type
  table$known_area_coverage_required <- historical_ch_coverage(details$state_hch_coverage)$minimum
  table$modern_year <- details$year_modern
  table$historical_boundary_year <- details$sample_year
  table$instrument_year <- details$iv_year
  table$sample_scope <- details$sample_scope
  table$fit_status <- if (is.null(details$fit_error)) "Estimated" else details$fit_error
  if (!is.null(details$fit_error) && identical(details$stock_wright$status, "available")) {
    table$stock_wright_S <- details$stock_wright$statistic
    table$stock_wright_p <- details$stock_wright$p_value
  }
  table
}

historical_state_metadata <- function(details) {
  if (!identical(details$analysis_level, "State")) return(character())
  c(`Regional definition` = "Historical county fixed-effect groups",
    `Boundary year` = details$sample_year,
    `Known-population area coverage` = historical_ch_coverage(details$state_hch_coverage)$label,
    `Groups before data exclusions` = details$historical_region_eligible_n,
    `Groups in the estimation` = paste(details$historical_region_names, collapse = ", "),
    `Covariance unit` = "Historical state or territory group")
}
