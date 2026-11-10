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
    c("GGpop", "GGvalid", "AWpop", "AWvalid", "HCHobs", "HCHnumer", "HCHcover"))
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

historical_state_metadata <- function(details) {
  if (!identical(details$analysis_level, "State")) return(character())
  c(`Regional definition` = "Historical county fixed-effect groups",
    `Boundary year` = details$sample_year,
    `Groups before data exclusions` = details$historical_region_eligible_n,
    `Groups in the estimation` = paste(details$historical_region_names, collapse = ", "),
    `Covariance unit` = "Historical state or territory group")
}
