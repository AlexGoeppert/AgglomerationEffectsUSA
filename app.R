# --- 1. Load Required Packages ------------------------------------------------

library(shiny)
library(fixest)
library(dplyr)
library(haven)
library(glue)
library(stringr)
library(shinycssloaders) # For withSpinner
library(DT)
library(ggplot2)

# --- 2. Load Data on Startup --------------------------------------------------

read_app_data <- function(path, level) {
  common <- c("year", "modern_state_fe", "simple_gamma", "Minc_return_calc",
    "employment_private_nonfarm", "employment_mining", "water_1820",
    "railroads_1840", "railroads_1850", "railroads_1861")
  specific <- if (level == "MSA") c("msafips", "msaname", "lat_DD", "lon_DD",
    "msa_schooling_09", "college_share_09", "employment", "msaarea", "ln_output_worker", "ln_output_worker_private",
    "gmp", "gmp_private_industry", "gmp_agriculture", "gmp_mining", "gmp_indstryid_12", "emply_indryid_100", "emply_indryid_500")
  else c("geofips", "geoname", "lat", "lon", "avg_schooling_10_14", "college_share",
    "employment_total", "area_acre", "gcp_total", "gcp_private_industry", "gcp_agriculture", "gcp_mining",
    "gcp_indstryid_12", "emply_c_indryid_70", "emply_c_indryid_100", "emply_c_indryid_500", "county_partof_MSA")
  available <- names(haven::read_dta(path, n_max = 0))
  missing <- setdiff(c(common, specific), available)
  if (length(missing)) stop(paste("Required data fields are missing:", paste(missing, collapse = ", ")))
  patterns <- c("^hist_state_fe_[0-9]{4}(_s)?$", "^(GG|AW)pop_[0-9]{4}(_s)?$",
    "^(rugged_mean|elev_mean|gaez_wheat_suit|gaez_maize_suit|tjan|tjul|precip|ocean_access10|lakes_access10|portage_access10)$",
    "^(river_access_|canal_access_|dist_.*_100km(_sq)?$)")
  patterns <- c(patterns, if (level == "MSA") c("^MSAol_[0-9]{4}_(5|[1-9]0)(_s)?$",
    "^MSApop(id)?_[0-9]{4}_(5|[1-9]0)(_s)?$", "^(nc|ac)[0-9]+$", "^ch_complete$")
    else c("^iv_overlap_(max_)?(5|[1-9]0)_[0-9]{4}(_s)?$", "^ivpop(id)?_overlap_max_(5|[1-9]0)_[0-9]{4}(_s)?$"))
  selected <- unique(c(common, specific, grep(paste(patterns, collapse = "|"), available, value = TRUE)))
  haven::read_dta(path, col_select = tidyselect::all_of(selected))
}

app_data_spec <- function() {
  years <- 2001:2022
  list(version = 1L, years = years, levels = c("MSA", "County"),
    sources = c(MSA = "MSA_analysis_data.dta", County = "master_county_build.dta", State = "State_CH_analysis_data.dta"),
    files = c(paste0("MSA-", years, ".rds"), paste0("County-", years, ".rds"), "state.rds"))
}

read_app_cache_manifest <- function(cache_dir) {
  spec <- app_data_spec()
  manifest_path <- file.path(cache_dir, "manifest.rds")
  if (!file.exists(manifest_path)) stop("The app data cache is incomplete: manifest.rds is missing. Rebuild the cache before deployment.")
  manifest <- readRDS(manifest_path)
  if (!is.list(manifest) || !identical(manifest$version, spec$version) ||
      !identical(manifest$years, spec$years) || !identical(manifest$files, spec$files) ||
      !identical(names(manifest$rows), spec$files) || anyNA(manifest$rows) || any(manifest$rows < 1) ||
      !identical(names(manifest$columns), c(spec$levels, "State")) ||
      any(vapply(manifest$columns, function(x) !is.character(x) || !"year" %in% x || anyDuplicated(x) > 0L, logical(1))))
    stop("The app data cache has an invalid manifest. Rebuild the cache before deployment.")
  paths <- file.path(cache_dir, spec$files)
  missing <- spec$files[!file.exists(paths)]
  if (length(missing)) stop(paste("The app data cache is incomplete; missing:", paste(missing, collapse = ", ")))
  if (!identical(names(manifest$bytes), spec$files) || anyNA(manifest$bytes) ||
      any(unname(manifest$bytes) != file.info(paths)$size))
    stop("The app data cache contains an incomplete or changed file. Rebuild the cache before deployment.")
  manifest
}

make_app_data_loader <- function(cache_dir = "app-data", source_dir = ".") {
  spec <- app_data_spec()
  manifest <- if (dir.exists(cache_dir)) read_app_cache_manifest(cache_dir) else NULL
  cache <- new.env(parent = emptyenv())
  read_cached <- function(filename, level) {
    data <- readRDS(file.path(cache_dir, filename))
    if (!is.data.frame(data) || !identical(names(data), manifest$columns[[level]]) ||
        nrow(data) != manifest$rows[[filename]])
      stop(paste("Invalid app data cache file:", filename))
    data
  }
  get_year <- function(level, year) {
    if (length(level) != 1L || !level %in% spec$levels || length(year) != 1L || is.na(year) ||
        !as.character(year) %in% as.character(spec$years)) stop("Choose a supported geography and modern year.")
    year <- as.integer(year)
    if (exists(level, envir = cache, inherits = FALSE) && identical(cache[[level]]$year, year))
      return(cache[[level]]$data)
    # A completed analysis retains its own rows when another session changes years.
    if (exists(level, envir = cache, inherits = FALSE)) rm(list = level, envir = cache)
    gc(verbose = FALSE)
    if (!is.null(manifest)) {
      data <- read_cached(paste0(level, "-", year, ".rds"), level)
    } else {
      panel <- read_app_data(file.path(source_dir, spec$sources[[level]]), level)
      data <- panel[!is.na(panel$year) & panel$year == year, , drop = FALSE]
      rm(panel)
      gc(verbose = FALSE)
    }
    if (!nrow(data) || anyNA(data$year) || any(data$year != year))
      stop(paste("The app dataset has invalid or missing rows for", level, year))
    cache[[level]] <- list(year = year, data = data)
    data
  }
  get_state <- function() {
    data <- if (is.null(manifest)) haven::read_dta(file.path(source_dir, spec$sources[["State"]])) else read_cached("state.rds", "State")
    if (!identical(sort(unique(as.integer(data$year))), spec$years) || anyNA(data$year))
      stop("The state dataset must contain all modern years from 2001 to 2022.")
    data
  }
  cache_info <- function() {
    lapply(as.list(cache), function(entry) list(year = entry$year, rows = nrow(entry$data),
      megabytes = as.numeric(object.size(entry$data)) / 1024^2))
  }
  list(get_year = get_year, get_state = get_state, cache_info = cache_info, cached = !is.null(manifest))
}

tryCatch({
  app_data_loader <- make_app_data_loader()
  get_app_year <- app_data_loader$get_year
  state_data <- app_data_loader$get_state()
}, error = function(e) {
  stop(paste("Could not load the app datasets:", conditionMessage(e)))
})

# --- 3. Territory Filtering Function ------------------------------------------

get_territory_exclusions <- function(data, fe_column_name, id_column) {
  territory_patterns <- c("territory", "territoy", "territ", "organised", "organized", "district", "columbia", "\\bdc\\b")
  
  if (!fe_column_name %in% names(data)) {
    return(c()) # Return empty vector if column doesn't exist
  }
  
  territory_ids <- data %>%
    filter(
      if_any(all_of(fe_column_name), ~ str_detect(tolower(as.character(.)), 
                                                  paste(territory_patterns, collapse = "|")))
    ) %>%
    pull(!!sym(id_column)) %>%
    unique()
  
  return(territory_ids)
}

# --- 4. Standardized help‑text strings  --------------------------------------
historical_census_years <- seq(1790, 1900, 10)
help_instrument_form <- paste("Levels uses historical population in thousands of people, or population density in people per km².",
  "Natural log uses ln(population) or ln(density), with no added constant. Observations with zero or negative values cannot enter that specification; their number is reported.",
  "The historical CH density index is already a weighted average of log densities and is used directly.")
help_state_instrument <- paste("Glaeser and Gottlieb (2009) sums full historical county populations matched to current county identities within each state.",
  "Area-weighted population allocates each historical reporting area's population by its geographic overlap with modern counties, then sums within states.",
  "Historical CH density index averages log historical reporting-area population density using population shares as weights. Reporting areas are assigned to the modern state containing most of their area; a unique majority assignment and at least 95% geographic coverage are required, and unknown populations remain unavailable. Jointly reported counties are treated as one reporting area.",
  "The historical CH index uses only historical data and a fixed rule. It requires both relevance and the assumption that historical settlement affects current productivity only through the modeled density channel.")
help_stock_wright <- paste("Stock–Wright LM S tests whether the CH elasticity is zero (theta = 1).",
  "Its p-value uses a chi-squared distribution with one degree of freedom and is robust to weak instruments when the model's moment assumptions hold.",
  "It tests the zero-effect hypothesis; it does not measure instrument strength.")
help_anderson_rubin <- paste("Anderson–Rubin Wald tests whether the CH elasticity is zero (theta = 1).",
  "At a proposed elasticity, the corresponding CH term is subtracted from the outcome. The remaining outcome is regressed on the historical instrument, controls and fixed effects.",
  "The test uses the instrument coefficient and the robust or clustered covariance from this auxiliary regression, with no small-sample correction and one chi-squared degree of freedom.",
  "It allows for weak identification under the model's moment and large-sample assumptions. It tests the proposed elasticity, not whether instruments are strong.",
  "Unlike Stock–Wright S, its covariance comes from the auxiliary regression that includes the instrument. The two tests can differ substantially when a few observations are influential.")
help_ch_instrument_wald <- paste("Local instrument Wald F measures instrument relevance for CH GMM.",
  "It regresses the CH index's derivative with respect to theta, evaluated at the fitted theta, on the historical instrument and the same controls and fixed effects.",
  "The statistic is the squared t ratio for the instrument, using the same sample and robust or clustered standard-error choice, without a small-sample correction.",
  "Partial R-squared measures the remaining association after controls and fixed effects.",
  "This treats the fitted derivative as fixed. It is a local relevance diagnostic, not a calibrated weak-instrument test for nonlinear GMM; a high value does not establish strong identification.")

# Master settings
help_analysis_level      <- "Pick metropolitan statistical areas (MSAs), counties, or states. State estimates use the Ciccone–Hall model and the 48 contiguous states."
help_state_ch <- paste("Ciccone and Hall (1996) relate state productivity to employment density within the state’s counties.",
  "The model estimates theta in log[sum(n^theta a^(1−theta))/sum(n)], where n is county employment and a is county land area. The table reports theta − 1.",
  "The outcome is log state GDP per job for all industries. BEA combined county units are kept together. This specification uses robust standard errors and no schooling adjustment or state fixed effects.",
  "There is one observation per modern state in the selected year. A separate fixed effect for each modern state would absorb all variation, so the density effect could not be estimated. Historical state/territory group effects need not absorb all variation, but are not implemented in this State specification. County and MSA analyses can use state fixed effects because they have multiple geographic units within states.",
  "Sector, schooling, college-share, mining-filter and geographic-control choices belong to the County and MSA specifications. The current State specification keeps these choices fixed; this is not a general restriction on state-level models.",
  "County matching sums historical populations assigned to current counties in each state. A known total reported jointly for several counties can be used when all belong to the same state. Area weighting allocates historical population by geographic overlap with current counties, assuming uniform density within each historical reporting area. The Levels option uses population in thousands; the Natural log option uses ln(population).",
  "Only states with valid historical population and complete county employment and land area enter the model. Area-weighted state population also requires at least 95% geographic coverage. Known unallocated population makes the affected state total unavailable. The historical year can therefore change the sample.",
  help_state_instrument, help_instrument_form,
  help_stock_wright, help_anderson_rubin, help_ch_instrument_wald, sep = "\n")
help_year_modern         <- "Year in which modern productivity and employment are measured."
help_approach <- paste(
  "Employment density uses log total employment per unit of area, or the Ciccone–Hall index for MSAs.",
  "Employment uses log total employment, without dividing by area. Its instrument is historical population, in thousands under Levels or in natural logs under Natural log.",
  "The selected matching method determines which historical county populations are used. Outcomes, controls and schooling adjustments keep their existing definitions.", sep = "\n")
help_analysis_type       <- paste(
  "Estimation method:",
  " • OLS – regress modern productivity on the employment measure selected under Approach.",
  " • IV – instrument that employment measure with the selected historical population or density measure.",
  " • First-stage regression – regress the selected modern employment measure on the historical instrument.",
  paste("For CH IV:", help_stock_wright, help_anderson_rubin, help_ch_instrument_wald),
  sep = "\n")

# Fixed effects & scope
help_use_fe              <- "State fixed effects account for factors shared by geographic units in the same state. Historical and modern effects remain available for County and MSA analyses, including MSA Ciccone–Hall. With one observation per modern state, separate modern-state effects would absorb all variation. Historical state/territory group effects are not implemented in the State specification."
help_fe_type             <- "Modern uses modern state identities. Historical uses state and territory identities in the selected historical sample year, so changing only the instrument year leaves the fixed-effect definition unchanged."
help_sample_scope        <- paste(
  "Which modern places stay in the sample?",
  " • Historical States Only – uses historical areas that were states at the census date.",
  " • Historical States & Territories – also includes historical U.S. territories.",
  "Scope classifies historical source areas by statehood at their census date. It is not a filter on when the modern destination state joined the Union: for example, a modern West Virginia location may be covered by historical Virginia.",
  "The historical sample year fixes the MSA/County sample classification and historical fixed effects. Instrument availability is checked separately in the instrument year, using the same source-scope choice.",
  "In the states-only scope, the original overlap measures retain District of Columbia population, but exclude units assigned a District of Columbia historical state effect. County matching and area weighting exclude District of Columbia population.",
  sep = "\n")

# Standardized settings (used for both MSA and County)
help_sectors             <- paste("Select the sector used for modern output per job. The employment measure on the right-hand side always uses total employment.",
  "Missing or suppressed BEA components make the corresponding sector outcome unavailable; the app reports those exclusions. Private non-farm removes agriculture, forestry, fishing and related activities from both GDP and employment. Private non-farm/mining also removes mining from both.")
help_sample_year         <- "Defines historical sample availability and, for MSA and County analyses, the historical state/territory classification and historical fixed effects. The selected instrument must also be available in its own census year."
help_iv_year             <- "Selects the historical census year used for the population or density instrument. The Approach and matching method determine how that population enters the regression."
help_schooling_adj       <- paste(
  "Adjust productivity for schooling using Mincerian return to schooling:",
  " • None – no adjustment.",
  " • Same Return – employs nationwide Mincerian return to schooling. ",
  " • Specific Return – allows Mincerian return to vary with the employment density of the modern geographic unit.",
  " • Run Both – shows separate results for both adjustments",
  "Each adjustment is centered on average schooling in that model's final estimation sample. The specific-return elasticity is evaluated at that reference schooling level, which is reported with the results.",
  "Schooling and return estimates are fixed across modern years: MSA schooling and college shares use the 2009 input; County schooling and shares use ACS 2010–2014. Standard errors treat the supplied returns and college coefficient as fixed.",
  sep = "\n")
help_apply_college_adj   <- "Subtract coef × modern college‑educated share from productivity."
help_college_coeff       <- "Subtracts the chosen coefficient times the modern college share from log productivity, following the human-capital spillover idea in Moretti (2004). The project's default is 0.75; it is a chosen adjustment parameter and can be changed. Standard errors treat it as fixed."
help_mining_filter_active<- "Exclude units with mining GDP shares above the chosen threshold or unavailable mining shares."
help_mining_threshold    <- "Keep units with a known mining share at or below this threshold. Missing or suppressed mining values are not treated as zero."
help_se_spec             <- paste(
  "Standard‑error option:",
  " • Cluster by historical instrument – overlap methods group observations by the selected historical county identity. Weighted density overlap groups identical instrument values. County matching and area-weighted population cluster by the modern MSA or county.",
  " • Cluster by state – clusters at the modern state level. ",
  " • Spatial correlation (Conley standard errors) – distance based correction using uniform kernel and a distance cutoff (distance cutoff chosen below).",
  " • Robust – heteroskedasticity robust standard errors.",
  sep = "\n")

# Spatial standard error settings
help_spatial_cutoff      <- "Distance in km beyond which spatial correlation is set to zero. Uses uniform kernel with constant weight within cutoff distance."
help_spatial_kernel      <- "Uniform kernel with constant weight within cutoff distance."

# MSA‑specific settings
area_population_omissions <- paste(
  "Where the census reports several counties jointly, area weighting uses their combined area and counts the reported population once. County matching leaves an ambiguous allocation unavailable.",
  "Walton (1810), Hopefield–St. Francis (1810) and Miller (1830) lack historical boundaries.",
  "The later boundary files also omit records covering 1,161 people in 1870, 134 in 1880 and 16,024 in 1900.",
  "Where these locations can be identified, affected totals are marked unavailable. Elsewhere, county and MSA totals include mapped records only.")
help_msa_instrument_type     <- paste(
  "How to construct the historical instrument:",
  " Overlap – selects the historical county with the highest population density among counties with at least the chosen percentage of their territory overlapping the modern MSA. Employment density uses that county's density; Employment uses its full population, without area weights; the scale selector chooses thousands of people or its natural log.",
  " Glaeser and Gottlieb (2009) – historical county identities are matched to current county identities and then to the project's current MSA county membership. Whole historical county populations are summed for the selected census year and territory scope, without area weights. The Levels option uses this total divided by 1,000 (thousands of people).",
  " Counties without a reliable match are left unresolved. A missing population for a matched county or a known source gap makes the MSA total unavailable.",
  " Area-weighted population – multiplies each historical reporting area's population by the fraction of its area inside the modern MSA, then sums these contributions. Most reporting areas are individual counties. The MSA is the union of the same current counties used for county matching. This assumes uniform population density within each historical reporting area. The Levels option uses the allocated population divided by 1,000 (thousands of people).",
  " Area weights use historical census years 1790–1900. Totals use mapped historical counties within the selected state or territory scope. A total remains unavailable if the unit has no mapped overlap or an overlapping historical county has unknown population; a zero recorded population remains zero.",
  area_population_omissions,
  sep = "\n")
help_msa_overlap_pct         <- "Minimum % of a historical county's area that must overlap with the MSA."

# County‑specific settings
help_county_msa_restriction  <- "Keep only counties that belong to a modern MSA."
help_county_instrument_type  <- paste(
  "How to construct the historical instrument:",
  " • Max density overlap – selects the historical county with the highest population density among counties meeting the chosen overlap percentage. Employment density uses that county's density; Employment uses its full population, without area weights; the scale selector chooses thousands of people or its natural log.",
  " • Weighted density overlap – averages historical county densities using area-overlap weights. This option is available for employment density only.",
  " • Glaeser and Gottlieb (2009) – matches historical county identities to the project's current county units and sums their full populations for the selected census year and territory scope. The Levels option uses the total divided by 1,000, without area weights. Uncertain matches remain unresolved; missing matched populations or known source gaps make the total unavailable.",
  " • Area-weighted population – multiplies each historical reporting area's population by the fraction of its area inside the modern county, then sums these contributions. Most reporting areas are individual counties. This assumes uniform population density within each historical reporting area. The Levels option uses the allocated population divided by 1,000 (thousands of people).",
  " Area weights use historical census years 1790–1900. Totals use mapped historical counties within the selected state or territory scope. A total remains unavailable if the unit has no mapped overlap or an overlapping historical county has unknown population; a zero recorded population remains zero.",
  area_population_omissions,
  sep = "\n")
help_county_overlap_threshold <- "Minimum overlap as a percentage of the modern county's area. A historical county qualifies when its overlap exceeds this percentage."

geographic_notes <- c(
  "Water access sum, 1820" = "The sum of three yes/no indicators: access to the coast, rivers and canals in the project's original 1820 layers. Each component equals 1 if its layer intersects the county or MSA, and 0 otherwise. The sum ranges from 0 to 3 and counts the types of water access present. It uses the original coast layer and buffered river and canal layers, intersected with the project's county and MSA boundaries.",
  "Railroads" = "Each selected year adds a separate yes/no indicator: 1 if a mapped railroad intersects the county or MSA in 1840, 1850 or 1861, and 0 otherwise. It measures the presence of a railroad, not the length of the network or distance to a station.",
  "Ruggedness and elevation" = "Adds two variables: terrain ruggedness (how uneven the terrain is) and mean elevation (height above sea level), both in metres. Nunn-Puga ruggedness and PRISM elevation are averaged over the county or MSA using geographic area weights, without population weights.",
  "Wheat and maize suitability" = "Adds separate GAEZ suitability indices for wheat and maize, averaged over the county or MSA using area weights. They describe rainfed production with low inputs under 1981-2010 conditions. Source values run from 0 to 10,000; higher values mean greater suitability. In the regression, both indices are divided by 1,000, so each coefficient refers to a 1,000-point increase in the source index.",
  "Temperature and precipitation" = "Adds three area-weighted PRISM averages: January temperature and July temperature in degrees Celsius, and annual precipitation in millimetres. These describe long-run climate in 1991-2020, independently of the selected modern or historical census year.",
  "Ocean and Great Lakes access" = "Adds two separate yes/no indicators. Each equals 1 if any part of the county or MSA is within 10 km of the ocean shoreline or Great Lakes shoreline, and 0 otherwise. The shorelines use modern Census geography.",
  "Historical river and canal access" = "Adds two separate yes/no indicators for access within 10 km of a river segment with documented steamboat operation and a canal operating in the selected waterway year. Distance is measured from the nearest part of the county or MSA. This river measure covers documented steamboat routes.",
  "Distance to shores and waterways" = "Adds distances to the ocean, Great Lakes, operating rivers and operating canals. Each runs from the county's or MSA's geometric centre to the nearest feature. Each distance is measured in 100 km units and enters the regression together with its square, allowing the relationship to curve as distance increases. The centre is based on geography, without population weights.",
  "Approximate portage access" = "One yes/no indicator for whether any part of the county or MSA is within 10 km of a candidate portage location. Candidates are river crossings of the mapped Coastal Plain-Piedmont boundary (the fall line), plus nearby river endpoints. Rapids near this transition could require goods to be unloaded and carried overland. The geographic idea follows Bleakley and Lin (2012). Our construction uses different river data and an additional nearby-endpoint rule; the candidates have not been verified as historical portages.",
  "Waterway reference year" = "Changes only the historical river and canal access and distance measures. It is independent of the historical population instrument and sample years. Water Access Sum 1820 and the railroad controls keep their stated years; modern shorelines and approximate portage locations also stay fixed.",
  "Coverage and sample size" = "Terrain, crop, climate and the separate water measures cover the contiguous United States. Area averages use grid cells with available values; areas with no usable coverage remain missing. Unknown operating dates can also leave water measures missing. Observations with missing values in selected controls are excluded from the regression, so adding controls can reduce the sample. Missing values are never treated as zero."
)

geographic_help <- function(measures) {
  tagList(lapply(measures, function(measure)
    p(tags$strong(paste0(measure, ": ")), geographic_notes[[measure]])))
}
help_controls <- geographic_help(c("Water access sum, 1820", "Railroads"))
help_geo_controls <- geographic_help(c("Ruggedness and elevation", "Wheat and maize suitability", "Temperature and precipitation"))
help_water_controls <- geographic_help(c("Ocean and Great Lakes access", "Historical river and canal access", "Distance to shores and waterways", "Approximate portage access"))

# --- Helper for labelled inputs ----------------------------------------------

labeledInput <- function(inputId, labelText, inputUI, helpId, helpText) {
  div(
    class = "labeled-input-container",
    tags$label(labelText, `for` = inputId, class = "control-label"),
    div(class = "input-button-row", inputUI, actionButton(helpId, NULL, icon = icon("question-circle"), class = "help-btn")),
    conditionalPanel(
      condition = glue("input.{helpId} % 2 == 1"),
      div(class = "help-text", helpText)
    )
  )
}

geography_settings_defaults <- function() {
  list(sectors = "1", sample_year = "1790", iv_year = "1840", overlap = "5",
    schooling_adj = "3", apply_college_adj = TRUE, college_coeff = "0.75",
    mining_filter_active = FALSE, mining_threshold = "0.01", se_spec = "cluster_instrument",
    spatial_cutoff = "100", controls = c("water_1820", "railroads_1840"))
}

geography_instrument_choices <- function(level, approach = "density") {
  choices <- c("Max Density Overlap" = if (level == "MSA") "overlap" else "max_density_overlap")
  if (level == "County" && !identical(approach, "employment"))
    choices <- c(choices, "Weighted Density Overlap" = "weighted_density_overlap")
  c(choices, "Glaeser and Gottlieb (2009)" = "county_population", "Area-weighted population" = "area_population")
}

geography_standard_errors <- function(level, instrument, ch = FALSE) {
  choices <- c("Cluster on Instrument" = "cluster_instrument", "Cluster on State" = "cluster_state",
    "Spatial (Conley)" = "spatial", "Robust" = "robust")
  if (any(instrument %in% c("county_population", "area_population"))) names(choices)[1] <- paste("Cluster on", level)
  else if (any(instrument %in% c("overlap", "max_density_overlap"))) names(choices)[1] <- "Cluster on Historical County"
  if (ch) choices <- choices[choices != "spatial"]
  choices
}

geography_settings_ui <- function(level) {
  prefix <- tolower(level)
  id <- function(name) paste0(prefix, "_", name)
  defaults <- geography_settings_defaults()
  field <- function(name, label, widget, help) labeledInput(id(name), label, widget, paste0("help_", id(name)), help)
  checkbox <- function(name, label, help) tagList(
    div(class = "input-button-row", style = "margin-bottom:15px;",
      checkboxInput(id(name), label, value = defaults[[name]]),
      actionButton(paste0("help_", id(name)), NULL, icon = icon("question-circle"), class = "help-btn")),
    conditionalPanel(paste0("input.help_", id(name), " % 2 == 1"), div(class = "help-text", help)))
  overlap_id <- id(if (level == "MSA") "overlap_pct" else "overlap_threshold")
  overlap_condition <- if (level == "MSA") "input.msa_instrument_type == 'overlap'" else
    "input.county_instrument_type == 'max_density_overlap' || input.county_instrument_type == 'weighted_density_overlap'"
  density_choices <- c("Average employment density" = "average")
  if (level == "MSA") density_choices <- c(density_choices, "Ciccone–Hall index" = "CH")
  tagList(
    h4("4. Geographic Unit Settings"),
    conditionalPanel("input.approach != 'employment'",
      selectInput(id("density_measure"), "Density measure:", density_choices, "average"),
      if (level == "County") p(class = "help-block", "With one county per observation, the Ciccone–Hall model reduces to the regression using log county employment density.")),
    if (level == "MSA") conditionalPanel("input.approach != 'employment' && input.msa_density_measure == 'CH'",
      p(class = "help-block", "Uses employment density within each MSA's counties. OLS uses nonlinear least squares; IV uses GMM. State fixed effects and controls remain available. A separate first-stage regression and Conley standard errors are not implemented for this model.")),
    field("sectors", "Sector:", selectInput(id("sectors"), NULL,
      c("All" = 1, "Private" = 2, "Manufacturing" = 3, "Private non-farm" = 4, "Private non-farm/mining" = 5), defaults$sectors), help_sectors),
    field("sample_year", "Year of the Historical Territory:", selectInput(id("sample_year"), NULL, historical_census_years, defaults$sample_year), help_sample_year),
    field("iv_year", textOutput(id("iv_year_label"), inline = TRUE), selectInput(id("iv_year"), NULL, historical_census_years, defaults$iv_year), help_iv_year),
    field("instrument_type", "Match of Historical to Modern Geographic Units:",
      selectInput(id("instrument_type"), NULL, geography_instrument_choices(level), if (level == "MSA") "overlap" else "max_density_overlap"),
      if (level == "MSA") help_msa_instrument_type else help_county_instrument_type),
    if (level == "MSA") p(class = "help-block", "Weighted density overlap is available at County level."),
    conditionalPanel(overlap_condition,
      labeledInput(overlap_id, "Minimum Overlap (%):", selectInput(overlap_id, NULL, c(5,10,20,30,40,50,60,70,80,90), defaults$overlap),
        paste0("help_", overlap_id), if (level == "MSA") help_msa_overlap_pct else help_county_overlap_threshold),
      p(class = "help-block", if (level == "MSA") "Percentage of the historical county's area overlapping the modern MSA."
        else "Percentage of the modern county's area overlapping the historical county.")),
    field("schooling_adj", "Modern Adjustment for Human Capital", selectInput(id("schooling_adj"), NULL,
      c("None" = 0, "Same Return" = 1, "Specific Return" = 2, "Run Both" = 3), defaults$schooling_adj), help_schooling_adj),
    checkbox("apply_college_adj", "Apply College Share Adjustment", help_apply_college_adj),
    field("college_coeff", "College Share Coefficient:", textInput(id("college_coeff"), NULL, defaults$college_coeff, placeholder = "0.75"), help_college_coeff),
    checkbox("mining_filter_active", "Filter by Mining Share", help_mining_filter_active),
    field("mining_threshold", "Max Mining Share:", textInput(id("mining_threshold"), NULL, defaults$mining_threshold, placeholder = "0.01"), help_mining_threshold),
    field("se_spec", "Standard Errors:", selectInput(id("se_spec"), NULL,
      geography_standard_errors(level, if (level == "MSA") "overlap" else "max_density_overlap"), defaults$se_spec), help_se_spec),
    conditionalPanel(paste0("input.", id("se_spec"), " == 'spatial'"),
      field("spatial_cutoff", "Spatial Cutoff (km):", textInput(id("spatial_cutoff"), NULL, defaults$spatial_cutoff, placeholder = "100"), help_spatial_cutoff)),
    if (level == "County") tagList(
      div(class = "input-button-row", style = "margin-bottom:15px;",
        checkboxInput("county_msa_restriction", "Restrict to counties in an MSA", value = FALSE),
        actionButton("help_county_msa_restriction", NULL, icon = icon("question-circle"), class = "help-btn")),
      conditionalPanel("input.help_county_msa_restriction % 2 == 1", div(class = "help-text", help_county_msa_restriction)))
  )
}

geography_transport_ui <- function(level) {
  id <- paste0(tolower(level), "_controls")
  labeledInput(id, "Historical Transport:", checkboxGroupInput(id, NULL,
    c("Water Access Sum 1820" = "water_1820", "Railroads 1840" = "railroads_1840", "Railroads 1850" = "railroads_1850", "Railroads 1861" = "railroads_1861"),
    geography_settings_defaults()$controls), paste0("help_", id), help_controls)
}

geography_switch_plan <- function(from, to, values) {
  plan <- list(updates = list(), messages = character())
  if (length(from) != 1L || length(to) != 1L || !from %in% c("MSA", "County") || !to %in% c("MSA", "County") || identical(from, to)) return(plan)
  source <- paste0(tolower(from), "_")
  destination <- paste0(tolower(to), "_")
  shared <- c("sectors", "sample_year", "iv_year", "schooling_adj", "apply_college_adj", "college_coeff",
    "mining_filter_active", "mining_threshold", "se_spec", "spatial_cutoff", "controls")
  for (name in shared) if (paste0(source, name) %in% names(values)) {
    value <- values[[paste0(source, name)]]
    if (name == "controls" && is.null(value)) value <- character()
    if (!is.null(value)) plan$updates[[paste0(destination, name)]] <- value
  }
  source_overlap <- paste0(source, if (from == "MSA") "overlap_pct" else "overlap_threshold")
  if (!is.null(values[[source_overlap]]))
    plan$updates[[paste0(destination, if (to == "MSA") "overlap_pct" else "overlap_threshold")]] <- values[[source_overlap]]
  method <- values[[paste0(source, "instrument_type")]]
  if (identical(method, "weighted_density_overlap") && to == "MSA") {
    retained <- values[["msa_instrument_type"]]
    choices <- geography_instrument_choices("MSA")
    label <- names(choices)[match(retained, choices)]
    if (!length(label) || is.na(label)) label <- "its selected matching method"
    plan$messages <- c(plan$messages, paste("Weighted density overlap is available only for County. MSA keeps", paste0(label, "."), "Review the matching method before running."))
  } else if (length(method) == 1L) {
    if (method %in% c("overlap", "max_density_overlap")) method <- if (to == "MSA") "overlap" else "max_density_overlap"
    if (method %in% geography_instrument_choices(to, values$approach)) plan$updates[[paste0(destination, "instrument_type")]] <- method
  }
  msa_ch <- !identical(values$approach, "employment") && identical(values$msa_density_measure, "CH")
  if (to == "MSA" && msa_ch && identical(plan$updates$msa_se_spec, "spatial")) {
    plan$updates$msa_se_spec <- "robust"
    plan$messages <- c(plan$messages, "MSA retains its Ciccone–Hall model. Conley standard errors are not implemented for this model; Robust is selected.")
  }
  if (to == "County" && msa_ch)
    plan$messages <- c(plan$messages, "County uses average employment density. With one county per observation, the Ciccone–Hall model reduces to the log density regression.")
  plan
}

observe_geography_settings <- function(input, session) {
  last_method_level <- reactiveVal(NULL)
  saved_nonstate_method <- reactiveVal(NULL)
  observeEvent(input$approach, {
    choices <- geography_instrument_choices("County", input$approach)
    selected <- isolate(input$county_instrument_type)
    if (is.null(selected) || !selected %in% choices) {
      if (identical(selected, "weighted_density_overlap"))
        showNotification("Weighted density overlap is available for Employment density only. Max Density Overlap is selected for Employment.", duration = 10, session = session)
      selected <- "max_density_overlap"
    }
    updateSelectInput(session, "county_instrument_type", choices = choices, selected = selected)
  }, ignoreInit = TRUE)
  observeEvent(list(input$msa_density_measure, input$analysis_level, input$msa_instrument_type, input$county_instrument_type, input$approach), {
    msa_ch <- !identical(input$approach, "employment") && identical(input$msa_density_measure, "CH")
    active_ch <- identical(input$analysis_level, "State") || (identical(input$analysis_level, "MSA") && msa_ch)
    methods <- if (active_ch) c("IV", "OLS") else c("IV", "OLS", "First-stage Regression")
    selected <- isolate(input$analysis_type)
    previous_level <- isolate(last_method_level())
    if (identical(input$analysis_level, "State") && any(previous_level %in% c("MSA", "County")))
      saved_nonstate_method(selected)
    if (!identical(input$analysis_level, "State") && identical(previous_level, "State")) {
      previous_method <- isolate(saved_nonstate_method())
      if (length(previous_method) == 1L && previous_method %in% methods) selected <- previous_method
      saved_nonstate_method(NULL)
    }
    last_method_level(input$analysis_level)
    if (is.null(selected) || !selected %in% methods) {
      if (!is.null(selected)) showNotification("The Ciccone–Hall model offers IV (GMM) and OLS (nonlinear least squares). IV is selected.", duration = 10, session = session)
      selected <- "IV"
    }
    updateSelectInput(session, "analysis_type", choices = methods, selected = selected)
    for (level in c("MSA", "County")) {
      prefix <- paste0(tolower(level), "_")
      choices <- geography_standard_errors(level, input[[paste0(prefix, "instrument_type")]], level == "MSA" && msa_ch)
      selected_se <- isolate(input[[paste0(prefix, "se_spec")]])
      if (is.null(selected_se) || !selected_se %in% choices) {
        if (identical(selected_se, "spatial") && level == "MSA" && msa_ch) {
          selected_se <- "robust"
          showNotification("Conley standard errors are not implemented for MSA Ciccone–Hall. Robust is selected.", duration = 10, session = session)
        } else selected_se <- "cluster_instrument"
      }
      updateSelectInput(session, paste0(prefix, "se_spec"), choices = choices, selected = selected_se)
    }
  }, ignoreInit = FALSE)
  previous <- reactiveVal(NULL)
  observeEvent(input$analysis_level, {
    level <- input$analysis_level
    if (!level %in% c("MSA", "County")) return()
    from <- previous()
    previous(level)
    if (is.null(from)) return()
    plan <- geography_switch_plan(from, level, isolate(reactiveValuesToList(input)))
    for (id in names(plan$updates)) {
      value <- plan$updates[[id]]
      if (grepl("_(apply_college_adj|mining_filter_active)$", id)) updateCheckboxInput(session, id, value = value)
      else if (grepl("_controls$", id)) updateCheckboxGroupInput(session, id, selected = value)
      else if (grepl("_(college_coeff|mining_threshold|spatial_cutoff)$", id)) updateTextInput(session, id, value = value)
      else updateSelectInput(session, id, selected = value)
    }
    for (message in plan$messages) showNotification(message, duration = 12, session = session)
  }, ignoreInit = FALSE, priority = -10)
}

# ============================================================================
# 5. User Interface (UI) -------------------------------------------------------
# ============================================================================
# Geographic controls use the names and units in the two prepared datasets.
format_estimate <- function(value) {
  if (!is.finite(value)) return("—")
  sprintf("%.4f", value)
}

scale_model_controls <- function(data, selected) {
  for (variable in intersect(selected, c("gaez_wheat_suit", "gaez_maize_suit")))
    data[[variable]] <- data[[variable]] / 1000
  data
}

resolve_geo_controls <- function(geo_groups = character(), water_groups = character(), water_year = 1820) {
  land <- list(terrain = c("rugged_mean", "elev_mean"),
               crop = c("gaez_wheat_suit", "gaez_maize_suit"),
               climate = c("tjan", "tjul", "precip"))
  water_year <- as.integer(water_year)
  if (length(water_year) != 1L || is.na(water_year) || !water_year %in% seq(1790, 1860, 10))
    stop("Choose a water-access year from 1790 to 1860, in ten-year steps.")
  distances <- c("dist_ocean_100km", "dist_lakes_100km",
                 paste0(c("dist_river_", "dist_canal_"), water_year, "_100km"))
  water <- list(shoreline = c("ocean_access10", "lakes_access10"),
                historical_access = paste0(c("river_access_", "canal_access_"), water_year),
                distance = as.vector(rbind(distances, paste0(distances, "_sq"))),
                portage = "portage_access10")
  if (any(!geo_groups %in% names(land)) || any(!water_groups %in% names(water)))
    stop("An unknown geographic control group was selected.")
  unique(unname(unlist(c(land[geo_groups], water[water_groups]), use.names = FALSE)))
}

is_county_population <- function(details) {
  identical(details$instrument_type, "county_population")
}

is_aggregated_population <- function(details) {
  any(details$instrument_type %in% c("county_population", "area_population"))
}

uses_population_instrument <- function(details) {
  is_aggregated_population(details) || identical(details$approach, "employment")
}

transform_historical_instrument <- function(values, form = "levels", population = FALSE, historical_ch = FALSE) {
  if (is.null(form)) form <- "levels"
  if (!form %in% c("levels", "log")) stop("Choose Levels or Natural log for the historical instrument.")
  values <- as.numeric(values)
  keep <- is.finite(values)
  nonpositive <- 0L
  if (historical_ch) return(list(values = values, keep = keep, nonpositive = nonpositive, form = "historical_ch"))
  if (form == "log") {
    nonpositive <- sum(keep & values <= 0)
    keep <- keep & values > 0
    result <- rep(NA_real_, length(values))
    result[keep] <- log(values[keep])
  } else {
    keep <- keep & values >= 0
    result <- if (population) values / 1000 else values
  }
  list(values = result, keep = keep, nonpositive = as.integer(nonpositive), form = form)
}

instrument_units <- function(details) {
  if (identical(details$instrument_type, "historical_ch")) return("Population-weighted mean log historical density (people/km²)")
  population <- uses_population_instrument(details)
  if (identical(details$instrument_form, "log")) {
    if (population) "Natural log of population (people)" else "Natural log of density (people/km²)"
  } else if (population) "Thousands of people" else "People/km²"
}

sample_exclusion_notes <- function(details) {
  notes <- character()
  if (isTRUE(details$log_nonpositive_n > 0L)) notes <- c(notes, paste(details$log_nonpositive_n,
    "observations excluded because the selected historical instrument is zero or negative and its natural log is undefined."))
  if (isTRUE(details$sector_missing_n > 0L)) notes <- c(notes, paste(details$sector_missing_n,
    "observations excluded because the selected sector outcome is unavailable: required BEA components are missing/suppressed or output per job is nonpositive."))
  if (isTRUE(details$mining_unknown_n > 0L)) notes <- c(notes, paste(details$mining_unknown_n,
    "observations excluded by the mining filter because their mining share is unknown."))
  if (isTRUE(details$mining_above_n > 0L)) notes <- c(notes, paste(details$mining_above_n,
    "observations excluded because their mining share exceeds the selected threshold."))
  if (isTRUE(details$fe_missing_n > 0L)) notes <- c(notes, paste(details$fe_missing_n,
    "observations in the selected historical sample lack a usable identity for the selected state fixed effects and are excluded."))
  if (identical(details$analysis_level, "State") && isTRUE(details$population_missing_n > 0L))
    notes <- c(notes, paste(details$population_missing_n,
      "states have an unavailable historical instrument for the selected sample/instrument years and construction."))
  notes
}

schooling_adjustment_name <- function(value) {
  unname(c("0" = "None", "1" = "Same Return", "2" = "Specific Return", "3" = "Run Both")[as.character(value)])
}

sector_exclusion_outcome <- function(private_output, agriculture_output, private_nonfarm_jobs,
                                     forestry_fishing_jobs, mining_output = 0, mining_jobs = 0) {
  output <- private_output - agriculture_output - mining_output
  jobs <- private_nonfarm_jobs - forestry_fishing_jobs - mining_jobs
  valid <- is.finite(output) & output > 0 & is.finite(jobs) & jobs > 0
  result <- rep(NA_real_, length(output))
  result[valid] <- log(output[valid] / jobs[valid])
  result
}

historical_sample_filter <- function(data, sample_year, scope, construction, use_fe, fe_type) {
  full <- paste0("hist_state_fe_", sample_year)
  suffix <- if (scope == "states_only") "_s" else ""
  fe_missing_n <- 0L
  if (scope == "states_only" && !construction %in% c("county_population", "area_population")) {
    if (!full %in% names(data)) stop(paste("Historical sample-state identifiers are missing:", full))
    identity <- trimws(as.character(data[[full]]))
    territory <- grepl("territ|organised|organized|district|columbia|\\bdc\\b", tolower(identity))
    data <- data[!is.na(identity) & identity != "" & identity != "0" & !territory, , drop = FALSE]
  }
  if (isTRUE(use_fe)) {
    column <- if (fe_type == "modern") "state_id" else paste0(full, suffix)
    if (!column %in% names(data)) stop(paste("Fixed-effect identifiers are missing:", column))
    identity <- trimws(as.character(data[[column]]))
    valid <- !is.na(identity) & identity != "" & identity != "0"
    fe_missing_n <- sum(!valid)
    data <- data[valid & !grepl("district|columbia|\\bdc\\b", tolower(identity)), , drop = FALSE]
  }
  attr(data, "fe_missing_n") <- fe_missing_n
  data
}

model_sample_mask <- function(data, variables, fe_part = "0", remove_singletons = FALSE, ch = FALSE) {
  variables <- unique(c(variables, if (fe_part != "0") fe_part))
  absent <- setdiff(variables, names(data))
  if (length(absent)) stop(paste("Required model fields are missing:", paste(absent, collapse = ", ")))
  keep <- complete.cases(data[variables])
  for (name in variables) {
    value <- data[[name]]
    keep <- keep & if (is.numeric(value)) is.finite(value) else !is.na(value) & trimws(as.character(value)) != ""
  }
  if (ch) {
    nc <- grep("^nc[0-9]+$", names(data), value = TRUE)
    ac <- sub("^nc", "ac", nc)
    if (!length(nc) || !all(c(ac, "ch_complete") %in% names(data))) stop("Complete CH county inputs are required.")
    n <- as.matrix(data[nc]); a <- as.matrix(data[ac])
    keep <- keep & !is.na(data$ch_complete) & data$ch_complete == 1 &
      rowSums(!is.finite(n) | n < 0) == 0 & rowSums(!is.finite(a) | a <= 0) == 0 & rowSums(n) > 0
  }
  keep[is.na(keep)] <- FALSE
  if (remove_singletons && fe_part != "0") {
    sizes <- table(as.character(data[[fe_part]][keep]))
    keep <- keep & as.character(data[[fe_part]]) %in% names(sizes[sizes > 1L])
  }
  keep
}

approach_name <- function(approach) {
  if (identical(approach, "employment")) "Employment" else "Employment density"
}

instrument_cluster_label <- function(details) {
  if (is_aggregated_population(details)) paste(details$analysis_level, "clusters")
  else if (details$instrument_type %in% c("overlap", "max_density_overlap")) "Historical county clusters"
  else "Instrument clusters"
}

instrument_name <- function(type, approach = "density") {
  if (identical(approach, "employment") && type %in% c("overlap", "max_density_overlap"))
    return("Max Density Overlap (density-selected county population)")
  switch(type, county_population = "Glaeser and Gottlieb (2009)",
    area_population = "Area-weighted population",
    historical_ch = "Historical CH density index",
    overlap = "Max Density Overlap", max_density_overlap = "Max Density Overlap",
    weighted_density_overlap = "Weighted Density Overlap", type)
}

control_label <- function(variable, instrument_type = NULL, approach = "density", instrument_form = "levels") {
  if (variable == "instrument" && identical(instrument_type, "historical_ch")) return("Historical CH density index")
  if (variable == "instrument" && identical(instrument_form, "log"))
    return(if (is_aggregated_population(list(instrument_type = instrument_type)) || identical(approach, "employment")) "Log historical population" else "Log historical density")
  if (variable == "instrument" && (is_aggregated_population(list(instrument_type = instrument_type)) || identical(approach, "employment")))
    return("Historical population (thousands of people)")
  if (identical(approach, "employment") && variable %in% c("RHS", "fit_RHS"))
    return(if (variable == "fit_RHS") "Log employment (instrumented)" else "Log employment")
  if (variable == "ch_elasticity") return("Ciccone–Hall elasticity (theta − 1)")
  if (grepl("^dist_.*_100km_sq$", variable))
    return(sub("distance (100 km)", "distance² [(100 km)²]",
               control_label(sub("_sq$", "", variable)), fixed = TRUE))
  labels <- c(fit_RHS = "Log employment density (instrumented)", RHS = "Log employment density",
              instrument = "Historical density instrument", `(Intercept)` = "Constant",
              water_1820 = "Water access sum, 1820", railroads_1840 = "Railroads, 1840",
              railroads_1850 = "Railroads, 1850", railroads_1861 = "Railroads, 1861",
              rugged_mean = "Terrain ruggedness (m)", elev_mean = "Mean elevation (m)",
              gaez_wheat_suit = "Wheat suitability (per 1,000 points)",
              gaez_maize_suit = "Maize suitability (per 1,000 points)",
              tjan = "January temperature (°C)", tjul = "July temperature (°C)",
              precip = "Annual precipitation (mm)", ocean_access10 = "Ocean access within 10 km",
              lakes_access10 = "Great Lakes access within 10 km",
              dist_ocean_100km = "Ocean distance (100 km)",
              dist_lakes_100km = "Great Lakes distance (100 km)",
              portage_access10 = "Approximate portage access within 10 km")
  if (variable %in% names(labels)) return(unname(labels[[variable]]))
  legacy <- sub("1$", "", variable)
  if (legacy %in% c("water_1820", "railroads_1840", "railroads_1850", "railroads_1861"))
    return(unname(labels[[legacy]]))
  year <- sub(".*([0-9]{4})$", "\\1", variable)
  if (grepl("^river_access_", variable)) return(paste0("River access within 10 km, ", year))
  if (grepl("^canal_access_", variable)) return(paste0("Canal access within 10 km, ", year))
  if (grepl("^dist_river_[0-9]{4}_100km$", variable)) return(paste0("River distance (100 km), ", sub("^dist_river_([0-9]{4})_100km$", "\\1", variable)))
  if (grepl("^dist_canal_[0-9]{4}_100km$", variable)) return(paste0("Canal distance (100 km), ", sub("^dist_canal_([0-9]{4})_100km$", "\\1", variable)))
  variable
}

# The CH term uses county employment and land area within each MSA.
ch_index_terms <- function(theta, employment, area) {
  positive <- employment > 0
  log_density <- matrix(0, nrow(employment), ncol(employment))
  log_density[positive] <- log(employment[positive] / area[positive])
  log_weight <- matrix(-Inf, nrow(employment), ncol(employment))
  log_weight[positive] <- log(employment[positive]) + (theta - 1) * log_density[positive]
  largest <- apply(log_weight, 1L, max)
  weight <- exp(log_weight - largest)
  total <- rowSums(weight)
  derivative <- rowSums(weight * log_density) / total
  list(value = largest + log(total) - log(rowSums(employment)),
       derivative = derivative,
       second_derivative = rowSums(weight * (log_density - derivative)^2) / total)
}

stock_wright_lm_s <- function(y, instrument, X = NULL, cluster = NULL, design_qr = NULL) {
  n <- length(y)
  groups <- if (is.null(cluster)) NA_integer_ else length(unique(cluster))
  unavailable <- function(reason) list(statistic = NA_real_, p_value = NA_real_, df = 1L,
    null_elasticity = 0, nobs = n, clusters = groups, status = "unavailable", reason = reason)
  if (!n || length(instrument) != n || any(!is.finite(y)) || any(!is.finite(instrument)))
    return(unavailable("The null-moment inputs are incomplete."))
  if (!is.null(cluster) && (length(cluster) != n || anyNA(cluster) || groups < 2L))
    return(unavailable("At least two complete clusters are needed."))
  if (is.null(design_qr)) {
    if (is.null(X)) X <- matrix(1, n, 1L)
    X <- as.matrix(X)
    if (nrow(X) != n || !ncol(X) || any(!is.finite(X)))
      return(unavailable("The nuisance-variable inputs are incomplete."))
    scale_x <- sqrt(colSums(X^2))
    scale_x[scale_x == 0] <- 1
    design_qr <- qr(sweep(X, 2L, scale_x, "/"), tol = 1e-10)
  }
  if (n <= design_qr$rank) return(unavailable("No residual variation remains after controls and fixed effects."))
  # Partial out the same nuisance regressors under H0: theta = 1. Use an uncentered
  # score covariance, with no HC1 or cluster small-sample adjustment.
  scale_y <- max(abs(y))
  scale_z <- max(abs(instrument))
  if (scale_y == 0 || scale_z == 0) return(unavailable("The null-moment variance is zero."))
  yn <- y / scale_y
  zn <- instrument / scale_z
  yr <- qr.resid(design_qr, yn)
  zr <- qr.resid(design_qr, zn)
  if (sqrt(sum(zr^2)) <= 1e-10 * sqrt(sum(zn^2)))
    return(unavailable("The instrument has no variation after controls and fixed effects."))
  if (sqrt(sum(yr^2)) <= 1e-10 * sqrt(sum(yn^2)))
    return(unavailable("The null-moment variance is zero."))
  score <- yr * zr
  scale_score <- max(abs(score))
  if (!is.finite(scale_score) || scale_score == 0) return(unavailable("The null-moment variance is zero."))
  score <- score / scale_score
  cluster_score <- if (is.null(cluster)) score else as.numeric(rowsum(score, cluster, reorder = FALSE))
  variance <- sum(cluster_score^2)
  if (!is.finite(variance) || variance <= .Machine$double.eps^2 * sum(score^2))
    return(unavailable("The null-moment variance is zero."))
  statistic <- sum(score)^2 / variance
  list(statistic = statistic, p_value = pchisq(statistic, df = 1L, lower.tail = FALSE), df = 1L,
    null_elasticity = 0, nobs = n, clusters = groups, status = "available", reason = "")
}

stock_wright_text <- function(diagnostic) {
  if (is.null(diagnostic)) return(NULL)
  if (!identical(diagnostic$status, "available"))
    return(paste("Stock–Wright LM S unavailable:", diagnostic$reason))
  p_text <- if (diagnostic$p_value < 0.0001) "p < 0.0001" else sprintf("p = %.4f", diagnostic$p_value)
  paste0("Stock–Wright LM S (H0: CH elasticity = 0): ", sprintf("%.4f", diagnostic$statistic),
    "; ", p_text, " (chi-squared, 1 df; N = ", diagnostic$nobs, ").")
}

auxiliary_instrument_wald <- function(response, instrument, X = NULL, cluster = NULL, design_qr = NULL) {
  n <- length(response)
  groups <- if (is.null(cluster)) NA_integer_ else length(unique(cluster))
  covariance <- if (is.null(cluster)) "HC0" else "CR0"
  unavailable <- function(reason) list(statistic = NA_real_, partial_r2 = NA_real_,
    coefficient = NA_real_, std_error = NA_real_, nobs = n, clusters = groups,
    covariance = covariance, status = "unavailable", reason = reason)
  if (!n || length(instrument) != n || any(!is.finite(response)) || any(!is.finite(instrument)))
    return(unavailable("The auxiliary outcome or instrument inputs are incomplete."))
  if (!is.null(cluster) && (length(cluster) != n || anyNA(cluster) || groups < 2L))
    return(unavailable("At least two complete clusters are needed."))
  if (is.null(design_qr)) {
    if (is.null(X)) X <- matrix(1, n, 1L)
    X <- as.matrix(X)
    if (nrow(X) != n || !ncol(X) || any(!is.finite(X)))
      return(unavailable("The control or fixed-effect inputs are incomplete."))
    scale_x <- sqrt(colSums(X^2))
    scale_x[scale_x == 0] <- 1
    design_qr <- qr(sweep(X, 2L, scale_x, "/"), tol = 1e-10)
  }
  if (n <= design_qr$rank + 1L)
    return(unavailable("Too few observations remain after controls and fixed effects."))
  scale_d <- max(abs(response))
  scale_z <- max(abs(instrument))
  if (scale_d == 0 || scale_z == 0)
    return(unavailable("The auxiliary outcome or instrument has no variation."))
  dn <- response / scale_d
  zn <- instrument / scale_z
  dr <- qr.resid(design_qr, dn)
  zr <- qr.resid(design_qr, zn)
  dd <- sum(dr^2)
  zz <- sum(zr^2)
  if (sqrt(zz) <= 1e-10 * sqrt(sum(zn^2)))
    return(unavailable("The instrument has no variation after controls and fixed effects."))
  if (sqrt(dd) <= 1e-10 * sqrt(sum(dn^2)))
    return(unavailable("The auxiliary outcome has no variation after controls and fixed effects."))
  cross <- sum(zr * dr)
  coefficient <- cross / zz
  score <- zr * (dr - coefficient * zr)
  sums <- if (is.null(cluster)) score else as.numeric(rowsum(score, cluster, reorder = FALSE))
  variance <- sum(sums^2) / zz^2
  if (!is.finite(variance) || variance <= 0)
    return(unavailable("The auxiliary instrument coefficient has zero or unavailable variance."))
  list(statistic = coefficient^2 / variance, partial_r2 = min(1, max(0, cross^2 / (zz * dd))),
    coefficient = coefficient * scale_d / scale_z, std_error = sqrt(variance) * scale_d / scale_z,
    nobs = n, clusters = groups, covariance = covariance,
    status = "available", reason = "")
}

ch_instrument_relevance <- function(derivative, instrument, X = NULL, cluster = NULL,
                                    design_qr = NULL, theta = NA_real_) {
  diagnostic <- auxiliary_instrument_wald(derivative, instrument, X, cluster, design_qr)
  diagnostic$evaluated_theta <- theta
  diagnostic
}

anderson_rubin_wald <- function(y_null, instrument, X = NULL, cluster = NULL,
                                design_qr = NULL, null_elasticity = 0) {
  diagnostic <- auxiliary_instrument_wald(y_null, instrument, X, cluster, design_qr)
  diagnostic$p_value <- if (diagnostic$status == "available")
    pchisq(diagnostic$statistic, df = 1L, lower.tail = FALSE) else NA_real_
  diagnostic$df <- 1L
  diagnostic$null_elasticity <- null_elasticity
  diagnostic
}

anderson_rubin_text <- function(diagnostic) {
  if (is.null(diagnostic)) return(NULL)
  if (!identical(diagnostic$status, "available"))
    return(paste("Anderson–Rubin Wald unavailable:", diagnostic$reason))
  p_text <- if (diagnostic$p_value < 0.0001) "p < 0.0001" else sprintf("p = %.4f", diagnostic$p_value)
  paste0("Anderson–Rubin Wald (H0: CH elasticity = ", diagnostic$null_elasticity, "): ",
    sprintf("%.4f", diagnostic$statistic), "; ", p_text, " (chi-squared, 1 df; N = ", diagnostic$nobs, ").")
}

fit_ch_model <- function(data, dep_var, controls_vec = character(), fe_part = "0",
                         method = c("IV", "OLS"), se_spec = "robust",
                         n_cols = grep("^nc[0-9]+$", names(data), value = TRUE),
                         a_cols = sub("^nc", "ac", n_cols)) {
  method <- match.arg(method)
  if (!se_spec %in% c("robust", "cluster_instrument", "cluster_state")) {
    stop("The CH model supports robust or clustered standard errors. Select one of these options.")
  }
  if (!length(n_cols) || length(n_cols) != length(a_cols)) stop("County components for the CH index are unavailable.")
  cluster_col <- switch(se_spec, cluster_instrument = "clusterID", cluster_state = "state_id", NULL)
  has_fe <- !is.null(fe_part) && !identical(as.character(fe_part), "0")
  variables <- unique(c(dep_var, controls_vec, n_cols, a_cols,
                        if (has_fe) fe_part, if (method == "IV") "instrument", cluster_col))
  absent <- setdiff(variables, names(data))
  if (length(absent)) stop(paste("CH model variables are unavailable:", paste(absent, collapse = ", ")))
  for (variable in controls_vec) {
    if (!is.numeric(data[[variable]])) {
      source <- trimws(as.character(data[[variable]]))
      converted <- suppressWarnings(as.numeric(source))
      if (any(is.na(converted) & !is.na(source) & !source %in% c("", "."))) {
        stop(paste("The CH control must contain numbers:", variable))
      }
      data[[variable]] <- converted
    }
  }
  valid <- complete.cases(data[variables])
  numeric_vars <- variables[vapply(data[variables], is.numeric, logical(1))]
  for (variable in numeric_vars) valid <- valid & is.finite(data[[variable]])
  employment <- as.matrix(data[n_cols])
  area <- as.matrix(data[a_cols])
  valid <- valid & rowSums(employment < 0, na.rm = TRUE) == 0 &
    rowSums(area <= 0, na.rm = TRUE) == 0 & rowSums(employment, na.rm = TRUE) > 0
  if ("ch_complete" %in% names(data)) valid <- valid & !is.na(data$ch_complete) & data$ch_complete == 1
  rows <- which(valid)
  if (!length(rows)) stop("No observations have complete county components and selected CH model variables.")
  d <- data[rows, , drop = FALSE]
  employment <- employment[rows, , drop = FALSE]
  area <- area[rows, , drop = FALSE]
  y <- d[[dep_var]]
  singleton_n <- 0L
  if (has_fe) {
    fixed_effect <- factor(d[[fe_part]])
    singleton_n <- as.integer(sum(table(fixed_effect) == 1L))
    X <- diag(nlevels(fixed_effect))[as.integer(fixed_effect), , drop = FALSE]
    colnames(X) <- paste0(".fe_", levels(fixed_effect))
  } else {
    X <- matrix(1, nrow(d), 1L, dimnames = list(NULL, "(Intercept)"))
  }
  X <- cbind(X, as.matrix(d[controls_vec]))
  storage.mode(X) <- "double"
  scales <- sqrt(colSums(X^2))
  scales[scales == 0] <- 1
  design_qr <- qr(sweep(X, 2L, scales, "/"), tol = 1e-10)
  selected <- sort(design_qr$pivot[seq_len(design_qr$rank)])
  omitted <- setdiff(colnames(X), colnames(X)[selected])
  if (length(omitted)) warning(paste("CH model omitted collinear controls:", paste(omitted, collapse = ", ")), call. = FALSE)
  X <- X[, selected, drop = FALSE]
  scales <- scales[selected]
  Xs <- sweep(X, 2L, scales, "/")
  design_qr <- qr(Xs, tol = 1e-10)
  n <- length(y)
  k <- ncol(X) + 1L
  stock_wright <- if (method == "IV") stock_wright_lm_s(y, d$instrument, design_qr = design_qr,
    cluster = if (is.null(cluster_col)) NULL else d[[cluster_col]]) else NULL
  anderson_rubin <- if (method == "IV") anderson_rubin_wald(y, d$instrument, design_qr = design_qr,
    cluster = if (is.null(cluster_col)) NULL else d[[cluster_col]]) else NULL
  fail <- function(message) stop(errorCondition(message, class = "ch_fit_error",
    stock_wright = stock_wright, anderson_rubin = anderson_rubin))
  tryCatch({
  if (n <= k) fail("Too few observations remain for the selected CH model and fixed effects.")
  partial <- function(x) qr.resid(design_qr, x)
  yr <- partial(y)
  index <- function(theta) ch_index_terms(theta, employment, area)
  objective <- function(theta) sum((yr - partial(index(theta)$value))^2)
  theta_grid <- sort(unique(c(1e-7, seq(0.05, 3, by = 0.05), 4, 8, 16, 32)))
  rss_grid <- vapply(theta_grid, objective, numeric(1))
  best <- which.min(rss_grid)
  boundary <- best %in% c(1L, length(theta_grid))
  if (boundary && method == "OLS") fail("The CH least-squares solution is at the search boundary; this specification is not identified reliably.")
  theta <- if (boundary) theta_grid[best] else
    optimize(objective, interval = theta_grid[c(best - 1L, best + 1L)], tol = 1e-11)$minimum
  if (method == "IV") {
    zr <- partial(d$instrument)
    if (sqrt(sum(zr^2)) <= 1e-10 * max(1, sqrt(sum(d$instrument^2)))) {
      fail("The historical instrument has no variation after the selected controls and fixed effects.")
    }
    zr <- zr / sqrt(sum(zr^2))
    moment <- function(theta) sum(zr * (yr - partial(index(theta)$value)))
    moment_derivative <- function(theta) -sum(zr * partial(index(theta)$derivative))
    # Add turning points so two nearby roots need not cross a coarse interval's endpoints.
    theta_grid <- sort(unique(c(theta_grid, seq(.01, 3, by = .01), seq(3.05, 8, by = .05), seq(8.2, 32, by = .2))))
    derivative_grid <- vapply(theta_grid, moment_derivative, numeric(1))
    turns <- which(head(derivative_grid, -1L) * tail(derivative_grid, -1L) < 0)
    if (length(turns)) theta_grid <- sort(unique(c(theta_grid, vapply(turns, function(j)
      uniroot(moment_derivative, interval = theta_grid[c(j, j + 1L)], tol = 1e-11)$root, numeric(1)))))
    moment_grid <- vapply(theta_grid, moment, numeric(1))
    crossings <- which(head(moment_grid, -1L) * tail(moment_grid, -1L) <= 0)
    if (!length(crossings)) fail("No positive-theta IV solution was found in the searched range (0 to 32).")
    roots <- vapply(crossings, function(j) uniroot(moment, interval = theta_grid[c(j, j + 1L)], tol = 1e-11)$root, numeric(1))
    roots <- roots[!duplicated(round(roots, 8L))]
    if (length(roots) > 1L) warning("The nonlinear IV model has multiple solutions. The solution closest to the NLS estimate is shown.", call. = FALSE)
    theta <- roots[which.min(abs(roots - theta))]
  }
  terms <- index(theta)
  instrument_relevance <- if (method == "IV") ch_instrument_relevance(terms$derivative,
    d$instrument, design_qr = design_qr,
    cluster = if (is.null(cluster_col)) NULL else d[[cluster_col]], theta = theta) else NULL
  beta <- qr.coef(design_qr, y - terms$value) / scales
  fitted <- terms$value + as.vector(X %*% beta)
  residual <- y - fitted
  derivative <- cbind(ch_elasticity = terms$derivative, X)
  derivative_scale <- sqrt(colSums(derivative^2))
  Js <- sweep(derivative, 2L, derivative_scale, "/")
  if (qr(Js, tol = 1e-10)$rank < k) fail("The CH parameter is not identified separately from the selected controls and fixed effects.")
  if (method == "IV") {
    Z <- cbind(zr, Xs)
    bread <- crossprod(Z, Js)
    if (rcond(bread) < 1e-12) fail("The nonlinear IV derivative is too weak to estimate a reliable covariance matrix.")
    influence <- sweep((Z * residual) %*% t(solve(bread)), 2L, derivative_scale, "/")
  } else {
    bread <- crossprod(Js)
    influence <- sweep((Js * residual) %*% solve(bread), 2L, derivative_scale, "/")
  }
  groups <- NA_integer_
  if (!is.null(cluster_col)) {
    cluster <- factor(d[[cluster_col]])
    groups <- nlevels(cluster)
    if (groups < 2L) fail("At least two clusters are needed for clustered CH standard errors.")
    influence <- rowsum(influence, cluster, reorder = FALSE)
    correction <- if (method == "OLS") groups / (groups - 1) * (n - 1) / (n - k) else 1
    df <- if (method == "OLS") groups - 1L else Inf
  } else {
    correction <- if (method == "OLS") n / (n - k) else 1
    df <- if (method == "OLS") n - k else Inf
  }
  full_vcov <- crossprod(influence) * correction
  dimnames(full_vcov) <- list(colnames(derivative), colnames(derivative))
  full_coef <- c(ch_elasticity = theta - 1, beta)
  exposed <- !startsWith(names(full_coef), ".fe_")
  result <- list(coefficients = full_coef[exposed], covariance = full_vcov[exposed, exposed, drop = FALSE],
                 theta = theta, residuals = residual, fitted.values = fitted,
                 rows = rows, nobs = as.integer(n), df.residual = df,
                 parameter_count = k, method = method, se_spec = se_spec,
                 clusters = as.integer(groups), collin.var = omitted,
                 singleton_n = singleton_n,
                 stock_wright = stock_wright,
                 anderson_rubin = anderson_rubin,
                 instrument_relevance = instrument_relevance,
                 inference = if (method == "IV") "Stata gmm: asymptotic normal inference; no small-sample covariance correction." else if (is.null(cluster_col)) "Stata nl: HC1 covariance; t inference with N minus parameter count degrees of freedom." else "Stata nl: cluster covariance with finite-sample correction; t inference with clusters minus one degrees of freedom.",
                 full_coefficients = full_coef, full_covariance = full_vcov,
                 index_value = terms$value, index_derivative = terms$derivative,
                 objective = sum(residual^2), iv_moment = if (method == "IV") moment(theta) else NULL)
  class(result) <- "ch_model"
  result
  }, error = function(error) {
    error$stock_wright <- stock_wright
    error$anderson_rubin <- anderson_rubin
    stop(error)
  })
}

coef.ch_model <- function(object, ...) object$coefficients
vcov.ch_model <- function(object, ...) object$covariance
nobs.ch_model <- function(object, ...) object$nobs
residuals.ch_model <- function(object, ...) object$residuals
fitted.ch_model <- function(object, ...) object$fitted.values
confint.ch_model <- function(object, parm, level = 0.95, ...) {
  if (missing(parm)) parm <- names(coef(object))
  b <- coef(object)[parm]
  se <- sqrt(diag(vcov(object)))[parm]
  quantile <- if (is.finite(object$df.residual)) qt((1 + level) / 2, object$df.residual) else qnorm((1 + level) / 2)
  result <- cbind(b - quantile * se, b + quantile * se)
  colnames(result) <- paste0(format(100 * c((1 - level) / 2, (1 + level) / 2)), " %")
  result
}
ch_coeftable <- function(model) {
  estimate <- coef(model)
  se <- sqrt(diag(vcov(model)))
  statistic <- estimate / se
  p <- if (is.finite(model$df.residual)) 2 * pt(abs(statistic), df = model$df.residual, lower.tail = FALSE) else 2 * pnorm(abs(statistic), lower.tail = FALSE)
  cbind(Estimate = estimate, `Std. Error` = se, `t value` = statistic, `Pr(>|t|)` = p)
}


app_model_table <- function(model) {
  if (inherits(model, "ch_model")) return(ch_coeftable(model))
  fixest::coeftable(model)
}
app_model_rows <- function(model) {
  if (inherits(model, "ch_model")) return(model$rows)
  fixest::obs(model)
}
app_model_se <- function(model) sqrt(diag(stats::vcov(model)))
app_model_pvalue <- function(model) {
  table <- app_model_table(model)
  stats::setNames(as.numeric(table[, 4]), rownames(table))
}
model_description <- function(details) {
  if (identical(details$density_measure, "CH")) {
    return(if (details$analysis_type == "IV") "Ciccone–Hall · nonlinear IV (GMM)" else "Ciccone–Hall · nonlinear least squares")
  }
  details$analysis_type
}

model_coefficients <- function(details) {
  rows <- lapply(names(details$models), function(model_name) {
    model <- details$models[[model_name]]
    table <- app_model_table(model)
    interval <- stats::confint(model, level = .95)
    terms <- rownames(table)
    output <- data.frame(model = model_name, term = terms,
               label = vapply(terms, control_label, character(1), instrument_type = details$instrument_type, approach = details$approach, instrument_form = details$instrument_form),
               estimate = table[, 1], std_error = table[, 2],
               statistic = table[, 3], p_value = table[, 4],
               conf_low = interval[terms, 1], conf_high = interval[terms, 2],
               observations = stats::nobs(model), geography = details$analysis_level,
               year = details$year_modern, method = model_description(details),
               density_measure = details$density_measure, approach = approach_name(details$approach),
               stringsAsFactors = FALSE, row.names = NULL)
    if (!is.null(details$schooling_reference)) output$schooling_reference_years <- unname(details$schooling_reference[model_name])
    if (!is.null(model$stock_wright)) {
      output$stock_wright_lm_s <- model$stock_wright$statistic
      output$stock_wright_p_value <- model$stock_wright$p_value
      output$stock_wright_df <- model$stock_wright$df
      output$stock_wright_null_elasticity <- model$stock_wright$null_elasticity
      output$stock_wright_note <- stock_wright_text(model$stock_wright)
    }
    if (!is.null(model$anderson_rubin)) {
      diagnostic <- model$anderson_rubin
      output$anderson_rubin_wald_chi_squared <- diagnostic$statistic
      output$anderson_rubin_p_value <- diagnostic$p_value
      output$anderson_rubin_df <- diagnostic$df
      output$anderson_rubin_null_elasticity <- diagnostic$null_elasticity
      output$anderson_rubin_covariance <- diagnostic$covariance
      output$anderson_rubin_note <- anderson_rubin_text(diagnostic)
    }
    if (!is.null(model$instrument_relevance)) {
      diagnostic <- model$instrument_relevance
      output$ch_local_instrument_wald_f <- diagnostic$statistic
      output$ch_local_partial_r2 <- diagnostic$partial_r2
      output$ch_local_evaluated_theta <- diagnostic$evaluated_theta
      output$ch_local_covariance <- diagnostic$covariance
      output$ch_local_relevance_note <- if (diagnostic$status == "available")
        help_ch_instrument_wald else paste("Local instrument Wald F unavailable:", diagnostic$reason)
    }
    output
  })
  do.call(rbind, rows)
}

main_coefficient <- function(details) {
  if (identical(details$density_measure, "CH")) return("ch_elasticity")
  switch(details$analysis_type, IV = "fit_RHS", OLS = "RHS", "instrument")
}

result_plot <- function(details) {
  estimates <- model_coefficients(details)
  estimates <- estimates[estimates$term == main_coefficient(details), , drop = FALSE]
  if (!nrow(estimates)) stop("The selected coefficient could not be estimated for this specification.")
  estimates$model <- factor(estimates$model, levels = rev(names(details$models)))
  ggplot2::ggplot(estimates, ggplot2::aes(x = estimate, y = model)) +
    ggplot2::geom_vline(xintercept = 0, color = "#999999", linetype = "dashed", linewidth = .5) +
    ggplot2::geom_segment(ggplot2::aes(x = conf_low, xend = conf_high, yend = model),
                          color = "#007BFF", linewidth = 1.5) +
    ggplot2::geom_point(color = "#000000", fill = "#ffffff", shape = 21, size = 4.5, stroke = 1.6) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = .65)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .12)) +
    ggplot2::labs(x = if (details$analysis_type == "First-stage Regression") {
                    paste(control_label("instrument", details$instrument_type, details$approach, details$instrument_form), "coefficient")
                  } else if (identical(details$density_measure, "CH")) "Ciccone–Hall elasticity (theta − 1)" else if (identical(details$approach, "employment")) "Log employment coefficient" else "Employment density coefficient",
                  y = NULL, title = paste(details$analysis_level, "·", details$year_modern, "·", approach_name(details$approach), "·", model_description(details)),
                  subtitle = "Point estimates and 95% confidence intervals",
                  caption = "Intervals use the selected standard-error specification.") +
    ggplot2::theme_minimal(base_size = 14, base_family = "sans") +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_line(color = "#e6e6e6"),
                    plot.title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                    plot.subtitle = ggplot2::element_text(color = "#555555", margin = ggplot2::margin(b = 22)),
                    axis.text = ggplot2::element_text(color = "#222222"),
                    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 14)),
                    plot.caption = ggplot2::element_text(color = "#555555", hjust = 0, margin = ggplot2::margin(t = 20)),
                    plot.margin = ggplot2::margin(20, 22, 16, 14),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA))
}

ui <- fluidPage(title = "Agglomeration Effects USA",
  titlePanel(div(class = "beamer-title",
    h2("Regression Interface: MSA, County and State Analysis"),
    p(style="margin-top: 10px; font-size: 14px;", 
      "Code, data and maps available at: ",
      a(icon("github"), " GitHub",
        href="https://github.com/AlexGoeppert/AgglomerationEffectsUSA", 
        target="_blank", style="color: #007BFF;")),
    div(class = "presentation-option", checkboxInput("presentation_mode", "Presentation view", FALSE))
  ), windowTitle = "Agglomeration Effects USA"),
  
  # Page and table styles
  tags$head(tags$style(HTML("
    .labeled-input-container{margin-bottom:15px}
    .input-button-row{display:flex;align-items:center;gap:8px}
    .input-button-row .shiny-input-container{flex-grow:1;width:auto!important;margin-bottom:0}
    .labeled-input-container .shiny-options-group{padding-top:5px}
    .help-btn{background:none;border:none;color:#007BFF;font-size:20px;padding:0 5px;cursor:pointer;align-self:center;line-height:1;transition:color .2s ease,transform .2s ease}
    .help-btn:hover{color:#0056b3;transform:scale(1.1)}
    .help-btn:focus{outline:none;box-shadow:none}
    .help-text{background-color:#f8f9fa;border-left:4px solid #007BFF;padding:10px 15px;margin-top:8px;font-size:13px;color:#495057;border-radius:0 4px 4px 0}
    #instrument_map_render img{max-width:100%;height:auto;border:1px solid #ddd;border-radius:4px}
    

    .regression-table {
      font-family: 'Computer Modern', 'Times New Roman', 'Times', serif;
      font-size: 16px;
      margin: 20px auto;
      border-collapse: collapse;
      width: 100%;
      max-width: 900px;
      background-color: white;
    }
    .regression-table th {
      color: #000;
      font-weight: normal;
      text-align: center;
      padding: 10px 14px;
      border-top: 2px solid #000;
      border-bottom: 1px solid #000;
      border-left: none;
      border-right: none;
      background-color: white;
    }
    .regression-table td {
      padding: 5px 14px;
      text-align: center;
      border: none;
      background-color: white;
    }
    .regression-table .row-label {
      text-align: left;
      font-weight: normal;
      background-color: white;
      padding-left: 10px;
    }
    .regression-table .coefficient {
      font-weight: normal;
      color: #000;
      font-size: 16px;

    }
    .regression-table .se {
      font-weight: normal;
      color: #000;
      font-size: 16px;
    }
    .regression-table .stats {
      background-color: white;
      font-weight: normal;
      color: #000;
      font-size: 16px;
    }
    .regression-table .top-border {
      border-top: 1px solid #000;
    }
    .regression-table .bottom-border {
      border-bottom: 2px solid #000;
    }
    .table-notes {
      font-family: 'Computer Modern', 'Times New Roman', 'Times', serif;
      font-size: 12px;
      font-style: italic;
      margin-top: 5px;
      text-align: left;
      max-width: 900px;
      margin-left: auto;
      margin-right: auto;
    }
    .analysis-details {
      background-color: #F0F0F0;
      padding: 15px;
      border-radius: 5px;
      margin-top: 20px;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      font-size: 14px;
      color: #495057;
      max-width: 900px;
      margin-left: auto;
      margin-right: auto;
    }
    .analysis-details h5 {
      margin-top: 0;
      color: #343a40;
      font-weight: 600;
    }

    body{background:#fff;color:#000}
    a{color:#007BFF}a:hover{color:#0056b3}
    .btn-primary{background-color:#007BFF;border-color:#007BFF}
    .btn-primary:hover,.btn-primary:focus,.btn-primary:active{background-color:#0056b3!important;border-color:#0056b3!important}
    input[type=checkbox]{accent-color:#007BFF}
    .irs-bar,.irs-bar-edge,.irs-single,.irs-from,.irs-to{background:#007BFF!important;border-color:#007BFF!important}
    .container-fluid{padding-left:25px;padding-right:25px}
    .beamer-title{border-bottom:2px solid #007BFF;padding-bottom:8px;margin-bottom:14px}
    .beamer-title h2{color:#000;font-family:Georgia,'Times New Roman',serif;font-weight:normal}
    .well{background:#fff;border-color:#ddd;box-shadow:none}
    .well h4{margin-top:19px;margin-bottom:14px;font-size:17px;font-family:Georgia,'Times New Roman',serif;color:#000;border-bottom:1px solid #ddd;padding-bottom:6px}
    .well h4:first-child{margin-top:0}
    .control-label{line-height:1.45}
    .presentation-option{margin:10px 0 16px;font-size:13px}
    .presentation-option .checkbox{margin:0}
    .presentation-option .shiny-input-container{margin:0}
    .geo-control-note,.sample-note,.estimate-caption{font-size:12px;line-height:1.6;color:#555;margin:10px 0}
    .sample-note{padding:8px 12px;background:#f8f9fa;border-left:3px solid #007BFF}
    .download-row{display:flex;flex-wrap:wrap;gap:8px;margin:15px 0 20px}
    .download-row .btn{font-size:12px;padding:6px 10px}
    .optional-chart{margin:20px 0}
    .optional-chart h4{font-size:17px}
    .data-notes-panel{margin:20px 0;font-size:13px;line-height:1.6}
    .data-notes-panel>summary{cursor:pointer;color:#007BFF;font-weight:600;margin-bottom:10px}
    .data-notes-panel .note-card{padding:12px;background:#f8f9fa;border:1px solid #eee;margin:10px 0}
    #results_table{overflow-x:auto}
    body.presentation-mode .original-sidebar{display:none}
    body.presentation-mode .original-results{width:100%}
    @media(max-width:767px){.container-fluid{padding-left:15px;padding-right:15px}.regression-table{font-size:14px}.regression-table th,.regression-table td{padding-left:8px;padding-right:8px}}
    @media print{.original-sidebar,.presentation-option,.download-row{display:none!important}.original-results{width:100%!important}.container-fluid{padding:0}.regression-table{page-break-inside:avoid}a[href]:after{content:none!important}}
  ")),
            tags$script(HTML("
    // Simple validation for text inputs used as numeric inputs
    $(document).ready(function() {
      $('#run_analysis').closest('.col-sm-4').addClass('original-sidebar');
      $('#results_table').closest('.col-sm-8').addClass('original-results');
      function updatePresentation() {
        document.body.classList.toggle('presentation-mode', $('#presentation_mode').prop('checked') === true);
        setTimeout(function() { $(window).trigger('resize'); }, 100);
      }
      $(document).on('change', '#presentation_mode', updatePresentation);
      $(document).on('shiny:connected', updatePresentation);
      // Add input validation and formatting
      var numericTextInputs = ['#msa_college_coeff', '#msa_mining_threshold', '#msa_spatial_cutoff', 
                               '#county_college_coeff', '#county_mining_threshold', '#county_spatial_cutoff'];
      
      numericTextInputs.forEach(function(selector) {
        $(document).on('input', selector, function() {
          var value = this.value;
          // Allow numbers, periods, and commas, but replace commas with periods
          value = value.replace(/[^0-9.,]/g, '').replace(/,/g, '.');
          // Ensure only one period
          var parts = value.split('.');
          if (parts.length > 2) {
            value = parts[0] + '.' + parts.slice(1).join('');
          }
          this.value = value;
        });
        
        $(document).on('blur', selector, function() {
          var value = parseFloat(this.value.replace(/,/g, '.'));
          if (!isNaN(value)) {
            this.value = value.toString();
          }
        });
      });
    });
  "))),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 # ── 1. Master Configuration ───────────────────────────
                 h4("1. Master Configuration"),
                 labeledInput("analysis_level", "Select Analysis Level:",
                              selectInput("analysis_level", NULL, c("MSA", "County", "State (Ciccone–Hall)" = "State")),
                              "help_analysis_level", help_analysis_level),
                 labeledInput("year_modern", "Select Modern Year:",
                              selectInput("year_modern", NULL, seq(2022, 2001, -1), 2010),
                              "help_year_modern", help_year_modern),
                 div(style="margin-top:20px;", actionButton("run_analysis", "Run Analysis", icon=icon("play-circle"), class="btn-primary btn-lg", width="100%")),
                 hr(),
                 
                 # Display options
                 h4("Display Options"),
                 checkboxInput("show_map", "Display Instrument Map", value = TRUE),
                 checkboxInput("show_coefficient_chart", "Display Coefficient Chart", value = FALSE),
                 conditionalPanel("input.show_map == true", sliderInput("map_size", "Map Size:", min = 25, max = 200, value = 75, post = "%")),
                 hr(),
                 
                 # ── 2. Analysis Settings ───────────────────────────────
                 h4("2. Analysis Settings"),
                 conditionalPanel("input.analysis_level != 'State'",
                 labeledInput("approach", "Approach:",
                              selectInput("approach", NULL, c("Employment density" = "density", "Employment" = "employment"), "density"),
                              "help_approach", help_approach)),
                 labeledInput("analysis_type", "Analysis Method:",
                              selectInput("analysis_type", NULL, c("IV", "OLS", "First-stage Regression"), "IV"),
                              "help_analysis_type", help_analysis_type),
                 conditionalPanel("input.analysis_level != 'State' || input.state_instrument_type != 'historical_ch'",
                   labeledInput("instrument_form", "Historical instrument scale:",
                     selectInput("instrument_form", NULL, c("Levels" = "levels", "Natural log" = "log"), "levels"),
                     "help_instrument_form", help_instrument_form)),
                 hr(),
                 
                 # ── 3. Fixed Effects & Sample Scope ───────────────────
                 conditionalPanel("input.analysis_level == 'State'", h4("3. Historical Scope")),
                 conditionalPanel("input.analysis_level != 'State'",
                 h4("3. Fixed Effects & Sample Scope"),
                 div(class="input-button-row", style="margin-bottom:10px;",
                     checkboxInput("use_fe", "Use State Fixed Effects", value = TRUE),
                     actionButton("help_use_fe", NULL, icon = icon("question-circle"), class = "help-btn")),
                 conditionalPanel("input.help_use_fe % 2 == 1", div(class="help-text", help_use_fe)),
                 conditionalPanel("input.use_fe == true",
                                  labeledInput("fe_type", "Fixed Effects Type:",
                                               selectInput("fe_type", NULL, c("Historical"="historical", "Modern"="modern"), "historical"),
                                               "help_fe_type", help_fe_type))),
                 labeledInput("sample_scope", "Historical Territory:",
                              selectInput("sample_scope", NULL, c("Historical States Only"="states_only", "Historical States & Territories"="states_territories"), "states_only"),
                              "help_sample_scope", help_sample_scope),
                 hr(),
                 
                 conditionalPanel("input.analysis_level == 'State'",
                   h4("4. State Settings"),
                   p(class = "help-block", "There is one observation per modern state, so a separate fixed effect for each modern state would absorb all variation. Historical state/territory group effects are not implemented in this State specification. It uses all industries and robust standard errors, without schooling adjustments, mining filters or geographic controls. County and MSA settings are retained when you return to those levels."),
                   labeledInput("state_model", "Ciccone–Hall model:",
                     p(class = "help-block", "All-industry state GDP per job and county employment density. Robust standard errors."),
                     "help_state_ch", help_state_ch),
                   labeledInput("state_sample_year", "Year of the Historical Territory:",
                     selectInput("state_sample_year", NULL, historical_census_years, 1900),
                     "help_state_sample_year", help_sample_year),
                   labeledInput("state_iv_year", "Historical Instrument Year:",
                     selectInput("state_iv_year", NULL, historical_census_years, 1900),
                     "help_state_iv_year", help_iv_year),
                   labeledInput("state_instrument_type", "Match of Historical to Modern Geographic Units:",
                     selectInput("state_instrument_type", NULL,
                       c("Glaeser and Gottlieb (2009)" = "county_population", "Area-weighted population" = "area_population", "Historical CH density index" = "historical_ch"), "area_population"),
                     "help_state_instrument_type", help_state_instrument)
                 ),
                 conditionalPanel("input.analysis_level == 'MSA'", geography_settings_ui("MSA")),
                 conditionalPanel("input.analysis_level == 'County'", geography_settings_ui("County")),
                 hr(),
                 conditionalPanel("input.analysis_level != 'State'",
                 div(id = "geographic_controls_panel",
                   h4("Geographic Controls"),
                   p(class = "geo-control-note", "Control for physical geography and historical transport. Each box adds one or more separate variables to the regression. Use (?) for definitions and units."),
                   conditionalPanel("input.analysis_level == 'MSA'", geography_transport_ui("MSA")),
                   conditionalPanel("input.analysis_level == 'County'", geography_transport_ui("County")),
                   labeledInput("geo_controls", "Terrain, Crop Suitability and Climate:",
                     checkboxGroupInput("geo_controls", NULL,
                       c("Ruggedness and Elevation" = "terrain", "Wheat and Maize Suitability" = "crop", "Temperature and Precipitation" = "climate"), selected = character()),
                     "help_geo_controls", help_geo_controls),
                   labeledInput("water_controls", "Separate Water Measures:",
                     checkboxGroupInput("water_controls", NULL,
                       c("Ocean and Great Lakes Access" = "shoreline", "Historical River and Canal Access" = "historical_access", "Distance to Shores and Waterways" = "distance", "Approximate Portage Access" = "portage"), selected = character()),
                     "help_water_controls", help_water_controls),
                   conditionalPanel("(input.water_controls || []).indexOf('historical_access') >= 0 || (input.water_controls || []).indexOf('distance') >= 0",
                     labeledInput("water_year", "Waterway Reference Year:",
                       selectInput("water_year", NULL, seq(1790, 1860, 10), 1820),
                       "help_water_year", geographic_notes[["Waterway reference year"]])),
                   p(class = "geo-control-note", "Terrain, crop, climate and the separate water measures cover the contiguous U.S. Missing selected values can reduce the regression sample."),
                   tags$details(class = "geo-control-note", tags$summary("Coverage and sample size"), p(geographic_notes[["Coverage and sample size"]]))
                 ))
    ), # sidebarPanel
    
    # Results
    mainPanel(
      h3(textOutput("results_header")),
      uiOutput("sample_note"),
      hr(),
      withSpinner(htmlOutput("results_table"), type = 6, color = "black"),
      conditionalPanel("input.run_analysis > 0",
        div(class = "download-row",
          downloadButton("download_coefficients", "Coefficients (CSV)"),
          downloadButton("download_results", "Results (HTML)"),
          conditionalPanel("input.show_coefficient_chart == true", downloadButton("download_plot", "Chart (PNG)"))
        )
      ),
      conditionalPanel("input.show_coefficient_chart == true && input.run_analysis > 0",
        div(class = "optional-chart",
          h4("Coefficient Estimates"),
          p(class = "estimate-caption", "Points show the selected coefficient; lines show 95% confidence intervals."),
          withSpinner(plotOutput("effect_plot", height = "360px"), type = 6, color = "#007BFF")
        )
      ),
      conditionalPanel("input.run_analysis > 0",
        div(class = "configuration-toggle", style = "margin: 16px 0;",
          actionButton("toggle_configuration", "Show regression configuration", icon = icon("list"), class = "btn-sm")),
        conditionalPanel("input.toggle_configuration % 2 == 1", htmlOutput("analysis_details"))),
      conditionalPanel("input.run_analysis > 0",
        tags$details(class = "data-notes-panel", tags$summary(textOutput("data_notes_heading", inline = TRUE)), uiOutput("data_notes"))
      ),
      conditionalPanel(condition = "input.show_map == true", hr(), h3(textOutput("map_header")), uiOutput("map_ui"))
    )
  )
)

state_ch_result <- function(input, data) {
  year <- suppressWarnings(as.integer(input$year_modern))
  sample_year <- suppressWarnings(as.integer(input$state_sample_year))
  iv_year <- suppressWarnings(as.integer(input$state_iv_year))
  method <- input$analysis_type
  construction <- input$state_instrument_type
  scope <- input$sample_scope
  if (length(method) != 1L || !method %in% c("IV", "OLS")) stop("Choose IV or OLS for the state Ciccone–Hall model.")
  if (length(construction) != 1L || !construction %in% c("county_population", "area_population", "historical_ch")) stop("Choose a state population or historical CH construction.")
  if (length(scope) != 1L || !scope %in% c("states_only", "states_territories")) stop("Choose a historical territory scope.")
  if (length(year) != 1L || !year %in% 2001:2022) stop("Choose a modern year from 2001 to 2022.")
  if (length(sample_year) != 1L || length(iv_year) != 1L ||
      !sample_year %in% historical_census_years || !iv_year %in% historical_census_years)
    stop("Choose a historical census year from 1790 to 1900.")
  historical_ch <- construction == "historical_ch"
  prefix <- if (historical_ch) "HCH" else if (construction == "county_population") "GG" else "AW"
  suffix <- if (scope == "states_only") "_s" else ""
  sample_col <- paste0(prefix, if (historical_ch) "_" else "pop_", sample_year, suffix)
  instrument_col <- paste0(prefix, if (historical_ch) "_" else "pop_", iv_year, suffix)
  valid_cols <- paste0(prefix, "valid_", c(sample_year, iv_year), suffix)
  population_cols <- if (historical_ch) paste0("HCHpop_", c(sample_year, iv_year), suffix) else character()
  needed <- unique(c("year", "statefips", "state_name", "LHS", "RHS", "ch_complete",
    sample_col, instrument_col, valid_cols, population_cols))
  absent <- setdiff(needed, names(data))
  if (length(absent)) stop(paste("State data fields are missing:", paste(absent, collapse = ", ")))
  df <- data[data$year %in% year, , drop = FALSE]
  if (!nrow(df)) stop("State data are unavailable for the selected modern year.")
  valid_population <- Reduce(`&`, lapply(df[valid_cols], function(x) !is.na(x) & x == 1)) &
    is.finite(df[[sample_col]]) & is.finite(df[[instrument_col]])
  if (historical_ch) valid_population <- valid_population &
    Reduce(`&`, lapply(df[population_cols], function(x) is.finite(x) & x > 0))
  else valid_population <- valid_population & df[[sample_col]] >= 0 & df[[instrument_col]] >= 0
  population_missing_n <- sum(!valid_population)
  df <- df[valid_population, , drop = FALSE]
  transformed <- transform_historical_instrument(df[[instrument_col]], input$instrument_form,
    population = !historical_ch, historical_ch = historical_ch)
  df$instrument <- transformed$values
  df <- df[transformed$keep, , drop = FALSE]
  if (!nrow(df)) {
    message <- "No states have a usable historical instrument under the selected construction and transformation."
    if (transformed$nonpositive > 0L) message <- paste(message, transformed$nonpositive,
      "zero or negative values were excluded by the log transformation.")
    stop(message)
  }
  df$state_id <- df$statefips
  df$clusterID <- df$statefips
  df <- df[is.finite(df$LHS) & is.finite(df$RHS), , drop = FALSE]
  warnings <- character()
  model <- withCallingHandlers(
    fit_ch_model(df, "LHS", method = method, se_spec = "robust"),
    warning = function(warning) {
      warnings <<- unique(c(warnings, conditionMessage(warning)))
      invokeRestart("muffleWarning")
    })
  models <- setNames(list(model), "Ciccone–Hall")
  list(models = models, first_stage_models = list(), analysis_type = method,
    approach = "density", density_measure = "CH", analysis_level = "State", year_modern = year,
    sectors = 1L, sample_year = sample_year, iv_year = iv_year, instrument_type = construction,
    instrument_form = transformed$form, log_nonpositive_n = transformed$nonpositive,
    overlap_pct = NULL, schooling_adj = 0L, apply_college_adj = FALSE, college_coeff = NULL,
    mining_filter_active = FALSE, mining_threshold = NULL, county_msa_restriction = NULL,
    se_spec = "robust", spatial_cutoff = NULL, fe_type = "No", sample_scope = scope,
    controls = "None", controls_vec = character(), n_obs = nobs(model), eligible_n = nrow(df),
    geo_missing_n = 0L, ch_missing_n = sum(is.na(df$ch_complete) | df$ch_complete != 1),
    population_missing_n = population_missing_n,
    model_n = setNames(nobs(model), "Ciccone–Hall"), model_clusters = NA_integer_,
    model_warnings = warnings, geo_controls = character(), water_controls = character(),
    water_year = NA_integer_, new_controls = character(),
    cluster_data = list(clusterID = df$clusterID, state_id = df$state_id),
    data_for_fs = df, fe_part = "0", vcov_arg = "hetero")
}

server <- function(input, output, session) {
  
  analysis_output <- reactiveVal(NULL)
  map_output <- reactiveVal(NULL)

  output$msa_iv_year_label <- renderText({
    if (is_aggregated_population(list(instrument_type = input$msa_instrument_type)) || identical(input$approach, "employment"))
      "Year of the Historical Population (Historical Census Year):"
    else "Year of the Historical Population Density (Historical Census Year):"
  })

  output$county_iv_year_label <- renderText({
    if (is_aggregated_population(list(instrument_type = input$county_instrument_type)) || identical(input$approach, "employment"))
      "Year of the Historical Population (Historical Census Year):"
    else "Year of the Historical Population Density (Historical Census Year):"
  })

  observe_geography_settings(input, session)

  estimated_sample <- function(model, details = analysis_output()) {
    details$data_for_fs[app_model_rows(model), , drop = FALSE]
  }

  instrument_wald_f <- function(model) {
    b <- coef(model)["instrument"]
    uncertainty <- se(model)["instrument"]
    if (length(b) != 1L || !is.finite(b) || !is.finite(uncertainty) || uncertainty <= 0) return(NA_real_)
    unname((b / uncertainty)^2)
  }
  
  # Simple helper function to safely convert text inputs to numeric
  safe_numeric <- function(value, default = 0) {
    if (is.null(value) || value == "" || is.na(value)) return(default)
    
    # Replace commas with periods and convert to numeric
    clean_value <- gsub(",", ".", as.character(value))
    result <- suppressWarnings(as.numeric(clean_value))
    
    if (is.na(result)) return(default)
    return(result)
  }
  
  observeEvent(input$run_analysis, {
    
    # --- 1. Run Regression Analysis ---
    reg_results <- tryCatch({
      if (identical(input$analysis_level, "State")) {
        state_ch_result(input, state_data)
      } else withProgress(message = 'Running Analysis...', value = 0, {
        
        isolate({
          analysis_level <- input$analysis_level
          year_modern <- as.numeric(input$year_modern)
          analysis_type <- input$analysis_type
          approach <- if (identical(input$approach, "employment")) "employment" else "density"
          density_measure <- if (approach == "employment") "employment" else if (analysis_level == "MSA" && identical(input$msa_density_measure, "CH")) "CH" else "average"
          if (density_measure == "CH" && !analysis_type %in% c("IV", "OLS")) stop("Choose IV or OLS for the Ciccone–Hall model.")
          use_fe <- input$use_fe
          fe_type <- input$fe_type
          sample_scope <- input$sample_scope
          instrument_form <- if (is.null(input$instrument_form)) "levels" else input$instrument_form
        })
        
        incProgress(0.1, detail = "Configuring analysis...")
        
        suffix <- if (sample_scope == "states_only") "_s" else ""
        instrument_id_col <- NULL
        mining_unknown_n <- 0L
        mining_above_n <- 0L
        
        if (analysis_level == "MSA") {
          isolate({
            sectors <- as.numeric(input$msa_sectors)
            sample_year <- as.numeric(input$msa_sample_year)
            iv_year <- as.numeric(input$msa_iv_year)
            instrument_type <- input$msa_instrument_type
            overlap_pct <- input$msa_overlap_pct
            controls_vec <- input$msa_controls
            schooling_adj <- as.numeric(input$msa_schooling_adj)
            apply_college_adj <- input$msa_apply_college_adj
            college_coeff <- safe_numeric(input$msa_college_coeff, 0.75)
            se_spec <- input$msa_se_spec
            mining_filter_active <- input$msa_mining_filter_active
            mining_threshold <- safe_numeric(input$msa_mining_threshold, 0.01)
            spatial_cutoff <- safe_numeric(input$msa_spatial_cutoff, 100)
          })
          
          df <- get_app_year("MSA", year_modern) %>%
            filter(year == year_modern) %>%
            rename(lat = lat_DD, lon = lon_DD, state_id = modern_state_fe,
                   avg_schooling = msa_schooling_09, college_share = college_share_09)
          
          if (instrument_type == "county_population") {
            sample_col <- glue("GGpop_{sample_year}{suffix}")
            instr_col <- glue("GGpop_{iv_year}{suffix}")
            overlap_pct <- NULL
          } else if (instrument_type == "area_population") {
            sample_col <- glue("AWpop_{sample_year}{suffix}")
            instr_col <- glue("AWpop_{iv_year}{suffix}")
            overlap_pct <- NULL
          } else if (instrument_type == "overlap") {
            if (approach == "employment") {
              sample_col <- glue("MSApop_{sample_year}_{overlap_pct}{suffix}")
              instr_col <- glue("MSApop_{iv_year}_{overlap_pct}{suffix}")
              instrument_id_col <- glue("MSApopid_{iv_year}_{overlap_pct}{suffix}")
            } else {
              sample_col <- glue("MSAol_{sample_year}_{overlap_pct}{suffix}")
              instr_col <- glue("MSAol_{iv_year}_{overlap_pct}{suffix}")
              instrument_id_col <- glue("MSApopid_{iv_year}_{overlap_pct}{suffix}")
            }
          } else stop("Choose a supported MSA instrument construction.")
          

          df <- df %>% mutate(RHS = if (approach == "employment") log(employment) else log(employment / (msaarea / 4046.8)))
          df <- switch(
            as.character(sectors),
            "1" = df %>% mutate(LHS = ln_output_worker),
            "2" = df %>% mutate(LHS = ln_output_worker_private),
            "3" = df %>% mutate(LHS = log(gmp_indstryid_12 / emply_indryid_500)),
            "4" = df %>% mutate(LHS = sector_exclusion_outcome(gmp_private_industry, gmp_agriculture,
              employment_private_nonfarm, emply_indryid_100)),
            "5" = df %>% mutate(LHS = sector_exclusion_outcome(gmp_private_industry, gmp_agriculture,
              employment_private_nonfarm, emply_indryid_100, gmp_mining, employment_mining)),
            stop("Invalid MSA sector specified.")
          )
          

          
        } else { # County Logic
          isolate({
            sectors <- as.numeric(input$county_sectors)
            sample_year <- as.numeric(input$county_sample_year)
            iv_year <- as.numeric(input$county_iv_year)
            instrument_type <- input$county_instrument_type
            overlap_threshold <- input$county_overlap_threshold
            controls_vec <- input$county_controls
            schooling_adj <- as.numeric(input$county_schooling_adj)
            apply_college_adj <- input$county_apply_college_adj
            college_coeff <- safe_numeric(input$county_college_coeff, 0.75)
            se_spec <- input$county_se_spec
            mining_filter_active <- input$county_mining_filter_active
            mining_threshold <- safe_numeric(input$county_mining_threshold, 0.01)
            county_msa_restriction <- input$county_msa_restriction
            spatial_cutoff <- safe_numeric(input$county_spatial_cutoff, 100)
          })
          
          df <- get_app_year("County", year_modern) %>%
            filter(year == year_modern) %>%
            rename(state_id = modern_state_fe)
          
          df <- df %>%
            group_by(geofips) %>%
            mutate(avg_schooling = mean(avg_schooling_10_14, na.rm = TRUE)) %>%
            ungroup()
          
          if (county_msa_restriction) {
            if (!"county_partof_MSA" %in% names(df)) stop("Error: 'county_partof_MSA' column required for MSA restriction not found.")
            df <- df %>% filter(county_partof_MSA == 1)
          }
          

          if (instrument_type == "county_population") {
            sample_col <- glue("GGpop_{sample_year}{suffix}")
            instr_col <- glue("GGpop_{iv_year}{suffix}")
            overlap_threshold <- NULL
          } else if (instrument_type == "area_population") {
            sample_col <- glue("AWpop_{sample_year}{suffix}")
            instr_col <- glue("AWpop_{iv_year}{suffix}")
            overlap_threshold <- NULL
          } else if (instrument_type == "weighted_density_overlap") {
            if (approach == "employment") stop("Choose Max Density Overlap, Glaeser and Gottlieb (2009), or Area-weighted population for Employment.")
            sample_col <- glue("iv_overlap_{overlap_threshold}_{sample_year}{suffix}")
            instr_col  <- glue("iv_overlap_{overlap_threshold}_{iv_year}{suffix}")
          } else if (instrument_type == "max_density_overlap") {
            if (approach == "employment") {
              sample_col <- glue("ivpop_overlap_max_{overlap_threshold}_{sample_year}{suffix}")
              instr_col <- glue("ivpop_overlap_max_{overlap_threshold}_{iv_year}{suffix}")
              instrument_id_col <- glue("ivpopid_overlap_max_{overlap_threshold}_{iv_year}{suffix}")
            } else {
              sample_col <- glue("iv_overlap_max_{overlap_threshold}_{sample_year}{suffix}")
              instr_col <- glue("iv_overlap_max_{overlap_threshold}_{iv_year}{suffix}")
              instrument_id_col <- glue("ivpopid_overlap_max_{overlap_threshold}_{iv_year}{suffix}")
            }
          } else {
            stop("Error: Invalid instrument_type for County level specified.")
          }
          
          df <- df %>% mutate(RHS = if (approach == "employment") log(employment_total) else log(employment_total / area_acre))
          df <- switch(
            as.character(sectors),
            "1" = df %>% mutate(LHS = log(gcp_total / employment_total)),
            "2" = df %>% mutate(LHS = log(gcp_private_industry / (employment_private_nonfarm + emply_c_indryid_70))),
            "3" = df %>% rename(gcp_manufact = gcp_indstryid_12) %>%
              mutate(LHS = log(gcp_manufact / emply_c_indryid_500)),
            "4" = df %>% mutate(LHS = sector_exclusion_outcome(gcp_private_industry, gcp_agriculture,
              employment_private_nonfarm, emply_c_indryid_100)),
            "5" = df %>% mutate(LHS = sector_exclusion_outcome(gcp_private_industry, gcp_agriculture,
              employment_private_nonfarm, emply_c_indryid_100, gcp_mining, employment_mining)),
            stop("Invalid County sector specified.")
          )
          

        }
        
        incProgress(0.3, detail = "Finalizing data preparation...")
        
        new_controls <- resolve_geo_controls(input$geo_controls, input$water_controls, input$water_year)
        controls_vec <- unique(c(controls_vec, new_controls))
        req_cols <- c(sample_col, instr_col, instrument_id_col, controls_vec)
        missing_cols <- req_cols[!req_cols %in% names(df)]
        if (length(missing_cols) > 0) {
          stop(glue("Error: Required column(s) not found: {paste(missing_cols, collapse=', ')}. Check selections or data file."))
        }
        df <- scale_model_controls(df, controls_vec)
        
        df <- df %>% mutate(across(all_of(c(sample_col, instr_col)), as.numeric))
        df <- df %>%
          mutate(.sample_iv = .data[[sample_col]], instrument = .data[[instr_col]])
        population_instrument <- instrument_type %in% c("county_population", "area_population") || approach == "employment"
        df <- df[is.finite(df$.sample_iv) & df$.sample_iv >= 0, , drop = FALSE]
        transformed <- transform_historical_instrument(df$instrument, instrument_form, population_instrument)
        log_nonpositive_n <- transformed$nonpositive
        df$instrument <- transformed$values
        df <- df[transformed$keep, , drop = FALSE]
        if (!nrow(df) && log_nonpositive_n > 0L) stop(paste("Natural log excludes all", log_nonpositive_n, "available observations because the historical instrument is zero or negative."))
        df <- df %>% select(-.sample_iv)
        df <- historical_sample_filter(df, sample_year, sample_scope, instrument_type, use_fe, fe_type)
        fe_missing_n <- attr(df, "fe_missing_n")
        if (mining_filter_active) {
          mining_share <- if (analysis_level == "MSA") df$gmp_mining / df$gmp else df$gcp_mining / df$gcp_total
          mining_unknown_n <- sum(!is.finite(mining_share))
          mining_above_n <- sum(is.finite(mining_share) & mining_share > mining_threshold)
          df <- df[is.finite(mining_share) & mining_share <= mining_threshold, , drop = FALSE]
        }
        if (instrument_type %in% c("county_population", "area_population")) {
          unit_id <- if (analysis_level == "MSA") "msafips" else "geofips"
          if (!unit_id %in% names(df)) stop("Modern geographic identifiers are required for the population instrument.")
          df$clusterID <- as.integer(as.factor(df[[unit_id]]))
        } else if (!is.null(instrument_id_col)) {
          if (any(is.na(df[[instrument_id_col]]) | trimws(df[[instrument_id_col]]) == ""))
            stop("Historical county identifiers are missing for the selected overlap instrument. Rebuild the instrument inputs.")
          df$clusterID <- as.integer(as.factor(df[[instrument_id_col]]))
        } else df$clusterID <- as.integer(as.factor(df$instrument))
        sector_missing_n <- sum(!is.finite(df$LHS))
        if (apply_college_adj) {
          if (!"college_share" %in% names(df)) stop("Error: 'college_share' column not found.")
          df <- df %>% mutate(LHS = LHS - college_coeff * college_share)
        }
        
        # Check for human capital adjustment columns before using them
        if (!"simple_gamma" %in% names(df) || !"Minc_return_calc" %in% names(df)) {
          stop("Error: 'simple_gamma' and/or 'Minc_return_calc' columns not found in the data. These are required for schooling adjustments.")
        }
        
        df$LHS_adj1 <- df$LHS_adj2 <- NA_real_
        df <- df %>% filter(if_all(c(RHS, LHS, instrument), ~is.finite(.)))
        if(nrow(df) == 0){
          stop("After all filtering, 0 observations remain. Check selections for missing data in key variables.")
        }
        
        eligible_n <- nrow(df)
        geo_missing_n <- if (length(new_controls)) {
          sum(!Reduce(`&`, lapply(df[new_controls], is.finite)))
        } else 0L

        ch_missing_n <- 0L
        if (density_measure == "CH") {
          if (!"ch_complete" %in% names(df)) stop("The MSA data do not contain the Ciccone–Hall county inputs. Rebuild the MSA data first.")
          ch_missing_n <- sum(is.na(df$ch_complete) | df$ch_complete != 1)
        }

        incProgress(0.5, detail = "Building regression models...")
        
        dep_vars_map <- list(
          "0" = setNames("LHS", "No Schooling Adj."),
          "1" = setNames("LHS_adj1", "Same Return Adj."),
          "2" = setNames("LHS_adj2", "Specific Return Adj."),
          "3" = setNames(c("LHS_adj1", "LHS_adj2"), c("Same Return Adj.", "Specific Return Adj.")))
        dep_vars <- dep_vars_map[[as.character(schooling_adj)]]
        if (analysis_type == "First-stage Regression") {
          dep_vars <- setNames("RHS", "First stage")
        }
        controls_part <- if (length(controls_vec) > 0) paste("+", paste(controls_vec, collapse = " + ")) else ""
        fe_part <- "0"
        if (use_fe) {
          if (fe_type == "modern") {
            fe_part <- "state_id"
          } else {
            fe_var_name <- glue("hist_state_fe_{sample_year}{suffix}")
            if (!fe_var_name %in% names(df)) {
              stop(glue("Error: Historical FE variable '{fe_var_name}' not found."))
            }
            df[[fe_var_name]] <- as.factor(df[[fe_var_name]])
            fe_part <- fe_var_name
          }
        }
        
        # Determine vcov argument based on standard error specification
        if (se_spec == "spatial") {
          if (!is.finite(spatial_cutoff) || spatial_cutoff <= 0) stop("Choose a positive spatial distance cutoff.")
          vcov_arg <- vcov_conley(lat = "lat", lon = "lon", cutoff = spatial_cutoff, distance = "spherical")
        } else {
          vcov_arg <- switch(se_spec,
                             "robust" = "hetero",
                             "cluster_instrument" = ~clusterID,
                             "cluster_state" = ~state_id,
                             stop("Invalid 'se_spec' defined.")
          )
        }
        
        incProgress(0.7, detail = "Estimating models...")
        models <- list()
        first_stage_models <- list()
        model_warnings <- character()
        schooling_reference <- setNames(rep(NA_real_, length(dep_vars)), names(dep_vars))
        record_model_warning <- function(warning) {
          model_warnings <<- unique(c(model_warnings, conditionMessage(warning)))
          invokeRestart("muffleWarning")
        }
        
        for (i in seq_along(dep_vars)) {
          dep_var_name <- dep_vars[i]
          model_name <- names(dep_vars)[i]
          if (dep_var_name %in% c("LHS_adj1", "LHS_adj2")) {
            return_column <- if (dep_var_name == "LHS_adj1") "simple_gamma" else "Minc_return_calc"
            sample_variables <- c("LHS", "RHS", "instrument", controls_vec, "avg_schooling", return_column,
              switch(se_spec, cluster_instrument = "clusterID", cluster_state = "state_id", spatial = c("lat", "lon"), NULL))
            keep <- model_sample_mask(df, sample_variables, fe_part,
              remove_singletons = density_measure != "CH", ch = density_measure == "CH")
            if (!any(keep)) stop("No complete observations remain for the selected schooling adjustment.")
            reference <- mean(df$avg_schooling[keep])
            schooling_reference[[model_name]] <- reference
            df[[dep_var_name]][keep] <- df$LHS[keep] - df[[return_column]][keep] * (df$avg_schooling[keep] - reference)
          }
          
          formula_str <- switch(analysis_type,
                                "OLS" = glue("{dep_var_name} ~ RHS {controls_part} | {fe_part}"),
                                "IV"  = glue("{dep_var_name} ~ 1 {controls_part} | {fe_part} | RHS ~ instrument"),
                                "First-stage Regression" = glue("RHS ~ instrument {controls_part} | {fe_part}"),
                                stop("Invalid 'analysis_type' specified.")
          )
          
          model <- withCallingHandlers(
            if (density_measure == "CH") {
              fit_ch_model(data = df, dep_var = dep_var_name, controls_vec = controls_vec,
                fe_part = fe_part, method = analysis_type, se_spec = se_spec)
            } else feols(as.formula(formula_str), data = df, vcov = vcov_arg, fixef.rm = "singletons"),
            warning = record_model_warning
          )
          models[[model_name]] <- model
          
          if (analysis_type == "IV" && density_measure != "CH") {
            first_stage_models[[model_name]] <- withCallingHandlers(
              summary(model, stage = 1),
              warning = record_model_warning
            )
          }
        }
        
        model_n <- vapply(models, nobs, integer(1))
        cluster_column <- switch(se_spec, cluster_instrument = "clusterID", cluster_state = "state_id", NULL)
        model_clusters <- vapply(models, function(model) {
          if (is.null(cluster_column)) return(NA_integer_)
          values <- df[[cluster_column]][app_model_rows(model)]
          as.integer(length(unique(values[!is.na(values)])))
        }, integer(1))

        incProgress(0.9, detail = "Formatting results...")
        
        # Return both models and metadata for professional formatting
        list(
          models = models,
          first_stage_models = first_stage_models,
          analysis_type = analysis_type,
          approach = approach,
          density_measure = density_measure,
          ch_missing_n = ch_missing_n,
          analysis_level = analysis_level,
          year_modern = year_modern,
          sectors = sectors,
          sample_year = sample_year,
          iv_year = iv_year,
          instrument_type = instrument_type,
          instrument_form = instrument_form, log_nonpositive_n = log_nonpositive_n,
          sector_missing_n = sector_missing_n, mining_unknown_n = mining_unknown_n, mining_above_n = mining_above_n,
          fe_missing_n = fe_missing_n,
          schooling_reference = schooling_reference,
          overlap_pct = if(analysis_level == "MSA") overlap_pct else overlap_threshold,
          schooling_adj = schooling_adj,
          apply_college_adj = apply_college_adj,
          college_coeff = if(apply_college_adj) college_coeff else NULL,
          mining_filter_active = mining_filter_active,
          mining_threshold = if(mining_filter_active) mining_threshold else NULL,
          county_msa_restriction = if(analysis_level == "County") county_msa_restriction else NULL,
          se_spec = se_spec,
          spatial_cutoff = if(se_spec == "spatial") spatial_cutoff else NULL,
          fe_type = if(use_fe) fe_type else "No",
          sample_scope = sample_scope,
          controls = if(length(controls_vec) > 0) paste(controls_vec, collapse = ", ") else "None",
          n_obs = unname(model_n[[1]]),
          eligible_n = eligible_n,
          geo_missing_n = geo_missing_n,
          model_n = model_n,
          model_clusters = model_clusters,
          model_warnings = model_warnings,
          geo_controls = input$geo_controls,
          water_controls = input$water_controls,
          water_year = as.integer(input$water_year),
          new_controls = new_controls,
          cluster_data = list(
            clusterID = if("clusterID" %in% names(df)) df$clusterID else NULL,
            state_id = if("state_id" %in% names(df)) df$state_id else NULL
          ),
          data_for_fs = df,
          controls_vec = controls_vec,
          fe_part = fe_part,
          vcov_arg = vcov_arg
        )
      })
    }, error = function(e) {
      list(error = paste("An error occurred during analysis:\n\n", e$message),
        stock_wright = e$stock_wright, anderson_rubin = e$anderson_rubin)
    })
    
    analysis_output(reg_results)
    
    # --- 2. Generate Map Path ---
    isolate({
      level <- input$analysis_level
      scope <- input$sample_scope
      scope_folder <- if (scope == "states_territories") "states_and_territories" else "states_only"
      
      map_path <- NULL
      map_title <- "Instrument Map: Historical density instrument map not available for the selected settings."
      
      selected_type <- if (level == "State") input$state_instrument_type else if (level == "MSA") input$msa_instrument_type else input$county_instrument_type
      population_map <- uses_population_instrument(list(instrument_type = selected_type, approach = input$approach))
      if (level == "State") {
        map_title <- paste("Instrument Map:", instrument_name(selected_type))
      } else if (population_map) {
        map_title <- "Instrument Map: Historical population (thousands of people)"
      } else if (level == "MSA") {
        year <- input$msa_iv_year
        pct <- input$msa_overlap_pct
        base_folder <- paste("www", "MSA Maps", scope_folder, sep="/")
        
        # Simplified logic - only overlap for MSA
        filename <- glue("overlap_{pct}pct_{year}.png")
        subfolder <- glue("{pct}pct")
        map_path <- paste(base_folder, "overlap", subfolder, filename, sep="/")
        map_title <- glue("Instrument Map: Maximum population density among {year} historical counties with {pct}% minimum overlap with modern MSA")
        
      } else { # County Logic
        type <- input$county_instrument_type
        year <- input$county_iv_year
        pct <- input$county_overlap_threshold
        base_folder <- paste("www", "County Maps", scope_folder, sep="/")
        
        if (type == "max_density_overlap") {
          filename <- glue("max_overlap_{pct}pct_{year}.png")
          subfolder <- glue("{pct}pct")
          map_path <- paste(base_folder, "max_overlap", subfolder, filename, sep="/")
          map_title <- glue("Instrument Map: Maximum population density among {year} historical counties with {pct}% minimum overlap with modern county")
        }
      }
      
      map_output(list(src = map_path, title = map_title,
        message = if (level == "State")
          "A map is not available for state instruments."
        else if (population_map)
          "A map is not available for the population instrument (thousands of people)."
        else if (as.integer(if (level == "MSA") input$msa_iv_year else input$county_iv_year) >= 1870)
          "The 1870–1900 instruments are available for estimation; map images for these years are not included."
        else NULL))
    })
  })
  
  # --- 3. Professional Table Formatting Functions ---
  format_coefficient <- function(coef, se, stars = "") {
    coef_str <- format_estimate(coef)
    se_str <- paste0("(", format_estimate(se), ")")
    if (stars != "") coef_str <- paste0(coef_str, stars)
    return(list(coef = coef_str, se = se_str))
  }
  
  get_significance_stars <- function(p_value) {
    if (is.na(p_value)) return("")
    if (p_value < 0.01) return("***")
    if (p_value < 0.05) return("**")
    if (p_value < 0.1) return("*")
    return("")
  }
  
  create_professional_table <- function(models, analysis_type, se_spec = NULL, cluster_data = NULL,
                                        first_stage_models = NULL, data_for_fs = NULL,
                                        controls_vec = NULL, fe_part = NULL, vcov_arg = NULL,
                                        model_clusters = NULL, instrument_type = NULL,
                                        approach = "density", analysis_level = "MSA", overlap_pct = NULL, instrument_form = "levels") {
    if (!length(models)) return("")
    escape <- function(x) as.character(htmltools::htmlEscape(x))
    model_names <- names(models)
    population_instrument <- uses_population_instrument(list(instrument_type = instrument_type, approach = approach))
    all_vars <- unique(unlist(lapply(models, function(model) names(coef(model)))))
    is_ch <- inherits(models[[1]], "ch_model")
    main_var <- if (is_ch) "ch_elasticity" else switch(analysis_type, IV = "fit_RHS", OLS = "RHS", "instrument")
    ordered_vars <- unique(c(intersect(main_var, all_vars), setdiff(all_vars, main_var)))
    cells <- function(values, css = "stats") {
      paste0('<td class="', css, '">', values, '</td>', collapse = "")
    }
    row <- function(label, values, css = "stats", row_class = "") {
      paste0('<tr class="', row_class, '"><td class="row-label ', css, '">',
             escape(label), '</td>', cells(values, css), '</tr>')
    }
    parts <- c('<div class="regression-table-scroll"><table class="regression-table">',
               paste0('<thead><tr><th class="row-label">Variable</th>',
                      paste0('<th>', escape(model_names), '</th>', collapse = ""), '</tr></thead><tbody>'))
    for (variable in ordered_vars) {
      format_value <- function(value) {
        if (population_instrument && variable == "instrument" &&
            is.finite(value) && value != 0 && abs(value) < 0.00005) return(sprintf("%.3e", value))
        format_estimate(value)
      }
      estimates <- vapply(models, function(model) {
        if (!variable %in% names(coef(model))) return("—")
        paste0(format_value(coef(model)[variable]), get_significance_stars(app_model_pvalue(model)[variable]))
      }, character(1))
      uncertainties <- vapply(models, function(model) {
        if (!variable %in% names(coef(model))) return("")
        paste0("(", format_value(app_model_se(model)[variable]), ")")
      }, character(1))
      parts <- c(parts, row(control_label(variable, instrument_type, approach, instrument_form), estimates, "coefficient"), row("", uncertainties, "se"))
    }
    parts <- c(parts, row("Observations", vapply(models, function(model) format(nobs(model), big.mark = ","), character(1)), row_class = "top-border"))
    diagnostic_models <- if (analysis_type == "IV") first_stage_models else if (analysis_type == "First-stage Regression") models else NULL
    if (length(diagnostic_models)) {
      fstats <- vapply(model_names, function(name) {
        value <- instrument_wald_f(diagnostic_models[[name]])
        if (is.finite(value)) sprintf("%.2f", value) else "—"
      }, character(1))
      parts <- c(parts, row("Instrument Wald F", fstats))
    }
    if (is_ch && analysis_type == "IV") {
      values <- vapply(models, function(model) {
        diagnostic <- model$stock_wright
        if (is.null(diagnostic) || !identical(diagnostic$status, "available")) "—" else sprintf("%.4f", diagnostic$statistic)
      }, character(1))
      probabilities <- vapply(models, function(model) {
        diagnostic <- model$stock_wright
        if (is.null(diagnostic) || !identical(diagnostic$status, "available")) return("—")
        if (diagnostic$p_value < 0.0001) "&lt;0.0001" else sprintf("%.4f", diagnostic$p_value)
      }, character(1))
      parts <- c(parts, row("Stock–Wright LM S", values), row("Stock–Wright p-value", probabilities))
      ar_values <- vapply(models, function(model) {
        diagnostic <- model$anderson_rubin
        if (is.null(diagnostic) || diagnostic$status != "available") "—" else sprintf("%.4f", diagnostic$statistic)
      }, character(1))
      ar_probabilities <- vapply(models, function(model) {
        diagnostic <- model$anderson_rubin
        if (is.null(diagnostic) || diagnostic$status != "available") return("—")
        if (diagnostic$p_value < 0.0001) "&lt;0.0001" else sprintf("%.4f", diagnostic$p_value)
      }, character(1))
      parts <- c(parts, row("Anderson–Rubin Wald χ²", ar_values), row("Anderson–Rubin p-value", ar_probabilities))
      relevance <- lapply(models, function(model) model$instrument_relevance)
      local_f <- vapply(relevance, function(diagnostic) {
        if (is.null(diagnostic) || diagnostic$status != "available") "—" else sprintf("%.2f", diagnostic$statistic)
      }, character(1))
      partial_r2 <- vapply(relevance, function(diagnostic) {
        if (is.null(diagnostic) || diagnostic$status != "available") "—" else sprintf("%.4f", diagnostic$partial_r2)
      }, character(1))
      parts <- c(parts, row("Local instrument Wald F", local_f), row("Local instrument partial R²", partial_r2))
    }
    if (!is.null(model_clusters) && any(!is.na(model_clusters))) {
      label <- if (se_spec == "cluster_instrument") {
        instrument_cluster_label(list(instrument_type = instrument_type, analysis_level = analysis_level, approach = approach))
      } else "State clusters"
      parts <- c(parts, row(label, ifelse(is.na(model_clusters), "—", model_clusters)))
    }
    parts <- c(parts, '</tbody></table></div>')
    note <- paste0('Approach: ', approach_name(approach), '. Standard errors in parentheses. *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1.')
    if (identical(approach, "employment"))
      note <- paste0(note, ' The employment measure is the natural log of total employment, without dividing by area.')
    if (identical(instrument_type, "county_population"))
      note <- paste0(note, ' The instrument sums full historical county populations matched to the modern ',
        if (analysis_level == "State") 'state county membership' else if (analysis_level == "MSA") 'MSA county membership' else 'county unit',
        ', without area weights.')
    else if (identical(instrument_type, "area_population"))
      note <- paste0(note, ' The instrument sums mapped historical populations multiplied by the fraction of each historical reporting area inside the modern ',
        if (analysis_level == "State") 'state' else if (analysis_level == "MSA") 'MSA' else 'county',
        '. This assumes uniform population density within each historical reporting area. ',
        escape(area_population_omissions))
    else if (identical(approach, "employment"))
      note <- paste0(note, ' The instrument is the full population of the highest-density historical county meeting the ',
        escape(overlap_pct), '% overlap threshold, without area weights.')
    if (identical(instrument_type, "historical_ch")) note <- paste0(note,
      ' The historical instrument is the population-weighted average of log population density across historical reporting areas assigned geographically to each modern state. It is the theta0 → 1 limit of the normalized historical CH index and is used directly.')
    note <- paste0(note, ' Instrument scale: ', instrument_units(list(instrument_type = instrument_type,
      approach = approach, instrument_form = instrument_form)), '.')
    if (length(diagnostic_models)) {
      note <- paste0(note, ' Instrument Wald F is the squared t statistic for the excluded instrument, using the selected standard errors and the model sample.')
    }
    if (is_ch) note <- paste0(note, ' Ciccone–Hall estimates theta in log[sum(n^theta a^(1−theta))/sum(n)], using county employment n and land area a. The reported elasticity is theta − 1. BEA combined county units are kept together.')
    if (is_ch) note <- paste0(note, ' ', escape(models[[1]]$inference))
    if (is_ch && analysis_type == "IV") {
      note <- paste0(note, ' Stock–Wright S and Anderson–Rubin Wald test zero CH elasticity (theta = 1). Their covariance estimates differ, so influential observations can lead to different results.',
        ' Local instrument Wald F and partial R² describe relevance for the CH derivative at fitted theta; they do not establish strong identification in nonlinear GMM.')
      for (name in model_names) {
        diagnostic <- models[[name]]$stock_wright
        if (!is.null(diagnostic) && !identical(diagnostic$status, "available"))
          note <- paste0(note, ' ', escape(paste0(name, ': ', stock_wright_text(diagnostic))))
        ar <- models[[name]]$anderson_rubin
        if (!is.null(ar) && ar$status != "available")
          note <- paste0(note, ' ', escape(paste0(name, ': ', anderson_rubin_text(ar))))
        relevance <- models[[name]]$instrument_relevance
        if (!is.null(relevance) && relevance$status != "available")
          note <- paste0(note, ' ', escape(paste0(name, ': Local instrument Wald F unavailable: ', relevance$reason)))
      }
    }
    if (is_ch && analysis_level == "State") note <- paste0(note, " State estimates use all-industry GDP per job, with no schooling adjustment or state fixed effects.")
    if (is_ch && any(vapply(models, function(m) m$singleton_n > 0L, logical(1)))) note <- paste0(note, ' As in Stata, CH retains states represented by one MSA; the linear model removes these observations.')
    if (se_spec == "spatial") note <- paste0(note, ' Conley standard errors use a uniform kernel.')
    parts <- c(parts, paste0('<div class="table-notes">', note, '</div>'))
    paste(parts, collapse = "\n")
  }

  # --- 4. Render Professional Outputs ---
  output$results_header <- renderText({
    req(analysis_output())
    if ("error" %in% names(analysis_output())) {
      "Analysis Error"
    } else {
      details <- analysis_output()
      paste(details$analysis_level, "results ·", approach_name(details$approach), "·", model_description(details))
    }
  })
  
  output$results_table <- renderUI({
    req(analysis_output())
    
    if ("error" %in% names(analysis_output())) {
      div(class = "help-text", style = "color: #dc3545; border-left-color: #dc3545;",
          p(analysis_output()$error),
          if (!is.null(analysis_output()$stock_wright)) p(stock_wright_text(analysis_output()$stock_wright)),
          if (!is.null(analysis_output()$anderson_rubin)) p(anderson_rubin_text(analysis_output()$anderson_rubin)))
    } else {
      details <- analysis_output()
      HTML(create_professional_table(
        models = details$models, 
        analysis_type = details$analysis_type,
        se_spec = details$se_spec,
        cluster_data = details$cluster_data,
        first_stage_models = details$first_stage_models,
        data_for_fs = details$data_for_fs,
        controls_vec = details$controls_vec,
        fe_part = details$fe_part,
        vcov_arg = details$vcov_arg,
        model_clusters = details$model_clusters,
        instrument_type = details$instrument_type,
        approach = details$approach, analysis_level = details$analysis_level, overlap_pct = details$overlap_pct, instrument_form = details$instrument_form
      ))
    }
  })
  
  output$analysis_details <- renderUI({
    req(analysis_output())
    
    if ("error" %in% names(analysis_output())) {
      return(NULL)
    }
    
    details <- analysis_output()
    
    # Helper function to format sector names
    get_sector_name <- function(sector_num) {
      sector_names <- c("1" = "All", "2" = "Private", "3" = "Manufacturing", 
                        "4" = "Private non-farm", "5" = "Private non-farm/mining")
      return(sector_names[as.character(sector_num)])
    }
    
    # Helper function to format schooling adjustment
    get_schooling_adj_name <- function(adj_num) {
      adj_names <- c("0" = "None", "1" = "Same Return", "2" = "Specific Return", "3" = "Run Both")
      return(adj_names[as.character(adj_num)])
    }
    
    # Build comprehensive analysis details
    se_description <- details$se_spec
    if (details$se_spec == "cluster_instrument" && !is.null(details$cluster_data$clusterID)) {
      se_description <- if (is_aggregated_population(details)) paste("Cluster on", details$analysis_level) else if (details$instrument_type %in% c("overlap", "max_density_overlap")) "Cluster on historical county" else "Cluster on historical instrument"
    } else if (details$se_spec == "cluster_state" && !is.null(details$cluster_data$state_id)) {
      se_description <- "Cluster on modern state"
    } else if (details$se_spec == "spatial") {
      spatial_cutoff_formatted <- sprintf("%.0f", as.numeric(details$spatial_cutoff))
      se_description <- paste0("Spatial (Conley). ", spatial_cutoff_formatted, " km cutoff. Uniform kernel")
    } else if (details$se_spec == "robust") {
      se_description <- "Robust (Heteroskedasticity-robust)"
    }
    
    # Create detailed information sections
    detail_sections <- list()
    
    # Core Analysis Settings
    detail_sections$core <- paste0(
      '<strong>Analysis Level:</strong> ', details$analysis_level, '<br>',
      '<strong>Modern Year:</strong> ', details$year_modern, '<br>',
      '<strong>Analysis Method:</strong> ', model_description(details), '<br>',
      '<strong>Approach:</strong> ', approach_name(details$approach), '<br>',
      '<strong>Employment measure:</strong> ', if (details$density_measure == 'CH') 'Ciccone–Hall index' else control_label('RHS', details$instrument_type, details$approach), '<br>',
      '<strong>Sector:</strong> ', get_sector_name(details$sectors), '<br>'
    )
    
    # Sample and Instrument Settings  
    detail_sections$sample <- paste0(
      '<strong> Year of the Historical Territory:</strong> ', details$sample_year, '<br>',
      '<strong>', if (uses_population_instrument(details)) 'Year of the Historical Population (Historical Census Year):' else 'Year of the Historical Population Density (Historical Census Year):', '</strong> ', details$iv_year, '<br>',
      '<strong>Match of Historical to Modern Geographic Units:</strong> ', instrument_name(details$instrument_type, details$approach),
      if (!is.null(details$overlap_pct)) paste0(' (', details$overlap_pct, '% overlap)') else '', '<br>',
      '<strong>Historical territory:</strong> ', details$sample_scope, '<br>',
      paste0('<strong>Instrument scale:</strong> ', instrument_units(details), '<br>')
    )
    
    # Fixed Effects and Controls
    detail_sections$controls <- paste0(
      '<strong>State Fixed Effects:</strong> ', details$fe_type, if (identical(details$fe_type, 'historical')) paste0(' (', details$sample_year, ')') else '', '<br>',
      '<strong>Control variables:</strong> ', if (length(details$controls_vec)) paste(vapply(details$controls_vec, control_label, character(1)), collapse = ', ') else 'None', '<br>'
    )
    
    # Adjustments
    adjustment_text <- paste0('<strong> Modern Adjustment for Human capital:</strong> ', get_schooling_adj_name(details$schooling_adj))
    if (details$apply_college_adj && !is.null(details$college_coeff)) {
      college_coeff_formatted <- sprintf("%.2f", as.numeric(details$college_coeff))
      adjustment_text <- paste0(adjustment_text, '<br><strong>College Share Adjustment:</strong> Yes (coefficient = ', college_coeff_formatted, ')' )
    } else {
      adjustment_text <- paste0(adjustment_text, '<br><strong>College Share Adjustment:</strong> No')
    }
    if (details$mining_filter_active && !is.null(details$mining_threshold)) {
      mining_threshold_formatted <- sprintf("%.3f", as.numeric(details$mining_threshold))
      adjustment_text <- paste0(adjustment_text, '<br><strong>Maximum mining share:</strong> Yes (max share = ', mining_threshold_formatted, ')')
    } else {
      adjustment_text <- paste0(adjustment_text, '<br><strong>Maximum mining share:</strong> No')
    }
    if (length(details$schooling_reference) && any(is.finite(details$schooling_reference))) {
      reference <- details$schooling_reference[is.finite(details$schooling_reference)]
      adjustment_text <- paste0(adjustment_text, '<br><strong>Reference schooling (years):</strong> ',
        paste(names(reference), sprintf('%.4f', reference), sep = ': ', collapse = '; '))
    }
    detail_sections$adjustments <- paste0(adjustment_text, '<br>')
    
    # Level-specific settings
    if (details$analysis_level == "County" && !is.null(details$county_msa_restriction)) {
      detail_sections$level_specific <- paste0('<strong>Restrict to MSA Counties:</strong> ', 
                                               if(details$county_msa_restriction) "Yes" else "No", '<br>')
    }
    
    # Standard Errors and Sample Size
    detail_sections$technical <- paste0(
      '<strong>Standard Errors:</strong> ', se_description, '<br>',
      '<strong>Observations used:</strong> ', if (length(unique(details$model_n)) == 1L) format(details$model_n[[1]], big.mark = ',') else paste(names(details$model_n), format(details$model_n, big.mark = ','), sep = ': ', collapse = '; '), '<br>',
      '<strong>Eligible before control and model exclusions:</strong> ', format(details$eligible_n, big.mark = ','), '<br>',
      '<strong>Missing selected geographic controls:</strong> ', format(details$geo_missing_n, big.mark = ','),
      if (identical(details$density_measure, 'CH')) paste0('<br><strong>Incomplete county inputs:</strong> ', details$ch_missing_n) else ''
    )
    
    # Combine all sections
    all_details <- paste(unlist(detail_sections), collapse = "")
    
    HTML(paste0(
      '<div class="analysis-details">',
      '<h5> Regression Analysis Configuration</h5>',
      all_details,
      '</div>'
    ))
  })
  
  output$map_header <- renderText({
    req(map_output())
    map_output()$title
  })
  
  output$map_ui <- renderUI({
    info <- map_output()
    req(info)
    
    if (!is.null(info$src) && file.exists(info$src)) {
      imageOutput("instrument_map_render", width = paste0(input$map_size, "%"), height = "auto")
    } else {
      div(class = "help-text",
          if (!is.null(info$message)) info$message else "Map image not found. Please check your selections and verify your file structure in the 'www' folder.")
    }
  })
  
  output$instrument_map_render <- renderImage({
    info <- map_output()
    req(info$src, file.exists(info$src))
    
    list(
      src = info$src,
      contentType = "image/png",
      alt = "Instrument Map"
    )
  }, deleteFile = FALSE)

  valid_result <- reactive({
    details <- analysis_output()
    req(details, is.null(details$error), length(details$models))
    details
  })

  observeEvent(input$toggle_configuration, {
    updateActionButton(session, "toggle_configuration", label =
      if (input$toggle_configuration %% 2L == 1L) "Hide regression configuration" else "Show regression configuration")
  }, ignoreInit = TRUE)

  output$effect_plot <- renderPlot({ result_plot(valid_result()) }, res = 120)

  output$sample_note <- renderUI({
    details <- analysis_output()
    if (is.null(details) || !is.null(details$error)) return(NULL)
    counts <- vapply(details$models, stats::nobs, numeric(1))
    eligible <- if (is.null(details$eligible_n)) details$n_obs else details$eligible_n
    missing_geo <- if (is.null(details$geo_missing_n)) 0L else details$geo_missing_n
    selected <- details$new_controls
    omitted <- unique(unlist(lapply(details$models, function(m) m$collin.var)))
    notices <- lapply(sample_exclusion_notes(details), p)
    if (isTRUE(details$ch_missing_n > 0L)) notices <- c(notices, list(p(paste(details$ch_missing_n,
      "eligible observations have incomplete county employment or land area for the Ciccone–Hall model."))))
    if (missing_geo > 0) notices <- c(notices, list(p(paste(format(missing_geo, big.mark = ","),
      "eligible observations have missing values in the selected geographic controls. Missing values are excluded, never set to zero."))))
    if (length(unique(counts)) > 1) notices <- c(notices, list(p("The models use different sample sizes because their required values differ.")))
    if (length(omitted)) notices <- c(notices, list(p(paste("Omitted because of collinearity:", paste(vapply(omitted, control_label, character(1)), collapse = "; "), "."))))
    if (length(details$model_warnings)) notices <- c(notices, lapply(details$model_warnings, function(warning) p(paste("Estimation note:", warning))))
    if ("portage_access10" %in% selected) notices <- c(notices, list(p("Portage access uses approximate fall-line/river candidates, not a verified historical portage inventory.")))
    if (!length(notices)) return(NULL)
    div(class = "sample-note", notices)
  })

  output$data_notes_heading <- renderText({
    if (identical(analysis_output()$analysis_level, "State")) "State data and method" else "Geographic data and sources"
  })

  output$data_notes <- renderUI({
    if (identical(analysis_output()$analysis_level, "State")) return(div(class = "data-notes", h4("State data"), p(help_state_ch)))
    div(class = "data-notes",
      h4("Geographic measures"),
      p("Each checkbox adds its component variables separately. For example, the climate box adds three controls; it does not create a single climate index."),
      tags$dl(lapply(names(geographic_notes), function(measure)
        tagList(tags$dt(measure), tags$dd(geographic_notes[[measure]])))),
      p("Historical river and canal dates come from the Atack transport data. All geographic controls, including the original water and railroad indicators, are grouped in the configuration's Geographic Controls section."))
  })

  output$download_coefficients <- downloadHandler(
    filename = function() {d <- valid_result(); paste0("agglomeration-", tolower(d$analysis_level), "-", d$year_modern, "-coefficients.csv")},
    content = function(file) {
      details <- valid_result()
      table <- model_coefficients(details)
      table$controls <- paste(details$controls_vec, collapse = "; ")
      if (!identical(details$analysis_level, "State")) table$water_year <- details$water_year
      table$standard_errors <- if (details$se_spec == "cluster_instrument") instrument_cluster_label(details) else details$se_spec
      table$fixed_effects <- details$fe_type
      table$sample_year <- details$sample_year
      table$instrument_year <- details$iv_year
      table$instrument_construction <- instrument_name(details$instrument_type, details$approach)
      if (length(details$model_warnings)) table$estimation_notes <- paste(details$model_warnings, collapse = "; ")
      table$instrument_units <- instrument_units(details)
      table$instrument_form <- if (is.null(details$instrument_form)) "levels" else details$instrument_form
      table$schooling_adjustment <- schooling_adjustment_name(details$schooling_adj)
      if (identical(details$fe_type, "historical")) table$fixed_effect_year <- details$sample_year
      if (length(sample_exclusion_notes(details))) table$sample_exclusions <- paste(sample_exclusion_notes(details), collapse = "; ")
      if (!is.null(details$overlap_pct)) table$overlap_threshold_pct <- details$overlap_pct
      utils::write.csv(table, file, row.names = FALSE, na = "")
    })

  output$download_plot <- downloadHandler(
    filename = function() {d <- valid_result(); paste0("agglomeration-", tolower(d$analysis_level), "-", d$year_modern, ".png")},
    content = function(file) {ggplot2::ggsave(file, result_plot(valid_result()), device = "png", width = 10, height = 5.5, dpi = 300, bg = "white")})

  output$download_results <- downloadHandler(
    filename = function() {d <- valid_result(); paste0("agglomeration-", tolower(d$analysis_level), "-", d$year_modern, "-results.html")},
    content = function(file) {
      details <- valid_result()
      chart_file <- tempfile(fileext = ".png")
      on.exit(unlink(chart_file))
      ggplot2::ggsave(chart_file, result_plot(details), device = "png", width = 9, height = 4.7, dpi = 180, bg = "white")
      chart_data <- base64enc::dataURI(file = chart_file, mime = "image/png")
      table <- create_professional_table(details$models, details$analysis_type, details$se_spec,
        details$cluster_data, details$first_stage_models, details$data_for_fs, details$controls_vec, details$fe_part, details$vcov_arg,
        model_clusters = details$model_clusters, instrument_type = details$instrument_type,
        approach = details$approach, analysis_level = details$analysis_level, overlap_pct = details$overlap_pct, instrument_form = details$instrument_form)
      esc <- htmltools::htmlEscape
      specifications <- c(Geography = details$analysis_level, Approach = approach_name(details$approach), Method = model_description(details), `Modern year` = details$year_modern,
        `Employment measure` = if (details$density_measure == 'CH') 'Ciccone–Hall index' else control_label('RHS', details$instrument_type, details$approach),
        Sector = c("All", "Private", "Manufacturing", "Private non-farm", "Private non-farm/mining")[details$sectors],
        `Historical sample year` = details$sample_year, `Instrument year` = details$iv_year,
        `Historical territory` = details$sample_scope, `Instrument construction` = instrument_name(details$instrument_type, details$approach),
        `Instrument units` = instrument_units(details),
        `Overlap threshold (%)` = details$overlap_pct, `State fixed effects` = details$fe_type,
        `Standard errors` = if (details$se_spec == "cluster_instrument") instrument_cluster_label(details) else details$se_spec, `Schooling adjustment` = schooling_adjustment_name(details$schooling_adj),
        `Reference schooling (years)` = if (any(is.finite(details$schooling_reference))) paste(names(details$schooling_reference), sprintf("%.4f", details$schooling_reference), sep = ": ", collapse = "; ") else NULL,
        `Historical fixed-effect year` = if (identical(details$fe_type, "historical")) details$sample_year else NULL,
        `College adjustment` = if (details$apply_college_adj) details$college_coeff else "None",
        `Mining threshold` = if (details$mining_filter_active) details$mining_threshold else "None",
        `Spatial cutoff (km)` = if (details$se_spec == "spatial") details$spatial_cutoff else "Not used",
        `MSA counties only` = if (is.null(details$county_msa_restriction)) "Not applicable" else as.character(details$county_msa_restriction),
        `Water-access year` = if (!identical(details$analysis_level, "State")) details$water_year else NULL,
        Controls = if (length(details$controls_vec)) paste(vapply(details$controls_vec, control_label, character(1)), collapse = "; ") else "None")
      spec_html <- paste0("<dt>", esc(names(specifications)), "</dt><dd>", esc(as.character(specifications)), "</dd>", collapse = "")
      export_notes <- c(sample_exclusion_notes(details), details$model_warnings)
      warning_html <- if (length(export_notes)) paste0('<h2>Estimation notes</h2><p>',
        paste(esc(export_notes), collapse = '</p><p>'), '</p>') else ''
      geo_html <- if (identical(details$analysis_level, "State"))
        paste0('<h2>State data</h2><p>', paste(esc(strsplit(help_state_ch, "\n", fixed = TRUE)[[1]]), collapse = '</p><p>'), '</p>')
      else paste0('<h2>Geographic controls</h2><dl>',
        paste0('<dt>', esc(names(geographic_notes)), '</dt><dd>', esc(unname(geographic_notes)), '</dd>', collapse = ''), '</dl>')
      html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>Agglomeration results</title>',
        '<style>body{font-family:Arial,sans-serif;color:#000000;max-width:1100px;margin:40px auto;padding:0 24px;line-height:1.5}h1{font-size:32px}img{max-width:100%}table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:7px 10px;text-align:right;border-bottom:1px solid #dddddd}.row-label{text-align:left}th{border-top:2px solid #000000}dt{font-weight:bold;margin-top:10px}dd{margin-left:0}.table-notes{font-size:12px;margin-top:15px}@media print{body{margin:0}}</style><body>',
        '<h1>Agglomeration effects in the United States</h1><p>', esc(paste(details$analysis_level, details$year_modern, model_description(details), sep = ' · ')),
        '</p><img alt="Coefficient estimates and 95% confidence intervals" src="', chart_data, '">', table, warning_html,
        '<h2>Specification</h2><dl>', spec_html, '</dl>', geo_html, '</body></html>')
      writeLines(html, file, useBytes = TRUE)
    })

}

shinyApp(ui = ui, server = server)
