# Known CH coefficients on fixed historical-region memberships, without data files.
local({
  core <- new.env(parent = globalenv())
  functions <- c("transform_historical_instrument", "historical_census_years", "historical_ch_coverage", "ch_index_terms", "stock_wright_lm_s",
    "auxiliary_instrument_wald", "ch_instrument_relevance", "anderson_rubin_wald", "fit_ch_model", "state_ch_result",
    "app_model_rows", "coef.ch_model", "vcov.ch_model", "nobs.ch_model", "residuals.ch_model", "fitted.ch_model", "confint.ch_model")
  for (expr in parse("app.R", encoding = "UTF-8")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% functions) eval(expr, core)
  }
  source("historical_state_regions.R", local = core, encoding = "UTF-8")
  for (generic in c("coef", "vcov", "nobs", "residuals", "fitted", "confint"))
    registerS3method(generic, "ch_model", core[[paste0(generic, ".ch_model")]], envir = asNamespace("stats"))
  i <- seq_len(24)
  regions <- sprintf("historical_group_%02d", i)
  n <- cbind(200+i^2*18,160+i^2*11,100+i^2*6)
  a <- cbind(35+i%%7,60+i%%5,25+i%%3)
  theta <- 1.12
  original <- data.frame(year=2010L,sample_year=1790L,scope="states_and_territories",
    region_id=regions,region_name=paste("Historical group",i),ch_complete=1L,
    LHS=9+log(rowSums(n^theta*a^(1-theta))/rowSums(n)),RHS=log(rowSums(n)/rowSums(a)))
  parts <- data.frame(region_id=rep(regions,each=3),geofips=seq_len(72),
    modern_statefips=rep(c(1L,2L,3L),24),ch_slot=rep(1:3,24),nc=as.vector(t(n)),ac=as.vector(t(a)))
  for (year in core$historical_census_years) {
    for (field in c("AWobs", "GGmatchedpop")) original[[paste0(field,"_",year)]] <- (i+5)^2*(year-1700)
    for (field in c("HCHcover", "HCHmappedcover")) original[[paste0(field,"_",year)]] <- 1
    for (field in c("AWmiss","AWunallocated","AWgap","HCHomitted_pop","GGmiss","GGgap","GGunallocated",
        "GGexcludedn","GGexcludedpop","GGunmatchedawpop","GGunlocatedpop")) original[[paste0(field,"_",year)]] <- 0
  }
  # Fixtures specify region membership independently of every instrument.
  selected_ids <- function(year,scope) if(scope=="states_only") regions[1:13] else regions
  core$get_historical_state_membership <- function(sample_year,scope) {
    p<-parts[parts$region_id %in% selected_ids(sample_year,scope),]
    p$region_name<-original$region_name[match(p$region_id,original$region_id)]
    p
  }
  core$historical_state_components <- function(year,sample_year,scope,membership) membership
  core$attach_historical_region_instruments <- function(data,...) data
  panel <- function(year=2010L,boundary=1790L,scope="states_territories") {
    d<-original[original$region_id %in% selected_ids(boundary,scope),]
    d$year<-as.integer(year);d$sample_year<-as.integer(boundary);d$scope<-core$historical_region_scope(scope);d
  }
  settings<-list(year_modern="2010",state_sample_year="1790",state_iv_year="1900",
    analysis_type="IV",state_instrument_type="area_population",sample_scope="states_territories")
  checks<-0L
  for(year in core$historical_census_years) for(method in c("IV","OLS"))
    for(construction in c("county_population","area_population")) for(scope in c("states_only","states_territories")) {
      chosen<-modifyList(settings,list(state_sample_year=as.character(year),state_iv_year=as.character(year),
        analysis_type=method,state_instrument_type=construction,sample_scope=scope))
      result<-core$state_ch_result(chosen,panel(boundary=year,scope=scope))
      m<-result$models[[1]]
      expected<-selected_ids(year,scope)
      stopifnot(abs(coef(m)[["ch_elasticity"]]-.12)<1e-6,abs(coef(m)[["(Intercept)"]]-9)<1e-6,
        identical(result$data_for_fs$region_id,expected),nobs(m)==length(expected),
        result$historical_region_eligible_n==length(expected),!"statefips" %in% names(result$data_for_fs))
      checks<-checks+1L
    }
  early<-core$state_ch_result(settings,panel())
  late<-core$state_ch_result(modifyList(settings,list(state_iv_year="1790")),panel())
  stopifnot(identical(early$data_for_fs$region_id,late$data_for_fs$region_id),
    identical(early$data_for_fs$nc1,late$data_for_fs$nc1),!identical(early$data_for_fs$instrument,late$data_for_fs$instrument))
  for(year in 2001:2022) {
    result<-core$state_ch_result(modifyList(settings,list(year_modern=as.character(year))),panel(year=year))
    stopifnot(result$year_modern==year,result$n_obs==24L)
  }
  changed<-panel();changed$AWobs_1790[1]<-changed$AWobs_1900[1]<-0
  changed$HCHcover_1790[2]<-.79;changed$AWobs_1900[3]<-NA_real_;changed$ch_complete[4]<-0L
  changed$AWmiss_1900[5]<-1;changed$AWgap_1900[5]<-1;changed$AWunallocated_1900[5]<-2
  result<-core$state_ch_result(modifyList(settings,list(analysis_type="OLS")),changed)
  stopifnot(result$population_missing_n==3L,result$ch_missing_n==1L,result$n_obs==20L,
    regions[5] %in% result$data_for_fs$region_id[result$models[[1]]$rows],
    !any(regions[1:4] %in% result$data_for_fs$region_id[result$models[[1]]$rows]))
  # With no area cutoff a positive identity match can survive zero mapped area.
  # A missing matched population still excludes the region under every cutoff.
  changed<-panel();changed$HCHcover_1790[1]<-0;changed$GGmatchedpop_1900[2]<-0
  matched<-modifyList(settings,list(analysis_type="OLS",state_instrument_type="county_population"))
  filtered<-core$state_ch_result(matched,changed)
  unfiltered<-core$state_ch_result(modifyList(matched,list(state_hch_coverage="none")),changed)
  stopifnot(filtered$n_obs==22L,unfiltered$n_obs==23L,
    regions[1] %in% unfiltered$data_for_fs$region_id,!regions[2] %in% unfiltered$data_for_fs$region_id)
  expect_error<-function(chosen=settings,d=panel(),message) {
    error<-tryCatch({core$state_ch_result(chosen,d);NULL},error=conditionMessage)
    stopifnot(is.character(error),grepl(message,error,fixed=TRUE))
  }
  expect_error(modifyList(settings,list(state_sample_year="1910")),message="Choose a historical census year")
  expect_error(modifyList(settings,list(sample_scope="unknown")),message="Choose a historical territory scope")
  expect_error(d=panel()[FALSE,],message="Historical regions are unavailable")
  changed<-panel();changed$ch_complete<-0L
  expect_error(d=changed,message="No observations have complete county components")
  changed<-panel();changed$HCHcover_1790<-0
  unavailable<-tryCatch(core$state_ch_result(settings,changed),error=identity)
  stopifnot(inherits(unavailable,"error"),unavailable$state_diagnostics$historical_inputs_usable_n==0L,
    nrow(unavailable$state_diagnostics$historical_group_inputs)==48L,
    all(!unavailable$state_diagnostics$historical_group_inputs$assessed_for_fit),
    all(is.na(unavailable$state_diagnostics$historical_group_inputs$in_estimation)))
  changed<-panel();changed$AWobs_1900<-1
  expect_error(d=changed,message="historical instrument has no variation")
  cat("State core checks passed:",checks,"known-coefficient historical-region specifications, all modern years, fixed membership, missing inputs and failures.\n")
})
