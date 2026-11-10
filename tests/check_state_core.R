# Known CH coefficients on fixed historical-region memberships, without data files.
local({
  core <- new.env(parent = globalenv())
  functions <- c("transform_historical_instrument", "historical_census_years", "ch_index_terms", "stock_wright_lm_s",
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
  for (year in core$historical_census_years) for (prefix in c("GG","AW")) {
    original[[paste0(prefix,"pop_",year)]] <- (i+5)^2*(year-1700)
    original[[paste0(prefix,"valid_",year)]] <- 1L
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
  changed<-panel();changed$AWpop_1790[1]<-changed$AWpop_1900[1]<-0
  changed$AWvalid_1790[2]<-0L;changed$AWpop_1900[3]<-NA_real_;changed$ch_complete[4]<-0L
  result<-core$state_ch_result(modifyList(settings,list(analysis_type="OLS")),changed)
  stopifnot(result$population_missing_n==2L,result$ch_missing_n==1L,result$n_obs==21L,
    result$data_for_fs$instrument[1]==0,!regions[4] %in% result$data_for_fs$region_id[result$models[[1]]$rows])
  expect_error<-function(chosen=settings,d=panel(),message) {
    error<-tryCatch({core$state_ch_result(chosen,d);NULL},error=conditionMessage)
    stopifnot(is.character(error),grepl(message,error,fixed=TRUE))
  }
  expect_error(modifyList(settings,list(state_sample_year="1910")),message="Choose a historical census year")
  expect_error(modifyList(settings,list(sample_scope="unknown")),message="Choose a historical territory scope")
  expect_error(d=panel()[FALSE,],message="Historical regions are unavailable")
  changed<-panel();changed$ch_complete<-0L
  expect_error(d=changed,message="No observations have complete county components")
  changed<-panel();changed$AWpop_1900<-0
  expect_error(d=changed,message="historical instrument has no variation")
  cat("State core checks passed:",checks,"known-coefficient historical-region specifications, all modern years, fixed membership, missing inputs and failures.\n")
})
