# Integration checks use annual cached inputs and keep schooling at the MSA outcome.
repo <- normalizePath(Sys.getenv("APP_REPO", unset="."), mustWork=TRUE)
work <- Sys.getenv("APP_TEST_OUTPUT", unset=tempdir())
dir.create(work,recursive=TRUE,showWarnings=FALSE)
setwd(repo)
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "English_United States.utf8")
options(encoding="UTF-8")
app <- new.env(parent=globalenv())
for(expr in parse("app.R",encoding="UTF-8")) {
  if(is.call(expr) && as.character(expr[[1]]) %in% c("tryCatch","shinyApp")) next
  eval(expr,app)
}
for(generic in c("coef","vcov","nobs","residuals","fitted","confint"))
  registerS3method(generic,"ch_model",app[[paste0(generic,".ch_model")]],envir=asNamespace("stats"))
loader <- app$make_app_data_loader()
app$get_app_year <- loader$get_year
app$state_data <- loader$get_state()
fixest::setFixest_notes(FALSE)
settings <- list(analysis_level="State",year_modern="2010",analysis_type="IV",
  state_ch_mode="county_adjusted",state_sample_year="1900",state_iv_year="1900",
  state_instrument_type="area_population",state_county_instrument="area_population",
  sample_scope="states_territories",instrument_form="levels",state_county_area=TRUE,
  state_geo_controls=c("terrain","crop","climate"),
  state_water_controls=c("shoreline","historical_access","distance","portage"),
  state_transport_controls=c("water_1820","railroads_1840"),state_water_year="1820",
  approach="density",msa_density_measure="average",msa_instrument_type="overlap",
  county_instrument_type="max_density_overlap",use_fe=FALSE,show_map=FALSE)
records <- list()
check <- function(result) {
  if(!is.null(result$error)) stop(result$error)
  for(name in names(result$models)) {
    m <- result$models[[name]]
    stopifnot(inherits(m,"ch_model"),all(is.finite(coef(m))),all(is.finite(vcov(m))),
      m$county_calibration$calibrated,m$adjustment_mode=="geography",m$county_calibration$county_fe,
      is.null(m$anderson_rubin),!any(c("schooling","college") %in% names(m$county_calibration$coefficients)),
      isTRUE(all.equal(unname(m$full_covariance),unname(crossprod(m$state_influence)),tolerance=1e-10)))
    if(result$analysis_type=="IV") stopifnot(m$stock_wright$status=="available",m$instrument_relevance$status=="available",abs(m$iv_moment)<1e-7)
    records[[length(records)+1L]] <<- data.frame(level=result$analysis_level,year=result$year_modern,
      model=name,method=result$analysis_type,n=nobs(m),county_n=m$county_calibration$nobs,
      elasticity=coef(m)[["ch_elasticity"]],se=sqrt(vcov(m)[1,1]))
  }
}
standard <- modifyList(settings,list(state_ch_mode="standard"))
before <- app$state_ch_result(standard,app$state_data)$models[[1]]
baseline <- app$state_ch_result(settings,app$state_data)
check(baseline)
stopifnot(abs(coef(baseline$models[[1]])[["ch_elasticity"]]-.2675751)<1e-5,
  abs(sqrt(vcov(baseline$models[[1]])[1,1])-.08590708)<1e-5,
  baseline$models[[1]]$county_calibration$nobs==2752L)
for(method in c("IV","OLS")) check(app$state_ch_result(modifyList(settings,list(analysis_type=method,
  state_instrument_type="county_population",state_county_instrument="county_population")),app$state_data))
check(app$state_ch_result(modifyList(settings,list(state_instrument_type="historical_ch")),app$state_data))
check(app$state_ch_result(modifyList(settings,list(sample_scope="states_only")),app$state_data))
after <- app$state_ch_result(standard,app$state_data)$models[[1]]
stopifnot(identical(before$coefficients,after$coefficients),identical(before$covariance,after$covariance))
original_static <- app$get_state_adjustment_inputs
static <- as.data.frame(original_static())
static$avg_schooling_10_14 <- static$college_share <- NA_real_
app$get_state_adjustment_inputs <- function(...) static
no_hc <- app$state_ch_result(settings,app$state_data)
stopifnot(identical(baseline$models[[1]]$coefficients,no_hc$models[[1]]$coefficients))
selected <- baseline$models[[1]]$state_ids[1]
static$rugged_mean[which(static$statefips==as.numeric(selected))[1]] <- NA_real_
missing <- app$state_ch_result(settings,app$state_data)
stopifnot(nobs(missing$models[[1]])==nobs(baseline$models[[1]])-1,!selected %in% missing$models[[1]]$county_calibration$states)
app$get_state_adjustment_inputs <- original_static
ui <- htmltools::renderTags(app$ui)$html
stopifnot(grepl('id="state_ch_mode"',ui,fixed=TRUE),grepl('id="msa_ch_mode"',ui,fixed=TRUE),
  !grepl('id="state_adjustment_type"',ui,fixed=TRUE),!grepl('value="human_capital"',ui,fixed=TRUE))
msa <- list(analysis_level="MSA",msa_density_measure="CH",msa_ch_mode="county_adjusted",msa_sectors="1",
  msa_sample_year="1900",msa_iv_year="1900",msa_instrument_type="area_population",msa_overlap_pct="5",
  msa_controls=c("water_1820","railroads_1840"),msa_schooling_adj="3",msa_apply_college_adj=TRUE,
  msa_college_coeff=".75",msa_se_spec="cluster_instrument",msa_mining_filter_active=FALSE,
  msa_mining_threshold=".01",msa_spatial_cutoff="100",geo_controls=character(),water_controls=character(),
  water_year="1790",fe_type="modern",use_fe=TRUE,msa_county_instrument="area_population",msa_county_area=TRUE,
  msa_geo_controls=settings$state_geo_controls,msa_water_controls=settings$state_water_controls,
  msa_transport_controls=settings$state_transport_controls,msa_water_year="1820")
shiny::testServer(app$server, {
  do.call(session$setInputs,c(modifyList(settings,msa),list(run_analysis=1L)))
  session$flushReact()
  result <- isolate(analysis_output())
  check(result)
  stopifnot(length(result$models)==2L,length(result$controls_vec)==0L,result$schooling_adj==3L,
    result$apply_college_adj,result$fe_part=="state_id",all(vapply(result$models,nobs,integer(1))>350L))
  html <- output$results_table$html
  writeLines(html,file.path(work,"county_geography_table.html"),useBytes=TRUE)
  stopifnot(grepl("Wright LM S",html,fixed=TRUE),grepl("Instrument Wald F",html,fixed=TRUE),grepl("No county schooling",html,fixed=TRUE))
  csv <- read.csv(output$download_coefficients)
  stopifnot(all(csv$ch_specification=="county_adjusted"),all(csv$county_water_year==1820),!"schooling_enters_ch_power" %in% names(csv))
  calibration <- read.csv(output$download_county_calibration)
  stopifnot(setequal(unique(calibration$model),names(result$models)),all(calibration$county_water_year==1820),
    !any(calibration$enters_aggregate_index[calibration$term=="county_elasticity"]))
  saved <- paste(readLines(output$download_results,warn=FALSE),collapse="\n")
  stopifnot(grepl("County geography-adjusted CH",saved,fixed=TRUE),!grepl("<h2>State data</h2>",saved,fixed=TRUE))
  session$setInputs(msa_schooling_adj="0",msa_apply_college_adj=FALSE,use_fe=FALSE,run_analysis=2L)
  session$flushReact()
  raw <- isolate(analysis_output()); check(raw)
  stopifnot(abs(coef(raw$models[[1]])[["ch_elasticity"]]-.184863)<1e-5)
  session$setInputs(msa_se_spec="cluster_state",run_analysis=3L)
  session$flushReact()
  linked <- isolate(analysis_output()); check(linked)
  stopifnot(abs(coef(linked$models[[1]])[["ch_elasticity"]]-coef(raw$models[[1]])[["ch_elasticity"]])<1e-8,linked$models[[1]]$clusters<48L)
  session$setInputs(analysis_type="OLS",run_analysis=4L)
  session$flushReact()
  check(isolate(analysis_output()))
})
write.csv(do.call(rbind,records),file.path(work,"state_adjustment_checks.csv"),row.names=FALSE)
cat("County geography-adjusted CH integration checks passed.\n")
