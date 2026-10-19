# Integration checks use annual cached inputs for all county-adjusted CH modes.
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
identity_records <- list()
parity_records <- list()
record_parity <- function(result, label) {
  m <- result$models[[1]]
  parity_records[[length(parity_records)+1L]] <<- data.frame(case=label,n=nobs(m),
    county_n=m$county_calibration$nobs,b=coef(m)[["ch_elasticity"]],
    se=sqrt(vcov(m)[1,1]),S=m$stock_wright$statistic,S_p=m$stock_wright$p_value,
    county_b=if(isTRUE(m$county_calibration$calibrated)) m$county_calibration$coefficients[["county_elasticity"]] else NA_real_,
    beta=m$adjustment$beta_school,kappa=m$adjustment$kappa_college)
}
check_county_iv_identity <- function(prepared, model, label) {
  calibration <- model$county_calibration
  cc <- prepared$county[match(calibration$county_ids, as.character(prepared$county$county_id)), , drop=FALSE]
  stopifnot(nrow(cc)==calibration$nobs, !anyNA(cc$county_id))
  settings <- prepared$settings
  hc <- app$sca_human_capital(cc,settings$mode,settings$beta_school,settings$kappa_college,settings$schooling_reference)
  cc$y_adjusted <- cc$y-hc$h-hc$college_effect
  controls <- c(prepared$geo_vars,settings$county_controls)
  formula <- as.formula(paste("y_adjusted ~",paste(controls,collapse=" + "),"| state_id | x ~ instrument"))
  ordinary <- fixest::feols(formula,cc,vcov=~joint_cluster,fixef.rm="none",
    ssc=fixest::ssc(K.adj=FALSE,G.adj=FALSE))
  calibration_terms <- c("county_elasticity",controls)
  ordinary_terms <- c("fit_x",controls)
  b_gap <- max(abs(calibration$coefficients[calibration_terms]-coef(ordinary)[ordinary_terms]))
  v_gap <- max(abs(calibration$covariance[calibration_terms,calibration_terms]-vcov(ordinary)[ordinary_terms,ordinary_terms]))
  stopifnot(nobs(ordinary)==calibration$nobs,b_gap<1e-7,v_gap<1e-7)
  identity_records[[length(identity_records)+1L]] <<- data.frame(case=label,n=calibration$nobs,
    log_area="ln_area" %in% settings$county_controls,coefficient_gap=b_gap,CR0_covariance_gap=v_gap)
}
check <- function(result) {
  if(!is.null(result$error)) stop(result$error)
  for(name in names(result$models)) {
    m <- result$models[[name]]
    stopifnot(inherits(m,"ch_model"),all(is.finite(coef(m))),all(is.finite(vcov(m))),
      m$adjustment_mode %in% c("geography","both","human_capital"),
      is.null(m$anderson_rubin),!any(c("schooling","college") %in% names(m$county_calibration$coefficients)),
      isTRUE(all.equal(unname(m$full_covariance),unname(crossprod(m$state_influence)),tolerance=1e-10)))
    if(m$adjustment_mode=="human_capital") {
      stopifnot(!m$county_calibration$calibrated,identical(m$county_calibration$nobs,0L),
        !m$county_calibration$county_fe,!length(m$county_calibration$coefficients),
        !length(m$county_calibration$gamma),is.null(m$county_calibration$instrument_relevance),
        !length(m$adjustment$geo_vars),!length(m$adjustment$county_controls_not_aggregated),
        ncol(m$index_gamma_derivative)==0L,all(m$calibration_state_influence==0),
        m$adjustment$beta_school==.10262306034564972,m$adjustment$kappa_college==.75)
      if(result$analysis_type=="IV") stopifnot(!m$stock_wright$calibration_uncertainty,
        !m$instrument_relevance$conditional_on_calibration)
    } else stopifnot(m$county_calibration$calibrated,m$county_calibration$county_fe)
    if(result$analysis_type=="IV") stopifnot(m$stock_wright$status=="available",m$instrument_relevance$status=="available",abs(m$iv_moment)<1e-7)
    records[[length(records)+1L]] <<- data.frame(level=result$analysis_level,year=result$year_modern,
      model=name,adjustment=m$adjustment_mode,method=result$analysis_type,n=nobs(m),county_n=m$county_calibration$nobs,
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
combined_state <- app$state_ch_result(modifyList(settings,list(state_adjustment_mode="both")),app$state_data)
check(combined_state)
record_parity(combined_state,"state_both_area_on")
check_county_iv_identity(app$prepare_county_geography(modifyList(settings,list(state_adjustment_mode="both")),combined_state$data_for_fs),combined_state$models[[1]],"State combined, log area on")
stopifnot(combined_state$adjustment_mode=="both",combined_state$schooling_adj==0L,!combined_state$apply_college_adj,
  combined_state$models[[1]]$adjustment$beta_school>0,
  combined_state$models[[1]]$adjustment$kappa_college==.75,
  combined_state$models[[1]]$adjustment$schooling_reference==0,
  max(abs(combined_state$models[[1]]$county_calibration$gamma-baseline$models[[1]]$county_calibration$gamma))>1e-6)
state_no_area_input <- modifyList(settings,list(state_adjustment_mode="both",state_county_area=FALSE))
combined_state_no_area <- app$state_ch_result(state_no_area_input,app$state_data)
check(combined_state_no_area)
record_parity(combined_state_no_area,"state_both_area_off")
check_county_iv_identity(app$prepare_county_geography(state_no_area_input,combined_state_no_area$data_for_fs),combined_state_no_area$models[[1]],"State combined, log area off")
state_default <- state_no_area_input
state_default$state_county_area <- NULL
stopifnot(identical(coef(app$state_ch_result(state_default,app$state_data)$models[[1]]),coef(combined_state_no_area$models[[1]])))
state_hc_input <- modifyList(settings,list(state_adjustment_mode="human_capital",state_county_area=FALSE))
state_hc <- app$state_ch_result(state_hc_input,app$state_data)
check(state_hc)
record_parity(state_hc,"state_human_capital_area_off")
stopifnot(length(state_hc$models)==1L,state_hc$schooling_adj==0L,!state_hc$apply_college_adj,
  state_hc$fe_part=="0",nobs(state_hc$models[[1]])>=nobs(combined_state_no_area$models[[1]]),
  !length(state_hc$county_geo_vars),is.na(state_hc$water_year),
  is.na(state_hc$county_calibration_instrument),!length(state_hc$geo_controls),!length(state_hc$water_controls))
check(app$state_ch_result(modifyList(state_hc_input,list(analysis_type="OLS")),app$state_data))
state_hc_states <- app$state_ch_result(modifyList(state_hc_input,list(sample_scope="states_only")),app$state_data)
check(state_hc_states)
stopifnot(state_hc_states$sample_scope=="states_only",
  all(state_hc_states$models[[1]]$state_ids %in% as.character(state_hc_states$data_for_fs$state_id)))
check(app$state_ch_result(modifyList(state_hc_input,list(state_instrument_type="historical_ch")),app$state_data))
state_hc_hidden <- modifyList(state_hc_input,list(state_county_area=TRUE,
  state_county_instrument="unused",state_geo_controls="unused",state_water_controls="unused",
  state_transport_controls="unused",state_water_year="unused",msa_schooling_adj="3",
  msa_apply_college_adj=TRUE,msa_college_coeff="2",use_fe=TRUE,fe_type="historical"))
state_hc_unchanged <- app$state_ch_result(state_hc_hidden,app$state_data)
stopifnot(identical(coef(state_hc$models[[1]]),coef(state_hc_unchanged$models[[1]])),
  identical(vcov(state_hc$models[[1]]),vcov(state_hc_unchanged$models[[1]])))

# State schooling and college preparation never loads the county calibration
# panel and works when the static input omits every geography field.
original_year_loader <- app$get_app_year
original_hc_static <- app$get_state_adjustment_inputs
hc_static <- as.data.frame(original_hc_static())
hc_static <- hc_static[c("geofips","statefips","ch_slot","ac","simple_gamma","avg_schooling_10_14","college_share")]
app$get_app_year <- function(...) stop("County calibration data must not be loaded.")
app$get_state_adjustment_inputs <- function(...) hc_static
hc_prepared <- app$prepare_county_geography(state_hc_hidden,state_hc$data_for_fs)
stopifnot(nrow(hc_prepared$county)==0L,!length(hc_prepared$geo_vars),
  !hc_prepared$settings$county_fe,!length(hc_prepared$settings$county_controls),
  is.na(hc_prepared$construction),is.na(hc_prepared$water_year))
app$get_app_year <- original_year_loader
app$get_state_adjustment_inputs <- original_hc_static
original_static <- app$get_state_adjustment_inputs
static <- as.data.frame(original_static())
static$avg_schooling_10_14 <- static$college_share <- NA_real_
app$get_state_adjustment_inputs <- function(...) static
no_hc <- app$state_ch_result(settings,app$state_data)
stopifnot(identical(baseline$models[[1]]$coefficients,no_hc$models[[1]]$coefficients))
missing_hc <- tryCatch(app$state_ch_result(modifyList(settings,list(state_adjustment_mode="both")),app$state_data),error=identity)
stopifnot(inherits(missing_hc,"error"))
selected <- baseline$models[[1]]$state_ids[1]
static$rugged_mean[which(static$statefips==as.numeric(selected))[1]] <- NA_real_
missing <- app$state_ch_result(settings,app$state_data)
stopifnot(nobs(missing$models[[1]])==nobs(baseline$models[[1]])-1,!selected %in% missing$models[[1]]$county_calibration$states)
app$get_state_adjustment_inputs <- original_static
ui <- htmltools::renderTags(app$ui)$html
ui_conditions <- gsub("&#39;","'",ui,fixed=TRUE)
stopifnot(grepl('id="state_ch_mode"',ui,fixed=TRUE),grepl('id="msa_ch_mode"',ui,fixed=TRUE),
  grepl('id="state_adjustment_mode"',ui,fixed=TRUE),grepl('id="msa_adjustment_mode"',ui,fixed=TRUE),
  grepl("input.state_adjustment_mode != 'human_capital'",ui_conditions,fixed=TRUE),
  grepl("input.msa_adjustment_mode != 'human_capital'",ui_conditions,fixed=TRUE))
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
  stopifnot(grepl("County-adjusted CH",saved,fixed=TRUE),!grepl("<h2>State data</h2>",saved,fixed=TRUE))
  geography_models <- lapply(result$models,coef)
  session$setInputs(msa_adjustment_mode="both",msa_county_area=FALSE,run_analysis=2L)
  session$flushReact()
  combined <- isolate(analysis_output()); check(combined)
  record_parity(combined,"msa_both_state_id_area_off")
  stopifnot(length(combined$models)==1L,combined$adjustment_mode=="both",
    combined$schooling_adj==0L,!combined$apply_college_adj,
    identical(combined$data_for_fs$LHS,combined$data_for_fs$ln_output_worker),
    combined$models[[1]]$adjustment$kappa_college==.75,
    max(abs(combined$models[[1]]$county_calibration$gamma-result$models[[1]]$county_calibration$gamma))>1e-6)
  combined_inputs <- modifyList(modifyList(settings,msa),list(msa_adjustment_mode="both",msa_county_area=FALSE))
  check_county_iv_identity(app$prepare_county_geography(combined_inputs,combined$data_for_fs,"MSA",combined$se_spec),combined$models[[1]],"MSA combined with FE, log area off")
  combined_csv <- read.csv(output$download_coefficients)
  stopifnot(all(combined_csv$human_capital_inside_ch),all(combined_csv$county_college_coefficient==.75),
    all(combined_csv$county_schooling_return>0),all(combined_csv$schooling_adjustment=="None"),
    !any(combined_csv$college_adjustment),!any(combined_csv$county_log_area_control),
    all(combined_csv$county_density_regressor=="Log employment density"))
  combined_html <- output$results_table$html
  stopifnot(grepl("ordinary log employment density",combined_html,fixed=TRUE),
    !grepl("effective employment density",combined_html,fixed=TRUE),!grepl("theta h",combined_html,fixed=TRUE))
  combined_calibration <- read.csv(output$download_county_calibration)
  stopifnot(all(combined_calibration$county_adjustment=="both"),all(combined_calibration$human_capital_inside_ch))
  # Hidden legacy outcome controls must not change the combined calculation.
  session$setInputs(msa_schooling_adj="0",msa_apply_college_adj=FALSE,msa_college_coeff="2",run_analysis=3L)
  session$flushReact()
  no_double <- isolate(analysis_output()); check(no_double)
  stopifnot(identical(coef(combined$models[[1]]),coef(no_double$models[[1]])),
    identical(vcov(combined$models[[1]]),vcov(no_double$models[[1]])))
  session$setInputs(use_fe=FALSE,run_analysis=31L)
  session$flushReact()
  combined_no_fe <- isolate(analysis_output()); check(combined_no_fe)
  record_parity(combined_no_fe,"msa_both_0_area_off")
  combined_no_fe_inputs <- modifyList(combined_inputs,list(use_fe=FALSE))
  check_county_iv_identity(app$prepare_county_geography(combined_no_fe_inputs,combined_no_fe$data_for_fs,"MSA",combined_no_fe$se_spec),combined_no_fe$models[[1]],"MSA combined without FE, log area off")
  session$setInputs(msa_adjustment_mode="geography",msa_county_area=TRUE,msa_schooling_adj="3",msa_apply_college_adj=TRUE,
    msa_college_coeff=".75",use_fe=TRUE,run_analysis=4L)
  session$flushReact()
  restored <- isolate(analysis_output()); check(restored)
  stopifnot(identical(geography_models,lapply(restored$models,coef)))
  session$setInputs(msa_schooling_adj="0",msa_apply_college_adj=FALSE,use_fe=FALSE,run_analysis=5L)
  session$flushReact()
  raw <- isolate(analysis_output()); check(raw)
  stopifnot(abs(coef(raw$models[[1]])[["ch_elasticity"]]-.184863)<1e-5)
  session$setInputs(msa_se_spec="cluster_state",run_analysis=6L)
  session$flushReact()
  linked <- isolate(analysis_output()); check(linked)
  stopifnot(abs(coef(linked$models[[1]])[["ch_elasticity"]]-coef(raw$models[[1]])[["ch_elasticity"]])<1e-8,linked$models[[1]]$clusters<48L)
  session$setInputs(analysis_type="OLS",run_analysis=7L)
  session$flushReact()
  check(isolate(analysis_output()))

  session$setInputs(msa_adjustment_mode="human_capital",msa_county_area=FALSE,
    analysis_type="IV",msa_se_spec="cluster_instrument",use_fe=TRUE,fe_type="modern",run_analysis=8L)
  session$flushReact()
  hc_msa <- isolate(analysis_output()); check(hc_msa)
  record_parity(hc_msa,"msa_human_capital_state_id_area_off")
  stopifnot(length(hc_msa$models)==1L,hc_msa$schooling_adj==0L,!hc_msa$apply_college_adj,
    hc_msa$fe_part=="state_id",identical(hc_msa$data_for_fs$LHS,hc_msa$data_for_fs$ln_output_worker),
    nobs(hc_msa$models[[1]])>=nobs(combined$models[[1]]),!length(hc_msa$county_geo_vars),
    is.na(hc_msa$county_water_year),is.na(hc_msa$county_calibration_instrument))
  hc_html <- output$results_table$html
  stopifnot(grepl("Schooling and college",hc_html,fixed=TRUE),
    !grepl("County calibration observations",hc_html,fixed=TRUE),
    !grepl("County calibration instrument F",hc_html,fixed=TRUE))
  hc_csv <- read.csv(output$download_coefficients)
  stopifnot(all(hc_csv$county_adjustment=="human_capital"),all(hc_csv$human_capital_inside_ch),
    all(hc_csv$county_schooling_return>0),all(hc_csv$county_college_coefficient==.75),
    all(hc_csv$schooling_adjustment=="None"),!any(hc_csv$college_adjustment),
    all(hc_csv$county_calibration_n==0L),all(is.na(hc_csv$county_calibration_method)),
    all(is.na(hc_csv$county_calibration_instrument)),all(is.na(hc_csv$county_calibration_scale)),
    all(is.na(hc_csv$county_density_regressor)),all(is.na(hc_csv$county_water_year)),
    !any(hc_csv$county_log_area_control),all(is.na(hc_csv$county_geography_variables)|hc_csv$county_geography_variables==""))
  hc_saved <- paste(readLines(output$download_results,warn=FALSE),collapse="\n")
  stopifnot(grepl("Schooling and college",hc_saved,fixed=TRUE),
    !grepl("County calibration instrument F",hc_saved,fixed=TRUE))

  # Hidden county calibration, geography, transport and outcome-adjustment
  # settings must leave coefficients, sample and covariance unchanged.
  session$setInputs(msa_schooling_adj="3",msa_apply_college_adj=TRUE,msa_college_coeff="2",
    msa_county_area=TRUE,msa_county_instrument="unused",msa_geo_controls="unused",
    msa_water_controls="unused",msa_transport_controls="unused",msa_water_year="unused",
    geo_controls=c("terrain","climate"),water_controls=c("distance","portage"),
    water_year="1790",msa_controls="railroads_1861",run_analysis=9L)
  session$flushReact()
  hc_hidden <- isolate(analysis_output()); check(hc_hidden)
  stopifnot(identical(coef(hc_msa$models[[1]]),coef(hc_hidden$models[[1]])),
    identical(vcov(hc_msa$models[[1]]),vcov(hc_hidden$models[[1]])),
    identical(hc_msa$models[[1]]$region_ids,hc_hidden$models[[1]]$region_ids))
  session$setInputs(use_fe=FALSE,msa_county_area=FALSE,run_analysis=10L)
  session$flushReact()
  hc_no_fe <- isolate(analysis_output()); check(hc_no_fe)
  stopifnot(hc_no_fe$fe_part=="0")
  record_parity(hc_no_fe,"msa_human_capital_0_area_off")
  session$setInputs(use_fe=TRUE,analysis_type="OLS",run_analysis=11L)
  session$flushReact()
  hc_ols <- isolate(analysis_output()); check(hc_ols)
  stopifnot(hc_ols$fe_part=="state_id",is.null(hc_ols$models[[1]]$stock_wright),
    is.null(hc_ols$models[[1]]$county_calibration$instrument_relevance))
})
write.csv(do.call(rbind,records),file.path(work,"state_adjustment_checks.csv"),row.names=FALSE)
write.csv(do.call(rbind,identity_records),file.path(work,"county_iv_estimator_identity.csv"),row.names=FALSE)
write.csv(do.call(rbind,parity_records),file.path(work,"combined_production_parity.csv"),row.names=FALSE)
cat("County geography, schooling and college, and combined CH integration checks passed.\n")
