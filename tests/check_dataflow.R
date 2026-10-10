Sys.setenv(OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',MKL_NUM_THREADS='1')
if (.Platform$OS.type == 'windows') invisible(Sys.setlocale('LC_CTYPE','.UTF-8'))

suppressPackageStartupMessages({library(shiny);library(fixest);library(dplyr);library(glue);library(ggplot2)})
setFixest_nthreads(1)
setFixest_notes(FALSE)
repo <- normalizePath(Sys.getenv('APP_REPO', unset='.'), mustWork=TRUE)
app <- new.env(parent=globalenv())
for (expr in parse(file.path(repo,'app.R'))) if (is.call(expr) &&
  identical(expr[[1]],as.name('<-')) && !identical(as.character(expr[[2]]),'ui')) eval(expr,app)
for (generic in c('coef','vcov','nobs','residuals','fitted','confint'))
  registerS3method(generic,'ch_model',app[[paste0(generic,'.ch_model')]],envir=asNamespace('stats'))
i <- seq_len(24)
x <- 2+i/20+sin(i*.41)
z <- exp(1+i/50+sin(i*.41)/3)
fixture <- data.frame(year=2010,msafips=10000+i,msaname=paste('MSA',i),
  modern_state_fe=rep(1:6,each=4),lat_DD=30+i/20,lon_DD=-90+i/20,
  msa_schooling_09=12+sin(i),college_share_09=.2,employment=exp(x)*100,
  msaarea=100*4046.8,ln_output_worker=3+.12*x+.1*cos(i*.7),
  simple_gamma=.1,Minc_return_calc=.08,gmp=100,gmp_mining=1,
  MSAol_1790_5=z,MSAol_1900_5=z*2,MSApopid_1900_5=paste0('county',i),
  GGpop_1790=z*1000,GGpop_1900=z*2000,
  hist_state_fe_1790=rep('Virginia',24),hist_state_fe_1790_s=rep('Virginia',24))
app$fixture <- fixture
app$get_app_year <- function(level,year) app$fixture
settings <- list(analysis_level='MSA',year_modern='2010',analysis_type='IV',use_fe=FALSE,
  fe_type='historical',sample_scope='states_territories',approach='density',instrument_form='levels',
  msa_density_measure='average',msa_sectors='1',msa_sample_year='1790',msa_iv_year='1900',
  msa_instrument_type='overlap',msa_overlap_pct='5',msa_controls=character(),msa_schooling_adj='0',
  msa_apply_college_adj=FALSE,msa_college_coeff='.75',msa_se_spec='robust',msa_mining_filter_active=FALSE,
  msa_mining_threshold='.01',msa_spatial_cutoff='100',county_instrument_type='max_density_overlap',
  county_se_spec='robust',geo_controls=character(),water_controls=character(),water_year='1820',
  show_map=FALSE,map_size=75,show_coefficient_chart=FALSE)

for (missing in c('one','column')) for (spec in c('robust','cluster_state','spatial','cluster_instrument')) {
  app$fixture <- fixture
  if (missing=='one') app$fixture$MSApopid_1900_5[1] <- NA_character_
  else app$fixture$MSApopid_1900_5 <- NULL
  shiny::testServer(app$server,{
    do.call(session$setInputs,c(modifyList(settings,list(msa_se_spec=spec)),list(run_analysis=1)))
    session$flushReact()
    result <- isolate(analysis_output())
    if (spec=='cluster_instrument') stopifnot(!is.null(result$error))
    else stopifnot(is.null(result$error),result$n_obs==24L)
    cat('Missing historical ID',missing,'/',spec,':',if(is.null(result$error)) paste('N',result$n_obs) else 'Expected cluster-ID error','\n')
  })
}
app$fixture <- fixture
shiny::testServer(app$server,{
  do.call(session$setInputs,c(modifyList(settings,list(msa_apply_college_adj=TRUE,
    msa_college_coeff='.4',msa_mining_filter_active=TRUE,msa_mining_threshold='.025',
    msa_se_spec='spatial',msa_spatial_cutoff='50')),list(run_analysis=1)))
  session$flushReact()
  result <- isolate(analysis_output())
  stopifnot(is.null(result$error))
  csv <- read.csv(output$download_coefficients)
  stopifnot(all(csv$college_adjustment),all(csv$college_coefficient==.4),
    all(csv$mining_filter_active),all(csv$mining_threshold==.025),
    all(csv$standard_error_method=='spatial'),all(csv$spatial_cutoff_km==50))
  cat('CSV preserves enabled college/mining adjustments and the selected Conley cutoff.\n')
})
app$fixture <- fixture
app$fixture$msa_schooling_09[1] <- NA_real_
shiny::testServer(app$server,{
  do.call(session$setInputs,c(modifyList(settings,list(msa_schooling_adj='1')),list(run_analysis=1)))
  session$flushReact()
  iv <- isolate(analysis_output())
  stopifnot(is.null(iv$error))
  header <- output$results_header
  csv <- read.csv(output$download_coefficients)
  quality_note <- app$data_quality_notes(iv)
  stopifnot(length(quality_note)==1L,all(csv$data_quality_notes==quality_note),
    grepl(quality_note,output$sample_note$html,fixed=TRUE))
  html <- paste(readLines(output$download_results,encoding='UTF-8',warn=FALSE),collapse='\n')
  stopifnot(grepl(quality_note,html,fixed=TRUE))
  stopifnot(all(csv$sample_scope=='states_territories'),all(csv$sector_code==1),
    all(csv$sector=='All'),all(csv$standard_error_method=='robust'),
    all(!csv$use_state_fixed_effects),all(!csv$college_adjustment),
    all(is.na(csv$college_coefficient)),all(!csv$mining_filter_active),
    all(is.na(csv$mining_threshold)),all(is.na(csv$county_msa_restriction)))
  session$setInputs(year_modern='2020',sample_scope='states_only',instrument_form='log')
  session$flushReact()
  unchanged <- isolate(analysis_output())
  stopifnot(identical(iv,unchanged),identical(header,output$results_header),
    identical(csv,read.csv(output$download_coefficients)))
  cat('Run results and CSV remain unchanged after inputs are edited without Run.\n')
  cat('CSV metadata fields:',paste(names(csv),collapse=', '),'\n')
  session$setInputs(year_modern='2010',sample_scope='states_territories',instrument_form='levels',
    analysis_type='First-stage Regression',run_analysis=2)
  session$flushReact()
  fs <- isolate(analysis_output())
  stopifnot(is.null(fs$error))
  stopifnot(nobs(iv$first_stage_models[[1]])==nobs(fs$models[[1]]),
    isTRUE(all.equal(coef(iv$first_stage_models[[1]]),coef(fs$models[[1]]),tolerance=1e-10)),
    isTRUE(all.equal(vcov(iv$first_stage_models[[1]]),vcov(fs$models[[1]]),tolerance=1e-10)))
  cat('IV first-stage N:',nobs(iv$first_stage_models[[1]]),'; standalone first-stage N:',nobs(fs$models[[1]]),'\n')
  session$setInputs(msa_iv_year='1790',analysis_type='IV',run_analysis=3)
  session$flushReact()
  earlier <- isolate(analysis_output())
  stopifnot(is.null(earlier$error),length(app$data_quality_notes(earlier))==0L)
  csv <- read.csv(output$download_coefficients)
  html <- paste(readLines(output$download_results,encoding='UTF-8',warn=FALSE),collapse='\n')
  stopifnot(all(is.na(csv$data_quality_notes) | csv$data_quality_notes==''),
    !grepl('Winchester, Virginia',html,fixed=TRUE),
    !grepl('Winchester, Virginia',paste(output$sample_note$html,collapse=''),fixed=TRUE))
  cat('The 1900 boundary note appears in the website, CSV and HTML only when relevant.\n')
  session$setInputs(instrument_form='log',run_analysis=4)
  session$flushReact()
  stopifnot(is.null(isolate(analysis_output())$error))
  info <- isolate(map_output())
  stopifnot(grepl('in levels',info$title,fixed=TRUE),
    grepl('before regression controls and sample exclusions',info$note,fixed=TRUE),
    grepl('regression uses the natural log',info$note,fixed=TRUE))
  session$setInputs(msa_instrument_type='county_population',run_analysis=5)
  session$flushReact()
  stopifnot(is.null(isolate(analysis_output())$error))
  info <- isolate(map_output())
  stopifnot(grepl('Natural log of population',info$title,fixed=TRUE),
    !grepl('thousands',paste(info$title,info$message),ignore.case=TRUE))
  cat('Map labels distinguish log population from precomputed density maps in levels.\n')
})

# Both schooling choices retain their own IV samples, including FE singleton removal.
for (fe in c(FALSE,TRUE)) {
  app$fixture <- fixture
  app$fixture$msa_schooling_09[1] <- NA_real_
  app$fixture$Minc_return_calc[2] <- NA_real_
  app$fixture$modern_state_fe <- rep(1:8,each=3)
  shiny::testServer(app$server,{
    do.call(session$setInputs,c(modifyList(settings,list(msa_schooling_adj='3',use_fe=fe,
      fe_type='modern')),list(run_analysis=1)))
    session$flushReact()
    iv <- isolate(analysis_output())
    stopifnot(is.null(iv$error),length(iv$models)==2L)
    session$setInputs(analysis_type='First-stage Regression',run_analysis=2)
    session$flushReact()
    fs <- isolate(analysis_output())
    stopifnot(is.null(fs$error),length(fs$models)==2L,
      identical(names(fs$models),paste('First stage:',names(iv$models))))
    for (j in seq_along(iv$models)) {
      left <- iv$first_stage_models[[j]]
      right <- fs$models[[j]]
      iv_ids <- iv$data_for_fs$msafips[app$app_model_rows(left)]
      fs_ids <- fs$data_for_fs$msafips[app$app_model_rows(right)]
      stopifnot(identical(iv_ids,fs_ids),nobs(left)==nobs(right),
        isTRUE(all.equal(coef(left),coef(right),tolerance=1e-10)),
        isTRUE(all.equal(vcov(left),vcov(right),tolerance=1e-10)))
    }
    cat('Both schooling first stages exactly match IV samples, coefficients and covariance; FE=',fe,'\n')
  })
}
