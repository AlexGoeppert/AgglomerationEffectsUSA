source("tests/check_state_core.R", encoding="UTF-8")
source("tests/setup.R", encoding="UTF-8")
source("tests/check_hch_coverage.R", encoding="UTF-8")
check_hch_coverage_options(app)

# Compare the observation definitions to the final County FE fields, not a
# separate admission calendar or modern-state classification.
for(year in app$historical_census_years) {
  memberships<-list();panels<-list()
  for(scope in c("states_only","states_territories")) {
    p<-app$state_data[app$state_data$year==2010 & app$state_data$sample_year==year &
      app$state_data$scope==app$historical_region_scope(scope),,drop=FALSE]
    m<-app$get_historical_state_membership(year,scope)
    label<-trimws(as.character(app$county_data[[paste0("hist_state_fe_",year,if(scope=="states_only") "_s" else "")]]))
    expected<-sort(unique(label[!is.na(label)&nzchar(label)&label!="0"]))
    stopifnot(setequal(p$region_name,expected),setequal(p$region_id,m$region_id),
      !anyDuplicated(p$region_id),!anyDuplicated(m$geofips),!"statefips" %in% names(p))
    parts<-app$historical_state_components(2010,year,scope,m)
    for(id in p$region_id) {
      selected<-parts[parts$region_id==id,]
      row<-p[p$region_id==id,]
      if(row$ch_complete==1L) stopifnot(abs(sum(selected$nc)-row$region_employment)<1e-6,
        abs(sum(selected$ac)-row$region_land_acres)<1e-6*(1+row$region_land_acres))
      if(row$outcome_complete==1L) stopifnot(abs(sum(selected$gdp)-row$region_gdp)<1e-6*(1+row$region_gdp),
        abs(row$LHS-log(row$region_gdp/row$region_employment))<1e-12)
    }
    memberships[[scope]]<-m;panels[[scope]]<-p
  }
  common<-intersect(panels$states_only$region_id,panels$states_territories$region_id)
  for(id in common) {
    stopifnot(setequal(memberships$states_only$geofips[memberships$states_only$region_id==id],
      memberships$states_territories$geofips[memberships$states_territories$region_id==id]))
    fields<-c("region_gdp","region_employment","region_land_acres","LHS","RHS")
    a<-unlist(panels$states_only[panels$states_only$region_id==id,fields]);
    b<-unlist(panels$states_territories[panels$states_territories$region_id==id,fields])
    stopifnot(isTRUE(all.equal(a,b,tolerance=1e-12)))
  }
}
full1790<-app$state_data[app$state_data$year==2010 & app$state_data$sample_year==1790 &
  app$state_data$scope=="states_and_territories",]
states1790<-app$state_data[app$state_data$year==2010 & app$state_data$sample_year==1790 &
  app$state_data$scope=="states_only",]
stopifnot(nrow(full1790)==16L,nrow(states1790)==13L,
  all(c("Northwest Territory","Southwest Territory","Vermont") %in% full1790$region_name),
  !any(c("Northwest Territory","Southwest Territory","Vermont") %in% states1790$region_name))

check_estimate <- function(result) {
  model <- result$models[[1]]
  d <- result$data_for_fs[model$rows, , drop = FALSE]
  ncols <- grep("^nc[0-9]+$", names(d), value = TRUE)
  n <- as.matrix(d[ncols])
  a <- as.matrix(d[sub("^nc", "ac", ncols)])
  theta <- unname(coef(model)[["ch_elasticity"]]) + 1
  weight <- n^theta * a^(1 - theta)
  log_density <- matrix(0, nrow(n), ncol(n))
  log_density[n > 0] <- log(n[n > 0] / a[n > 0])
  index <- log(rowSums(weight) / rowSums(n))
  derivative <- rowSums(weight * log_density) / rowSums(weight)
  intercept <- mean(d$LHS - index)
  residual <- d$LHS - index - intercept
  D <- cbind(derivative, 1)
  Z <- if (result$analysis_type == "IV") cbind(as.numeric(scale(d$instrument)), 1) else D
  moment <- colSums(Z * residual) / sqrt(colSums(Z^2))
  stopifnot(max(abs(moment)) < 1e-5, abs(coef(model)[["(Intercept)"]] - intercept) < 1e-8)
  influence <- (Z * residual) %*% t(solve(crossprod(Z, D)))
  correction <- if (result$analysis_type == "OLS") nrow(d) / (nrow(d) - 2L) else 1
  covariance <- crossprod(influence) * correction
  stopifnot(isTRUE(all.equal(unname(vcov(model)), unname(covariance), tolerance = 1e-7)))
}
settings<-list(analysis_level="State",year_modern="2010",analysis_type="IV",state_ch_mode="standard",
  state_sample_year="1790",state_iv_year="1790",state_instrument_type="historical_ch",
  state_hch_coverage="none",sample_scope="states_territories",instrument_form="levels",show_map=FALSE)
rows<-list()
for(year in c(1790L,1850L,1890L,1900L)) for(scope in c("states_only","states_territories"))
  for(construction in c("county_population","area_population","historical_ch")) for(method in c("OLS","IV")) {
    chosen<-modifyList(settings,list(state_sample_year=as.character(year),state_iv_year=as.character(year),
      sample_scope=scope,state_instrument_type=construction,analysis_type=method))
    result<-tryCatch(app$state_ch_result(chosen,app$state_data),error=identity)
    if(inherits(result,"error")) {
      stopifnot(method=="IV",grepl("No positive-theta IV solution",conditionMessage(result),fixed=TRUE),
        result$stock_wright$status=="available")
      cat("Identification failure reported:",year,scope,construction,"\n")
      next
    }
    check_estimate(result)
    m<-result$models[[1]]
    membership<-app$get_historical_state_membership(year,scope)
    stopifnot(all(result$data_for_fs$region_id %in% membership$region_id),
      result$historical_region_eligible_n==length(unique(membership$region_id)),
      result$n_obs<=result$historical_region_eligible_n,all(is.finite(coef(m))),all(is.finite(vcov(m))),
      result$fe_type=="No",result$covariance_unit=="Historical state or territory group")
    rows[[length(rows)+1L]]<-data.frame(year,scope,construction,method,n=nobs(m),
      elasticity=coef(m)[["ch_elasticity"]],se=sqrt(vcov(m)["ch_elasticity","ch_elasticity"]))
  }
# Changing the instrument year cannot redraw the regional outcome or county sum.
early<-app$state_ch_result(settings,app$state_data)
later<-app$state_ch_result(modifyList(settings,list(state_iv_year="1900")),app$state_data)
common<-intersect(early$data_for_fs$region_id,later$data_for_fs$region_id)
a<-early$data_for_fs[match(common,early$data_for_fs$region_id),]
b<-later$data_for_fs[match(common,later$data_for_fs$region_id),]
fields<-c("LHS","RHS",grep("^(nc|ac)[0-9]+$",names(a),value=TRUE))
stopifnot(isTRUE(all.equal(a[fields],b[fields],check.attributes=FALSE)))

shiny::testServer(app$server, {
  do.call(session$setInputs,modifyList(settings,list(state_iv_year="1900")))
  session$setInputs(run_analysis=1L)
  result<-isolate(analysis_output())
  if(!is.null(result$error)) stop(result$error)
  csv<-read.csv(output$download_coefficients,stringsAsFactors=FALSE)
  diagnostics<-read.csv(output$download_hch_diagnostics,stringsAsFactors=FALSE)
  html<-paste(readLines(output$download_results,warn=FALSE),collapse="\n")
  stopifnot(all(csv$historical_boundary_year==1790),all(csv$historical_groups_before_exclusions==16),
    all(csv$covariance_unit=="Historical state or territory group"),
    all(nzchar(csv$historical_groups_in_estimation)),!"statefips" %in% names(diagnostics),
    all(nzchar(diagnostics$region_name[diagnostics$record_type=="state"])),
    grepl("Historical county fixed-effect groups",output$analysis_details$html,fixed=TRUE),
    grepl("Northwest Territory",html,fixed=TRUE),
    !grepl("Anderson.{0,12}Rubin|Local instrument Wald F",output$results_table$html,ignore.case=TRUE))
  before<-output$results_header
  session$setInputs(state_sample_year="1900",sample_scope="states_only")
  stopifnot(identical(before,output$results_header),isolate(analysis_output())$sample_year==1790L)
  session$setInputs(state_sample_year="1910",run_analysis=2L)
  stopifnot(grepl("Choose a historical census year",isolate(analysis_output())$error,fixed=TRUE))
})
results<-do.call(rbind,rows)
jsonlite::write_json(results,file.path(work,"state_app_results.json"),pretty=TRUE,digits=15)
print(results,row.names=FALSE)
cat("State historical-region checks passed: exact FE groups, common boundaries, county sums, all estimators, independent covariance and exports.\n")
