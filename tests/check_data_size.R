source("tests/setup.R")
app$msa_data <- app$read_app_data("MSA_analysis_data.dta", "MSA")
app$county_data <- app$read_app_data("master_county_build.dta", "County")
gc()
sizes <- vapply(list(MSA = app$msa_data, County = app$county_data, State = app$state_data),
  function(data) as.numeric(object.size(data)) / 1024^2, numeric(1))
print(sizes)
jsonlite::write_json(list(megabytes = as.list(sizes), total_megabytes = sum(sizes)),
  file.path(work, "data_memory.json"), pretty = TRUE, auto_unbox = TRUE)
if (sum(sizes) > 700) stop("App data exceed the 700 MiB validation limit; reduce loading before deployment.")
cat("Data loading fits the app memory budget.\n")
