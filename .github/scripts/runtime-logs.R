token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")
if (!nzchar(token) || !nzchar(secret)) stop("Deployment credentials are unavailable.")
rsconnect::setAccountInfo(name = "agoepper", token = token, secret = secret)
rm(token, secret)

output <- Sys.getenv("APP_TEST_OUTPUT", unset = tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)
apps <- rsconnect::applications(account = "agoepper", server = "shinyapps.io")
app <- apps[apps$name == "AgglomerationEffectsUSA",
  intersect(c("id", "name", "url", "status", "size", "instances", "updated_time"), names(apps)), drop = FALSE]
if (nrow(app) != 1L) stop("The target app was not found uniquely.")
write.csv(app, file.path(output, "application_settings.csv"), row.names = FALSE)
print(app, row.names = FALSE)

logs <- capture.output(rsconnect::showLogs(appName = "AgglomerationEffectsUSA",
  account = "agoepper", server = "shinyapps.io", entries = 200, streaming = FALSE))
writeLines(logs, file.path(output, "shiny_runtime.log"))
cat(paste(logs, collapse = "\n"), "\n")
