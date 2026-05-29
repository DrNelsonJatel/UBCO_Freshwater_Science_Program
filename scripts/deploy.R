#!/usr/bin/env Rscript
# scripts/deploy.R
# Deploy the UBCO Freshwater Science degree planner to shinyapps.io.
#
# Live URL once deployed: https://obwb.shinyapps.io/ubco-freshwater-planner/
#
# Prereqs:
#   - data/courses.parquet (run data-raw/01_scrape_ubco_calendar.R)
#   - data/designation_mappings/{pag.yml,rpbio.yml}
#   - rsconnect credentials cached for the "obwb" shinyapps account
#     (set up once via rsconnect::setAccountInfo).
#
# Run from project root: Rscript scripts/deploy.R

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect",
                    repos = "https://packagemanager.posit.co/cran/latest")
}

APP_NAME <- "ubco-freshwater-planner"
ACCOUNT  <- "obwb"

stopifnot(file.exists("data/courses.parquet"))
stopifnot(file.exists("data/designation_mappings/pag.yml"))
stopifnot(file.exists("data/designation_mappings/rpbio.yml"))

cat("Deploying", APP_NAME, "to", ACCOUNT, "...\n")

rsconnect::deployApp(
  appDir         = ".",
  appPrimaryDoc  = "app/app.R",
  appFiles       = c(
    "app/app.R",
    "data/courses.parquet",
    "data/designation_mappings/pag.yml",
    "data/designation_mappings/rpbio.yml"
  ),
  appName        = APP_NAME,
  account        = ACCOUNT,
  forceUpdate    = TRUE,
  launch.browser = FALSE
)

cat(sprintf("Done. Live at https://%s.shinyapps.io/%s/\n", ACCOUNT, APP_NAME))
