# app/app.R
# Interactive degree planner for the UBCO Freshwater Science program.
# Embedded as an iframe inside the Quarto static site.
#
# *** SCAFFOLD - not yet implemented. ***
#
# v1 goals:
#   - Read data/courses.parquet (the daily-scraped catalogue).
#   - Read data/designation_mappings/{pag,rpbio}.yml.
#   - Let the student drag courses into year slots (year 1, 2, 3, 4).
#   - Toggle designation goals (PAg, RPBio, neither, both).
#   - Live gap analysis: prerequisites missing, designation categories
#     under-covered, total credit count vs the 120/78/42/36 thresholds.
#   - Prereq graph (visNetwork) of the selected courses.
#   - Export a plan as a PDF or text summary.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})

ui <- page_sidebar(
  title = "UBCO Freshwater Science Planner",
  sidebar = sidebar(
    title = "Goals",
    checkboxGroupInput("designations", "Target designations:",
                       choices = c("PAg" = "pag", "RPBio" = "rpbio")),
    radioButtons("year_in_program", "Current year:",
                 choices = c("Year 1", "Year 2", "Year 3", "Year 4", "Done")),
    hr(),
    helpText("Scaffold UI - the planner is not yet implemented. ",
             "Drag-drop year slots, prereq DAG, and gap analysis are queued.")
  ),
  card(
    card_header("Plan"),
    p("This is a placeholder. Once data/courses.parquet exists and the
       designation YAMLs are curated, this panel renders the planner.")
  )
)

server <- function(input, output, session) {
  # TODO: load courses + designation mappings, wire the planner.
}

shinyApp(ui, server)
