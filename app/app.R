# app/app.R
# UBCO Freshwater Science - interactive degree planner v1.
#
# Reads:
#   data/courses.parquet               (scraped UBC Okanagan calendar)
#   data/designation_mappings/pag.yml  (BCIA UBCO approved list)
#   data/designation_mappings/rpbio.yml (CAB credentialing mapping)
#
# Lets the student:
#   1. Tick the UBCO courses they have already completed (filterable
#      DT table per subject).
#   2. Toggle one or both designation goals (PAg, RPBio).
#   3. See live gap analysis:
#        - total credits + thresholds (120 / 78 Sci / 42 upper / 36 upper Sci)
#        - PAg category coverage: foundational / lower agrology / senior agrology
#        - RPBio category coverage: 6 core areas + 5 three-of-five subject areas
#        - biology-count check against the 13-biology RPBio threshold
#
# Local dev:
#   shiny::runApp("app")           from the project root
# Deploy:
#   rsconnect::deployApp("app", appFiles = c(...))   to shinyapps.io

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(dplyr)
  library(arrow)
  library(yaml)
  library(jsonlite)
  library(stringr)
  library(htmltools)
  library(visNetwork)
})

# pagedown gives us HTML -> PDF via headless Chrome. We require it
# lazily inside the download handler so the app still loads if the
# package is unavailable.

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------- Data load (works whether CWD is project root or /app) ----------
.proj <- if (dir.exists("data")) "." else ".."
.data_dir <- file.path(.proj, "data")

# Wrap the four data loads so that a missing or corrupt file gives a
# user-friendly error rather than a stack trace at app start.
.load_required <- function(path, loader, label) {
  if (!file.exists(path)) {
    stop(sprintf(
      "Required data file missing: %s\nThe planner cannot start without %s. ",
      path, label),
      "Run data-raw/01_scrape_ubco_calendar.R and the YAML scripts ",
      "in the project repository to regenerate the data layer.",
      call. = FALSE)
  }
  tryCatch(loader(path),
    error = function(e) stop(sprintf(
      "Could not parse %s (%s).\nCheck the file is valid %s and ",
      path, conditionMessage(e), label),
      "redeploy. Contact nelson.jatel@ubc.ca if this persists.",
      call. = FALSE))
}

courses  <- .load_required(file.path(.data_dir, "courses.parquet"),
                            arrow::read_parquet, "the scraped courses parquet")
pag      <- .load_required(file.path(.data_dir, "designation_mappings", "pag.yml"),
                            yaml::read_yaml, "the PAg designation YAML")
rpbio    <- .load_required(file.path(.data_dir, "designation_mappings", "rpbio.yml"),
                            yaml::read_yaml, "the RPBio designation YAML")
pathway  <- .load_required(file.path(.data_dir, "program_pathway.yml"),
                            yaml::read_yaml, "the year-by-year recommended pathway YAML")

# Normalise both sources to a consistent "BIOL 116" / "FWSC 375" style.
# The scrape produces "BIOL_O 116"; the YAMLs use "BIOL 116".
short_code <- function(x) stringr::str_replace(x, "_O", "")
courses$short_code <- short_code(courses$code)
courses$level_band <- dplyr::case_when(
  suppressWarnings(as.integer(courses$number)) <  200 ~ "100",
  suppressWarnings(as.integer(courses$number)) <  300 ~ "200",
  suppressWarnings(as.integer(courses$number)) <  400 ~ "300",
  TRUE                                                ~ "400+"
)

# Build a lookup so the planner can render label + title for any short
# code from the pathway file.
code_to_title <- setNames(courses$title, courses$short_code)
code_to_credits <- setNames(courses$credits, courses$short_code)

# Subject -> long-form labels for the UI.
SUBJECT_LABELS <- c(
  FWSC_O = "FWSC - Freshwater Science",
  BIOL_O = "BIOL - Biology",
  EESC_O = "EESC - Earth, Environmental & Geographic Sciences",
  CHEM_O = "CHEM - Chemistry",
  GEOG_O = "GEOG - Geography",
  GISC_O = "GISC - Geographic Information Sciences",
  MATH_O = "MATH - Mathematics",
  PHYS_O = "PHYS - Physics",
  STAT_O = "STAT - Statistics",
  INDG_O = "INDG - Indigenous Studies",
  ENGL_O = "ENGL - English",
  ECON_O = "ECON - Economics"
)

# Helpers to flatten the YAMLs.
pag_category_codes <- function(cat) vapply(cat$courses, function(c) c$code, character(1))
rpbio_area_codes   <- function(area) vapply(area$courses_eligible, function(c) c$code, character(1))

# ------- UI ---------------------------------------------------------------
ui <- page_sidebar(
  title = "UBCO Freshwater Science - Degree Planner",
  theme = bs_theme(
    version = 5, bootswatch = "cosmo",
    "primary" = "#1f3a5f"
  ),
  sidebar = sidebar(
    width = 320,
    title = "Your plan",
    checkboxGroupInput("goals", "Target designations:",
                       choices = c("PAg (BC Institute of Agrologists)" = "pag",
                                   "RPBio (College of Applied Biology)" = "rpbio")),
    hr(),
    radioButtons("year_in_program", "Current year in program:",
                 choices = c("Year 1" = 1L, "Year 2" = 2L, "Year 3" = 3L,
                             "Year 4" = 4L, "Beyond Year 4 / Completed" = 5L),
                 selected = 2L),
    hr(),
    helpText("Tick the courses you have completed (or transferred in) on
              the right. Live gap analysis updates below as you change
              selections."),
    hr(),
    tags$details(
      tags$summary(class = "small", "Save / load plan"),
      div(style = "margin-top:8px;",
        downloadButton("download_plan", "Save plan to file",
                        class = "btn-outline-secondary btn-sm",
                        style = "width:100%; margin-bottom:6px;"),
        fileInput("upload_plan", label = NULL, accept = ".json",
                   buttonLabel = "Load plan...",
                   placeholder = "no file selected",
                   width = "100%"),
        p(class = "small text-muted", style = "margin-bottom:0;",
          "Saves your ticks, goals, and current year as a small JSON
           file. Reload it next session, or email it to the program
           advisor before an advising appointment.")
      )
    ),
    hr(),
    div(class = "small text-muted",
        "MVP - results are illustrative only. ",
        "Confirm with the program advisor and the UBC Okanagan Academic Calendar.")
  ),

  navset_tab(
    nav_panel(
      "Step 1: Tick completed courses",
      div(class = "p-3",
          h4("Tick courses by year"),
          p(class = "text-muted small",
            "Each year panel lists the curated recommended courses
             first. Use the 'Other Y[NNN]-level courses' selector
             below the recommendations to add anything not in the
             default path (transfers, electives outside the
             recommendations, courses taken in a different year).
             Selections from every panel feed into Steps 2 and 3."),
          uiOutput("progress_summary"),
          uiOutput("year_accordion"),
          tags$hr(),
          h5("Transfer credits or anything not at a UBCO 100-400 level"),
          p(class = "text-muted small",
            "If you have credits from another institution or anything
             not surfaced above, search for them here."),
          selectizeInput("other_completed", label = NULL,
                          choices = NULL, multiple = TRUE,
                          width = "100%",
                          options = list(placeholder = "Search by code or title..."))
      )
    ),
    nav_panel(
      "Board: years side-by-side",
      div(class = "p-3",
          h4("Your plan, as a board"),
          p(class = "text-muted small",
            "Five columns, one per year of the curated pathway. Each card
             is a recommended course for that year; cards fill solid
             when ticked and stay outlined when not. \"Other\" courses
             you have ticked at that level appear under a dashed
             divider at the bottom of the column. Use the column
             headers as a scannable equivalent of the accordion in
             Step 1, especially on a wide screen."),
          uiOutput("year_board")
      )
    ),
    nav_panel(
      "Map: prerequisite graph",
      div(class = "p-3",
          h4("Your courses + prerequisite chains"),
          p(class = "text-muted small",
            "Curated map of the ", tags$strong("~70 Freshwater Science core
             and recommended courses"), ". Arrows point from a prerequisite to
             the course that needs it. Nodes are coloured by level
             (100, 200, 300, 400+). Ticked courses are filled solid;
             un-ticked courses are outlined. Hover any node for the full
             title and a list of its prereqs from the live UBC Okanagan
             Academic Calendar. Pan and zoom with mouse / trackpad."),
          visNetwork::visNetworkOutput("prereq_map", height = "640px"),
          tags$div(class = "small text-muted", style = "padding-top:6px;",
            tags$strong("Caveats. "),
            "Prereq strings on the UBC calendar are natural-language
             (\"either (a) X or (b) all of Y and Z\"). The arrows below
             collapse that structure into the codes mentioned, so
             treat them as a guide to ", tags$em("which courses sit
             upstream of which"), " rather than as a deterministic
             plan-validator. Confirm with the program advisor before
             registering.")
      )
    ),
    nav_panel(
      "Step 2: Credit + degree summary",
      div(class = "p-3",
          h4("How you sit against the degree thresholds"),
          uiOutput("credit_summary"),
          tags$hr(),
          h5("Courses you have ticked"),
          DT::DTOutput("completed_table")
      )
    ),
    nav_panel(
      "Step 3: Designation gap analysis",
      div(class = "p-3",
          # JS hook the server uses to open the report in a new tab and
          # auto-trigger the browser's print dialog (which has "Save as
          # PDF" built in on every modern browser).
          tags$head(tags$script(htmltools::HTML("
            Shiny.addCustomMessageHandler('openPrintReport', function(msg) {
              var w = window.open('', '_blank');
              if (!w) {
                alert('Pop-up blocked. Please allow pop-ups for this site, then click again.');
                return;
              }
              w.document.open();
              w.document.write(msg.html);
              w.document.close();
              setTimeout(function() { w.print(); }, 600);
            });
          "))),
          div(style = "margin-bottom: 14px;",
              actionButton("open_report",
                            "Open printable report (Save as PDF from browser)",
                            class = "btn-primary",
                            icon = icon("print")),
              downloadButton("download_report_html",
                              "Or download HTML",
                              class = "btn-outline-secondary",
                              style = "margin-left:8px;")),
          uiOutput("designation_summary")
      )
    )
  )
)

# ------- Server -----------------------------------------------------------
server <- function(input, output, session) {

  # ------- Year-based course picker accordion ------------------------------
  # Build the accordion server-side so the panel for each year auto-
  # expands when the student's current-year radio matches.

  YEAR_TO_BANDS <- list(
    year_1 = "100",
    year_2 = "200",
    year_3 = "300",
    year_4 = "400+",
    summer = "400+"
  )

  output$year_accordion <- renderUI({
    current <- input$year_in_program
    panels <- lapply(names(pathway), function(yk) {
      yspec <- pathway[[yk]]
      checkbox_id <- paste0("y_", yk, "_recommended")
      other_id    <- paste0("y_", yk, "_other")
      level <- YEAR_TO_BANDS[[yk]]

      # Filter the scraped courses for "other at this level" -
      # everything at the level band that isn't already a recommended
      # code.
      rec_codes <- yspec$recommended
      other_pool <- courses[courses$level_band == level &
                            !(courses$short_code %in% rec_codes), ,
                            drop = FALSE]
      other_choices <- setNames(other_pool$short_code,
                                 sprintf("%s — %s (%g cr)",
                                         other_pool$short_code,
                                         other_pool$title,
                                         other_pool$credits))

      # Pretty checkbox labels for the recommended list.
      rec_choices <- setNames(
        rec_codes,
        vapply(rec_codes, function(c) {
          ttl <- code_to_title[c]
          cr  <- code_to_credits[c]
          if (is.na(ttl)) c
          else sprintf("%s — %s (%g cr)", c, ttl, cr)
        }, character(1))
      )

      open_by_default <- identical(yk, sprintf("year_%s", current)) ||
                         (yk == "summer" && current == "5")

      tips <- yspec$tips %||% list()
      tips_block <- if (length(tips)) {
        tags$div(
          style = paste("background:#fff8e1; border-left:4px solid #f59e0b;",
                         "border-radius:6px; padding:10px 14px;",
                         "margin:10px 0 14px;"),
          tags$div(style = "font-weight:600; color:#5b3d00; margin-bottom:4px;",
                   "Year-specific tips"),
          tags$ul(style = "margin:0; padding-left:18px; font-size:0.9em;",
                  lapply(tips, function(t) tags$li(t)))
        )
      } else NULL

      year_rec_codes <- yspec$recommended %||% character(0)
      year_rec_have  <- length(intersect(year_rec_codes,
                                          completed_codes()))
      year_rec_total <- length(year_rec_codes)
      chip_colour <- if (year_rec_have == year_rec_total && year_rec_total > 0)
        "#1f7a3b" else if (year_rec_have > 0) "#d9822b" else "#a0a4ad"
      title_with_chip <- tags$span(
        yspec$label,
        tags$span(style = sprintf(paste("background:%s; color:white;",
                                          "padding:2px 8px; border-radius:999px;",
                                          "font-size:0.78em; margin-left:10px;"),
                                   chip_colour),
                  sprintf("%d / %d ticked", year_rec_have, year_rec_total))
      )

      bslib::accordion_panel(
        title = title_with_chip,
        value = yk,
        if (nzchar(yspec$description %||% ""))
          p(class = "text-muted small", yspec$description),
        tips_block,
        tags$h6("Recommended for this year"),
        checkboxGroupInput(checkbox_id, label = NULL,
                            choices = rec_choices,
                            selected = isolate(input[[checkbox_id]])),
        tags$details(
          tags$summary(class = "text-muted small",
                       sprintf("Other %s-level courses (%d available)",
                               level, nrow(other_pool))),
          div(style = "margin-top:8px;",
            selectizeInput(other_id, label = NULL,
                            choices = other_choices,
                            selected = isolate(input[[other_id]]),
                            multiple = TRUE, width = "100%",
                            options = list(
                              placeholder = sprintf("Search %s-level courses...",
                                                     level)))
          )
        )
      )
    })
    bslib::accordion(!!!panels,
                      open = names(pathway)[
                        identical(input$year_in_program, "5") + 1L
                      ],
                      multiple = TRUE)
  })

  # Populate the "Transfer / other" selectize once.
  observe({
    all_choices <- setNames(courses$short_code,
                             sprintf("%s — %s (%g cr)",
                                     courses$short_code,
                                     courses$title,
                                     courses$credits))
    updateSelectizeInput(session, "other_completed",
                         choices = all_choices, server = TRUE,
                         selected = isolate(input$other_completed))
  })

  completed_codes <- reactive({
    yr_ids <- c(
      paste0("y_", names(pathway), "_recommended"),
      paste0("y_", names(pathway), "_other")
    )
    picks <- unlist(lapply(yr_ids, function(id) input[[id]]),
                    use.names = FALSE)
    picks <- c(picks, input$other_completed)
    picks <- picks[!is.na(picks) & nzchar(picks)]
    unique(picks)
  })

  completed_rows <- reactive({
    codes <- completed_codes()
    if (!length(codes)) return(courses[0, , drop = FALSE])
    courses[courses$short_code %in% codes, , drop = FALSE]
  })

  # ---- Credit summary -----------------------------------------------------
  output$credit_summary <- renderUI({
    rows <- completed_rows()
    if (!nrow(rows)) {
      return(tags$div(class = "text-muted",
                      "Tick courses in Step 1 to populate this summary."))
    }
    total       <- sum(rows$credits, na.rm = TRUE)
    sci_rows    <- rows[rows$subject %in%
                         c("BIOL_O","CHEM_O","EESC_O","FWSC_O","GEOG_O",
                           "GISC_O","MATH_O","PHYS_O","STAT_O"), , drop = FALSE]
    sci_credits <- sum(sci_rows$credits, na.rm = TRUE)
    upper_rows  <- rows[suppressWarnings(as.integer(rows$number)) >= 300, ,
                        drop = FALSE]
    upper_cr    <- sum(upper_rows$credits, na.rm = TRUE)
    upper_sci   <- sum(upper_rows[upper_rows$subject %in%
                                    c("BIOL_O","CHEM_O","EESC_O","FWSC_O","GEOG_O",
                                      "GISC_O","MATH_O","PHYS_O","STAT_O"), "credits"],
                       na.rm = TRUE)

    chip <- function(label, value, target, units = "cr") {
      pct <- if (target > 0) min(value / target, 1) else 0
      ok  <- value >= target
      colour <- if (ok) "#1f7a3b" else if (pct >= 0.7) "#d9822b" else "#b30000"
      tags$div(
        class = "card mb-2",
        tags$div(class = "card-body p-3",
          tags$div(class = "d-flex justify-content-between align-items-baseline",
            tags$strong(label),
            tags$span(style = sprintf("color:%s; font-weight:600;", colour),
                      sprintf("%g / %g %s", value, target, units))
          ),
          tags$div(class = "progress mt-2", style = "height:6px;",
            tags$div(class = "progress-bar", role = "progressbar",
                     style = sprintf("width:%.1f%%; background:%s;",
                                     pct * 100, colour))
          )
        )
      )
    }

    tagList(
      chip("Total credits",                total, 120),
      chip("Science credits (BIOL/CHEM/EESC/FWSC/GEOG/GISC/MATH/PHYS/STAT)",
           sci_credits, 78),
      chip("Upper-level credits (300+)",   upper_cr, 42),
      chip("Upper-level Science credits",  upper_sci, 36)
    )
  })

  output$completed_table <- DT::renderDT({
    rows <- completed_rows()
    if (!nrow(rows)) return(DT::datatable(
      data.frame(Code = character(), Title = character(), Credits = integer()),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side:top; text-align:left;
                  font-size:0.9em; color:#54607a;",
        "Courses you have ticked so far. ",
        htmltools::tags$em("(None yet, tick courses in Step 1 to populate.)"))))
    DT::datatable(
      rows |> dplyr::transmute(Code = short_code, Title = title,
                                Credits = credits, LTP = ltp,
                                Prereqs = vapply(prereq_codes, function(j) {
                                  if (is.na(j) || !nzchar(j)) "" else
                                    paste(jsonlite::fromJSON(j), collapse = ", ")
                                }, character(1))),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side:top; text-align:left;
                  font-size:0.9em; color:#54607a;",
        sprintf("Courses you have ticked (%d), Code, Title, Credits, LTP pattern, and parsed prerequisites.",
                 nrow(rows))),
      options = list(pageLength = 15, dom = "tip",
                     columnDefs = list(list(className = "dt-left",
                                             targets = "_all")))
    )
  })

  # ---- Designation gap analysis ------------------------------------------
  render_category <- function(name, eligible_codes, completed) {
    have <- intersect(eligible_codes, completed)
    need <- setdiff(eligible_codes,  completed)
    n_have <- length(have)
    tags$div(class = "card mb-3",
      tags$div(class = "card-body p-3",
        tags$h5(class = "mb-2", name),
        tags$div(class = "d-flex gap-2 mb-2",
          tags$span(class = "badge bg-success",
                    sprintf("Have: %d", n_have)),
          tags$span(class = "badge bg-secondary",
                    sprintf("Eligible total: %d", length(eligible_codes)))
        ),
        if (n_have)
          tags$div(class = "small text-muted",
                   tags$strong("Already ticked: "),
                   paste(have, collapse = ", "))
        else
          tags$div(class = "small text-danger",
                   "No completed courses recognised in this category yet.")
      )
    )
  }

  output$designation_summary <- renderUI({
    completed <- short_code(completed_codes())
    if (!length(input$goals)) {
      return(tags$div(class = "text-muted p-3",
        "Tick one or both target designations in the sidebar to see the
         gap analysis here."))
    }
    if (!length(completed)) {
      return(tags$div(class = "text-muted p-3",
        "Tick some completed courses in Step 1 first."))
    }
    blocks <- list()
    if ("pag" %in% input$goals) {
      cats <- pag$categories
      blocks[[length(blocks) + 1]] <- tags$div(
        tags$h4("PAg - BC Institute of Agrologists"),
        tags$p(class = "small text-muted",
          "Need: 8 foundational + 20 agrology (with at least 8 senior)."),
        render_category(cats$foundational$name,
                         pag_category_codes(cats$foundational), completed),
        render_category(cats$agrology_lower$name,
                         pag_category_codes(cats$agrology_lower), completed),
        render_category(cats$agrology_senior$name,
                         pag_category_codes(cats$agrology_senior), completed),
        tags$hr()
      )
    }
    if ("rpbio" %in% input$goals) {
      cats <- rpbio$categories
      blocks[[length(blocks) + 1]] <- tags$div(
        tags$h4("RPBio - College of Applied Biologists"),
        tags$p(class = "small text-muted",
          "Need: 25 science / 13 biology / at least 3 of the 5
           subject areas (genetics, cell, physiology, systematics,
           evolution). UBCO does not currently offer a systematics
           course - confirm a transfer plan with the program advisor."),
        tags$h5("Required core areas"),
        render_category(cats$communications$name,
                         rpbio_area_codes(cats$communications), completed),
        render_category(cats$chemistry$name,
                         rpbio_area_codes(cats$chemistry), completed),
        render_category(cats$numeracy$name,
                         rpbio_area_codes(cats$numeracy), completed),
        render_category(cats$statistics_second_year_or_higher$name,
                         rpbio_area_codes(cats$statistics_second_year_or_higher),
                         completed),
        render_category(cats$applied_biology_management$name,
                         rpbio_area_codes(cats$applied_biology_management),
                         completed),
        render_category(cats$ecology$name,
                         rpbio_area_codes(cats$ecology), completed),
        tags$h5("Three-of-five subject areas"),
        render_category(cats$genetics$name,
                         rpbio_area_codes(cats$genetics), completed),
        render_category(cats$cell_biology$name,
                         rpbio_area_codes(cats$cell_biology), completed),
        render_category(cats$physiology$name,
                         rpbio_area_codes(cats$physiology), completed),
        render_category(cats$systematics_taxonomy$name,
                         rpbio_area_codes(cats$systematics_taxonomy), completed),
        render_category(cats$evolution$name,
                         rpbio_area_codes(cats$evolution), completed),
        tags$hr(),
        tags$h5("Biology-count check (RPBio needs 13)"),
        tags$div(class = "card",
          tags$div(class = "card-body p-3",
            tags$p(sprintf("You have %d of the 13 biology courses CAB counts.",
                            length(intersect(completed,
                                             rpbio$biology_count_eligible_codes)))),
            if (length(intersect(completed, rpbio$biology_count_eligible_codes)))
              tags$div(class = "small text-muted",
                tags$strong("Eligible biology courses on your transcript: "),
                paste(intersect(completed, rpbio$biology_count_eligible_codes),
                       collapse = ", "))
          )
        )
      )
    }
    do.call(tagList, blocks)
  })

  # ---- Report generation -------------------------------------------------
  # Build a single HTML document summarising the student's plan + gap
  # analysis. The HTML download is always available; the PDF button
  # routes through pagedown::chrome_print() if Chrome is reachable,
  # otherwise falls back to the same HTML so a click is never lost.
  build_report_html <- function() {
    completed <- short_code(completed_codes())
    rows      <- completed_rows()
    goals     <- input$goals %||% character(0)
    year_in   <- as.integer(input$year_in_program %||% 0L)

    when <- format(Sys.time(), "%d %B %Y, %H:%M %Z")

    total       <- sum(rows$credits, na.rm = TRUE)
    sci_rows    <- rows[rows$subject %in%
                         c("BIOL_O","CHEM_O","EESC_O","FWSC_O","GEOG_O",
                           "GISC_O","MATH_O","PHYS_O","STAT_O"), , drop = FALSE]
    sci_credits <- sum(sci_rows$credits, na.rm = TRUE)
    upper_rows  <- rows[suppressWarnings(as.integer(rows$number)) >= 300, ,
                        drop = FALSE]
    upper_cr    <- sum(upper_rows$credits, na.rm = TRUE)
    upper_sci   <- sum(upper_rows[upper_rows$subject %in%
                                    c("BIOL_O","CHEM_O","EESC_O","FWSC_O","GEOG_O",
                                      "GISC_O","MATH_O","PHYS_O","STAT_O"),
                                   "credits"], na.rm = TRUE)

    threshold_row <- function(label, value, target) {
      ok <- value >= target
      tags$tr(
        tags$td(label),
        tags$td(sprintf("%g", value)),
        tags$td(sprintf("%g", target)),
        tags$td(if (ok) "Met" else "Not yet",
                style = sprintf("color:%s; font-weight:600;",
                                if (ok) "#1f7a3b" else "#b30000"))
      )
    }

    designation_block <- function() {
      if (!length(goals)) {
        return(tags$p("No designation goals selected."))
      }
      tags$div(
        if ("pag" %in% goals) {
          cats <- pag$categories
          tags$section(
            tags$h2("PAg - BC Institute of Agrologists"),
            tags$p(em("Need: 8 foundational + 20 agrology with at least 8 senior.")),
            cat_block(cats$foundational, completed),
            cat_block(cats$agrology_lower, completed),
            cat_block(cats$agrology_senior, completed)
          )
        },
        if ("rpbio" %in% goals) {
          cats <- rpbio$categories
          n_bio <- length(intersect(completed,
                                     rpbio$biology_count_eligible_codes))
          tags$section(
            tags$h2("RPBio - College of Applied Biologists"),
            tags$p(em("Need: 25 science / 13 biology / at least 3 of the 5 subject areas.")),
            tags$h3("Required core areas"),
            cat_block_rp(cats$communications, completed),
            cat_block_rp(cats$chemistry, completed),
            cat_block_rp(cats$numeracy, completed),
            cat_block_rp(cats$statistics_second_year_or_higher, completed),
            cat_block_rp(cats$applied_biology_management, completed),
            cat_block_rp(cats$ecology, completed),
            tags$h3("Three-of-five subject areas"),
            cat_block_rp(cats$genetics, completed),
            cat_block_rp(cats$cell_biology, completed),
            cat_block_rp(cats$physiology, completed),
            cat_block_rp(cats$systematics_taxonomy, completed),
            cat_block_rp(cats$evolution, completed),
            tags$h3("Biology-count check"),
            tags$p(sprintf("You have %d of the 13 biology courses CAB counts.", n_bio))
          )
        }
      )
    }

    cat_block <- function(cat, completed) {
      have <- intersect(pag_category_codes(cat), completed)
      tags$div(style = "margin: 10px 0; padding: 10px; border: 1px solid #d6deea; border-radius: 6px;",
        tags$strong(cat$name),
        tags$br(),
        tags$span(sprintf("Have: %d of %d eligible. ",
                          length(have), length(pag_category_codes(cat)))),
        if (length(have))
          tags$div(style = "color:#54607a; font-size:0.9em;",
                   paste("Counted:", paste(have, collapse = ", ")))
      )
    }
    cat_block_rp <- function(cat, completed) {
      have <- intersect(rpbio_area_codes(cat), completed)
      tags$div(style = "margin: 8px 0; padding: 8px 10px; border: 1px solid #d6deea; border-radius: 6px;",
        tags$strong(cat$name),
        tags$br(),
        tags$span(sprintf("Have: %d of %d eligible. ",
                          length(have), length(rpbio_area_codes(cat)))),
        if (length(have))
          tags$div(style = "color:#54607a; font-size:0.9em;",
                   paste("Counted:", paste(have, collapse = ", ")))
        else
          tags$div(style = "color:#b30000; font-size:0.9em;",
                   "No completed courses recognised in this category yet.")
      )
    }

    completed_table <- if (nrow(rows)) {
      tags$table(class = "course-list",
        tags$thead(tags$tr(tags$th("Code"), tags$th("Title"),
                            tags$th("Credits"))),
        tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
          tags$tr(tags$td(rows$short_code[i]),
                  tags$td(rows$title[i]),
                  tags$td(rows$credits[i]))
        }))
      )
    } else tags$p(em("(No courses ticked.)"))

    htmltools::tagList(
      tags$head(
        tags$meta(charset = "utf-8"),
        tags$title("UBCO Freshwater Science - Personal degree plan"),
        tags$style(htmltools::HTML("
          body { font-family: -apple-system, 'Helvetica Neue', sans-serif;
                 max-width: 900px; margin: 28px auto; padding: 0 24px;
                 color: #1f3a5f; line-height: 1.45; }
          h1 { color: #1f3a5f; border-bottom: 3px solid #1f3a5f;
                padding-bottom: 6px; }
          h2 { color: #1f3a5f; margin-top: 28px; }
          h3 { color: #2c517f; margin-top: 18px; }
          table { border-collapse: collapse; width: 100%; margin: 12px 0; }
          th, td { border: 1px solid #d6deea; padding: 6px 10px;
                    text-align: left; }
          th { background: #f4f7fb; }
          .course-list { font-size: 0.92em; }
          .meta { background: #f4f7fb; padding: 12px 16px;
                   border-radius: 8px; font-size: 0.95em; }

          /* Print rules: keep groupings together so the report does
             not break mid-table or orphan a heading at the bottom of
             a page. The browser's Save-as-PDF dialog respects these. */
          @media print {
            @page { margin: 18mm 16mm; }
            body  { margin: 0; padding: 0; }

            /* Headings stay with the content that follows. */
            h1, h2, h3, h4 {
              page-break-after: avoid;
              break-after: avoid;
            }
            h2 { page-break-before: auto; break-before: auto; }

            /* Keep tables together when they fit on one page;
               for tall tables, never orphan a single row. */
            table       { page-break-inside: avoid; break-inside: avoid; }
            thead       { display: table-header-group; }
            tfoot       { display: table-footer-group; }
            tr          { page-break-inside: avoid; break-inside: avoid; }

            /* Sections (designation card, contact block, meta box)
               stay together as a unit. */
            section, .meta, [class*='course-list'],
            div[style*='border'] {
              page-break-inside: avoid;
              break-inside: avoid;
            }

            /* Big-table fallback: if the PAg senior-agrology table
               (85 rows) cannot fit on one page, allow breaks but
               repeat headers and don't orphan trailing rows. */
            table.course-list { page-break-inside: auto;
                                break-inside: auto; }

            /* Hide any tooltips / hover-only elements that might
               accidentally render in print. */
            [class*='popover'], [class*='tooltip'] { display: none !important; }
          }
        "))
      ),
      tags$body(
        tags$h1("Personal degree plan - UBCO Freshwater Science"),
        tags$div(class = "meta",
          tags$div(tags$strong("Generated: "), when),
          tags$div(tags$strong("Year in program: "),
                   if (year_in == 5L) "Beyond Year 4 / Completed"
                   else sprintf("Year %d", year_in)),
          tags$div(tags$strong("Designation goals: "),
                   if (length(goals)) paste(goals, collapse = ", ")
                   else "none selected")
        ),
        tags$h2("Credit and degree-threshold summary"),
        tags$table(
          tags$thead(tags$tr(tags$th("Threshold"),
                              tags$th("You have"),
                              tags$th("Target"),
                              tags$th("Status"))),
          tags$tbody(
            threshold_row("Total credits", total, 120),
            threshold_row("Science credits", sci_credits, 78),
            threshold_row("Upper-level credits (300+)", upper_cr, 42),
            threshold_row("Upper-level Science credits", upper_sci, 36)
          )
        ),
        tags$h2("Designation gap analysis"),
        designation_block(),
        tags$h2("Courses you ticked"),
        completed_table,

        # ---- Advisor contact block (printed on every report) ----------
        tags$div(
          style = paste("margin: 24px 0; padding: 18px 22px;",
                         "background: #f4f7fb; border: 1px solid #d6deea;",
                         "border-radius: 10px;"),
          tags$h2(style = "margin-top: 0;", "Need more help?"),
          tags$p(
            tags$strong("Dr. Nelson Jatel, PAg"), tags$br(),
            "Lecturer and Freshwater Science Program Advisor", tags$br(),
            "Department of Earth, Environmental and Geographic Sciences,
             UBC Okanagan"),
          tags$p(
            tags$strong("Email: "),
            tags$a(href = "mailto:nelson.jatel@ubc.ca?subject=UBCO%20Freshwater%20Science%20-%20Advising%20appointment",
                    "nelson.jatel@ubc.ca")
          ),
          tags$p(class = "muted-note", style = "margin-bottom:0;",
            "Email to book a 30-minute advising appointment to walk
             through your plan, ask about designation pathways
             (PAg, RPBio), or talk through Co-op, NSERC USRA, and
             Honours options.")
        ),

        tags$hr(),
        tags$p(style = "font-size:0.85em; color:#54607a;",
          em("Illustrative report. The "),
          tags$a(href = "https://okanagan.calendar.ubc.ca/",
                  "UBC Okanagan Academic Calendar"),
          em(" is the authoritative source for all degree requirements. "),
          em("Confirm any planning decision with the Freshwater Science "),
          em("Program Advisor (nelson.jatel@ubc.ca).")
        )
      )
    )
  }

  output$download_report_html <- downloadHandler(
    filename = function()
      sprintf("UBCO_FWSc_plan_%s.html", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      htmltools::save_html(build_report_html(), file = file,
                            libdir = NULL)
    },
    contentType = "text/html"
  )

  # "Open printable report" button: render the report to an HTML
  # string and push it to the JS handler in the UI, which opens the
  # HTML in a new tab and triggers the browser's print dialog. Modern
  # browsers (Chrome, Edge, Safari, Firefox) all have "Save as PDF"
  # in the print dialog by default, so the student gets a real PDF
  # without any server-side PDF rendering.
  # ---- Progress summary band + per-year chip math ------------------------
  # Numbers used by both the top-of-Step-1 summary and the per-year
  # accordion-title chips. Single source of truth so they can't drift.
  recommended_per_year <- reactive({
    setNames(
      lapply(pathway, function(yspec) yspec$recommended %||% character(0)),
      names(pathway)
    )
  })

  pag_required_codes <- reactive({
    cats <- pag$categories
    unique(c(pag_category_codes(cats$foundational),
              pag_category_codes(cats$agrology_lower),
              pag_category_codes(cats$agrology_senior)))
  })
  rpbio_required_codes <- reactive({
    cats <- rpbio$categories
    unique(unlist(lapply(cats, function(c) rpbio_area_codes(c))))
  })

  output$progress_summary <- renderUI({
    completed <- completed_codes()
    goals     <- input$goals %||% character(0)
    if (!length(completed) && !length(goals)) return(NULL)

    chip <- function(label, have, total, colour) {
      pct <- if (total > 0) round(100 * have / total) else 0
      tags$span(
        class = "badge",
        style = sprintf(paste("background:%s; color:white;",
                                "padding:6px 10px; border-radius:999px;",
                                "font-size:0.85em; margin-right:6px;"),
                         colour),
        sprintf("%s: %d / %d (%d%%)", label, have, total, pct)
      )
    }

    chips <- tagList()
    rec_total <- sum(lengths(recommended_per_year()))
    rec_have  <- sum(completed %in% unlist(recommended_per_year()))
    chips <- tagAppendChild(
      chips,
      chip("Recommended courses ticked", rec_have, rec_total, "#1f3a5f")
    )

    if ("pag" %in% goals) {
      need <- pag_required_codes()
      have <- length(intersect(completed, need))
      chips <- tagAppendChild(chips,
                               chip("PAg-eligible UBCO courses ticked",
                                    have, length(need), "#1f7a3b"))
    }
    if ("rpbio" %in% goals) {
      need <- rpbio_required_codes()
      have <- length(intersect(completed, need))
      chips <- tagAppendChild(chips,
                               chip("RPBio-eligible UBCO courses ticked",
                                    have, length(need), "#7a2c8a"))
    }

    tags$div(
      style = paste("background:#f4f7fb; border:1px solid #d6deea;",
                     "border-radius:8px; padding:12px 16px; margin:10px 0 14px;"),
      tags$div(style = "font-weight:600; color:#1f3a5f; margin-bottom:8px;",
               "Live progress"),
      chips
    )
  })

  # ---- Save / load plan ---------------------------------------------------
  output$download_plan <- downloadHandler(
    filename = function()
      sprintf("UBCO_FWSc_plan_%s.json", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      payload <- list(
        version = "1.0",
        saved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        year_in_program = input$year_in_program %||% "",
        goals = input$goals %||% character(0),
        completed = completed_codes()
      )
      writeLines(jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE),
                 file)
    },
    contentType = "application/json"
  )

  observeEvent(input$upload_plan, {
    f <- input$upload_plan
    req(f)
    tryCatch({
      payload <- jsonlite::fromJSON(f$datapath, simplifyVector = TRUE)
      if (!is.null(payload$goals))
        updateCheckboxGroupInput(session, "goals",
                                  selected = as.character(payload$goals))
      if (!is.null(payload$year_in_program))
        updateRadioButtons(session, "year_in_program",
                            selected = as.character(payload$year_in_program))
      to_set <- as.character(payload$completed %||% character(0))
      # Split the restored codes by year-recommended vs other.
      for (yk in names(pathway)) {
        rec_id   <- paste0("y_", yk, "_recommended")
        other_id <- paste0("y_", yk, "_other")
        rec_codes <- pathway[[yk]]$recommended %||% character(0)
        rec_pick  <- intersect(to_set, rec_codes)
        updateCheckboxGroupInput(session, rec_id, selected = rec_pick)
        # Anything from this year's level band that isn't in
        # recommended goes to the "other" selectize.
        level <- YEAR_TO_BANDS[[yk]]
        other_pool <- courses$short_code[courses$level_band == level]
        other_pick <- intersect(to_set, setdiff(other_pool, rec_codes))
        updateSelectizeInput(session, other_id, selected = other_pick,
                              server = TRUE,
                              choices = setNames(
                                courses$short_code[courses$level_band == level],
                                sprintf("%s, %s (%g cr)",
                                         courses$short_code[courses$level_band == level],
                                         courses$title[courses$level_band == level],
                                         courses$credits[courses$level_band == level])))
      }
      # Everything left (no year mapping) goes to "other_completed".
      placed <- unique(unlist(lapply(names(pathway), function(yk) {
        c(intersect(to_set, pathway[[yk]]$recommended %||% character(0)),
          intersect(to_set, courses$short_code[
            courses$level_band == YEAR_TO_BANDS[[yk]]]))
      })))
      transfers <- setdiff(to_set, placed)
      updateSelectizeInput(session, "other_completed", selected = transfers,
                            server = TRUE,
                            choices = setNames(courses$short_code,
                              sprintf("%s, %s (%g cr)",
                                      courses$short_code, courses$title,
                                      courses$credits)))
      showNotification(
        sprintf("Plan loaded: %d courses, goals: %s, year: %s.",
                length(to_set),
                paste(payload$goals %||% "none", collapse = ", "),
                payload$year_in_program %||% "n/a"),
        type = "message", duration = 6)
    }, error = function(e) {
      showNotification(
        paste("Could not load plan:", conditionMessage(e)),
        type = "error", duration = 10)
    })
  })

  # ---- Year board (years-as-columns, Notion-style) -----------------------
  # Read-only horizontal board: one column per year of the curated
  # pathway. Cards are coloured by tick state. "Other ticked at this
  # level" courses fall to the bottom of the column under a dashed
  # rule, so transfers and electives still show up where they belong.
  output$year_board <- renderUI({
    completed <- completed_codes()
    palette <- c(year_1 = "#94c5d4", year_2 = "#6aa6c4",
                  year_3 = "#3f7da9", year_4 = "#1f3a5f",
                  summer = "#5c4e7a")

    card <- function(code, ticked) {
      ttl <- code_to_title[code] %||% NA_character_
      cr  <- code_to_credits[code] %||% NA_real_
      bg  <- if (isTRUE(ticked)) "#e3efe8" else "#ffffff"
      border <- if (isTRUE(ticked)) "#1f7a3b" else "#c8d1de"
      check <- if (isTRUE(ticked))
        tags$span(style = "color:#1f7a3b; font-weight:700;", "✓ ")
      else
        tags$span(style = "color:#c8d1de;", "○ ")
      tags$div(
        style = sprintf(paste("background:%s; border:1px solid %s;",
                                "border-radius:8px; padding:8px 10px;",
                                "margin-bottom:6px; font-size:0.85em;",
                                "line-height:1.25;"),
                         bg, border),
        tags$div(check,
                  tags$strong(code),
                  if (!is.na(cr)) tags$span(class = "text-muted",
                    style = "float:right; font-size:0.85em;",
                    sprintf("%g cr", cr))),
        if (!is.na(ttl))
          tags$div(class = "text-muted", style = "font-size:0.85em;", ttl)
      )
    }

    columns <- lapply(names(pathway), function(yk) {
      yspec <- pathway[[yk]]
      rec <- yspec$recommended %||% character(0)
      have <- intersect(rec, completed)
      n_have <- length(have); n_total <- length(rec)
      chip_colour <- if (n_have == n_total && n_total > 0) "#1f7a3b"
                     else if (n_have > 0) "#d9822b"
                     else "#a0a4ad"
      header_colour <- palette[[yk]] %||% "#1f3a5f"

      # "Other ticked at this level": courses from this column's level
      # band that are ticked but not in the recommended list.
      level <- YEAR_TO_BANDS[[yk]]
      other_ticked <- completed[
        completed %in% courses$short_code[courses$level_band == level] &
        !completed %in% rec
      ]
      # Don't double-list: only assign "other ticked" to the FIRST
      # year column matching that level band. summer + year_4 share
      # "400+", so attribute 400+ extras to year_4.
      if (yk == "summer") other_ticked <- character(0)

      tags$div(
        style = paste("flex:1 1 0; min-width:200px; max-width:280px;",
                       "background:#f7f9fc; border:1px solid #d6deea;",
                       "border-radius:10px; padding:12px;"),
        tags$div(
          style = sprintf(paste("border-bottom:3px solid %s;",
                                  "padding-bottom:6px; margin-bottom:10px;"),
                           header_colour),
          tags$div(style = "font-weight:700; color:#1f3a5f;",
                    yspec$label),
          tags$span(style = sprintf(paste("background:%s; color:white;",
                                            "padding:2px 8px;",
                                            "border-radius:999px;",
                                            "font-size:0.75em;"),
                                     chip_colour),
                    sprintf("%d / %d ticked", n_have, n_total))
        ),
        lapply(rec, function(c) card(c, c %in% completed)),
        if (length(other_ticked)) tagList(
          tags$div(style = paste("margin:10px 0 6px;",
                                   "border-top:1px dashed #c8d1de;",
                                   "padding-top:8px;",
                                   "font-size:0.75em; color:#54607a;"),
                    sprintf("Other ticked at %s-level (%d)",
                            level, length(other_ticked))),
          lapply(other_ticked, function(c) card(c, TRUE))
        )
      )
    })

    tags$div(
      style = paste("display:flex; gap:12px; flex-wrap:wrap;",
                     "align-items:flex-start; margin-top:6px;"),
      columns
    )
  })

  # ---- Prerequisite map (visNetwork) -------------------------------------
  # Build nodes + edges from the live scraped prereq_codes column. The
  # node set is restricted to the curated pathway + every code that
  # appears in either PAg or RPBio mapping plus any direct prereq of
  # those. Keeps the map readable (~70 nodes vs. 626).
  map_data <- reactive({
    in_pathway <- unique(unlist(lapply(pathway, function(y) y$recommended)))
    in_pag     <- pag_required_codes()
    in_rpbio   <- rpbio_required_codes()
    core_codes <- short_code(unique(c(in_pathway, in_pag, in_rpbio)))
    # Pull rows in the courses table by short_code.
    core_rows <- courses[courses$short_code %in% core_codes, , drop = FALSE]
    # Add direct prereqs of core courses (one hop upstream).
    prereq_codes_list <- lapply(core_rows$prereq_codes, function(j) {
      if (is.na(j) || !nzchar(j)) character(0)
      else short_code(jsonlite::fromJSON(j))
    })
    direct_prereqs <- unique(unlist(prereq_codes_list))
    keep_codes <- unique(c(core_rows$short_code, direct_prereqs))
    nodes_df <- courses[courses$short_code %in% keep_codes, , drop = FALSE]
    # Build edges: prereq -> course.
    edges_list <- lapply(seq_len(nrow(nodes_df)), function(i) {
      j <- nodes_df$prereq_codes[i]
      if (is.na(j) || !nzchar(j)) return(NULL)
      pre <- short_code(jsonlite::fromJSON(j))
      pre <- intersect(pre, nodes_df$short_code)
      if (!length(pre)) return(NULL)
      data.frame(from = pre, to = nodes_df$short_code[i],
                 stringsAsFactors = FALSE)
    })
    edges_df <- do.call(rbind, edges_list)
    list(nodes = nodes_df, edges = edges_df)
  })

  output$prereq_map <- visNetwork::renderVisNetwork({
    md <- map_data()
    if (!nrow(md$nodes)) return(visNetwork(data.frame(), data.frame()))
    completed <- completed_codes()

    # Level palette.
    level_colour <- c(`100` = "#94c5d4", `200` = "#6aa6c4",
                      `300` = "#3f7da9", `400+` = "#1f3a5f")
    nodes <- data.frame(
      id    = md$nodes$short_code,
      label = md$nodes$short_code,
      title = sprintf("<b>%s</b><br>%s<br><i>%g cr</i><br><span style='color:#666;'>%s</span>",
                       md$nodes$short_code,
                       md$nodes$title,
                       md$nodes$credits,
                       ifelse(is.na(md$nodes$prereq_raw) | !nzchar(md$nodes$prereq_raw),
                              "(no listed prerequisite)",
                              paste("Prereq:", md$nodes$prereq_raw))),
      level = match(md$nodes$level_band, names(level_colour)),
      color.background = ifelse(md$nodes$short_code %in% completed,
                                 level_colour[md$nodes$level_band],
                                 "#ffffff"),
      color.border = level_colour[md$nodes$level_band],
      color.highlight.background = level_colour[md$nodes$level_band],
      color.highlight.border = "#000000",
      font.color = ifelse(md$nodes$short_code %in% completed, "#ffffff", "#1f3a5f"),
      borderWidth = 2,
      shape = "box",
      stringsAsFactors = FALSE
    )

    edges <- if (!is.null(md$edges) && nrow(md$edges))
      data.frame(from = md$edges$from, to = md$edges$to,
                 arrows = "to", color = list(color = "#a9b6c8",
                                              highlight = "#1f3a5f"),
                 smooth = FALSE,
                 stringsAsFactors = FALSE)
    else data.frame()

    vn <- visNetwork(nodes, edges, width = "100%") |>
      visNetwork::visHierarchicalLayout(
        direction = "UD",
        sortMethod = "directed",
        levelSeparation = 110,
        nodeSpacing = 130,
        treeSpacing = 200,
        blockShifting = TRUE,
        edgeMinimization = TRUE) |>
      visNetwork::visOptions(
        highlightNearest = list(enabled = TRUE, degree = 2,
                                 hover = TRUE),
        nodesIdSelection = list(enabled = TRUE,
                                 main = "Find a course...",
                                 style = "width:240px;")) |>
      visNetwork::visInteraction(
        navigationButtons = TRUE,
        tooltipDelay = 200) |>
      visNetwork::visLegend(
        useGroups = FALSE,
        addNodes = list(
          list(label = "100-level", shape = "box", color = level_colour[["100"]]),
          list(label = "200-level", shape = "box", color = level_colour[["200"]]),
          list(label = "300-level", shape = "box", color = level_colour[["300"]]),
          list(label = "400+ level", shape = "box", color = level_colour[["400+"]]),
          list(label = "Ticked (filled)", shape = "box",
               color = list(background = level_colour[["300"]],
                            border = level_colour[["300"]])),
          list(label = "Not ticked (outline)", shape = "box",
               color = list(background = "#ffffff",
                            border = level_colour[["300"]]))),
        position = "right", width = 0.18)
    vn
  })

  observeEvent(input$open_report, {
    tmp <- tempfile(fileext = ".html")
    htmltools::save_html(build_report_html(), file = tmp, libdir = NULL)
    html_str <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    unlink(tmp)
    session$sendCustomMessage("openPrintReport",
                               list(html = html_str))
  })

}

shinyApp(ui, server)
