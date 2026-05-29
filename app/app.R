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
})

# pagedown gives us HTML -> PDF via headless Chrome. We require it
# lazily inside the download handler so the app still loads if the
# package is unavailable.

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------- Data load (works whether CWD is project root or /app) ----------
.proj <- if (dir.exists("data")) "." else ".."
.data_dir <- file.path(.proj, "data")

courses <- arrow::read_parquet(file.path(.data_dir, "courses.parquet"))
pag     <- yaml::read_yaml(file.path(.data_dir, "designation_mappings", "pag.yml"))
rpbio   <- yaml::read_yaml(file.path(.data_dir, "designation_mappings", "rpbio.yml"))

# Normalise both sources to a consistent "BIOL 116" / "FWSC 375" style.
# The scrape produces "BIOL_O 116"; the YAMLs use "BIOL 116".
short_code <- function(x) stringr::str_replace(x, "_O", "")
courses$short_code <- short_code(courses$code)

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
    div(class = "small text-muted",
        "MVP - results are illustrative only. ",
        "Confirm with the program advisor and the UBC Okanagan Academic Calendar.")
  ),

  navset_tab(
    nav_panel(
      "Step 1: Tick completed courses",
      div(class = "p-3",
          h4("Tick everything on your transcript"),
          p(class = "text-muted small",
            "Filter by subject prefix, course number, or title. The
             selection drives every panel in Steps 2 and 3."),
          DT::DTOutput("course_picker", height = "62vh")
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
          div(style = "margin-bottom: 14px;",
              downloadButton("download_report",
                              "Download report (PDF)",
                              class = "btn-primary"),
              downloadButton("download_report_html",
                              "Download report (HTML)",
                              class = "btn-outline-secondary",
                              style = "margin-left:8px;")),
          uiOutput("designation_summary")
      )
    )
  )
)

# ------- Server -----------------------------------------------------------
server <- function(input, output, session) {

  # Picker table: every course, sortable + filterable.
  picker_df <- reactive({
    courses |>
      dplyr::transmute(
        Code    = short_code,
        Title   = title,
        Credits = credits,
        Subject = SUBJECT_LABELS[subject]
      ) |>
      dplyr::arrange(Code)
  })

  output$course_picker <- DT::renderDT({
    DT::datatable(
      picker_df(),
      filter = "top",
      rownames = FALSE,
      selection = list(mode = "multiple", target = "row"),
      options = list(pageLength = 25, scrollX = FALSE,
                     dom = "tip",
                     columnDefs = list(list(className = "dt-left",
                                             targets = "_all")))
    )
  }, server = TRUE)

  completed_codes <- reactive({
    sel <- input$course_picker_rows_selected
    if (!length(sel)) return(character(0))
    picker_df()$Code[sel]
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
      rownames = FALSE))
    DT::datatable(
      rows |> dplyr::transmute(Code = short_code, Title = title,
                                Credits = credits, LTP = ltp,
                                Prereqs = vapply(prereq_codes, function(j) {
                                  if (is.na(j) || !nzchar(j)) "" else
                                    paste(jsonlite::fromJSON(j), collapse = ", ")
                                }, character(1))),
      rownames = FALSE,
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
          @media print {
            body { margin: 0; padding: 18mm; }
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

  output$download_report <- downloadHandler(
    filename = function()
      sprintf("UBCO_FWSc_plan_%s.pdf", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      htmltools::save_html(build_report_html(), file = tmp_html,
                            libdir = NULL)
      tryCatch({
        if (!requireNamespace("pagedown", quietly = TRUE)) {
          stop("pagedown not available")
        }
        pagedown::chrome_print(tmp_html, output = file, format = "pdf",
                                wait = 1)
      }, error = function(e) {
        # Fallback: ship the HTML with a .pdf extension renamed to
        # .html so the user still gets something useful. We rename
        # by writing the HTML to `file` directly.
        showNotification(
          paste0("PDF rendering unavailable on the server (",
                 conditionMessage(e),
                 "). Downloading the HTML version instead - ",
                 "use your browser's File > Print > Save as PDF."),
          type = "warning", duration = 15)
        file.copy(tmp_html, file, overwrite = TRUE)
      })
    },
    contentType = "application/pdf"
  )

}

shinyApp(ui, server)
