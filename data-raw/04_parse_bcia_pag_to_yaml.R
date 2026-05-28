#!/usr/bin/env Rscript
# data-raw/04_parse_bcia_pag_to_yaml.R
# Parse the BCIA UBCO Approved PAg Course List text into a structured
# YAML the designation page + planner can read directly.
#
# Input:  docs/reference/BCIA_UBCO_PAg_2025-06-27.txt
#         (committed text-layer extraction of the BCIA PDF)
# Output: data/designation_mappings/pag.yml
#
# The text uses a consistent table format:
#
#   100-200 Agrology Courses
#
#          Course ID            Title
#   BIOL 204                    Vertebrate Structure and Function
#   EESC 205 (GEOG 205)         Introduction to Hydrology
#   ...
#
#   300-400+ Agrology Courses
#   ...
#
#   Foundational Natural Science Courses
#   ...
#
# The parser tracks the current section and emits one row per course
# code, capturing the section, the primary code, any cross-listings,
# and the title.

suppressPackageStartupMessages({
  library(stringr)
  library(yaml)
})

INPUT  <- "docs/reference/BCIA_UBCO_PAg_2025-06-27.txt"
OUTPUT <- "data/designation_mappings/pag.yml"

# Recognised section headings (verbatim from the BCIA PDF).
SECTION_HEADINGS <- c(
  "100-200 Agrology Courses"            = "agrology_100_200",
  "300-400+ Agrology Courses"           = "agrology_300_plus",
  "Foundational Natural Science Courses" = "foundational_natural_science"
)

# Line patterns:
#   "BIOL 204                    Vertebrate Structure and Function"
#   "EESC 205 (GEOG 205)         Introduction to Hydrology"
#   "FDSY 221 (GEOG 221)         Food Systems 1: System Thinking"
COURSE_LINE_RE <- "^([A-Z]{3,5}\\s+\\d{3}[A-Z]?)(?:\\s+\\(([^)]+)\\))?\\s{2,}(.+?)\\s*$"

main <- function() {
  if (!file.exists(INPUT)) stop("Missing ", INPUT, call. = FALSE)
  lines <- readLines(INPUT, warn = FALSE)
  current_section <- NA_character_
  rows <- list()
  for (raw in lines) {
    line <- stringr::str_trim(raw)
    if (!nzchar(line)) next
    # Section heading?
    sec_match <- SECTION_HEADINGS[line]
    if (!is.na(sec_match)) {
      current_section <- unname(sec_match)
      next
    }
    # Skip the table header row.
    if (stringr::str_detect(line, "^Course ID")) next
    if (is.na(current_section)) next
    m <- stringr::str_match(line, COURSE_LINE_RE)
    if (is.na(m[1, 1])) next
    primary <- stringr::str_squish(m[1, 2])
    cross   <- stringr::str_squish(m[1, 3])  # may be NA
    title   <- stringr::str_squish(m[1, 4])
    # Cross-listing may itself be a comma-separated list of codes.
    cross_codes <- if (!is.na(cross) && nzchar(cross)) {
      codes <- stringr::str_extract_all(
        cross, "[A-Z]{3,5}\\s+\\d{3}[A-Z]?")[[1]]
      stringr::str_squish(codes)
    } else character(0)
    rows[[length(rows) + 1]] <- list(
      code        = primary,
      title       = title,
      cross_codes = cross_codes,
      section     = current_section
    )
  }
  if (!length(rows)) stop("Parsed zero courses; revisit the parser.",
                          call. = FALSE)

  # Group by section so the YAML is human-readable.
  by_sec <- split(rows, vapply(rows, function(r) r$section,
                                character(1)))

  payload <- list(
    designation        = "PAg",
    designation_body   = "BC Institute of Agrologists (BCIA)",
    source_document    = INPUT,
    source_document_url = paste0(
      "https://www.bcia.com/sites/default/files/docs/resources/",
      "UBCO%20Approved%20Courses_June%2027,%202025.pdf"
    ),
    source_dated       = "2025-06-27",
    parsed_at          = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
                                 tz = "UTC"),
    categories         = list(
      foundational      = list(
        name        = "Foundational (8 required, 100/200-level)",
        description = "Entry-level foundational courses from BCIA's Academic Worksheet categories.",
        courses     = lapply(by_sec[["foundational_natural_science"]] %||% list(),
                              function(r) {
                                list(code = r$code,
                                     title = r$title,
                                     cross_codes = r$cross_codes)
                              })
      ),
      agrology_lower    = list(
        name        = "Agrology - 100/200 level",
        description = "Lower-level agrology courses BCIA accepts toward the 20-course requirement.",
        courses     = lapply(by_sec[["agrology_100_200"]] %||% list(),
                              function(r) {
                                list(code = r$code,
                                     title = r$title,
                                     cross_codes = r$cross_codes)
                              })
      ),
      agrology_senior   = list(
        name        = "Agrology - 300/400+ level (>=8 senior)",
        description = "Senior-level agrology courses BCIA accepts toward the 20-course / 8-senior requirement.",
        courses     = lapply(by_sec[["agrology_300_plus"]] %||% list(),
                              function(r) {
                                list(code = r$code,
                                     title = r$title,
                                     cross_codes = r$cross_codes)
                              })
      )
    )
  )

  if (!dir.exists(dirname(OUTPUT))) dir.create(dirname(OUTPUT),
                                                recursive = TRUE)
  yaml::write_yaml(payload, OUTPUT)

  message(sprintf("Wrote %s", OUTPUT))
  message(sprintf("  foundational:    %d courses",
                  length(payload$categories$foundational$courses)))
  message(sprintf("  agrology_lower:  %d courses",
                  length(payload$categories$agrology_lower$courses)))
  message(sprintf("  agrology_senior: %d courses",
                  length(payload$categories$agrology_senior$courses)))
  total <- sum(vapply(payload$categories,
                       function(c) length(c$courses),
                       integer(1)))
  message(sprintf("  total:           %d distinct rows", total))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0L) main()
