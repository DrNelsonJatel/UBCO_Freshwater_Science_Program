#!/usr/bin/env Rscript
# data-raw/01_scrape_ubco_calendar.R
# Daily scrape of the UBC Okanagan Academic Calendar for the subjects
# relevant to the Freshwater Science program. Writes the normalised
# course table to data/courses.parquet.
#
# Run manually:
#   Rscript data-raw/01_scrape_ubco_calendar.R
# Run from CI:
#   .github/workflows/refresh-daily.yml on the 12:00 UTC cron.
#
# Output schema (one row per course):
#   subject       chr   e.g. "FWSC_O"
#   number        int   e.g. 375L
#   code          chr   e.g. "FWSC_O 375"
#   title         chr   "Flora and Fauna of Inland Waters"
#   credits       int   3L
#   ltp           chr   "[3-3-0]" (lecture-lab-other pattern)
#   description   chr   one paragraph plain text
#   prereq_raw    chr   "BIOL_O 125 OR (BIOL_O 117 AND BIOL_O 122)"
#   equiv_raw     chr   "Credit will be granted for only one of BIOL_O 375 or FWSC_O 375."
#   prereq_codes  list  list-column of parsed prereq code sets
#   url           chr   source URL
#   fetched_at    POSIXct  scrape timestamp (UTC)
#
# *** SCAFFOLD - not yet implemented. ***
# Once wired:
#   1. Iterate over the SUBJECTS vector below.
#   2. For each, GET the subject page with httr2 + a polite User-Agent.
#   3. rvest::html_elements() to peel out one node per course (the
#      calendar uses heading + paragraph pairs).
#   4. Pass each course block to parse_course() which returns one row.
#   5. Bind, validate, dedupe, write to data/courses.parquet.
#
# Critical correctness note: the calendar's "Prerequisite:" string is
# free-form natural language. Patterns include:
#   "Prerequisite: BIOL_O 125."
#   "Prerequisite: One of BIOL_O 125 or BIOL_O 117 and BIOL_O 122."
#   "Prerequisite: All of CHEM_O 121, 123."
#   "Prerequisite: Third-year standing in [list of programs]."
# Build a small dedicated parser; do not regex-and-pray.

suppressPackageStartupMessages({
  library(rvest)
  library(httr2)
  library(dplyr)
  library(stringr)
  library(arrow)
})

UBCO_CALENDAR_BASE <- "https://okanagan.calendar.ubc.ca"
UA <- "limnology.ca UBCO Freshwater Pathways v0.1 (contact: njatel@limnology.ca)"

# Subjects relevant to the Freshwater Science program. Add as we discover
# more cross-listed dependencies.
SUBJECTS <- c(
  "fwsco",  # FWSC_O - the program-specific code
  "biolo",  # BIOL_O
  "eesco",  # EESC_O
  "chemo",  # CHEM_O
  "geogo",  # GEOG_O
  "matho",  # MATH_O
  "physo",  # PHYS_O
  "stato",  # STAT_O
  "indgo",  # INDG_O
  "englo"   # ENGL_O
)

fetch_subject <- function(subject_slug) {
  url <- sprintf("%s/course-descriptions/subject/%s",
                  UBCO_CALENDAR_BASE, subject_slug)
  message(sprintf("[fetch] %s", url))
  req <- httr2::request(url) |>
    httr2::req_user_agent(UA) |>
    httr2::req_retry(max_tries = 3)
  resp <- httr2::req_perform(req)
  Sys.sleep(1L)  # polite delay between subject pages
  httr2::resp_body_html(resp)
}

parse_course <- function(course_node) {
  # TODO: implement. Each course block has a heading (code + title +
  # credits) and a paragraph (description + Prerequisite: ... +
  # Equivalency: ... + standing requirements).
  stop("parse_course() not implemented yet.", call. = FALSE)
}

parse_prereq <- function(prereq_raw) {
  # TODO: implement. Convert free-form "BIOL_O 125 OR (BIOL_O 117 AND
  # BIOL_O 122)" into a list of code-sets. The planner's gap-finder
  # depends on this being correct.
  stop("parse_prereq() not implemented yet.", call. = FALSE)
}

main <- function() {
  if (!dir.exists("data")) dir.create("data")
  out_rows <- list()
  for (slug in SUBJECTS) {
    page <- fetch_subject(slug)
    # TODO: extract per-course nodes, pass each to parse_course().
    # course_nodes <- rvest::html_elements(page, ...)
    # rows <- lapply(course_nodes, parse_course)
    # out_rows[[slug]] <- do.call(rbind, rows)
  }
  message("SCAFFOLD: nothing written. Implement parse_course() + parse_prereq().")
  # courses <- dplyr::bind_rows(out_rows)
  # arrow::write_parquet(courses, "data/courses.parquet")
}

if (sys.nframe() == 0L) main()
