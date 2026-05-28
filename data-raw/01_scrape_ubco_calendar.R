#!/usr/bin/env Rscript
# data-raw/01_scrape_ubco_calendar.R
# Daily scrape of the UBC Okanagan Academic Calendar for the subjects
# relevant to the Freshwater Science program. Writes the normalised
# course table to data/courses.parquet (one row per course).
#
# Run manually:
#   Rscript data-raw/01_scrape_ubco_calendar.R
# Run from CI:
#   .github/workflows/refresh-daily.yml on the 12:00 UTC cron.

suppressPackageStartupMessages({
  library(rvest)
  library(httr2)
  library(dplyr)
  library(stringr)
  library(arrow)
  library(purrr)
  library(tibble)
})

UBCO_CALENDAR_BASE <- "https://okanagan.calendar.ubc.ca"
UA <- paste0("limnology.ca UBCO Freshwater Pathways v0.1 ",
             "(contact: nelson.jatel@ubc.ca)")

SUBJECTS <- c(
  "fwsco",   # FWSC_O - the program-specific code
  "biolo",   # BIOL_O
  "eesco",   # EESC_O
  "chemo",   # CHEM_O
  "geogo",   # GEOG_O
  "matho",   # MATH_O
  "physo",   # PHYS_O
  "stato",   # STAT_O
  "indgo",   # INDG_O
  "englo",   # ENGL_O
  "gisco",   # GISC_O - GIS / Remote Sensing courses
  "econo"    # ECON_O - PAg requirement category
)

#' Fetch the rendered HTML of one subject's course-descriptions page.
fetch_subject <- function(subject_slug, sleep_seconds = 1L) {
  url <- sprintf("%s/course-descriptions/subject/%s",
                  UBCO_CALENDAR_BASE, subject_slug)
  message(sprintf("[fetch] %s", url))
  req <- httr2::request(url) |>
    httr2::req_user_agent(UA) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2L)
  resp <- httr2::req_perform(req)
  Sys.sleep(sleep_seconds)
  list(url = url, html = httr2::resp_body_html(resp))
}

#' Parse a single h3 + sibling-p block into a tidy row.
#'
#' Heading format: "FWSC_O 375 (3) Flora and Fauna of Inland Waters"
#' where the (N) is credits, optional trailing [L-T-O] sometimes appears
#' inside the heading or in the leading paragraph.
#'
#' Description paragraph contains plain text with embedded labels
#' "Prerequisite:", "Equivalency:", and sometimes a [L-T-O] pattern in
#' the first sentence.
parse_course_heading <- function(heading_text) {
  # Pattern: SUBJECT_O NUMBER (CREDITS) TITLE   (occasionally followed
  # by a [L-T-O] inside the heading).
  re <- "^([A-Z]+_O)\\s+(\\d+[A-Z]?)\\s+\\((\\d+(?:[-\\.]\\d+)?)\\)\\s+(.+?)\\s*(\\[\\d+[\\d\\-\\.]*\\])?\\s*$"
  m <- stringr::str_match(heading_text, re)
  list(
    subject     = m[1, 2],
    number      = m[1, 3],
    credits     = m[1, 4],
    title       = m[1, 5],
    ltp_in_head = m[1, 6]   # may be NA
  )
}

#' Extract the [L-T-O] pattern from a body paragraph, if present.
extract_ltp <- function(text) {
  m <- stringr::str_match(text, "\\[(\\d+[\\d\\-\\.]*)\\]")
  m[1, 1]
}

#' Extract a labelled metadata line ("Prerequisite:", "Equivalency:", etc.)
extract_label <- function(text, label) {
  pattern <- sprintf("(?m)%s\\s*([^\\n]+?)(?=\\.\\s|$)", label)
  m <- stringr::str_match(text, pattern)
  out <- m[1, 2]
  if (is.na(out)) return(NA_character_)
  stringr::str_trim(out)
}

#' Parse one subject page into a tibble of courses.
parse_subject_page <- function(page) {
  html <- page$html
  headings <- rvest::html_elements(html, "h3")
  if (!length(headings)) {
    message("  (no h3 headings on this page)")
    return(tibble::tibble())
  }
  rows <- purrr::map_dfr(headings, function(h3) {
    # Heading text
    h_text <- stringr::str_squish(rvest::html_text2(h3))
    if (!stringr::str_detect(h_text, "^[A-Z]+_O\\s+\\d")) return(NULL)
    parsed <- parse_course_heading(h_text)
    # Sibling paragraphs until the next h3.
    sibs <- rvest::html_elements(h3, xpath = "following-sibling::*")
    take <- character(0)
    for (n in sibs) {
      tag <- rvest::html_name(n)
      if (tag == "h3") break
      if (tag == "p")  take <- c(take, rvest::html_text2(n))
    }
    body <- stringr::str_trim(paste(take, collapse = "\n\n"))
    # Pull out the labelled metadata.
    prereq <- extract_label(body, "Prerequisite:")
    coreq  <- extract_label(body, "Corequisite:")
    equiv  <- extract_label(body, "Equivalency:")
    ltp_b  <- extract_ltp(body)
    ltp    <- if (!is.na(parsed$ltp_in_head)) parsed$ltp_in_head else ltp_b
    # Description = body minus the labelled meta lines.
    description <- body |>
      stringr::str_replace_all("Prerequisite:\\s*[^\\n]+?\\.", "") |>
      stringr::str_replace_all("Corequisite:\\s*[^\\n]+?\\.",  "") |>
      stringr::str_replace_all("Equivalency:\\s*[^\\n]+?\\.",  "") |>
      stringr::str_replace_all("\\[\\d+[\\d\\-\\.]*\\]", "") |>
      stringr::str_squish()
    tibble::tibble(
      subject     = parsed$subject,
      number      = parsed$number,
      code        = paste(parsed$subject, parsed$number),
      title       = parsed$title,
      credits     = suppressWarnings(as.numeric(parsed$credits)),
      ltp         = ltp,
      description = description,
      prereq_raw  = prereq,
      coreq_raw   = coreq,
      equiv_raw   = equiv,
      url         = page$url
    )
  })
  rows
}

#' Light parser for the natural-language prereq strings. Returns a
#' list-column with a flat character vector of all course codes
#' mentioned. The planner can do the real DAG resolution later.
extract_prereq_codes <- function(prereq_raw) {
  if (is.na(prereq_raw) || !nzchar(prereq_raw)) return(character(0))
  # Match "BIOL_O 125", "BIOL 125", "BIOL_O 117 and BIOL_O 122",
  # "BIOL_O 201, 308", "STAT_O 230 or BIOL_O 202", etc.
  codes <- stringr::str_extract_all(
    prereq_raw,
    "[A-Z]{3,5}(?:_O)?\\s*\\d{3}[A-Z]?"
  )[[1]]
  # Normalise: strip extra whitespace, ensure _O suffix for UBCO codes.
  out <- stringr::str_squish(codes)
  out <- ifelse(stringr::str_detect(out, "_O"),
                 out,
                 stringr::str_replace(out, "\\s+", "_O "))
  unique(out)
}

#' Top-level: scrape all SUBJECTS, normalise, write parquet.
main <- function() {
  if (!dir.exists("data")) dir.create("data")
  pages <- purrr::map(SUBJECTS, fetch_subject)
  per_subject <- purrr::map(pages, parse_subject_page)
  courses <- dplyr::bind_rows(per_subject)
  courses$prereq_codes <- purrr::map(courses$prereq_raw,
                                      extract_prereq_codes)
  courses$fetched_at <- Sys.time()
  courses <- dplyr::arrange(courses, subject,
                             suppressWarnings(as.integer(number)))
  message(sprintf("Scraped %d courses across %d subjects.",
                  nrow(courses),
                  length(unique(courses$subject))))
  # Parquet needs flat columns; serialise the prereq list-col as JSON
  # for now.
  courses$prereq_codes <- vapply(courses$prereq_codes,
                                  function(x) jsonlite::toJSON(x),
                                  character(1))
  arrow::write_parquet(courses, "data/courses.parquet")
  message("Wrote data/courses.parquet (",
          format(file.info("data/courses.parquet")$size,
                 big.mark = ","), " bytes).")
}

if (sys.nframe() == 0L) main()
