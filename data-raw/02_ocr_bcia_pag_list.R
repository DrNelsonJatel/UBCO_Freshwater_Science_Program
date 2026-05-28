#!/usr/bin/env Rscript
# data-raw/02_ocr_bcia_pag_list.R
# OCR the BCIA UBCO-approved PAg course list PDF into text, then
# convert to a curated YAML mapping UBCO course codes -> BCIA
# category. Source PDF re-published by BCIA roughly annually.
#
# *** SCAFFOLD - not yet implemented. ***
#
# Run:
#   Rscript data-raw/02_ocr_bcia_pag_list.R
#
# Output:
#   docs/reference/BCIA_UBCO_PAg_<date>.txt  (raw OCR text)
#   data/designation_mappings/pag.yml         (curated mapping)
#
# Why this is manual: BCIA publishes the list as an image-based PDF
# with no text layer. We OCR locally with tesseract (or pdftotext if
# they ever publish a text-layered version), then a human curates the
# YAML because the categorisation is a judgement call.

suppressPackageStartupMessages({
  library(httr2)
})

SOURCE_PDF_URL <- paste0(
  "https://www.bcia.com/sites/default/files/docs/resources/",
  "UBCO%20Approved%20Courses_June%2027,%202025.pdf"
)
LOCAL_PDF      <- "docs/reference/BCIA_UBCO_PAg_2025-06-27.pdf"
LOCAL_TXT      <- "docs/reference/BCIA_UBCO_PAg_2025-06-27.txt"
CURATED_YAML   <- "data/designation_mappings/pag.yml"

download_pdf <- function() {
  if (file.exists(LOCAL_PDF)) {
    message(sprintf("[skip] %s already present.", LOCAL_PDF))
    return(invisible(NULL))
  }
  if (!dir.exists(dirname(LOCAL_PDF))) dir.create(dirname(LOCAL_PDF),
                                                   recursive = TRUE)
  message(sprintf("[download] %s -> %s", SOURCE_PDF_URL, LOCAL_PDF))
  httr2::request(SOURCE_PDF_URL) |>
    httr2::req_user_agent("limnology.ca UBCO Freshwater Pathways") |>
    httr2::req_perform(path = LOCAL_PDF)
}

ocr_pdf <- function() {
  # TODO: brew install tesseract; Rscript via tesseract::ocr() or
  # shell out to pdftoppm + tesseract per page. The PDF is 6 pages.
  stop("OCR step not yet implemented. See TODO in this script.",
       call. = FALSE)
}

main <- function() {
  download_pdf()
  # ocr_pdf()
  message("SCAFFOLD: download stub only. Implement ocr_pdf() + the YAML curator.")
}

if (sys.nframe() == 0L) main()
