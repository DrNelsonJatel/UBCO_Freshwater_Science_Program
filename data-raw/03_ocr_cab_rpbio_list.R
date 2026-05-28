#!/usr/bin/env Rscript
# data-raw/03_ocr_cab_rpbio_list.R
# OCR the CAB RPBio Credentialing Standard PDF, then curate the
# academic-requirements section into a YAML mapping the five required
# subject areas (genetics, cellular, physiology, systematics,
# evolution) to UBCO course codes.
#
# *** SCAFFOLD - not yet implemented. ***
#
# Output:
#   docs/reference/CAB_RPBio_Standard_<date>.txt
#   data/designation_mappings/rpbio.yml

suppressPackageStartupMessages({
  library(httr2)
})

SOURCE_PDF_URL <- paste0(
  "https://cab-bc.org/wp-content/uploads/",
  "Credentialing_Standard_June-2025.pdf"
)
LOCAL_PDF      <- "docs/reference/CAB_RPBio_Standard_2025-06.pdf"
LOCAL_TXT      <- "docs/reference/CAB_RPBio_Standard_2025-06.txt"
CURATED_YAML   <- "data/designation_mappings/rpbio.yml"

main <- function() {
  if (!file.exists(LOCAL_PDF)) {
    if (!dir.exists(dirname(LOCAL_PDF)))
      dir.create(dirname(LOCAL_PDF), recursive = TRUE)
    message(sprintf("[download] %s", SOURCE_PDF_URL))
    httr2::request(SOURCE_PDF_URL) |>
      httr2::req_user_agent("limnology.ca UBCO Freshwater Pathways") |>
      httr2::req_perform(path = LOCAL_PDF)
  }
  message("SCAFFOLD: download stub only. Implement OCR + YAML curator.")
}

if (sys.nframe() == 0L) main()
