# UBCO Freshwater Science Program: Pathways and Resources

Interactive student-facing website for the **Bachelor of Science Freshwater
Science** major at the UBC Okanagan campus. Curated and maintained by
**Dr. Nelson Jatel, PAg**, Lecturer and Freshwater Science Program Advisor,
Department of Earth, Environmental and Geographic Sciences, UBC Okanagan.

**Live site**: https://drnelsonjatel.github.io/UBCO_Freshwater_Science_Program/

**Interactive degree planner**: https://obwb.shinyapps.io/ubco-freshwater-planner/

**Reviewer cover note** (for the program committee):
https://drnelsonjatel.github.io/UBCO_Freshwater_Science_Program/review/

**Official UBCO program pages** (authoritative):

- [UBC Undergraduate Admissions, Freshwater Science](https://you.ubc.ca/ubc_programs/freshwater-science/)
- [Department of EESC, Freshwater Science](https://eesc.ok.ubc.ca/undergraduate/freshwater-science/)
- [UBC Academic Calendar, Freshwater Science requirements](https://okanagan.calendar.ubc.ca/faculties-schools-and-colleges/faculty-science/bachelor-science-programs/major-programs/freshwater-science)

## What it does

- **Course catalogue**: every UBCO course relevant to Freshwater Science
  (FWSC_O, BIOL_O, EESC_O, CHEM_O, GEOG_O, GISC_O, MATH_O, PHYS_O, STAT_O,
  INDG_O, ENGL_O, ECON_O) with prerequisites, equivalencies, lecture-lab
  patterns, and "what's offered" notes. **Live data**, scraped from the
  UBC Okanagan Academic Calendar.
- **Designation pathways**: concrete checklists for **PAg** (BC Institute
  of Agrologists) and **RPBio** (College of Applied Biology) registration:
  which UBCO courses count, which boxes are still empty, and what to take
  next term. The PAg mapping is rendered from BCIA's UBCO-specific approved
  course list (June 2025). The RPBio mapping is curated by the program
  advisor from the CAB Credentialing Standard (June 2025) because CAB does
  not publish a UBCO-specific list.
- **Complementary pathways**: Chemistry, Biology, and hydrology + water
  resources electives bundles for students layering depth on top of the
  Freshwater Science degree.
- **Program Learning Outcomes**: the eight v2 PLOs finalised in the April
  2026 memo, with the I / R / M curriculum-mapping methodology and a
  pointer to the EESC 301 pilot.
- **Interactive degree planner**: a Shiny app that lets a student tick
  their completed courses year by year and reports live credit, threshold,
  and designation-coverage gap analysis. Exports a printable plan via the
  browser's built-in print to PDF.

## Stack

| Surface | Stack |
|---|---|
| Static content (catalogue, outcomes, skills, designations, methodology) | Quarto |
| Course catalogue data | rvest scraper, normalised to `data/courses.parquet` |
| Designation mappings | YAML (`data/designation_mappings/{pag,rpbio}.yml`) |
| Recommended year-by-year pathway | YAML (`data/program_pathway.yml`) |
| Interactive degree planner | R Shiny |
| Hosting (Quarto site) | GitHub Pages |
| Hosting (Shiny planner) | shinyapps.io |

## Status

**v0.9, May 2026**: ready for review by the Freshwater Science Program
Committee and the Department of EESC. See the
[reviewer cover note](https://drnelsonjatel.github.io/UBCO_Freshwater_Science_Program/review/)
for the three specific judgement calls the program committee is invited to
weigh in on.

Live surfaces:

| Surface | Status |
|---|---|
| Quarto site (home, courses, designations, outcomes, methodology, review) | Live |
| Course catalogue | Live data (scraped from the live UBC Okanagan Academic Calendar) |
| Designation PAg page | Live data (rendered from BCIA's June 2025 PDF) |
| Designation RPBio page | Curated by program advisor; gap-warning callout for systematics |
| Designation Complementary page | Curated by program advisor |
| Program Learning Outcomes | Curated (v2 PLO list verbatim from April 2026 memo) |
| Interactive planner | Live MVP at shinyapps.io with year-based accordion and printable report |
| Skills scaffolding | In development (draft vocabulary only) |
| Curriculum-map matrix (PLO x course I/R/M) | In development (EESC 301 pilot only) |

## Licence

- Code: MIT (`LICENSE`).
- Content (curriculum maps, designation checklists, methodology docs):
  CC BY 4.0.
