# CLAUDE.md, UBCO Freshwater Science Program tool

Student-facing interactive website + degree planner for the **Bachelor of
Science Freshwater Science** major at the UBC Okanagan campus. Curated and
maintained by Dr. Nelson Jatel, PAg, Lecturer and Freshwater Science Program
Advisor. Repo at `DrNelsonJatel/UBCO_Freshwater_Science_Program` (public).

**Public URL**: https://drnelsonjatel.github.io/UBCO_Freshwater_Science_Program/

A prior Squarespace embed at `limnologyresearch.com/ubco-freshwater` was
retired 2026-05-28; the GitHub Pages URL above is now the single public
address. A future move to a dedicated domain (e.g. `ubcofwsc.ca`) is
possible but unplanned.

## Audience priority

1. Current Freshwater Science students (the degree-planner is the headline)
2. Prospective students browsing the program (catalogue + designations)
3. Program lead (Nelson, PAg) tracking outcomes and skills coverage

## Stack

- **Quarto** for static content (course catalogue, learning outcomes,
  skills scaffold, designation narratives, methodology, news).
- **Shiny** for the interactive degree planner. Embedded as iframe so the
  Quarto content side stays editable in plain markdown.
- **`targets`** pipeline for the daily data refresh + caching.
- **GitHub Actions** cron for the refresh + a separate workflow for
  Quarto-site deploy.
- **`rvest` + `httr2`** for scraping the UBCO Academic Calendar
  (`okanagan.calendar.ubc.ca`).

## Source-of-truth data feeds

| Feed | URL | Cadence |
|---|---|---|
| UBCO course descriptions | `okanagan.calendar.ubc.ca/course-descriptions/subject/<code>` | Daily scrape |
| UBCO program structure | `okanagan.calendar.ubc.ca/.../freshwater-science` | Daily scrape |
| BCIA PAg UBCO-approved course list | `bcia.com/sites/.../UBCO Approved Courses_June 27, 2025.pdf` | Manual (image-based PDF; OCR + curate to YAML) |
| CAB RPBio Credentialing Standard | `cab-bc.org/wp-content/uploads/Credentialing_Standard_June-2025.pdf` | Manual (same) |

## Standing conventions

- **British / Canadian English** in all UI copy and docs (favoured, realised,
  modelled, programme is acceptable in academic context but `program` is
  what UBC uses so `program` it is).
- **No em-dashes** in UI labels or docs. Use commas, colons, or periods.
- **ISO 8601 dates** everywhere internally; pretty-printed `12 May 2026`
  format in UI copy.
- **File-naming convention** for downloads and exports:
  `UBCO_FW_<DocType>_<Version>_<YYYYMMDD>.<ext>`.
- **Course code formatting**: subject + space + number, e.g. `FWSC_O 375`,
  matching UBCO calendar style.

## State at scaffold time (2026-05-28)

- Directories created: `R/`, `app/modules/`, `data-raw/`, `data/`,
  `docs/reference/`, `courses/`, `designations/`, `outcomes/`, `skills/`,
  `.github/workflows/`.
- Files stubbed (not yet functional): `_quarto.yml`, `index.qmd`,
  `designations/pag.qmd`, `designations/rpbio.qmd`,
  `data-raw/01_scrape_ubco_calendar.R`, `.github/workflows/refresh-daily.yml`.
- No Quarto render has been run yet, no scraping has run yet, no Shiny
  app yet, no data files yet.

## Roadmap (rough, to be expanded into GitHub issues)

1. **Scrape v1**, pull every relevant subject's course descriptions and
   normalise to a tidy parquet (`data/courses.parquet`). Subjects:
   FWSC_O, BIOL_O, EESC_O, CHEM_O, GEOG_O, MATH_O, PHYS_O, STAT_O,
   INDG_O, ENGL_O.
2. **OCR the two PDFs** locally and check the resulting text into
   `docs/reference/` so it's diff-trackable.
3. **Hand-curate PAg and RPBio YAML** files mapping UBCO course codes to
   designation categories. These are the authoritative inputs to the
   designation pathway pages and the planner's gap-finder.
4. **Quarto site v1**, landing page, course catalogue list, PAg page,
   RPBio page, methodology page. Deploy via GitHub Pages or limnology.ca.
5. **Shiny planner v1**, drag-drop year planner with designation
   gap-finder. Deploy to shinyapps.io (free tier) or Cloud Run if the
   limnology.ca infra needs it.
6. **Learning outcomes v1**, YAML schema, program / department / course
   levels, cross-reference viz.
7. **Skills scaffold v1**, controlled vocabulary, course tagging, Sankey
   or heatmap viz.
8. **Communication tool**, start with an announcements feed in Quarto.
   Discussion forum (Discourse) only if it earns its keep.

## Citations and references

Any third-party material referenced (academic papers on curriculum
design, accreditation standards, etc.) goes through `docs/references.bib`
with the anti-hallucination policy that has worked for the OBWB
manuscript: every entry must trace to a real source verified via Zotero
or a confirmed publisher URL; never type a DOI from memory.

## Gotchas

- UBCO calendar HTML is stable and clean, but the `Prerequisite:` and
  `Equivalency:` strings use free-form natural language and need a tiny
  parser. Plan on a few iterations to get the prereq DAG right.
- The two designation PDFs (BCIA + CAB) are image-based with no
  text layer; remote WebFetch parsing failed. Need a local OCR step
  (`tesseract`) before they can be converted to YAML.
- `Prerequisite: one of X or Y` vs. `both X and Y` patterns matter for
  the planner's gap-finder; do not over-simplify the parser.
