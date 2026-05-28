# UBCO Freshwater Science Program — Pathways & Resources

Interactive student-facing website for the **Freshwater Science** undergraduate
program at the UBC Okanagan campus. Built and maintained by
[Limnology Research Corp](https://limnology.ca) on behalf of the program lead
(Nelson R. Jatel, PAg).

## What it does

- **Course catalogue** — every UBCO course relevant to Freshwater Science
  (FWSC_O, BIOL_O, EESC_O, CHEM_O, GEOG_O, MATH_O, PHYS_O, STAT_O, …) with
  prerequisites, equivalencies, lecture-lab patterns, and "next-year
  offered?" status. Data scraped daily from the UBCO Academic Calendar.
- **Designation pathways** — concrete checklists for **PAg** (BC Institute
  of Agrologists) and **RPBio** (College of Applied Biology) registration:
  which UBCO courses count, which boxes are still empty, what to take next
  term. Authoritative mapping curated from the BCIA UBCO-specific
  approved-course list (last updated 2025-06-27) and the CAB Credentialing
  Standard (June 2025).
- **Learning outcomes** — program-level, department-level, and
  course-level outcomes mapped against each other.
- **Skills scaffolding** — controlled-vocabulary skills (e.g., "R basics",
  "field hydrology", "statistical inference") traced across the four-year
  arc so students can see when each skill is introduced, practised, and
  expected.
- **Interactive degree planner** — Shiny app that lets a student plan
  their remaining terms against any combination of program requirements
  and designation pathways, and flags gaps.
- **Departmental announcements** — lightweight news feed for the program.

## Stack

| Surface | Stack |
|---|---|
| Static content (catalogue, outcomes, skills, designations narrative) | Quarto |
| Interactive degree planner | Shiny, embedded as iframe |
| Data refresh (daily) | targets pipeline + GitHub Actions cron |
| Hosting | `limnology.ca` subpath |

## Status

Scaffold stage — directory structure + initial Quarto config + workflow
stubs. No content yet, no Shiny app yet, no data scraped yet. See
`docs/methodology.md` for the build plan.

## Licence

- Code: MIT (`LICENSE`).
- Content (curriculum maps, designation checklists, methodology docs):
  CC BY 4.0.
