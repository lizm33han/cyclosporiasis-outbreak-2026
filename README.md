# Cyclosporiasis Outbreak Surveillance: Summer 2026

A beginner epidemiology project using R to explore the 2026 U.S. cyclosporiasis
(*Cyclospora*) outbreak with CDC NNDSS surveillance data. Version 2 (October 2026)
covers MMWR weeks 1-39.

This project is a learning exercise, not an official analysis.

**[Read the full report](cyclosporiasis_report.md)**: a single R Markdown
document with the analysis, plots, and findings in one place. The sections below
document the data and process; the report is the narrative built from them.

## 1. What changed in version 2

The July version used an NNDSS export that stopped at MMWR week 25 (992 national
cumulative cases). This update uses the October 10 export through week 39.

- The 2026 national cumulative total reached **32,286** cases, **10.4x** the
  2022-2025 average at the same week (3,100), with every year compared as first
  published.
- Michigan accounts for 45% of the national total and Ohio for 20%. The five
  largest states account for 76%.
- The July report flagged that recent weeks were likely undercounted. The data now
  show the scale: 2025's cumulative total at week 25 was first published as 272 and
  is now reported as 744.
- A correction to the July documentation: NNDSS weekly rows are **not** revised
  after publication (all 2026 rows for weeks 1-25 are identical between the July and
  October exports). Revisions are only visible in the "previous year" cumulative
  column of the following year's rows. See Section 2.

## 2. Background and data

### The disease

Cyclosporiasis is an intestinal illness caused by the microscopic parasite
*Cyclospora cayetanensis*.

- **Transmission:** foodborne or waterborne only. Freshly shed oocysts are not
  immediately infectious (it takes 1-2 weeks in the environment), so direct
  person-to-person spread does not occur. Case curves tend to reflect exposure to a
  shared contaminated food source rather than an exponential transmission chain.
- **Incubation:** 2-14 days (average about 1 week).
- **Seasonality:** U.S. cases cluster May to August every year, which is why this
  project builds a seasonal baseline from prior years.

### Data sources

| File | Description | Source | Retrieved |
|---|---|---|---|
| `data/NNDSS_Weekly_Data_2022-2026_20260703.csv` | Weekly case counts by state and region, MMWR years 2022-2026 (2026 stops at week 25). Used for the **2022-2025 baseline years**. | [NNDSS Weekly Data, data.cdc.gov](https://data.cdc.gov/NNDSS/NNDSS-Weekly-Data/x9gk-5huc) | 2026-07-03 |
| `data/NNDSS_Weekly_Data_2026_20261010.csv` | Same table, 2026 only, MMWR weeks 1-39. Used for **2026**. | same | 2026-10-10 |

NNDSS (National Notifiable Diseases Surveillance System) is the system through
which states report weekly case counts for nationally notifiable conditions.

### How the weekly tables work

- Each row is one reporting area in one MMWR week, **frozen as of its publication
  week**. Earlier rows are never rewritten.
- **Current week** is the count reported that week. It captures only about 39% of
  what ends up in the cumulative total (12,663 vs. 32,286 through week 39),
  because late reports are folded into the cumulative column but never added back
  to old `Current week` values.
- **Cumulative YTD** is therefore the more reliable measure. The weekly measure
  used in this project is the **change in the cumulative column** ("reports added"),
  a reporting curve rather than an onset-date epidemic curve.
- **Revisions** show up only in the "Cumulative YTD Previous MMWR Year" column of
  the next year's rows. Comparing 2025 as first published with 2025 as settled
  shows early-season counts revised upward by a factor of 2.7 at week 25 and 1.25
  at week 39. All baseline comparisons here use every year as first published, so
  they are like for like. The 2026 counts are provisional and will be revised
  upward as well.

### Four data-cleaning issues

1. **Inconsistent capitalization across years.** 2022-2024 use ALL CAPS
   (`"MICHIGAN"`), 2025-2026 use Title Case (`"Michigan"`). An exact-match filter
   silently drops years. Fixed with `str_to_title()`.
2. **Thousands separators stored as text.** Counts over 999 appear as `"1,171"`, and
   `as.numeric("1,171")` silently returns `NA`. Fixed by stripping commas.
3. **The `-` flag does not reliably mean zero.** It appears on blank cells (where it
   does mean zero) and on populated cells (where it is only an annotation). Rule:
   trust a populated value; read the flag only for blank cells. `N`, `NC` and `U`
   become `NA`.
4. **Aggregate labels do not unify with case normalization.** `"US RESIDENTS"`
   becomes `"Us Residents"`, not `"U.S. Residents"`. The project uses only the
   `"Total"` row and the 50 states plus DC, which do unify.

All four are handled in `scripts/00_load_and_clean.R`.

### Other data notes

- Idaho, Mississippi and Pennsylvania carry the "not reportable" flag for
  cyclosporiasis. Kentucky has no 2022 values, so its baseline uses three years.
- New York City reports separately from New York State and is excluded from the
  state ranking.
- Colorado's 2024 cumulative total jumped by 144 in week 39 and was reversed in week
  49. As first published, it inflates the 2024 baseline at week 39 slightly (about 36
  cases in the four-year national average), which does not change any conclusion.
- Week 53 (2025 only) is excluded from cross-year comparisons.
- A small number of weekly changes in the cumulative column are negative (37 of
  17,290 rows). They are left as published.
- State-level weekly series are lumpy because states send reports in batches.
  Cumulative totals are more reliable than single weeks.

## 3. Repo structure

```
cyclosporiasis-2026/
├── README.md
├── cyclosporiasis_report.Rmd           # the full report (start here)
├── cyclosporiasis_report.md            # knitted output (renders on GitHub)
├── cyclosporiasis_report_files/        # plot images used by the knitted report
├── data/
│   ├── NNDSS_Weekly_Data_2022-2026_20260703.csv   # baseline years (2022-2025)
│   └── NNDSS_Weekly_Data_2026_20261010.csv        # 2026, weeks 1-39
├── scripts/
│   ├── 00_load_and_clean.R            # shared loader: reads, cleans, builds `cyclo`
│   ├── 02_nndss_analysis.R            # national season, revisions, reporting lag
│   ├── 03_seasonal_baseline.R         # Michigan vs. its own history, share of national
│   └── 04_per_state_comparison.R      # ranks the 50 states + DC
└── output/                            # figures from the scripts (gitignored except .gitkeep)
```

Scripts 02-04 each source `00_load_and_clean.R`, so the cleaning logic lives in one
place. The report sources it too and contains its own copies of the key analysis
code so the code and output sit side by side.

## 4. Workflow (Posit Cloud + GitHub)

1. In Posit Cloud, open the project at the repo root so relative paths (`data/...`)
   resolve.
2. Open `cyclosporiasis_report.Rmd` and click **Knit**. Individual scripts can be run
   on their own; each one sources `scripts/00_load_and_clean.R` first.
3. **To roll the analysis forward when new weeks are published:** download a fresh
   NNDSS export (Cyclosporiasis, current year), save it as a new dated file in
   `data/`, then update `current_file` and `export_date` at the top of
   `scripts/00_load_and_clean.R`. `latest_week` updates automatically, so no week
   numbers are hard-coded elsewhere. Keep the old export; comparing two snapshots
   shows how much the counts were revised.
4. Commit the `.Rmd`, the knitted `.md`, and the `cyclosporiasis_report_files/`
   folder together. The `.md` references the images by relative path, so all three
   are needed for the plots to display on GitHub.

## 5. Findings (data through MMWR week 39)

- 2026 national cumulative cases are **32,286**, 10.4x the 2022-2025 average at the
  same week (3,100). Against 2025 as settled (4,042) the ratio is 8.0x, but that
  compares a provisional count with a settled one.
- Reports added peaked in **MMWR week 30** (5,320 in one week) and fell sharply by
  week 36.
- **Michigan:** 14,660 cumulative cases (45% of the national total) against 26-36
  at the same point in 2022-2025. In July its week-25 count was 0, 0, 1 and 0 in
  2022-2025 against 46 in 2026.
- **Ohio:** 6,527 (20% of the national total). The top five states (Michigan, Ohio,
  Illinois, North Carolina, Kansas) account for 76%. 38 of the 48 states and DC
  that report to NNDSS are above their own 2022-2025 level.
- Weekly additions rose again in the last published week (week 39), nationally and
  in Michigan. This file cannot say whether that is a late batch of reports or a new
  increase; it needs rechecking when later weeks are published.

## 6. Limitations

- NNDSS data are provisional, and the most recent weeks are the least complete.
- The weekly tables describe reporting, not exposure. They carry no onset dates and
  no food-source information.
- NNDSS counts and CDC outbreak-page counts are different measures and will not
  match.
- All counts are undercounts. Cyclosporiasis testing is not routine.
- NNDSS aggregates by state, not by county.

## 7. Next steps

- [ ] Re-pull NNDSS when weeks 40 and later are published and compare with this
  snapshot to measure how much 2026 was revised.
- [ ] Map state-level cumulative counts with `tigris` and `sf`.
- [ ] If county-level data become public for Michigan or Ohio, add them as a
  separate, clearly labeled dataset.

## 8. Changelog

**Version 2 (October 2026)**
- Added the October 10 NNDSS export (weeks 1-39); the July export is kept for the
  2022-2025 baseline.
- Added `scripts/00_load_and_clean.R` and removed every hard-coded week 25.
- Added a section on first-published vs. settled counts and corrected the July
  statement that earlier weeks are revised in later exports.
- Changed the weekly measure from the `Current week` column to reports added
  (change in cumulative), because `Current week` captures only about 39% of cases.
- Fixed a vintage mismatch in the July year-over-year plot (provisional 2026 vs.
  settled 2025); all comparisons now use every year as first published.
- Rebuilt the state ranking on cumulative cases rather than a single week.
- Retired `01_data_import_exploration.R` and removed `national_summary_2026-07-03.csv`,
  neither of which the analysis uses.

**Version 1 (July 2026):** initial analysis through MMWR week 25.

## 9. Sources

- Centers for Disease Control and Prevention. *NNDSS Weekly Data* [Data set].
  data.cdc.gov. https://data.cdc.gov/NNDSS/NNDSS-Weekly-Data/x9gk-5huc
  (exports retrieved July 3 and October 10, 2026).
- ABC News. (2026, July 29). *More than 18,000 cyclosporiasis cases reported across
  45 states: CDC*.
  https://www.goodmorningamerica.com/wellness/story/18000-cyclosporiasis-cases-reported-45-states-cdc-135194065
- NC State Extension. (2026, July 21). *Update on cyclosporiasis cases and
  information*.
  https://ncfreshproducesafety.ces.ncsu.edu/news/update-on-cyclosporiasis-cases-and-information/

## 10. AI assistance disclosure

Portions of this project's R code, data-cleaning logic, and documentation were
drafted with assistance from Claude (Anthropic). All research questions,
methodological decisions, and interpretation of findings are my own.
