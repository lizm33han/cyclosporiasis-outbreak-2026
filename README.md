# Cyclosporiasis Outbreak Surveillance — Summer 2026

A beginner epidemiology project using R to explore the 2026 U.S. cyclosporiasis
(Cyclospora) outbreak, with a focus on Michigan's late-June cluster.

This project is a learning exercise, not an official analysis.

**📄 [Read the full report](cyclosporiasis_report.md)** — a single
R Markdown document with all analysis, plots, and findings in one place.
The sections below document the underlying data and process; the report
is the polished narrative built from it.

## 1. Background

Cyclosporiasis is an intestinal illness caused by the microscopic parasite
*Cyclospora cayetanensis*. Key facts relevant to this analysis:

- **Transmission:** foodborne/waterborne only — people are infected by eating
  food or drinking water contaminated with feces containing the parasite.
  Freshly shed oocysts are *not* immediately infectious; it takes 1–2 weeks in
  the environment for them to sporulate. **Direct person-to-person spread does
  not occur.** This matters for analysis: don't expect an exponential,
  transmission-chain-driven curve like you'd see with measles or Ebola. Case
  curves for cyclosporiasis outbreaks tend to reflect exposure to a shared
  contaminated food source, clustered in time.
- **Incubation:** 2–14 days (average ~1 week) from exposure to symptom onset.
- **Symptoms:** watery diarrhea (often described as "explosive"), fatigue,
  cramping, nausea, low-grade fever; can last weeks and relapse.
- **Seasonality:** U.S. cases cluster May–August every year; this is expected,
  not itself evidence of an unusual outbreak.
- **Known 2026 event:** CDC reported 145 domestically acquired cases across
  17 states as of June 16, 2026, with an additional, separately reported
  cluster of 170+ cases across 7 southeast Michigan counties by June 30, 2026.
  No common food source has been confirmed as of this writing.

## 2. Data sources

| File | Description | Source | Retrieved |
|---|---|---|---|
| `data/NNDSS_Weekly_Data_2022-2026_20260703.csv` | **Primary source.** Weekly case counts by state/region, MMWR years 2022–2026 (2022–2024 and 2025 complete; 2026 partial through week 25), including each area's previous-52-week max and cumulative year-to-date totals | [NNDSS Weekly Data, data.cdc.gov](https://data.cdc.gov/NNDSS/NNDSS-Weekly-Data/x9gk-5huc) | 2026-07-04 |
| `data/national_summary_2026-07-03.csv` | Supplementary demographic details not present in NNDSS (median age, % female, hospitalizations, travel-associated case count) | [CDC Surveillance of Cyclosporiasis](https://www.cdc.gov/cyclosporiasis/php/surveillance/index.html) | 2026-07-03 |

NNDSS (National Notifiable Diseases Surveillance System) is a nationwide
system where states report weekly case counts for CDC/CSTE-designated
notifiable conditions. It's downloadable directly from data.cdc.gov, updated
weekly, and includes each jurisdiction's **previous 52-week maximum** —
a real, built-in baseline for "is this unusual?"

### Why a multi-year file instead of just 2026

A single comparison year can't tell you whether that year was itself
unusual. Pulling 2022–2026 lets us build an actual seasonal baseline — a
min/max range and average across 4 complete years — so we can ask "is 2026
unusual relative to normal year-to-year variation?" instead of "is 2026
different from one specific other year?" This mirrors how CDC's own
aberration-detection method works: it uses a multi-year historical window
rather than a single comparison year, because one year is too noisy to
trust on its own.

### Four data-cleaning gotchas found in this file

Worth internalizing these as general lessons for working with any
multi-year government export, not just this one:

1. **Inconsistent capitalization across years.** 2022–2024 use ALL CAPS
   with no periods (`"MICHIGAN"`, `"US RESIDENTS"`); 2025–2026 use Title
   Case with periods (`"Michigan"`, `"U.S. Residents"`). A naive filter on
   an exact string match would silently drop 3 of 5 years with no error —
   just a truncated dataset that looks complete. Fixed by normalizing case
   (`str_to_title()`) before any filtering or joining across years.

2. **Thousands-separator commas stored as text.** Once a count exceeds 999
   (e.g., a full year's cumulative total), it's formatted as `"1,171"`
   rather than `"1171"`. `as.numeric("1,171")` silently returns `NA`, not
   1171 — another failure mode with no error message. Fixed by stripping
   commas before numeric conversion.

3. **The `-` flag does not reliably mean "value is zero."** This is the
   most important one, because it changes actual numbers, not just parsing
   mechanics. Checked empirically across all ~16,000 rows: the `-` flag
   appears on 12,431 *blank* cells (where it does mean zero) **and** on
   1,701 *populated* cells with real values like `"18"` (where it's just a
   no-issue annotation, unrelated to the value). The correct rule: trust a
   populated value whenever one exists; only consult the flag to interpret
   a genuinely blank cell (`-` → 0, `N`/`NC`/`U` → not applicable). See
   `scripts/03_seasonal_baseline.R` Section 2 for the verification code.

4. **Aggregate/territory labels don't unify with simple case normalization.**
   `str_to_title()` cleanly unifies all 50 states + DC across all 5 years,
   but not everything: `"US RESIDENTS"` (2022–2024, no periods) title-cases
   to `"Us Residents"`, which still doesn't match `"U.S. Residents"`
   (2025–2026, which already has periods). Same issue for `"U.S.
   Territories"`. Worse, `"Commonwealth Of Northern Mariana Islands"`
   (2022–2024) and `"Northern Mariana Islands"` (2025–2026) are a genuine
   name change, not just a formatting difference. None of this affects the
   50-state analysis in `04_per_state_comparison.R`, but it would need
   explicit handling (a manual lookup table, not just case normalization)
   if the project is extended to include territories or the national
   aggregate rows from the multi-year file.

### Understanding the NNDSS file

Each row is one (reporting area × week) combination for Cyclosporiasis.
Key columns:

- **Reporting Area**: a state, a Census region (e.g. "East North Central"),
  "U.S. Residents" (the national total for residents), or "Total" (grand
  total including territories and non-U.S. residents)
- **MMWR WEEK**: epidemiological week number
- **Current week**: cases reported that week
- **Current week, flag**: see Gotcha 3 above — don't assume `-` means zero
  without checking whether the cell is actually blank
- **Previous 52 week Max**: the highest single-week count for that area in
  the prior year — a baseline for "is the current week unusual?"
- **Cumulative YTD Current/Previous MMWR Year**: running total for this
  year vs. the same weeks last year

### Critical caveat: "Current week" undercounts due to reporting lag

If you sum the `Current week` column across all 25 weeks of 2026, you get
447. But the actual `Cumulative YTD Current MMWR Year` reported for week 25
is 992 — more than double. This gap isn't a data error; it grows every week
and it's a fundamental feature of provisional surveillance data:

- **`Current week`** is a frozen snapshot: it shows however many cases had
  reached CDC by the time that week's table was published, and it is never
  revised afterward — not even in a later download of the same file.
- **Cumulative YTD** is recalculated from the complete dataset every time
  CDC publishes, so it captures cases that occurred in an earlier week but
  weren't reported to CDC until later (a delay caused by the chain of
  illness → doctor visit → lab test → confirmation → state health dept
  → CDC). CDC's own documentation confirms this pattern.

**Practical implication:** don't read the most recent 1-2 weeks of the
`Current week` column as "how many people actually got sick that week" —
read it as "how many had been reported as of this snapshot," which will
climb higher as backfill arrives. This effect is called **right-censoring**
or **reporting lag**. The week 25 spike (180) is therefore likely an
*underestimate* of that week's true case count.

See `scripts/02_nndss_analysis.R` Section 7 for a plot showing this gap
growing over time. Michigan shows the same effect at smaller scale (a gap
of 14 cases by week 25) — see Section 6 of that script.

### Remaining limitations to document

- NNDSS data is explicitly **provisional** — CDC states cumulative counts
  "can increase or decrease as additional information becomes available."
  Don't treat any single week's number as final.
- NNDSS aggregates by state/region, not by county — so it can't replace a
  granular county-level picture for Michigan if that ever becomes available.
- Reporting completeness varies by jurisdiction (Pennsylvania is voluntary,
  for instance), which affects cross-state comparisons.

## 3. Repo structure

```
cyclosporiasis-2026/
├── README.md
├── cyclosporiasis_report.Rmd           # the full report - start here
├── cyclosporiasis_report.md            # knitted output (renders on GitHub)
├── cyclosporiasis_report_files/        # plot images embedded in the knitted report
├── data/
│   ├── NNDSS_Weekly_Data_2022-2026_20260703.csv   # primary source (5 years)
│   └── national_summary_2026-07-03.csv            # supplementary demographics
├── scripts/
│   ├── 02_nndss_analysis.R            # 2026-only analysis: weekly curve, YoY vs. 2025, reporting lag
│   ├── 03_seasonal_baseline.R         # 2022-2026 analysis: real seasonal baseline, not just 1 comparison year
│   └── 04_per_state_comparison.R      # ranks all 50 states + DC by departure from their own baseline
└── output/          (generated figures land here; gitignored except .gitkeep)
```

The scripts in `scripts/` and the `.Rmd` cover the same analysis — the
scripts were built first and are still useful for running one piece of the
analysis quickly, while `cyclosporiasis_report.Rmd` consolidates everything
into a single narrative document with the code, output, and interpretation
together. Knitting the `.Rmd` regenerates `cyclosporiasis_report.md` and
`cyclosporiasis_report_files/` from scratch, so those two stay in sync with
the data automatically — no manual copy-pasting of numbers into prose.

## 4. Workflow (Posit Cloud + GitHub)

1. **Posit Cloud:** create a new RStudio project, and either:
   - clone this repo directly via the Git pane (`File > New Project >
     Version Control > Git`), or
   - upload the folder via the Files pane if starting fresh.
2. **Fastest path to the full picture:** open `cyclosporiasis_report.Rmd`
   and click **Knit**. It reads the same CSVs with relative paths
   (`data/...`), so as long as you open the `.Rproj` at the repo root,
   paths resolve correctly on any machine. To dig into or re-run one piece
   of the analysis on its own, `scripts/02_nndss_analysis.R`,
   `03_seasonal_baseline.R`, and `04_per_state_comparison.R` cover the same
   logic individually.
3. **Committing to GitHub:** use the Git tab in Posit Cloud (or terminal:
   `git add`, `git commit`, `git push`). Suggested commit message convention:
   - `data: add YYYY-MM-DD CDC snapshot`
   - `analysis: <what you added>`
   - `docs: update README`
4. **Updating data over time:** NNDSS revises provisional counts
   retroactively, so when you re-pull, save as a new dated CSV (don't
   overwrite the old one) and re-check earlier weeks, not just new ones.

## 5. Findings so far

- [x] National 2026 cumulative cases through week 25 are **3.2x** the
  2022–2025 average at the same point in the season (992 vs. an average of
  314)
- [x] Michigan's week-25 case count was 0, 0, 1, and 0 in 2022–2025
  respectively, vs. **46** in 2026 — a genuine anomaly, not normal seasonal
  noise
- [x] Extended the comparison to all 50 states + DC: Michigan is the most
  extreme outlier, but not the only one — New York (9.8x baseline), Ohio
  (7.6x), Florida (7.5x), and Virginia (20x, off a very low base) are all
  well above their own historical norms, suggesting a broader pattern worth
  investigating rather than an isolated Michigan event
- [x] Identified and corrected a reporting-lag effect: the `Current week`
  column undercounts the most recent weeks; cumulative totals are the more
  trustworthy measure of true burden

## 6. Next steps

- [ ] Re-pull NNDSS data in a few weeks and append to the time series
  (watch for retroactive revisions to earlier weeks, not just new ones)
- [ ] Map affected states using `tigris`/`sf`, now that we have real
  per-state counts and a ratio-to-baseline metric to color by
- [ ] Once/if county-level Michigan data becomes available from MDHHS,
  layer it in as a separate, clearly-labeled supplementary dataset
- [ ] If a common food source is identified, cross-reference the
  affected-states list against known supply-chain/distribution overlap

## 7. Known caveats to keep in mind throughout

- All counts are undercounts — cyclosporiasis testing is not routine, and
  CDC explicitly states the true burden is likely higher than reported.
- Data is preliminary and subject to change per CDC's own disclaimer.
