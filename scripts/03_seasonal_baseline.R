# =============================================================================
# 03_seasonal_baseline.R
#
# Purpose: Use 5 years of NNDSS data (2022-2026) to build a real seasonal
#          baseline for Cyclosporiasis, rather than relying on a single
#          comparison year. Answers: is 2026 unusual relative to normal
#          year-to-year variation, not just relative to 2025?
#
# Data source: NNDSS Weekly Data, data.cdc.gov
#              https://data.cdc.gov/NNDSS/NNDSS-Weekly-Data/x9gk-5huc
# File covers: MMWR weeks 1-52/53, years 2022-2025 (complete), plus
#              weeks 1-25 of 2026 (partial/provisional)
# Downloaded/retrieved: 2026-07-04
#
# Run this from an R project opened at the repo root so relative paths work.
# =============================================================================

library(tidyverse)

# ---- 1. Import ---------------------------------------------------------------

raw <- read_csv("data/NNDSS_Weekly_Data_2022-2026_20260703.csv", show_col_types = FALSE)

glimpse(raw)
cat("Years present:", paste(sort(unique(raw$`Current MMWR Year`)), collapse = ", "), "\n")

# ---- 2. Three gotchas found while inspecting this file, fixed explicitly ---
#
# GOTCHA 1 - Inconsistent capitalization across years:
#   2022-2024 use ALL CAPS with no periods ("MICHIGAN", "US RESIDENTS").
#   2025-2026 use Title Case with periods ("Michigan", "U.S. Residents").
#   A naive filter like `Reporting Area == "Michigan"` would SILENTLY drop
#   2022-2024 entirely and you'd never see an error - just a truncated
#   dataset. We normalize case up front so every year is comparable.
#
# GOTCHA 2 - Thousands-separator commas in larger numbers:
#   Values like "1,171" appear as TEXT in the numeric columns once counts
#   exceed 999. as.numeric("1,171") returns NA, not 1171 - another silent
#   failure mode. We strip commas before converting to numeric.
#
# GOTCHA 3 - The "-" flag does NOT reliably mean "value is zero":
#   Checked empirically across all ~16,000 rows: flag "-" appears on 12,431
#   blank cells (where it does mean zero) AND on 1,701 POPULATED cells with
#   real values like "18" (where it's just a no-issue annotation). An
#   earlier version of this cleaning function assumed "-" always meant zero
#   and would have silently overwritten real values with 0 for those 1,701
#   rows. The correct rule: trust a populated value whenever one exists;
#   only use the flag to interpret a blank cell.

raw <- raw %>%
  mutate(reporting_area_norm = str_to_title(`Reporting Area`))

# Sanity check: this should now show ONE consistent name per area across years
raw %>%
  filter(str_detect(reporting_area_norm, "Michigan")) %>%
  distinct(`Current MMWR Year`, `Reporting Area`, reporting_area_norm) %>%
  arrange(`Current MMWR Year`)

clean_flagged_col <- function(value_col, flag_col) {
  numeric_val <- suppressWarnings(as.numeric(str_remove_all(as.character(value_col), ",")))
  case_when(
    !is.na(numeric_val)             ~ numeric_val,   # a real value was reported - always trust it
    flag_col == "-"                 ~ 0,              # blank + "-" flag = zero
    flag_col %in% c("N", "NC", "U") ~ NA_real_,        # not reportable / not calculated / unavailable
    TRUE ~ NA_real_
  )
}

cyclo <- raw %>%
  filter(Label == "Cyclosporiasis") %>%
  mutate(
    current_week_clean    = clean_flagged_col(`Current week`, `Current week, flag`),
    cum_ytd_current_clean = clean_flagged_col(`Cumulative YTD Current MMWR Year`, `Cumulative YTD Current MMWR Year, flag`)
  )

# ---- 3. Handle week 53 -------------------------------------------------------
# 2025 has a week 53 (some MMWR years do, depending on calendar alignment).
# 2022-2024 and 2026 top out at week 52 or 25 respectively. Week 53 has no
# equivalent in most years, so we exclude it from cross-year week-by-week
# comparison to keep every week's baseline built from the same set of years.

cyclo <- cyclo %>% filter(`MMWR WEEK` <= 52)

# ---- 4. Build the national (Total) seasonal baseline ------------------------

national_all_years <- cyclo %>%
  filter(reporting_area_norm == "Total") %>%
  select(`Current MMWR Year`, `MMWR WEEK`, current_week_clean, cum_ytd_current_clean)

# Baseline = 2022-2025 (complete years). 2026 is the partial year we're
# evaluating against that baseline.
baseline_years <- national_all_years %>% filter(`Current MMWR Year` %in% 2022:2025)
year_2026      <- national_all_years %>% filter(`Current MMWR Year` == 2026)

seasonal_baseline <- baseline_years %>%
  group_by(`MMWR WEEK`) %>%
  summarise(
    baseline_min  = min(current_week_clean, na.rm = TRUE),
    baseline_max  = max(current_week_clean, na.rm = TRUE),
    baseline_mean = mean(current_week_clean, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 5. Plot: 2026 against the 4-year seasonal band -------------------------

plot_data <- seasonal_baseline %>%
  filter(`MMWR WEEK` <= 25) %>%   # only compare where 2026 has data
  left_join(year_2026, by = "MMWR WEEK")

ggplot(plot_data, aes(x = `MMWR WEEK`)) +
  geom_ribbon(aes(ymin = baseline_min, ymax = baseline_max),
              fill = "grey80", alpha = 0.6) +
  geom_line(aes(y = baseline_mean), color = "grey40", linetype = "dashed") +
  geom_line(aes(y = current_week_clean), color = "#C44E52", linewidth = 1) +
  labs(
    title = "2026 U.S. Cyclosporiasis Cases vs. 2022-2025 Seasonal Range",
    subtitle = "Grey band = min-max range across 2022-2025. Dashed line = 4-year average.\nRed line = 2026 (the year being evaluated).",
    x = "MMWR Week", y = "Cases (current week)",
    caption = "Source: NNDSS Weekly Data, data.cdc.gov. 2026 data are provisional."
  ) +
  theme_minimal(base_size = 12)

ggsave("output/seasonal_baseline_national_2026-07-04.png", width = 9, height = 5, dpi = 150)

# ---- 6. Quantify: how does 2026 compare at the same point in the season? ---
# This directly answers "is 2026 actually unusual, or does this happen most
# years?" using 4 years of real comparison instead of 1.

week25_comparison <- national_all_years %>%
  filter(`MMWR WEEK` == 25) %>%
  arrange(`Current MMWR Year`) %>%
  select(`Current MMWR Year`, current_week_clean, cum_ytd_current_clean)

print(week25_comparison)

cat("\nCumulative cases through week 25, by year:\n")
for (i in seq_len(nrow(week25_comparison))) {
  cat(sprintf("  %s: %d cumulative cases (week 25 alone: %d)\n",
              week25_comparison$`Current MMWR Year`[i],
              week25_comparison$cum_ytd_current_clean[i],
              week25_comparison$current_week_clean[i]))
}

baseline_avg_cum <- week25_comparison %>%
  filter(`Current MMWR Year` %in% 2022:2025) %>%
  summarise(avg = mean(cum_ytd_current_clean, na.rm = TRUE)) %>%
  pull(avg)

cum_2026 <- week25_comparison %>% filter(`Current MMWR Year` == 2026) %>% pull(cum_ytd_current_clean)

cat(sprintf(
  "\n2026's cumulative total through week 25 (%d) is %.1fx the 2022-2025 average (%.0f) at the same point in the season.\n",
  cum_2026, cum_2026 / baseline_avg_cum, baseline_avg_cum
))

# ---- 7. Same analysis for Michigan ------------------------------------------
# This is the more important check: does Michigan's 52-week max (7-8, used
# in script 02) actually reflect a stable historical pattern, or was the
# comparison year itself unusual? With 4 years of data we can check directly
# instead of trusting a single CDC-calculated number.

michigan_all_years <- cyclo %>%
  filter(reporting_area_norm == "Michigan") %>%
  select(`Current MMWR Year`, `MMWR WEEK`, current_week_clean, cum_ytd_current_clean)

mi_week25_comparison <- michigan_all_years %>%
  filter(`MMWR WEEK` == 25) %>%
  arrange(`Current MMWR Year`)

print(mi_week25_comparison)

mi_baseline_seasonal <- michigan_all_years %>%
  filter(`Current MMWR Year` %in% 2022:2025) %>%
  group_by(`MMWR WEEK`) %>%
  summarise(
    baseline_min = min(current_week_clean, na.rm = TRUE),
    baseline_max = max(current_week_clean, na.rm = TRUE),
    .groups = "drop"
  )

mi_plot_data <- mi_baseline_seasonal %>%
  filter(`MMWR WEEK` <= 25) %>%
  left_join(michigan_all_years %>% filter(`Current MMWR Year` == 2026), by = "MMWR WEEK")

ggplot(mi_plot_data, aes(x = `MMWR WEEK`)) +
  geom_ribbon(aes(ymin = baseline_min, ymax = baseline_max), fill = "grey80", alpha = 0.6) +
  geom_line(aes(y = current_week_clean), color = "#C44E52", linewidth = 1) +
  labs(
    title = "2026 Michigan Cyclosporiasis Cases vs. 2022-2025 Range",
    subtitle = "Grey band = min-max range across the prior 4 complete years. Red = 2026.",
    x = "MMWR Week", y = "Cases (current week)",
    caption = "Source: NNDSS Weekly Data, data.cdc.gov. 2026 data are provisional and likely to revise upward (see script 02)."
  ) +
  theme_minimal(base_size = 12)

ggsave("output/seasonal_baseline_michigan_2026-07-04.png", width = 9, height = 5, dpi = 150)

cat("\nMichigan, cases in week 25 by year:\n")
for (i in seq_len(nrow(mi_week25_comparison))) {
  cat(sprintf("  %s: %d\n", mi_week25_comparison$`Current MMWR Year`[i], mi_week25_comparison$current_week_clean[i]))
}
cat("\nConclusion: check whether Michigan's week-25 count in prior years was ever\n")
cat("close to 2026's level. If prior years were consistently near zero, that\n")
cat("strengthens the case that 2026 is a genuine anomaly rather than normal noise.\n")

# ---- Next steps --------------------------------------------------------------
# - Once 2026 data extends past week 25, re-run this script with the fuller
#   year and watch how the red line moves relative to the grey band
# - Consider the same seasonal-band treatment for other states once/if
#   county-level or additional state-level detail becomes relevant
