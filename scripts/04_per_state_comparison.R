# =============================================================================
# 04_per_state_comparison.R
#
# Purpose: Extend the seasonal-baseline approach from 03 to every U.S. state,
#          not just Michigan. Which states show the biggest departure from
#          their own 2022-2025 pattern in 2026?
#
# Data source: NNDSS Weekly Data, data.cdc.gov (same file as script 03)
# Run this from an R project opened at the repo root so relative paths work.
# =============================================================================

library(tidyverse)

# ---- 1. Import & clean (same approach as 03_seasonal_baseline.R) -----------

raw <- read_csv("data/NNDSS_Weekly_Data_2022-2026_20260703.csv", show_col_types = FALSE) %>%
  mutate(reporting_area_norm = str_to_title(`Reporting Area`))

clean_flagged_col <- function(value_col, flag_col) {
  numeric_val <- suppressWarnings(as.numeric(str_remove_all(as.character(value_col), ",")))
  case_when(
    !is.na(numeric_val)             ~ numeric_val,
    flag_col == "-"                 ~ 0,
    flag_col %in% c("N", "NC", "U") ~ NA_real_,
    TRUE ~ NA_real_
  )
}

cyclo <- raw %>%
  filter(Label == "Cyclosporiasis", `MMWR WEEK` <= 52) %>%
  mutate(current_week_clean = clean_flagged_col(`Current week`, `Current week, flag`))

# ---- 2. Restrict to the 50 states + DC --------------------------------------
# GOTCHA (found while building this script): simple str_to_title()
# normalization unifies the 50 states + DC cleanly across all 5 years, but
# NOT the aggregate/territory labels - "U.S. Residents" (2025-2026) vs.
# "Us Residents" (title-cased from "US RESIDENTS" in 2022-2024) still don't
# match, because the older years never had periods to begin with. Same
# problem for "U.S. Territories" and "Commonwealth Of Northern Mariana
# Islands" vs. "Northern Mariana Islands" (a genuine name change, not just
# case). None of this affects the 50-state list used here, but it would
# need separate handling if you extend this script to territories later.

us_states <- c(
  "Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
  "Delaware","District Of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois",
  "Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts",
  "Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada",
  "New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota",
  "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
  "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
  "West Virginia","Wisconsin","Wyoming"
)

state_data <- cyclo %>% filter(reporting_area_norm %in% us_states)

# Sanity check: should be 51 areas x 5 years-ish (2026 partial) worth of weeks
n_distinct(state_data$reporting_area_norm)  # should be 51

# ---- 3. Rank states by week-25 departure from their own 2022-2025 baseline -
# Week 25 chosen because it's the latest week common to all years in this
# snapshot (2026 only goes through week 25). Using each state's OWN baseline
# (not a national one) means a state with a small typical caseload isn't
# unfairly compared to a state like Texas.

week25 <- state_data %>%
  filter(`MMWR WEEK` == 25) %>%
  select(reporting_area_norm, `Current MMWR Year`, current_week_clean)

baseline_2022_2025 <- week25 %>%
  filter(`Current MMWR Year` %in% 2022:2025) %>%
  group_by(reporting_area_norm) %>%
  summarise(baseline_avg = mean(current_week_clean, na.rm = TRUE), .groups = "drop")

value_2026 <- week25 %>%
  filter(`Current MMWR Year` == 2026) %>%
  select(reporting_area_norm, cases_2026 = current_week_clean)

state_ranking <- baseline_2022_2025 %>%
  full_join(value_2026, by = "reporting_area_norm") %>%
  mutate(
    cases_2026 = replace_na(cases_2026, 0),
    baseline_avg = replace_na(baseline_avg, 0),
    # Ratio is undefined (Inf) when baseline is 0 and 2026 has cases -
    # label these explicitly as "new" rather than showing Inf in a table
    ratio_label = case_when(
      baseline_avg == 0 & cases_2026 == 0 ~ "no change (both 0)",
      baseline_avg == 0 & cases_2026 > 0  ~ "new (baseline was 0)",
      TRUE ~ paste0(round(cases_2026 / baseline_avg, 1), "x")
    )
  ) %>%
  arrange(desc(cases_2026))

print(state_ranking %>% select(reporting_area_norm, baseline_avg, cases_2026, ratio_label), n = 15)

# ---- 4. Small multiples: top 9 states by 2026 week-25 case count -----------
# Shows each state's weekly pattern across the baseline years (grey band) vs
# 2026 (red line) - the same visual language as the Michigan/national plots
# in script 03, so it's easy to compare across scripts.

top_states <- state_ranking %>% slice_max(cases_2026, n = 9) %>% pull(reporting_area_norm)

top_state_weekly <- state_data %>%
  filter(reporting_area_norm %in% top_states, `MMWR WEEK` <= 25)

baseline_band <- top_state_weekly %>%
  filter(`Current MMWR Year` %in% 2022:2025) %>%
  group_by(reporting_area_norm, `MMWR WEEK`) %>%
  summarise(
    baseline_min = min(current_week_clean, na.rm = TRUE),
    baseline_max = max(current_week_clean, na.rm = TRUE),
    .groups = "drop"
  )

year_2026_states <- top_state_weekly %>%
  filter(`Current MMWR Year` == 2026) %>%
  select(reporting_area_norm, `MMWR WEEK`, current_week_clean)

facet_data <- baseline_band %>%
  left_join(year_2026_states, by = c("reporting_area_norm", "MMWR WEEK")) %>%
  mutate(reporting_area_norm = factor(reporting_area_norm, levels = top_states))

ggplot(facet_data, aes(x = `MMWR WEEK`)) +
  geom_ribbon(aes(ymin = baseline_min, ymax = baseline_max), fill = "grey80", alpha = 0.6) +
  geom_line(aes(y = current_week_clean), color = "#C44E52", linewidth = 0.8) +
  facet_wrap(~ reporting_area_norm, scales = "free_y", ncol = 3) +
  labs(
    title = "2026 vs. 2022-2025 Range: Top 9 States by Week-25 Case Count",
    subtitle = "Grey band = min-max range across 2022-2025. Red line = 2026. Y-axis scales vary by state.",
    x = "MMWR Week", y = "Cases (current week)",
    caption = "Source: NNDSS Weekly Data, data.cdc.gov. 2026 data are provisional and subject to upward revision."
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave("output/top_states_small_multiples_2026-07-04.png", width = 10, height = 8, dpi = 150)

# ---- 5. Takeaway -------------------------------------------------------------
cat("\nStates with the largest week-25 case counts in 2026, and how that compares\n")
cat("to their own 2022-2025 baseline average:\n\n")
state_ranking %>%
  select(reporting_area_norm, baseline_avg, cases_2026, ratio_label) %>%
  slice_max(cases_2026, n = 10) %>%
  print(n = 10)

cat("\nMichigan stands out as the most extreme departure from baseline, but it's\n")
cat("not the only state running well above its typical pattern - New York, Ohio,\n")
cat("Florida, and Virginia are all several times their own historical week-25 norm.\n")
cat("This suggests something broader than a single Michigan-specific event may be\n")
cat("contributing to the 2026 season, consistent with CDC's public statements that\n")
cat("the cause of the multi-state rise hasn't been identified yet.\n")

# ---- Next steps --------------------------------------------------------------
# - If a common food source is later identified, cross-reference states with
#   documented supply-chain/distribution overlap for that product
# - Consider a proper choropleth map (tigris/sf) using cases_2026 or the
#   ratio column, now that we have real per-state numbers for all 50 states
