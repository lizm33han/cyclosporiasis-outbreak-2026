# =============================================================================
# 04_per_state_comparison.R
#
# Purpose: Rank the 50 states + DC by how far 2026 cumulative cases are above
#          each state's own 2022-2025 level at the same MMWR week.
#
# Data: NNDSS Weekly Data, data.cdc.gov (see 00_load_and_clean.R)
# Run from an R project opened at the repo root so relative paths work.
# =============================================================================

source("scripts/00_load_and_clean.R")

state_data <- cyclo %>% filter(area %in% us_states)
stopifnot(n_distinct(state_data$area) == 51)

# ---- 1. Rank by cumulative cases at the latest published week ----------------
# Each state is compared with itself (its mean cumulative count in 2022-2025,
# as first published, at the same week), so a state with a small usual caseload
# is not judged against a large one.
now_vals <- state_data %>%
  filter(year == current_year, week == latest_week) %>%
  select(area, cases_2026 = cum_ytd)

baseline_vals <- state_data %>%
  filter(year %in% baseline_years, week == latest_week) %>%
  group_by(area) %>%
  summarise(baseline_avg = mean(cum_ytd, na.rm = TRUE), .groups = "drop")

national_cum <- cyclo %>%
  filter(area == "Total", year == current_year, week == latest_week) %>%
  pull(cum_ytd)

state_ranking <- now_vals %>%
  full_join(baseline_vals, by = "area") %>%
  mutate(
    cases_2026   = replace_na(cases_2026, 0),
    baseline_avg = replace_na(baseline_avg, 0),
    share_of_national = cases_2026 / national_cum,
    ratio_label = case_when(
      baseline_avg == 0 & cases_2026 == 0 ~ "no change (both 0)",
      baseline_avg == 0 & cases_2026 > 0  ~ "new (baseline was 0)",
      TRUE ~ paste0(round(cases_2026 / baseline_avg, 1), "x")
    )
  ) %>%
  arrange(desc(cases_2026))

print(state_ranking %>% select(area, baseline_avg, cases_2026, share_of_national, ratio_label), n = 15)

cat(sprintf("\nThe top 5 states account for %.0f%% of the national cumulative total (week %d).\n",
            100 * sum(head(state_ranking$cases_2026, 5)) / national_cum, latest_week))

# ---- 2. Small multiples: top 9 states ----------------------------------------
top_states <- state_ranking %>% slice_max(cases_2026, n = 9) %>% pull(area)

top_weekly <- state_data %>% filter(area %in% top_states, week <= latest_week)

band <- top_weekly %>%
  filter(year %in% baseline_years) %>%
  group_by(area, week) %>%
  summarise(lo = min_na(weekly_additions), hi = max_na(weekly_additions), .groups = "drop")

facet_data <- band %>%
  left_join(top_weekly %>% filter(year == current_year) %>%
              select(area, week, weekly_additions),
            by = c("area", "week")) %>%
  mutate(area = factor(area, levels = top_states))

p_states <- ggplot(facet_data, aes(x = week)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.7) +
  geom_line(aes(y = weekly_additions), color = accent_color, linewidth = 0.7) +
  facet_wrap(~ area, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "2026 vs. 2022-2025 range: top 9 states by cumulative cases",
    subtitle = "Reports added each week. Grey band = min-max across 2022-2025. Red = 2026. Y-axis scales differ by state.",
    x = "MMWR week", y = "Reports added that week",
    caption = paste0("Source: NNDSS Weekly Data, data.cdc.gov (export ", export_date, "). Provisional data.")
  ) +
  theme_epi(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave(paste0("output/top_states_small_multiples_", export_date, ".png"),
       p_states, width = 10, height = 8, dpi = 150)
