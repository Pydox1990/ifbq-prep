# 00_day1_baseline.R ----------------------------------------------------------
library(tidyverse)
library(janitor)
library(here)
library(survey)
library(lme4)
library(gtsummary)
library(gt)
library(skimr)

# ── 1 Import ────────────────────────────────────────────────────────────────
raw <- read_csv(here("data_raw", "hh_schools_sample.csv"))
glimpse(raw)


# ── 2 Clean ─────────────────────────────────────────────────────────────────
clean <- raw |>
  clean_names() |>
  mutate(
    district       = as_factor(district),
    school_type    = as_factor(school_type),
    rating_overall = factor(
      rating_overall,
      levels  = c("Sehr gut", "Gut", "Befriedigend", "Mangelhaft"),
      ordered = TRUE
    )
  )

levels(clean$rating_overall)
table(clean$rating_overall, useNA = "ifany")

# ── 3 Quick EDA ─────────────────────────────────────────────────────────────
skim(clean)   # glance at structure & missingness

clean |>
  ggplot(aes(rating_overall)) +
  geom_bar() +
  labs(title = "Distribution of overall ratings",
       x = NULL, y = "Schools") +
  theme_minimal()

# ── 4 Survey design (equal weights vs pupils_enrolled) ──────────────────────
des_equal <- svydesign(
  ids    = ~school_id,
  strata = ~district,
  weights = ~1,
  data   = clean
)

des_weight <- svydesign(                  # <-- patched: build directly
  ids     = ~school_id,
  strata  = ~district,
  weights = ~pupils_enrolled,             # formula OK here
  data    = clean
)

# Compare unweighted vs weighted mean rating
svymean(~rating_overall_num, design = des_equal)
svymean(~rating_overall_num, design = des_weight)

# ── 5 Post-stratification (stub for tomorrow) ───────────────────────────────
# post <- postStratify(des_weight, ...)

# ── 6 Random-intercept model ────────────────────────────────────────────────
mod <- lmer(rating_overall_num ~ 1 + (1 | district), data = clean)
summary(mod)

gtsave(tbl, file = here("reports", "tbl_model_day1.html"))
