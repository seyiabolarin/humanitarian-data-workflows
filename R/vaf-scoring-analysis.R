# Public offline adaptation. Uses local CSV inputs; no service access or remote writes.
source("R/local_io.R")
# =============================================================================
# vaf_scoring_analysis.R
# VAF Scoring — Standalone Analysis Script
#
# Analyses the main scoring blocks, FCS, and rCSI with disaggregations.
#
# Prerequisites:
#   - dplyr, lubridate, stringr, tidyr, ggplot2 installed
# =============================================================================


library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)

# =============================================================================
# 0 — CONFIG
# =============================================================================


OUTPUT_DIR <- "outputs/vaf-analysis"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

cat("========================================================\n")
cat(" VAF Scoring Analysis\n")
cat(sprintf(" Run at: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat("========================================================\n\n")


# =============================================================================
# 1 — PULL DATA  (mirrors scoring.R STEP 1, no pipeline_status filter)
#     Remove or adjust the filter() below to target specific records.
# =============================================================================

cat("[1/4] Pulling household records...\n")

raw_hh <- read_input("vaf-scoring-analysis-households.csv") %>%
  # ── SCOPE FILTER — adjust as needed ──────────────────────────────────────
  # To analyse all records: comment out or remove this filter
  # To match scoring.R behaviour: keep "Verified" | "Scored"
  filter(pipeline_status %in% c("Verified", "Scored"))

cat(sprintf("  → %d household record(s) retrieved.\n\n", nrow(raw_hh)))

# ---------------------------------------------------------------------------
cat("[2/4] Pulling member records...\n")

date_lookup <- raw_hh %>% select(id_assessment, date_assessment)

raw_members <- read_input("vaf-scoring-analysis-members.csv") %>%
  filter(hh_id %in% raw_hh$id_assessment)

cat(sprintf("  → %d member record(s) retrieved.\n\n", nrow(raw_members)))


# =============================================================================
# 2 — REPLICATE SCORING LOGIC
# =============================================================================

cat("[3/4] Scoring households...\n")

# ---------------------------------------------------------------------------
# 2a: Member-level aggregates
# ---------------------------------------------------------------------------

PLW_CHOICE <- "Yes, the woman is currently pregnant and/or currently breastfeeding a child under 2 years old"

members_calc <- raw_members %>%
  left_join(date_lookup, by = c("hh_id" = "id_assessment")) %>%
  mutate(
    child_age_months = as.integer(
      interval(as.Date(member_dob), as.Date(date_assessment)) / months(1)
    ),
    .any_disability = (
      member_wg_seeing_sev        %in% c("A lot of difficulty", "Cannot do at all") |
        member_wg_hearing_sev       %in% c("A lot of difficulty", "Cannot do at all") |
        member_wg_walking_sev       %in% c("A lot of difficulty", "Cannot do at all") |
        member_wg_remembering_sev   %in% c("A lot of difficulty", "Cannot do at all") |
        member_wg_selfcare_sev      %in% c("A lot of difficulty", "Cannot do at all") |
        member_wg_communicating_sev %in% c("A lot of difficulty", "Cannot do at all")
    ),
    calc_hoh_disable_yn = if_else(member_hoh == "Yes" & .any_disability, 1L, 0L),
    calc_mem_disable_yn = if_else(member_hoh == "No"  & .any_disability, 1L, 0L),
    calc_disable_total  = if_else(.any_disability, 1L, 0L),
    calc_hoh_illness_yn = if_else(member_hoh == "Yes" & member_health_chronic %in% c("Yes, mild - general","Yes, moderate","Yes, Severe"), 1L, 0L),
    calc_mem_illness_yn = if_else(member_hoh == "No"  & member_health_chronic %in% c("Yes, mild - general","Yes, moderate","Yes, Severe"), 1L, 0L),
    calc_hoh_plw_yn     = if_else(member_hoh == "Yes" & member_plw == PLW_CHOICE, 1L, 0L),
    calc_mem_plw_yn     = if_else(member_hoh == "No"  & member_plw == PLW_CHOICE, 1L, 0L),
    calc_child_hoh_yn   = if_else(member_hoh == "Yes" & member_age < 18,           1L, 0L),
    calc_fem_hoh_yn     = if_else(member_hoh == "Yes" & member_gender == "Female", 1L, 0L),
    calc_elderly_hoh_yn = if_else(member_hoh == "Yes" & member_age >= 50,          1L, 0L),
    calc_hoh_working_yn = if_else(member_hoh == "Yes" & member_working == "Yes",   1L, 0L),
    calc_is_boy         = if_else(member_gender == "Male"   & member_age < 18,  1L, 0L),
    calc_is_girl        = if_else(member_gender == "Female" & member_age < 18,  1L, 0L),
    calc_is_adult_man   = if_else(member_gender == "Male"   & member_age >= 18 & member_age < 50, 1L, 0L),
    calc_is_adult_woman = if_else(member_gender == "Female" & member_age >= 18 & member_age < 50, 1L, 0L),
    calc_is_elderly_man   = if_else(member_gender == "Male"   & member_age >= 50, 1L, 0L),
    calc_is_elderly_woman = if_else(member_gender == "Female" & member_age >= 50, 1L, 0L),
    
    # MAD
    .dds = (
      if_else(mad_breastfed    == "Yes", 1L, 0L) +
        if_else(mad_fg_grains    == "Yes", 1L, 0L) +
        if_else(mad_fg_legumes   == "Yes", 1L, 0L) +
        if_else(mad_fg_dairy     == "Yes", 1L, 0L) +
        if_else(mad_fg_flesh     == "Yes", 1L, 0L) +
        if_else(mad_fg_eggs      == "Yes", 1L, 0L) +
        if_else(mad_fg_vita_veg  == "Yes", 1L, 0L) +
        if_else(mad_fg_other_veg == "Yes", 1L, 0L)
    ),
    .mdd_fail  = .dds < 5,
    .mmf_fail  = if_else(
      mad_breastfed == "Yes",
      if_else(child_age_months <= 8, mad_meal_freq < 2, mad_meal_freq < 3),
      mad_meal_freq < 4
    ),
    .mmff_fail  = if_else(mad_breastfed == "No", mad_milk_feeds < 2, FALSE),
    .criteria_failed = as.integer(.mdd_fail) + as.integer(.mmf_fail) + as.integer(.mmff_fail),
    calc_child_mad_category = case_when(
      mad_yn != "Yes, this child" ~ NA_integer_,
      .criteria_failed == 0       ~ 0L,
      .criteria_failed == 1       ~ 4L,
      .criteria_failed >= 2       ~ 8L
    )
  )

hh_agg <- members_calc %>%
  group_by(hh_id) %>%
  summarise(
    hh_size              = n(),
    hoh_disable_count    = sum(calc_hoh_disable_yn, na.rm = TRUE),
    mem_disable_count    = sum(calc_mem_disable_yn, na.rm = TRUE),
    disable_total        = sum(calc_disable_total,  na.rm = TRUE),
    hoh_illness_count    = sum(calc_hoh_illness_yn, na.rm = TRUE),
    mem_illness_count    = sum(calc_mem_illness_yn, na.rm = TRUE),
    hoh_plw_count        = sum(calc_hoh_plw_yn,     na.rm = TRUE),
    mem_plw_count        = sum(calc_mem_plw_yn,     na.rm = TRUE),
    child_hoh_count      = sum(calc_child_hoh_yn,   na.rm = TRUE),
    fem_hoh_count        = sum(calc_fem_hoh_yn,     na.rm = TRUE),
    elderly_hoh_count    = sum(calc_elderly_hoh_yn, na.rm = TRUE),
    hoh_working_sum      = sum(calc_hoh_working_yn, na.rm = TRUE),
    n_boys               = sum(calc_is_boy,          na.rm = TRUE),
    n_girls              = sum(calc_is_girl,         na.rm = TRUE),
    n_adult_men          = sum(calc_is_adult_man,    na.rm = TRUE),
    n_adult_women        = sum(calc_is_adult_woman,  na.rm = TRUE),
    n_elderly_men        = sum(calc_is_elderly_man,  na.rm = TRUE),
    n_elderly_women      = sum(calc_is_elderly_woman, na.rm = TRUE),
    hh_mad_count         = sum(calc_child_mad_category > 0, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# 2b: HH-level scoring
# ---------------------------------------------------------------------------

hh_scored <- raw_hh %>%
  left_join(hh_agg, by = c("id_assessment" = "hh_id")) %>%
  mutate(
    
    # HOH flags (Yes/No)
    calc_child_hoh_yn   = if_else(child_hoh_count   > 0, "Yes", "No"),
    calc_fem_hoh_yn     = if_else(fem_hoh_count     > 0, "Yes", "No"),
    calc_elderly_hoh_yn = if_else(elderly_hoh_count > 0, "Yes", "No"),
    calc_hoh_disable_yn = if_else(hoh_disable_count > 0, "Yes", "No"),
    calc_mem_disable_yn = if_else(mem_disable_count > 0, "Yes", "No"),
    calc_hoh_illness_yn = if_else(hoh_illness_count > 0, "Yes", "No"),
    calc_mem_illness_yn = if_else(mem_illness_count > 0, "Yes", "No"),
    calc_hoh_plw_yn     = if_else(hoh_plw_count     > 0, "Yes", "No"),
    calc_mem_plw_yn     = if_else(mem_plw_count     > 0, "Yes", "No"),
    calc_hoh_working_yn = if_else(hoh_working_sum   > 0, "Yes", "No"),
    calc_hh_plw_count   = hoh_plw_count + mem_plw_count,
    calc_disable_total  = disable_total,
    
    n_children  = n_boys + n_girls,
    n_adults    = n_adult_men + n_adult_women,
    n_elderly   = n_elderly_men + n_elderly_women,
    calc_dependency_ratio = if_else(n_adults > 0, (n_children + n_elderly) / n_adults, NA_real_),
    
    # FCS
    .staples = pmin(fcs1 + fcs2, 7),
    calc_fcs_cumulative = .staples*2 + fcs3*3 + fcs4*1 + fcs5*1 + fcs6*4 + fcs7*4 + fcs8*0.5 + fcs9*0.5,
    calc_fcs_category   = case_when(
      is.na(calc_fcs_cumulative) ~ NA_character_,
      calc_fcs_cumulative < 29   ~ "POOR",
      calc_fcs_cumulative < 42   ~ "BORDERLINE",
      TRUE                       ~ "ACCEPTABLE"
    ),
    calc_fcs_category = factor(calc_fcs_category, levels = c("POOR", "BORDERLINE", "ACCEPTABLE")),
    
    # rCSI
    calc_rcsi_cumulative = rcsi_1*1 + rcsi_2*2 + rcsi_3*1 + rcsi_4*3 + rcsi_5*1,
    calc_rcsi_category   = case_when(
      is.na(calc_rcsi_cumulative) ~ NA_character_,
      calc_rcsi_cumulative <  4  ~ "Acceptable food security",
      calc_rcsi_cumulative >= 19 ~ "Severe food insecurity",
      TRUE                       ~ "Food stress"
    ),
    calc_rcsi_category = factor(calc_rcsi_category,
                                levels = c("Acceptable food security", "Food stress", "Severe food insecurity")),
    
    # Point scores
    calc_point_score_fcs = case_when(
      calc_fcs_category == "POOR"       ~ 10L,
      calc_fcs_category == "BORDERLINE" ~ 6L,
      TRUE                              ~ 0L
    ),
    calc_point_score_rcsi = case_when(
      calc_rcsi_cumulative > 29 ~ 10L,
      calc_rcsi_cumulative > 18 ~ 6L,
      TRUE                      ~ 0L
    ),
    calc_point_score_children_mad = case_when(
      hh_mad_count > 1 ~ 12L,
      hh_mad_count > 0 ~  8L,
      TRUE             ~  0L
    ),
    calc_point_score_hoh_category =
      if_else(calc_child_hoh_yn   == "Yes", 10L, 0L) +
      if_else(calc_elderly_hoh_yn == "Yes",  8L, 0L) +
      if_else(calc_fem_hoh_yn     == "Yes",  6L, 0L),
    calc_point_score_dependency = case_when(
      is.na(calc_dependency_ratio) ~ 6L,
      calc_dependency_ratio > 3    ~ 6L,
      calc_dependency_ratio >= 2   ~ 4L,
      TRUE                         ~ 0L
    ),
    calc_point_score_disability_hoh = if_else(calc_hoh_disable_yn == "Yes", 7L, 0L),
    calc_point_score_disability_hh  = if_else(
      (calc_disable_total - if_else(calc_hoh_disable_yn == "Yes", 1L, 0L)) > 0, 4L, 0L
    ),
    calc_point_score_illness_hoh  = if_else(calc_hoh_illness_yn == "Yes", 7L, 0L),
    calc_point_score_illness_mem  = if_else(calc_mem_illness_yn == "Yes", 4L, 0L),
    calc_point_score_plw_hoh      = if_else(calc_hoh_plw_yn     == "Yes", 4L, 0L),
    calc_point_score_plw_mem      = if_else(calc_mem_plw_yn     == "Yes", 4L, 0L),
    calc_point_score_income       = if_else(hh_main_income == "No working member in the HH earning income", 2L, 0L),
    calc_point_score_income_impact = if_else(hh_income_shock_impact == "Consequent decrease of income in the past 4 weeks due to a shock", 2L, 0L),
    calc_point_score_prod_asset   = if_else(hh_productive_asset == "No assets owned", 2L, 0L),
    calc_point_score_shelter_type = case_when(
      hh_shelter_type == "No shelter"                           ~ 6L,
      hh_shelter_type == "Makeshift - Tent - Emergency shelter" ~ 4L,
      hh_shelter_type == "Traditional shelter"                  ~ 2L,
      TRUE                                                      ~ 0L
    ),
    calc_point_score_shelter_cond = case_when(
      hh_shelter_cond == "Very poor" ~ 5L,
      hh_shelter_cond == "Poor"      ~ 3L,
      TRUE                           ~ 0L
    ),
    calc_point_score_water_cost = if_else(hh_water_cost  == "Yes", 2L, 0L),
    calc_point_score_soap       = if_else(hh_soap_access == "Yes", 1L, 0L, missing = 0L),
    calc_point_score_enrolled_wfp    = if_else(enrolled_wfp_yn            == "Yes", 1L, 0L),
    calc_point_score_referred        = if_else(referred_for_assistance_yn == "Yes", 1L, 0L),
    calc_point_score_received_assist = if_else(received_assist_3m_yn      == "Yes", 1L, 0L),
    
    # Sub-totals
    calc_total_score_demographic  = calc_point_score_hoh_category + calc_point_score_dependency,
    calc_total_score_socio_eco    = calc_point_score_disability_hoh + calc_point_score_disability_hh +
      calc_point_score_illness_hoh + calc_point_score_plw_hoh +
      calc_point_score_illness_mem + calc_point_score_plw_mem,
    calc_total_score_shelter      = calc_point_score_shelter_type + calc_point_score_shelter_cond +
      calc_point_score_water_cost + calc_point_score_soap,
    calc_total_score_income_asset = calc_point_score_income + calc_point_score_income_impact +
      calc_point_score_prod_asset,
    calc_total_score_food_sec     = calc_point_score_fcs + calc_point_score_rcsi + calc_point_score_children_mad,
    calc_total_score_exclusion    = calc_point_score_enrolled_wfp + calc_point_score_received_assist +
      calc_point_score_referred,
    
    # Total vulnerability score
    calc_total_vuln_assessment =
      calc_total_score_demographic  +
      calc_total_score_socio_eco    +
      calc_total_score_shelter      +
      calc_total_score_income_asset +
      calc_total_score_food_sec,
    
    # Classification
    calc_full_classification = case_when(
      calc_total_score_exclusion  > 0  ~ "Exclusion",
      calc_total_vuln_assessment >= 73 ~ "Extreme",
      calc_total_vuln_assessment >= 50 ~ "High",
      calc_total_vuln_assessment >= 25 ~ "Vulnerable",
      TRUE                             ~ "Not Prioritised"
    ),
    calc_full_classification = factor(calc_full_classification,
                                      levels = c("Exclusion", "Extreme", "High", "Vulnerable", "Not Prioritised"))
    
  ) %>%
  select(-.staples)

cat(sprintf("  → %d household(s) scored.\n\n", nrow(hh_scored)))


# =============================================================================
# 3 — ANALYSIS OUTPUTS
# =============================================================================

cat("[4/4] Running analysis...\n\n")

# ---------------------------------------------------------------------------
# Helper: quick summary table (n, %, mean score per group)
# ---------------------------------------------------------------------------
pct_tbl <- function(df, group_var) {
  df %>%
    count({{ group_var }}) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    arrange(desc(n))
}

disagg_score <- function(df, group_var) {
  df %>%
    group_by({{ group_var }}) %>%
    summarise(
      n                   = n(),
      mean_total_score    = round(mean(calc_total_vuln_assessment, na.rm = TRUE), 1),
      mean_food_sec_score = round(mean(calc_total_score_food_sec,  na.rm = TRUE), 1),
      mean_fcs            = round(mean(calc_fcs_cumulative,         na.rm = TRUE), 1),
      mean_rcsi           = round(mean(calc_rcsi_cumulative,        na.rm = TRUE), 1),
      pct_fcs_poor        = round(mean(calc_fcs_category == "POOR",       na.rm = TRUE) * 100, 1),
      pct_fcs_borderline  = round(mean(calc_fcs_category == "BORDERLINE", na.rm = TRUE) * 100, 1),
      pct_rcsi_severe     = round(mean(calc_rcsi_category == "Severe food insecurity", na.rm = TRUE) * 100, 1),
      pct_extreme         = round(mean(calc_full_classification == "Extreme",   na.rm = TRUE) * 100, 1),
      pct_high            = round(mean(calc_full_classification == "High",      na.rm = TRUE) * 100, 1),
      pct_excluded        = round(mean(calc_full_classification == "Exclusion", na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_total_score))
}

sep <- function(title) {
  cat("\n", strrep("─", 60), "\n")
  cat(sprintf(" %s\n", title))
  cat(strrep("─", 60), "\n")
}


# ==========================================================================
# SECTION A — OVERALL SCORE DISTRIBUTIONS
# ==========================================================================

sep("A. OVERALL VULNERABILITY SCORE DISTRIBUTION")

score_summary <- hh_scored %>%
  summarise(
    n           = n(),
    mean_score  = round(mean(calc_total_vuln_assessment, na.rm = TRUE), 1),
    median_score = median(calc_total_vuln_assessment, na.rm = TRUE),
    sd_score    = round(sd(calc_total_vuln_assessment, na.rm = TRUE), 1),
    min_score   = min(calc_total_vuln_assessment, na.rm = TRUE),
    max_score   = max(calc_total_vuln_assessment, na.rm = TRUE)
  )
print(score_summary)

sep("A1. CLASSIFICATION BREAKDOWN")
class_tbl <- pct_tbl(hh_scored, calc_full_classification)
print(class_tbl)
write.csv(class_tbl, file.path(OUTPUT_DIR, "A1_classification.csv"), row.names = FALSE)


# ==========================================================================
# SECTION B — SCORING BLOCK SUB-TOTALS
# ==========================================================================

sep("B. MEAN SCORE BY BLOCK (all HHs)")

block_means <- hh_scored %>%
  summarise(
    `Demographic (max 24)`   = round(mean(calc_total_score_demographic,  na.rm = TRUE), 1),
    `Socio-Economic (max 26)` = round(mean(calc_total_score_socio_eco,   na.rm = TRUE), 1),
    `Shelter & WASH (max 14)` = round(mean(calc_total_score_shelter,      na.rm = TRUE), 1),
    `Income & Assets (max 6)` = round(mean(calc_total_score_income_asset, na.rm = TRUE), 1),
    `Food Security (max 32)`  = round(mean(calc_total_score_food_sec,     na.rm = TRUE), 1),
    `Exclusion (max 3)`       = round(mean(calc_total_score_exclusion,    na.rm = TRUE), 1)
  ) %>%
  pivot_longer(everything(), names_to = "Block", values_to = "Mean Score")
print(block_means)
write.csv(block_means, file.path(OUTPUT_DIR, "B_block_means.csv"), row.names = FALSE)

sep("B1. SCORE BLOCK DISTRIBUTION BY CLASSIFICATION")

block_by_class <- hh_scored %>%
  group_by(calc_full_classification) %>%
  summarise(
    n              = n(),
    Demographic    = round(mean(calc_total_score_demographic,  na.rm = TRUE), 1),
    `Socio-Eco`    = round(mean(calc_total_score_socio_eco,   na.rm = TRUE), 1),
    Shelter        = round(mean(calc_total_score_shelter,      na.rm = TRUE), 1),
    `Income/Asset` = round(mean(calc_total_score_income_asset, na.rm = TRUE), 1),
    `Food Sec`     = round(mean(calc_total_score_food_sec,     na.rm = TRUE), 1),
    .groups = "drop"
  )
print(block_by_class)
write.csv(block_by_class, file.path(OUTPUT_DIR, "B1_blocks_by_classification.csv"), row.names = FALSE)


# ==========================================================================
# SECTION C — FCS ANALYSIS
# ==========================================================================

sep("C. FCS — OVERALL DISTRIBUTION")

fcs_summary <- hh_scored %>%
  summarise(
    n             = n(),
    mean_fcs      = round(mean(calc_fcs_cumulative, na.rm = TRUE), 1),
    median_fcs    = median(calc_fcs_cumulative, na.rm = TRUE),
    sd_fcs        = round(sd(calc_fcs_cumulative,   na.rm = TRUE), 1),
    pct_poor      = round(mean(calc_fcs_category == "POOR",       na.rm = TRUE) * 100, 1),
    pct_borderline = round(mean(calc_fcs_category == "BORDERLINE", na.rm = TRUE) * 100, 1),
    pct_acceptable = round(mean(calc_fcs_category == "ACCEPTABLE", na.rm = TRUE) * 100, 1)
  )
print(fcs_summary)

sep("C1. FCS CATEGORY BREAKDOWN")
fcs_cat_tbl <- pct_tbl(hh_scored, calc_fcs_category)
print(fcs_cat_tbl)

sep("C2. FCS BY FOOD GROUPS (mean days consumed)")
fcs_fg <- hh_scored %>%
  summarise(
    `Cereals/Tubers (fcs1+fcs2, capped 7)` = round(mean(pmin(fcs1 + fcs2, 7), na.rm = TRUE), 1),
    `Pulses/Legumes (fcs3)`                 = round(mean(fcs3, na.rm = TRUE), 1),
    `Vegetables (fcs4)`                     = round(mean(fcs4, na.rm = TRUE), 1),
    `Fruit (fcs5)`                          = round(mean(fcs5, na.rm = TRUE), 1),
    `Meat/Fish (fcs6)`                      = round(mean(fcs6, na.rm = TRUE), 1),
    `Dairy (fcs7)`                          = round(mean(fcs7, na.rm = TRUE), 1),
    `Sugar/Honey (fcs8)`                    = round(mean(fcs8, na.rm = TRUE), 1),
    `Oils/Fats (fcs9)`                      = round(mean(fcs9, na.rm = TRUE), 1)
  ) %>%
  pivot_longer(everything(), names_to = "Food Group", values_to = "Mean Days")
print(fcs_fg)
write.csv(fcs_fg, file.path(OUTPUT_DIR, "C2_fcs_food_groups.csv"), row.names = FALSE)

sep("C3. FCS BY AREA STATE")
fcs_state <- disagg_score(hh_scored, area_state) %>%
  select(area_state, n, mean_fcs, pct_fcs_poor, pct_fcs_borderline)
print(fcs_state)
write.csv(fcs_state, file.path(OUTPUT_DIR, "C3_fcs_by_state.csv"), row.names = FALSE)

sep("C4. FCS BY POPULATION TYPE (hh_status)")
fcs_pop <- disagg_score(hh_scored, hh_status) %>%
  select(hh_status, n, mean_fcs, pct_fcs_poor, pct_fcs_borderline)
print(fcs_pop)

sep("C5. FCS BY FEMALE-HEADED HH")
fcs_fhh <- disagg_score(hh_scored, calc_fem_hoh_yn) %>%
  select(calc_fem_hoh_yn, n, mean_fcs, pct_fcs_poor, pct_fcs_borderline)
print(fcs_fhh)

sep("C6. FCS BY INTERVENTION TYPE")
fcs_int <- disagg_score(hh_scored, intervention_type) %>%
  select(intervention_type, n, mean_fcs, pct_fcs_poor, pct_fcs_borderline)
print(fcs_int)
write.csv(fcs_int, file.path(OUTPUT_DIR, "C6_fcs_by_intervention.csv"), row.names = FALSE)

sep("C7. FCS BY PARTNER")
fcs_partner <- disagg_score(hh_scored, partner_name) %>%
  select(partner_name, n, mean_fcs, pct_fcs_poor, pct_fcs_borderline)
print(fcs_partner)
write.csv(fcs_partner, file.path(OUTPUT_DIR, "C7_fcs_by_partner.csv"), row.names = FALSE)

sep("C8. FCS BY COMMUNITY TYPE")
fcs_comm <- disagg_score(hh_scored, community_type) %>%
  select(community_type, n, mean_fcs, pct_fcs_poor, pct_fcs_borderline)
print(fcs_comm)


# ==========================================================================
# SECTION D — rCSI ANALYSIS
# ==========================================================================

sep("D. rCSI — OVERALL DISTRIBUTION")

rcsi_summary <- hh_scored %>%
  summarise(
    n                = n(),
    mean_rcsi        = round(mean(calc_rcsi_cumulative, na.rm = TRUE), 1),
    median_rcsi      = median(calc_rcsi_cumulative, na.rm = TRUE),
    sd_rcsi          = round(sd(calc_rcsi_cumulative,   na.rm = TRUE), 1),
    pct_acceptable   = round(mean(calc_rcsi_category == "Acceptable food security",  na.rm = TRUE) * 100, 1),
    pct_food_stress  = round(mean(calc_rcsi_category == "Food stress",               na.rm = TRUE) * 100, 1),
    pct_severe       = round(mean(calc_rcsi_category == "Severe food insecurity",    na.rm = TRUE) * 100, 1)
  )
print(rcsi_summary)

sep("D1. rCSI CATEGORY BREAKDOWN")
rcsi_cat_tbl <- pct_tbl(hh_scored, calc_rcsi_category)
print(rcsi_cat_tbl)

sep("D2. rCSI STRATEGY BREAKDOWN (mean frequency used per week)")
rcsi_strategies <- hh_scored %>%
  summarise(
    `S1 — Rely on less preferred foods (×1)`              = round(mean(rcsi_1, na.rm = TRUE), 2),
    `S2 — Borrow food/money (×2)`                         = round(mean(rcsi_2, na.rm = TRUE), 2),
    `S3 — Reduce meal portions (×1)`                      = round(mean(rcsi_3, na.rm = TRUE), 2),
    `S4 — Reduce number of meals/day (×3)`                = round(mean(rcsi_4, na.rm = TRUE), 2),
    `S5 — No food for adults so children can eat (×1)`    = round(mean(rcsi_5, na.rm = TRUE), 2)
  ) %>%
  pivot_longer(everything(), names_to = "Strategy", values_to = "Mean Days/Week")
print(rcsi_strategies)
write.csv(rcsi_strategies, file.path(OUTPUT_DIR, "D2_rcsi_strategies.csv"), row.names = FALSE)

sep("D3. rCSI BY AREA STATE")
rcsi_state <- disagg_score(hh_scored, area_state) %>%
  select(area_state, n, mean_rcsi, pct_rcsi_severe)
print(rcsi_state)
write.csv(rcsi_state, file.path(OUTPUT_DIR, "D3_rcsi_by_state.csv"), row.names = FALSE)

sep("D4. rCSI BY POPULATION TYPE (hh_status)")
rcsi_pop <- disagg_score(hh_scored, hh_status) %>%
  select(hh_status, n, mean_rcsi, pct_rcsi_severe)
print(rcsi_pop)

sep("D5. rCSI BY FEMALE-HEADED HH")
rcsi_fhh <- disagg_score(hh_scored, calc_fem_hoh_yn) %>%
  select(calc_fem_hoh_yn, n, mean_rcsi, pct_rcsi_severe)
print(rcsi_fhh)

sep("D6. rCSI BY INTERVENTION TYPE")
rcsi_int <- disagg_score(hh_scored, intervention_type) %>%
  select(intervention_type, n, mean_rcsi, pct_rcsi_severe)
print(rcsi_int)
write.csv(rcsi_int, file.path(OUTPUT_DIR, "D6_rcsi_by_intervention.csv"), row.names = FALSE)

sep("D7. rCSI BY PARTNER")
rcsi_partner <- disagg_score(hh_scored, partner_name) %>%
  select(partner_name, n, mean_rcsi, pct_rcsi_severe)
print(rcsi_partner)
write.csv(rcsi_partner, file.path(OUTPUT_DIR, "D7_rcsi_by_partner.csv"), row.names = FALSE)

sep("D8. rCSI BY COMMUNITY TYPE")
rcsi_comm <- disagg_score(hh_scored, community_type) %>%
  select(community_type, n, mean_rcsi, pct_rcsi_severe)
print(rcsi_comm)


# ==========================================================================
# SECTION E — COMBINED FCS × rCSI CROSS-TABULATION
# ==========================================================================

sep("E. FCS × rCSI CROSS-TABULATION (% of HHs)")

fcs_rcsi_cross <- hh_scored %>%
  filter(!is.na(calc_fcs_category), !is.na(calc_rcsi_category)) %>%
  count(calc_fcs_category, calc_rcsi_category) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(calc_fcs_category, calc_rcsi_category)
print(fcs_rcsi_cross)
write.csv(fcs_rcsi_cross, file.path(OUTPUT_DIR, "E_fcs_rcsi_crosstab.csv"), row.names = FALSE)


# ==========================================================================
# SECTION F — FULL DISAGGREGATED SCORE TABLE
# ==========================================================================

sep("F. FULL DISAGGREGATED TABLE — BY STATE")
full_state <- disagg_score(hh_scored, area_state)
print(full_state, n = Inf)
write.csv(full_state, file.path(OUTPUT_DIR, "F_full_disagg_by_state.csv"), row.names = FALSE)

sep("F1. FULL DISAGGREGATED TABLE — BY LGA")
full_lga <- disagg_score(hh_scored, area_lga)
print(full_lga, n = Inf)
write.csv(full_lga, file.path(OUTPUT_DIR, "F1_full_disagg_by_lga.csv"), row.names = FALSE)

sep("F2. FULL DISAGGREGATED TABLE — BY PARTNER")
full_partner <- disagg_score(hh_scored, partner_name)
print(full_partner, n = Inf)
write.csv(full_partner, file.path(OUTPUT_DIR, "F2_full_disagg_by_partner.csv"), row.names = FALSE)


# ==========================================================================
# SECTION G — PLOTS  (saved to analysis_output/)
# ==========================================================================

sep("G. GENERATING PLOTS...")

theme_vaf <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

# G1 — Classification bar
p_class <- ggplot(hh_scored, aes(x = calc_full_classification, fill = calc_full_classification)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "Exclusion"     = "#7F7F7F",
    "Extreme"       = "#D62728",
    "High"          = "#FF7F0E",
    "Vulnerable"    = "#FFDD57",
    "Not Prioritised" = "#2CA02C"
  ), guide = "none") +
  labs(title = "VAF Classification — All Households",
       x = NULL, y = "Number of Households") +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "G1_classification.png"), p_class, width = 7, height = 4, dpi = 150)

# G2 — FCS distribution
p_fcs <- ggplot(hh_scored %>% filter(!is.na(calc_fcs_cumulative)),
                aes(x = calc_fcs_cumulative, fill = calc_fcs_category)) +
  geom_histogram(binwidth = 3, colour = "white") +
  geom_vline(xintercept = c(28, 42), linetype = "dashed", colour = "grey30") +
  scale_fill_manual(values = c("POOR" = "#D62728", "BORDERLINE" = "#FF7F0E", "ACCEPTABLE" = "#2CA02C"),
                    name = "FCS Category") +
  labs(title = "Food Consumption Score (FCS) Distribution",
       x = "FCS Cumulative Score", y = "Number of Households") +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "G2_fcs_distribution.png"), p_fcs, width = 8, height = 4, dpi = 150)

# G3 — rCSI distribution
p_rcsi <- ggplot(hh_scored %>% filter(!is.na(calc_rcsi_cumulative)),
                 aes(x = calc_rcsi_cumulative, fill = calc_rcsi_category)) +
  geom_histogram(binwidth = 2, colour = "white") +
  geom_vline(xintercept = c(4, 19), linetype = "dashed", colour = "grey30") +
  scale_fill_manual(
    values = c("Acceptable food security" = "#2CA02C",
               "Food stress"              = "#FF7F0E",
               "Severe food insecurity"   = "#D62728"),
    name = "rCSI Category") +
  labs(title = "Reduced Coping Strategy Index (rCSI) Distribution",
       x = "rCSI Cumulative Score", y = "Number of Households") +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "G3_rcsi_distribution.png"), p_rcsi, width = 8, height = 4, dpi = 150)

# G4 — FCS category by state (stacked %)
if (n_distinct(hh_scored$area_state) > 1) {
  p_fcs_state <- hh_scored %>%
    filter(!is.na(calc_fcs_category), !is.na(area_state)) %>%
    count(area_state, calc_fcs_category) %>%
    group_by(area_state) %>%
    mutate(pct = n / sum(n)) %>%
    ggplot(aes(x = reorder(area_state, -pct * (calc_fcs_category == "POOR")),
               y = pct, fill = calc_fcs_category)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c("POOR" = "#D62728", "BORDERLINE" = "#FF7F0E", "ACCEPTABLE" = "#2CA02C"),
                      name = NULL) +
    labs(title = "FCS Category by State", x = NULL, y = "% of Households") +
    coord_flip() + theme_vaf
  ggsave(file.path(OUTPUT_DIR, "G4_fcs_by_state.png"), p_fcs_state, width = 8, height = 5, dpi = 150)
}

# G5 — rCSI category by state
if (n_distinct(hh_scored$area_state) > 1) {
  p_rcsi_state <- hh_scored %>%
    filter(!is.na(calc_rcsi_category), !is.na(area_state)) %>%
    count(area_state, calc_rcsi_category) %>%
    group_by(area_state) %>%
    mutate(pct = n / sum(n)) %>%
    ggplot(aes(x = reorder(area_state, pct * (calc_rcsi_category == "Severe food insecurity")),
               y = pct, fill = calc_rcsi_category)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(
      values = c("Acceptable food security" = "#2CA02C",
                 "Food stress"              = "#FF7F0E",
                 "Severe food insecurity"   = "#D62728"),
      name = NULL) +
    labs(title = "rCSI Category by State", x = NULL, y = "% of Households") +
    coord_flip() + theme_vaf
  ggsave(file.path(OUTPUT_DIR, "G5_rcsi_by_state.png"), p_rcsi_state, width = 8, height = 5, dpi = 150)
}

# G6 — Score block contribution (stacked bar by classification)
block_long <- hh_scored %>%
  select(calc_full_classification,
         Demographic   = calc_total_score_demographic,
         `Socio-Eco`   = calc_total_score_socio_eco,
         Shelter       = calc_total_score_shelter,
         `Income/Asset` = calc_total_score_income_asset,
         `Food Security` = calc_total_score_food_sec) %>%
  pivot_longer(-calc_full_classification, names_to = "Block", values_to = "Score") %>%
  group_by(calc_full_classification, Block) %>%
  summarise(mean_score = mean(Score, na.rm = TRUE), .groups = "drop")

p_blocks <- ggplot(block_long, aes(x = calc_full_classification, y = mean_score, fill = Block)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Set2", name = "Score Block") +
  labs(title = "Mean Score Block Contribution by Classification",
       x = NULL, y = "Mean Points") +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "G6_score_blocks_by_class.png"), p_blocks, width = 8, height = 5, dpi = 150)

cat(sprintf("\n  → Plots saved to '%s/'\n", OUTPUT_DIR))


# ==========================================================================
# DONE
# ==========================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat(sprintf(" Analysis complete — %d household(s) analysed.\n", nrow(hh_scored)))
cat(sprintf(" CSV outputs and plots saved to: %s/\n", OUTPUT_DIR))
cat(strrep("=", 60), "\n\n")

# =============================================================================
# vaf_analysis_addon.R
# Add-on: Extended ggplot outputs + Demographic / Vulnerability Profiling
#
# Run AFTER vaf_scoring_analysis.R — requires these objects in your environment:
#   hh_scored      (scored household data frame)
#   members_calc   (member-level data frame with demographic flags)
#   OUTPUT_DIR     (set to "analysis_output" by default below)
#
# If running standalone, source the main script first:
#   source("vaf_scoring_analysis.R")
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- "outputs/vaf-analysis"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

sep <- function(title) {
  cat("\n", strrep("─", 60), "\n")
  cat(sprintf(" %s\n", title))
  cat(strrep("─", 60), "\n")
}

theme_vaf <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

vaf_fill <- scale_fill_manual(
  values = c(
    "Exclusion"      = "#7F7F7F",
    "Extreme"        = "#D62728",
    "High"           = "#FF7F0E",
    "Vulnerable"     = "#FFDD57",
    "Not Prioritised"= "#2CA02C"
  ), name = NULL
)

cat("========================================================\n")
cat(" VAF Add-on: Extended Plots + Demographic Profiling\n")
cat(sprintf(" Run at: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat("========================================================\n\n")


# =============================================================================
# SECTION H — EXTENDED GGPLOT OUTPUTS
# =============================================================================

sep("H1. Vulnerability score histogram with classification bands")

p_h1 <- ggplot(hh_scored, aes(x = calc_total_vuln_assessment, fill = calc_full_classification)) +
  geom_histogram(binwidth = 3, colour = "white") +
  geom_vline(xintercept = c(25, 50, 73), linetype = "dashed", colour = "grey30", linewidth = 0.6) +
  annotate("text", x = c(12, 37, 61, 85), y = Inf, vjust = 1.5, size = 3, colour = "grey30",
           label = c("Not Prioritised", "Vulnerable", "High", "Extreme")) +
  vaf_fill +
  labs(
    title    = "Total Vulnerability Score Distribution",
    subtitle = "Dashed lines = classification thresholds (25 / 50 / 73)",
    x = "Total Vulnerability Score (max 103)", y = "Number of Households"
  ) +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "H1_vuln_score_histogram.png"), p_h1, width = 9, height = 5, dpi = 150)
cat("  ✓ H1 saved\n")


sep("H2. Score block contribution — stacked bar (mean points per block) by classification")

block_long <- hh_scored %>%
  select(
    calc_full_classification,
    Demographic    = calc_total_score_demographic,
    `Socio-Eco`    = calc_total_score_socio_eco,
    Shelter        = calc_total_score_shelter,
    `Income/Asset` = calc_total_score_income_asset,
    `Food Security` = calc_total_score_food_sec
  ) %>%
  pivot_longer(-calc_full_classification, names_to = "Block", values_to = "Score") %>%
  group_by(calc_full_classification, Block) %>%
  summarise(mean_score = mean(Score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Block = factor(Block, levels = c("Demographic","Socio-Eco","Shelter","Income/Asset","Food Security")))

p_h2 <- ggplot(block_long, aes(x = calc_full_classification, y = mean_score, fill = Block)) +
  geom_col(position = "stack", colour = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(mean_score > 0.5, round(mean_score, 1), "")),
            position = position_stack(vjust = 0.5), size = 2.8, colour = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Set2", name = "Score Block") +
  labs(
    title    = "Mean Score Block Contribution by Classification",
    subtitle = "Numbers show mean points contributed per block",
    x = NULL, y = "Mean Points"
  ) +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "H2_score_blocks_stacked.png"), p_h2, width = 9, height = 5, dpi = 150)
cat("  ✓ H2 saved\n")


sep("H3. FCS × rCSI scatter — coloured by classification")

p_h3 <- hh_scored %>%
  filter(!is.na(calc_fcs_cumulative), !is.na(calc_rcsi_cumulative)) %>%
  ggplot(aes(x = calc_fcs_cumulative, y = calc_rcsi_cumulative, colour = calc_full_classification)) +
  geom_jitter(alpha = 0.55, size = 1.8, width = 0.8, height = 0.4) +
  geom_vline(xintercept = c(28, 42), linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_hline(yintercept = c(4, 19),  linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  scale_colour_manual(
    values = c(
      "Exclusion" = "#7F7F7F", "Extreme" = "#D62728",
      "High" = "#FF7F0E", "Vulnerable" = "#FFDD57", "Not Prioritised" = "#2CA02C"
    ), name = NULL
  ) +
  labs(
    title    = "FCS vs rCSI — All Households",
    subtitle = "Dashed lines = FCS thresholds (28/42) and rCSI thresholds (4/19)",
    x = "FCS Cumulative Score", y = "rCSI Cumulative Score"
  ) +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "H3_fcs_rcsi_scatter.png"), p_h3, width = 9, height = 6, dpi = 150)
cat("  ✓ H3 saved\n")


sep("H4. FCS category by population type (IDP / host / other) — stacked %")

p_h4 <- hh_scored %>%
  filter(!is.na(calc_fcs_category), !is.na(hh_status)) %>%
  count(hh_status, calc_fcs_category) %>%
  group_by(hh_status) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = hh_status, y = pct, fill = calc_fcs_category)) +
  geom_col() +
  geom_text(aes(label = ifelse(pct > 0.04, percent(pct, 1), "")),
            position = position_stack(vjust = 0.5), size = 3, colour = "white", fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("POOR" = "#D62728", "BORDERLINE" = "#FF7F0E", "ACCEPTABLE" = "#2CA02C"),
                    name = "FCS Category") +
  labs(title = "FCS Category by Population Type", x = NULL, y = "% of Households") +
  coord_flip() + theme_vaf
ggsave(file.path(OUTPUT_DIR, "H4_fcs_by_pop_type.png"), p_h4, width = 9, height = 4, dpi = 150)
cat("  ✓ H4 saved\n")


sep("H5. rCSI strategy heatmap — mean days used by classification")

strategy_labels <- c(
  rcsi_1 = "Less preferred foods (×1)",
  rcsi_2 = "Borrow food/money (×2)",
  rcsi_3 = "Reduce portions (×1)",
  rcsi_4 = "Reduce meals/day (×3)",
  rcsi_5 = "Adults skip meals (×1)"
)

rcsi_heat <- hh_scored %>%
  select(calc_full_classification, rcsi_1, rcsi_2, rcsi_3, rcsi_4, rcsi_5) %>%
  pivot_longer(-calc_full_classification, names_to = "Strategy", values_to = "Days") %>%
  group_by(calc_full_classification, Strategy) %>%
  summarise(mean_days = mean(Days, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Strategy = recode(Strategy, !!!strategy_labels),
    Strategy = str_wrap(Strategy, 24)
  )

p_h5 <- ggplot(rcsi_heat, aes(x = calc_full_classification, y = Strategy, fill = mean_days)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(mean_days, 1)), size = 3, colour = "white", fontface = "bold") +
  scale_fill_gradient(low = "#FFF5EB", high = "#8B1A00", name = "Mean days/week") +
  labs(
    title    = "rCSI Strategy Intensity by Classification",
    subtitle = "Mean days per week each coping strategy was used",
    x = NULL, y = NULL
  ) +
  theme_vaf + theme(legend.position = "right")
ggsave(file.path(OUTPUT_DIR, "H5_rcsi_strategy_heatmap.png"), p_h5, width = 9, height = 5, dpi = 150)
cat("  ✓ H5 saved\n")


sep("H6. Classification breakdown by partner (stacked %)")

p_h6 <- hh_scored %>%
  filter(!is.na(partner_name)) %>%
  count(partner_name, calc_full_classification) %>%
  group_by(partner_name) %>%
  mutate(
    pct       = n / sum(n),
    total_hhs = sum(n),
    pct_extreme = sum(n[calc_full_classification == "Extreme"]) / total_hhs
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x    = reorder(str_wrap(partner_name, 30), pct_extreme),
    y    = pct,
    fill = calc_full_classification
  )) +
  geom_col() +
  geom_text(aes(label = ifelse(pct > 0.05, percent(pct, 1), "")),
            position = position_stack(vjust = 0.5), size = 2.6, colour = "white", fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  vaf_fill +
  labs(title = "Classification by Partner (ordered by % Extreme)",
       x = NULL, y = "% of Households") +
  coord_flip() + theme_vaf
ggsave(file.path(OUTPUT_DIR, "H6_classification_by_partner.png"), p_h6, width = 10, height = 6, dpi = 150)
cat("  ✓ H6 saved\n")


sep("H7. Classification breakdown by intervention type")

p_h7 <- hh_scored %>%
  filter(!is.na(intervention_type)) %>%
  count(intervention_type, calc_full_classification) %>%
  group_by(intervention_type) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = reorder(str_wrap(intervention_type, 28), pct * (calc_full_classification == "Extreme")),
             y = pct, fill = calc_full_classification)) +
  geom_col() +
  geom_text(aes(label = ifelse(pct > 0.05, percent(pct, 1), "")),
            position = position_stack(vjust = 0.5), size = 2.8, colour = "white", fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  vaf_fill +
  labs(title = "Classification by Intervention Type", x = NULL, y = "% of Households") +
  coord_flip() + theme_vaf
ggsave(file.path(OUTPUT_DIR, "H7_classification_by_intervention.png"), p_h7, width = 9, height = 5, dpi = 150)
cat("  ✓ H7 saved\n")


sep("H8. FCS food group radar-style bar — mean days consumed")

fg_means <- hh_scored %>%
  summarise(
    `Cereals &\nTubers`   = mean(pmin(fcs1 + fcs2, 7), na.rm = TRUE),
    `Pulses &\nLegumes`   = mean(fcs3, na.rm = TRUE),
    `Vegetables`          = mean(fcs4, na.rm = TRUE),
    `Fruit`               = mean(fcs5, na.rm = TRUE),
    `Meat &\nFish`        = mean(fcs6, na.rm = TRUE),
    `Dairy`               = mean(fcs7, na.rm = TRUE),
    `Sugar &\nHoney`      = mean(fcs8, na.rm = TRUE),
    `Oils &\nFats`        = mean(fcs9, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Food Group", values_to = "Mean Days") %>%
  mutate(`Food Group` = factor(`Food Group`, levels = `Food Group`))

p_h8 <- ggplot(fg_means, aes(x = `Food Group`, y = `Mean Days`)) +
  geom_col(fill = "#1F77B4", alpha = 0.85) +
  geom_text(aes(label = round(`Mean Days`, 1)), vjust = -0.4, size = 3.2) +
  geom_hline(yintercept = 7, linetype = "dashed", colour = "grey50") +
  annotate("text", x = 0.6, y = 7.15, label = "Max (7 days)", size = 3, colour = "grey50", hjust = 0) +
  scale_y_continuous(limits = c(0, 7.8)) +
  labs(
    title    = "FCS — Mean Days Each Food Group Consumed (past 7 days)",
    subtitle = "All households",
    x = NULL, y = "Mean days/week"
  ) +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "H8_fcs_food_groups_bar.png"), p_h8, width = 9, height = 5, dpi = 150)
cat("  ✓ H8 saved\n")


sep("H9. rCSI category by female-headed vs male-headed HH")

p_h9 <- hh_scored %>%
  filter(!is.na(calc_rcsi_category), !is.na(calc_fem_hoh_yn)) %>%
  mutate(hoh_type = if_else(calc_fem_hoh_yn == "Yes", "Female-headed", "Male-headed")) %>%
  count(hoh_type, calc_rcsi_category) %>%
  group_by(hoh_type) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = hoh_type, y = pct, fill = calc_rcsi_category)) +
  geom_col() +
  geom_text(aes(label = ifelse(pct > 0.04, percent(pct, 1), "")),
            position = position_stack(vjust = 0.5), size = 3.5, colour = "white", fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("Acceptable food security" = "#2CA02C", "Food stress" = "#FF7F0E", "Severe food insecurity" = "#D62728"),
    name = NULL) +
  labs(title = "rCSI Category by HoH Gender", x = NULL, y = "% of Households") +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "H9_rcsi_by_hoh_gender.png"), p_h9, width = 7, height = 5, dpi = 150)
cat("  ✓ H9 saved\n")


# =============================================================================
# SECTION I — DEMOGRAPHIC / VULNERABILITY PROFILING
# =============================================================================

sep("I1. HOUSEHOLD SIZE DISTRIBUTION")

hh_size_summary <- hh_scored %>%
  filter(!is.na(hh_size)) %>%
  summarise(
    n        = n(),
    mean_hh  = round(mean(hh_size), 1),
    median_hh = median(hh_size),
    pct_large = round(mean(hh_size >= 7) * 100, 1)
  )
print(hh_size_summary)

p_i1 <- hh_scored %>%
  filter(!is.na(hh_size)) %>%
  ggplot(aes(x = hh_size, fill = calc_full_classification)) +
  geom_histogram(binwidth = 1, colour = "white") +
  geom_vline(xintercept = hh_size_summary$mean_hh, linetype = "dashed", colour = "grey30") +
  annotate("text", x = hh_size_summary$mean_hh + 0.3, y = Inf, vjust = 1.5,
           label = paste0("Mean = ", hh_size_summary$mean_hh), size = 3, colour = "grey30") +
  vaf_fill +
  labs(title = "Household Size Distribution", x = "Number of Members", y = "Number of Households") +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "I1_hh_size_distribution.png"), p_i1, width = 9, height = 5, dpi = 150)
cat("  ✓ I1 saved\n")


sep("I2. AGE-SEX PYRAMID (all surveyed members)")

pyramid_data <- members_calc %>%
  filter(!is.na(member_age), !is.na(member_gender),
         member_gender %in% c("Male", "Female")) %>%
  mutate(
    age_band = cut(member_age,
                   breaks = c(0, 5, 10, 15, 18, 25, 35, 50, 65, Inf),
                   labels = c("0–4","5–9","10–14","15–17","18–24","25–34","35–49","50–64","65+"),
                   right  = FALSE, include.lowest = TRUE)
  ) %>%
  count(member_gender, age_band) %>%
  group_by(member_gender) %>%
  mutate(
    total = sum(n),
    pct   = n / total * 100,
    pct   = if_else(member_gender == "Male", -pct, pct)
  )

p_i2 <- ggplot(pyramid_data, aes(x = age_band, y = pct, fill = member_gender)) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "%"),
    limits = c(-max(abs(pyramid_data$pct)) * 1.1, max(abs(pyramid_data$pct)) * 1.1)
  ) +
  scale_fill_manual(values = c("Male" = "#1F77B4", "Female" = "#E377C2"), name = NULL) +
  geom_hline(yintercept = 0, colour = "grey40") +
  labs(
    title    = "Age-Sex Population Pyramid",
    subtitle = "% of same-sex members (Male ← | → Female)",
    x = "Age Band", y = "% of Members"
  ) +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "I2_age_sex_pyramid.png"), p_i2, width = 8, height = 6, dpi = 150)
cat("  ✓ I2 saved\n")


sep("I3. DEPENDENCY RATIO vs VULNERABILITY SCORE")

p_i3 <- hh_scored %>%
  filter(!is.na(calc_dependency_ratio), calc_dependency_ratio <= 10) %>%
  ggplot(aes(x = calc_dependency_ratio, y = calc_total_vuln_assessment, colour = calc_full_classification)) +
  geom_jitter(alpha = 0.45, size = 1.6, width = 0.05, height = 0.5) +
  geom_smooth(method = "loess", se = TRUE, colour = "grey30", fill = "grey85", linewidth = 0.8) +
  geom_vline(xintercept = c(2, 3), linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  scale_colour_manual(
    values = c("Exclusion" = "#7F7F7F", "Extreme" = "#D62728",
               "High" = "#FF7F0E", "Vulnerable" = "#FFDD57", "Not Prioritised" = "#2CA02C"),
    name = NULL
  ) +
  labs(
    title    = "Dependency Ratio vs Total Vulnerability Score",
    subtitle = "Dashed lines at DR = 2 and 3 (scoring thresholds)",
    x = "Dependency Ratio (dependents / working-age adults)", y = "Total Vulnerability Score"
  ) +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "I3_dependency_vs_vuln_score.png"), p_i3, width = 9, height = 6, dpi = 150)
cat("  ✓ I3 saved\n")


sep("I4. VULNERABILITY DRIVERS — % of HHs with each risk factor")

driver_tbl <- hh_scored %>%
  summarise(
    `Female-headed HH`                = mean(calc_fem_hoh_yn     == "Yes", na.rm = TRUE),
    `Child-headed HH`                 = mean(calc_child_hoh_yn   == "Yes", na.rm = TRUE),
    `Elderly-headed HH`               = mean(calc_elderly_hoh_yn == "Yes", na.rm = TRUE),
    `HoH with disability`             = mean(calc_hoh_disable_yn == "Yes", na.rm = TRUE),
    `Member with disability`          = mean(calc_mem_disable_yn == "Yes", na.rm = TRUE),
    `HoH with chronic illness`        = mean(calc_hoh_illness_yn == "Yes", na.rm = TRUE),
    `Member with chronic illness`     = mean(calc_mem_illness_yn == "Yes", na.rm = TRUE),
    `PLW in household`                = mean(calc_hh_plw_count   >  0,    na.rm = TRUE),
    `Dependency ratio ≥ 2`            = mean(calc_dependency_ratio >= 2,  na.rm = TRUE),
    `No income in HH`                 = mean(hh_main_income == "No working member in the HH earning income", na.rm = TRUE),
    `Income shock (past 4 weeks)`     = mean(hh_income_shock_impact == "Consequent decrease of income in the past 4 weeks due to a shock", na.rm = TRUE),
    `No productive assets`            = mean(hh_productive_asset == "No assets owned", na.rm = TRUE),
    `Inadequate shelter`              = mean(hh_shelter_type %in% c("No shelter","Makeshift - Tent - Emergency shelter"), na.rm = TRUE),
    `Poor/very poor shelter condition`= mean(hh_shelter_cond %in% c("Poor","Very poor"), na.rm = TRUE),
    `Pays for water`                  = mean(hh_water_cost  == "Yes", na.rm = TRUE),
    `No soap access`                  = mean(hh_soap_access == "Yes", na.rm = TRUE),
    `FCS Poor`                        = mean(calc_fcs_category == "POOR",                    na.rm = TRUE),
    `FCS Borderline`                  = mean(calc_fcs_category == "BORDERLINE",              na.rm = TRUE),
    `rCSI Severe food insecurity`     = mean(calc_rcsi_category == "Severe food insecurity", na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Risk Factor", values_to = "pct") %>%
  arrange(desc(pct)) %>%
  mutate(
    pct_label = percent(pct, 0.1),
    domain = case_when(
      str_detect(`Risk Factor`, "HH|HoH|headed|PLW|Dependency") ~ "Demographic",
      str_detect(`Risk Factor`, "income|asset|Income")           ~ "Income/Asset",
      str_detect(`Risk Factor`, "shelter|water|soap|Shelter|Water|Soap") ~ "Shelter/WASH",
      str_detect(`Risk Factor`, "FCS|rCSI")                      ~ "Food Security",
      TRUE                                                       ~ "Other"
    )
  )

print(driver_tbl %>% select(`Risk Factor`, pct_label, domain), n = Inf)
write.csv(driver_tbl %>% select(-pct_label), file.path(OUTPUT_DIR, "I4_vulnerability_drivers.csv"), row.names = FALSE)

p_i4 <- ggplot(driver_tbl,
               aes(x = reorder(`Risk Factor`, pct), y = pct, fill = domain)) +
  geom_col() +
  geom_text(aes(label = percent(pct, 0.1)), hjust = -0.1, size = 2.8) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.18))) +
  scale_fill_brewer(palette = "Set2", name = "Domain") +
  labs(title = "Vulnerability Drivers — % of Households Affected",
       subtitle = "All scored households",
       x = NULL, y = "% of Households") +
  coord_flip() + theme_vaf + theme(legend.position = "right")
ggsave(file.path(OUTPUT_DIR, "I4_vulnerability_drivers.png"), p_i4, width = 11, height = 8, dpi = 150)
cat("  ✓ I4 saved\n")


sep("I5. VULNERABILITY PROFILING — Key flags by classification")

profile_tbl <- hh_scored %>%
  group_by(calc_full_classification) %>%
  summarise(
    n                     = n(),
    `Mean HH size`        = round(mean(hh_size,              na.rm = TRUE), 1),
    `% Female HoH`        = round(mean(calc_fem_hoh_yn    == "Yes", na.rm = TRUE) * 100, 1),
    `% Child HoH`         = round(mean(calc_child_hoh_yn  == "Yes", na.rm = TRUE) * 100, 1),
    `% Elderly HoH`       = round(mean(calc_elderly_hoh_yn == "Yes", na.rm = TRUE) * 100, 1),
    `% HoH disabled`      = round(mean(calc_hoh_disable_yn == "Yes", na.rm = TRUE) * 100, 1),
    `% Any disability`    = round(mean(calc_disable_total   > 0,    na.rm = TRUE) * 100, 1),
    `% PLW`               = round(mean(calc_hh_plw_count    > 0,    na.rm = TRUE) * 100, 1),
    `Mean dep. ratio`     = round(mean(calc_dependency_ratio, na.rm = TRUE), 2),
    `% FCS Poor`          = round(mean(calc_fcs_category   == "POOR",                  na.rm = TRUE) * 100, 1),
    `% rCSI Severe`       = round(mean(calc_rcsi_category  == "Severe food insecurity", na.rm = TRUE) * 100, 1),
    `% No income`         = round(mean(hh_main_income == "No working member in the HH earning income", na.rm = TRUE) * 100, 1),
    `% No assets`         = round(mean(hh_productive_asset == "No assets owned", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

cat("\n")
print(profile_tbl, n = Inf, width = Inf)
write.csv(profile_tbl, file.path(OUTPUT_DIR, "I5_vulnerability_profile_by_class.csv"), row.names = FALSE)


sep("I6. DEMOGRAPHIC COMPOSITION BY CLASSIFICATION — stacked %")

demo_long <- hh_scored %>%
  select(calc_full_classification,
         Boys       = n_boys,
         Girls      = n_girls,
         `Adult Men`   = n_adult_men,
         `Adult Women` = n_adult_women,
         `Elderly Men` = n_elderly_men,
         `Elderly Women` = n_elderly_women
  ) %>%
  pivot_longer(-calc_full_classification, names_to = "Group", values_to = "Count") %>%
  group_by(calc_full_classification, Group) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(calc_full_classification) %>%
  mutate(pct = if_else(sum(Count) > 0, Count / sum(Count), NA_real_)) %>%
  ungroup() %>%
  mutate(Group = factor(Group, levels = c("Boys","Girls","Adult Men","Adult Women","Elderly Men","Elderly Women")))

p_i6 <- ggplot(demo_long, aes(x = calc_full_classification, y = pct, fill = Group)) +
  geom_col(position = "stack") +
  geom_text(aes(label = ifelse(pct > 0.04, percent(pct, 1), "")),
            position = position_stack(vjust = 0.5), size = 2.6, colour = "white", fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "Boys"         = "#AEC7E8", "Girls"         = "#FFBB78",
      "Adult Men"    = "#1F77B4", "Adult Women"   = "#FF7F0E",
      "Elderly Men"  = "#17BECF", "Elderly Women" = "#BCBD22"
    ), name = NULL
  ) +
  labs(
    title    = "Household Demographic Composition by Classification",
    subtitle = "% of total member-count per classification group",
    x = NULL, y = "Share of Members"
  ) +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "I6_demographic_composition_by_class.png"), p_i6, width = 10, height = 6, dpi = 150)
cat("  ✓ I6 saved\n")


sep("I7. DISABILITY PREVALENCE BY CLASSIFICATION")

disability_df <- hh_scored %>%
  group_by(calc_full_classification) %>%
  summarise(
    n                   = n(),
    `HoH disabled`      = round(mean(calc_hoh_disable_yn == "Yes", na.rm = TRUE) * 100, 1),
    `Member disabled`   = round(mean(calc_mem_disable_yn == "Yes", na.rm = TRUE) * 100, 1),
    `Any HH disabled`   = round(mean(calc_disable_total   > 0,    na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  pivot_longer(-c(calc_full_classification, n), names_to = "Type", values_to = "pct")

p_i7 <- ggplot(disability_df, aes(x = calc_full_classification, y = pct, fill = Type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.9), vjust = -0.3, size = 2.8) +
  scale_fill_brewer(palette = "Paired", name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Disability Prevalence by Classification",
       x = NULL, y = "% of Households") +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "I7_disability_by_classification.png"), p_i7, width = 9, height = 5, dpi = 150)
cat("  ✓ I7 saved\n")


sep("I8. VULNERABILITY SCORE BOXPLOT BY STATE")

p_i8 <- hh_scored %>%
  filter(!is.na(area_state)) %>%
  ggplot(aes(x = reorder(area_state, calc_total_vuln_assessment, FUN = median),
             y = calc_total_vuln_assessment, fill = area_state)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.4, show.legend = FALSE) +
  geom_hline(yintercept = c(25, 50, 73), linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  annotate("text", x = 0.7, y = c(26, 51, 74), label = c("Vulnerable", "High", "Extreme"),
           size = 2.8, colour = "grey40", hjust = 0) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "Vulnerability Score Distribution by State",
    subtitle = "Ordered by median score; dashed lines = classification thresholds",
    x = NULL, y = "Total Vulnerability Score"
  ) +
  coord_flip() + theme_vaf
ggsave(file.path(OUTPUT_DIR, "I8_vuln_score_boxplot_by_state.png"), p_i8, width = 9, height = 6, dpi = 150)
cat("  ✓ I8 saved\n")


sep("I9. PLW PREVALENCE BY CLASSIFICATION")

plw_df <- hh_scored %>%
  group_by(calc_full_classification) %>%
  summarise(
    n            = n(),
    pct_hoh_plw  = round(mean(calc_hoh_plw_yn == "Yes", na.rm = TRUE) * 100, 1),
    pct_mem_plw  = round(mean(calc_mem_plw_yn == "Yes", na.rm = TRUE) * 100, 1),
    pct_any_plw  = round(mean(calc_hh_plw_count > 0,    na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

p_i9 <- plw_df %>%
  pivot_longer(c(pct_hoh_plw, pct_mem_plw, pct_any_plw),
               names_to = "Type", values_to = "pct") %>%
  mutate(Type = recode(Type,
                       pct_hoh_plw = "HoH is PLW",
                       pct_mem_plw = "Member is PLW",
                       pct_any_plw = "Any PLW in HH"
  )) %>%
  ggplot(aes(x = calc_full_classification, y = pct, fill = Type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.9), vjust = -0.3, size = 2.8) +
  scale_fill_brewer(palette = "Set1", name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "PLW (Pregnant/Lactating Women) Prevalence by Classification",
       x = NULL, y = "% of Households") +
  theme_vaf
ggsave(file.path(OUTPUT_DIR, "I9_plw_by_classification.png"), p_i9, width = 9, height = 5, dpi = 150)
cat("  ✓ I9 saved\n")


# =============================================================================
# DONE
# =============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat(" Add-on complete.\n")
cat(sprintf(" Plots + tables saved to: %s/\n", OUTPUT_DIR))
cat(strrep("=", 60), "\n\n")
cat("Files generated:\n")
cat("  Extended plots : H1–H9\n")
cat("  Demographic    : I1–I9\n\n")