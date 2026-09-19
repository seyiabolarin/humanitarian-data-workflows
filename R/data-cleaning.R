# Public offline adaptation. Uses local CSV inputs; no service access or remote writes.
source("R/local_io.R")
# -------------------------
# FILTER SETTINGS
# -------------------------

START_DATE <- NULL
N_DAYS <- NULL
PARTNER_FILTER <- NULL
STATE_FILTER <- NULL
LGA_FILTER <- NULL
INTERVENTION_FILTER <- NULL

 # Loading the ActivityInfo
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# =============================================================================
# cleaning.R
# DRC Nigeria — Data Cleaning Script
#
# Call run_cleaning(collected_hhs) to execute.
#
# collected_hhs must be a data frame with at minimum:
#   record_id        — ActivityInfo ._id of the HH record
#   hh_id_assessment — the human-readable HH ID
#
# Returns:
#   $flagged_hh_record_ids — character vector of HH record IDs that got flags
#   $n_flags               — total number of flags generated
#   $report                — full cleaning_report data frame
#
# Checks covered:
#   HH level : C01–C09, C20, C22, C23, C24
#   Member   : C10–C19, C21
# =============================================================================

MIN_DURATION_MINS <- 3
MAX_DURATION_MINS <- 60

WG_PROBLEM_VALUES <- c("A lot of difficulty", "Cannot do at all")

# =============================================================================

# MAIN FUNCTION
# =============================================================================
run_cleaning <- function(collected_hhs = NULL) {
  
  n_hhs <- if (is.null(collected_hhs)) {
    "all"
  } else {
    nrow(collected_hhs)
  }
  
  message(sprintf("Running cleaning checks on %s HH(s)...", n_hhs)) 
  # ---------------------------------------------------------------------------
  # STEP 1 — PULL HH RECORDS  (full field set, filtered to collected batch)
  # ---------------------------------------------------------------------------
  
  raw_hh <- read_input("data-cleaning-households.csv") 
  
  raw_hh <- raw_hh %>%
    mutate(
      interview_dt = parse_date_time(
        interview_start,
        orders = c("ymd HMS z", "ymd HMS")
      )
    )
  
  # -------------------------
  # EXISTING FILTER
  # -------------------------
  
  if (!is.null(collected_hhs)) {
    raw_hh <- raw_hh %>%
      filter(record_id %in% collected_hhs$record_id)
  }
  
  # -------------------------
  # DATE FILTER (DYNAMIC RANGE)
  # -------------------------
  
  if (!is.null(START_DATE) && !is.null(N_DAYS)) {
    
    start_dt <- ymd(START_DATE)
    end_dt   <- start_dt + days(N_DAYS) - seconds(1)
    
    message(paste("Filtering from", start_dt, "to", end_dt))
    
    raw_hh <- raw_hh %>%
      filter(
        !is.na(interview_dt) &
          interview_dt >= start_dt &
          interview_dt <= end_dt
      )
  }
  
  # -------------------------
  # PARTNER FILTER
  # -------------------------
  
  if (!is.null(PARTNER_FILTER) && length(PARTNER_FILTER) > 0 && any(nzchar(PARTNER_FILTER))) {
    
    pattern <- paste(PARTNER_FILTER[nzchar(PARTNER_FILTER)], collapse = "|")
    
    raw_hh <- raw_hh %>%
      filter(
        !is.na(partner_name) &
          str_detect(tolower(partner_name), tolower(pattern))
      )
  }
  
  # -------------------------
  # STATE FILTER
  # -------------------------
  
  if (!is.null(STATE_FILTER) && length(STATE_FILTER) > 0 && any(nzchar(STATE_FILTER))) {
    
    pattern <- paste(STATE_FILTER[nzchar(STATE_FILTER)], collapse = "|")
    
    raw_hh <- raw_hh %>%
      filter(
        !is.na(state) &
          str_detect(tolower(state), tolower(pattern))
      )
  } 
  
  # -------------------------
  # LGA FILTER
  # -------------------------
  
  if (!is.null(LGA_FILTER) && length(LGA_FILTER) > 0 && any(nzchar(LGA_FILTER))) {
    
    pattern <- paste(LGA_FILTER[nzchar(LGA_FILTER)], collapse = "|")
    
    raw_hh <- raw_hh %>%
      filter(
        !is.na(lga_filter_field) &
          str_detect(tolower(lga_filter_field), tolower(pattern))
      )
  }
  
  
  # -------------------------
  # INTERVENTION FILTER
  # -------------------------
  
  if (!is.null(INTERVENTION_FILTER) && length(INTERVENTION_FILTER) > 0 && any(nzchar(INTERVENTION_FILTER))) {
    
    pattern <- paste(INTERVENTION_FILTER[nzchar(INTERVENTION_FILTER)], collapse = "|")
    
    raw_hh <- raw_hh %>%
      filter(
        !is.na(intervention_type) &
          str_detect(tolower(intervention_type), tolower(pattern))
      )
  }
  
  message(sprintf("  Pulled %d HH record(s).", nrow(raw_hh)))
  
  # Check for empty input
  if (nrow(raw_hh) == 0) {
    message("  No HH records found after filtering — stopping.")
    return(list(
      flagged_hh_record_ids = character(0),
      n_flags = 0,
      report = data.frame()
    ))
  }
  
  # ---------------------------------------------------------------------------
  # STEP 2 — PULL MEMBER RECORDS  (scoped to the same batch)
  # ---------------------------------------------------------------------------
  
  raw_members <- read_input("data-cleaning-members.csv") %>%
    filter(hh_id_assessment %in% raw_hh$hh_id_assessment)
  
  message(sprintf("  Pulled %d member record(s).", nrow(raw_members)))
  
  # ---------------------------------------------------------------------------
  # STEP 3 — PREPARE
  # ---------------------------------------------------------------------------
  
  parse_duration_mins <- function(start_col, end_col) {
    as.numeric(difftime(ymd_hms(end_col, quiet = TRUE),
                        ymd_hms(start_col, quiet = TRUE), units = "mins"))
  }
  
  hh <- raw_hh %>%
    mutate(
      date_assessment       = as.Date(date_assessment),
      hh_idp_when_displaced = as.Date(hh_idp_when_displaced),
      hh_member_count       = as.integer(hh_member_count),
      across(fcs1:fcs10, as.numeric),
      across(rcsi_1:rcsi_5, as.numeric),
      fcs_all_zero  = rowSums(!is.na(across(fcs1:fcs10))) == 10 & rowSums(across(fcs1:fcs10), na.rm = TRUE) == 0,
      fcs_max       = pmax(fcs1,fcs2,fcs3,fcs4,fcs5,fcs6,fcs7,fcs8,fcs9,fcs10, na.rm = TRUE),
      rcsi_max      = pmax(rcsi_1,rcsi_2,rcsi_3,rcsi_4,rcsi_5, na.rm = TRUE),
      hh_working_count = as.integer(hh_working_count),
      duration_mins = parse_duration_mins(interview_start, interview_end)
    )
  
  members <- raw_members %>%
    mutate(
      #  GUARANTEE record_id is usable
      record_id = as.character(record_id),
      
      dob              = as.Date(dob),
      member_age       = as.numeric(member_age),
      child_age_months = as.integer(child_age_months),
      
      #  GUARANTEE full_name is never empty garbage
      full_name = ifelse(
        is.na(first_name) & is.na(last_name),
        NA,
        str_trim(paste(first_name, last_name))
      ),
      
      duration_mins = parse_duration_mins(interview_start, interview_end)
    ) %>%
    
    left_join(
      hh %>% select(hh_id_assessment, hh_record_id = record_id),
      by = "hh_id_assessment")
  
  # ── NEW: build per-HH location lookup from members and join onto hh ───────
  # Takes the first member row per household — all members share the same
  # calculated location fields pulled from the parent HH form.
  hh_location <- raw_members %>%
    group_by(hh_id_assessment) %>%
    slice(1) %>%
    ungroup() %>%
    select(hh_id_assessment, lga, ward, comm_vill)
  
  hh <- hh %>%
    left_join(hh_location, by = "hh_id_assessment")
  
  hoh_counts <- members %>%
    group_by(hh_id_assessment) %>%
    summarise(hoh_count = sum(member_hoh == "Yes", na.rm = TRUE), .groups = "drop")
  
  hh <- hh %>% left_join(hoh_counts, by = "hh_id_assessment")
  
  # ---------------------------------------------------------------------------
  # STEP 4 — FLAG HELPERS
  # ---------------------------------------------------------------------------
  
  clean_flags  <- list()
  flag_counter <- 0L
  
  add_flag <- function(check_id, check_name, level, record_id, hh_id,
                       hh_record_id = NA, member_record_id = NA,
                       name = NA, staff = NA, partner = NA,
                       lga = NA, ward = NA, comm_vill = NA,   # ── NEW
                       issue, action) {
    flag_counter <<- flag_counter + 1L
    clean_flags[[flag_counter]] <<- data.frame(
      flag_id = paste0(
        "CLEAN-",
        format(Sys.time(), "%Y%m%d%H%M%S"),
        "-",
        flag_counter
      ),
      check_id         = check_id,
      check_name       = check_name,
      level            = level,
      record_id        = as.character(record_id),
      hh_id            = as.character(hh_id),
      hh_record_id     = as.character(ifelse(is.null(hh_record_id)    || length(hh_record_id)    == 0, NA, hh_record_id)),
      member_record_id = if (!is.null(member_record_id) && length(member_record_id) > 0 && !is.na(member_record_id)) {
        as.character(member_record_id)
      } else {
        NA_character_
      },
      
      name = if (!is.null(name) && length(name) > 0 && !is.na(name)) {
        as.character(name)
      } else {
        NA_character_
      },
      
      staff = if (!is.null(staff) && length(staff) > 0 && !is.na(staff)) {
        as.character(staff)
      } else {
        NA_character_
      },
      
      partner = if (!is.null(partner) && length(partner) > 0 && !is.na(partner)) {
        as.character(partner)
      } else {
        NA_character_
      },
      lga              = as.character(ifelse(is.null(lga)       || length(lga)       == 0, NA, lga)),       # ── NEW
      ward             = as.character(ifelse(is.null(ward)      || length(ward)      == 0, NA, ward)),      # ── NEW
      comm_vill        = as.character(ifelse(is.null(comm_vill) || length(comm_vill) == 0, NA, comm_vill)), # ── NEW
      issue            = as.character(issue),
      action           = as.character(action),
      flag_status      = "Pending Review",
      reviewed_by      = NA_character_,
      review_date      = NA_character_,
      notes            = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  
  run_check <- function(df, check_id, check_name, level, issue_fn, action_fn,
                        has_name = FALSE) {
    if (nrow(df) == 0) return(invisible(NULL))
    is_member <- (level == "Member")
    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      add_flag(
        check_id,
        check_name,
        level,
        row$record_id,
        row$hh_id_assessment,
        
        hh_record_id     = if (is_member) row$hh_record_id else row$record_id,
        member_record_id = if (is_member) row$record_id    else NA,
        name             = if (has_name) row$full_name else NA,
        staff            = row$staff_select,
        partner          = if ("partner" %in% names(row)) row$partner else row$partner_name,
        lga              = if ("lga" %in% names(row)) row$lga else NA,
        ward             = if ("ward" %in% names(row)) row$ward else NA,
        comm_vill        = if ("comm_vill" %in% names(row)) row$comm_vill else NA,
        issue            = issue_fn(row),
        action           = action_fn(row)
      )
      
    }
  }
  
  # ---------------------------------------------------------------------------
  # STEP 5 — HH LEVEL CHECKS
  # ---------------------------------------------------------------------------
  
  run_check(
    hh %>% filter(consent_yn != "Yes" | is.na(consent_yn)),
    "C01", "Consent not given", "Household",
    issue_fn  = function(r) sprintf("HH %s has no consent recorded.", r$hh_id_assessment),
    action_fn = function(r) "Verify with enumerator. If consent was not obtained, record must be removed."
  )
  
  run_check(
    hh %>% filter(is.na(gps_coord) | gps_coord == ""),
    "C02", "GPS coordinates missing", "Household",
    issue_fn  = function(r) sprintf("HH %s has no GPS coordinates recorded.", r$hh_id_assessment),
    action_fn = function(r) "Enumerator to re-visit and record coordinates, or confirm location manually."
  )
  
  run_check(
    hh %>% filter(!is.na(date_assessment) & date_assessment > Sys.Date()),
    "C03", "Date of assessment in the future", "Household",
    issue_fn  = function(r) sprintf("HH %s has assessment date %s which is in the future.", r$hh_id_assessment, r$date_assessment),
    action_fn = function(r) "Correct the date of assessment in the form."
  )
  
  run_check(
    hh %>% filter(!is.na(hh_member_count) & (hh_member_count == 0 | hh_member_count > 15)),
    "C04", "HH size implausible", "Household",
    issue_fn  = function(r) sprintf("HH %s has %d members recorded. Expected range is 1–15.", r$hh_id_assessment, r$hh_member_count),
    action_fn = function(r) "Verify actual household size with enumerator and correct member roster."
  )
  
  run_check(
    hh %>% filter(hh_status == "IDP" & is.na(hh_idp_when_displaced)),
    "C05", "IDP status but no displacement date", "Household",
    issue_fn  = function(r) sprintf("HH %s is recorded as IDP but no displacement date was entered.", r$hh_id_assessment),
    action_fn = function(r) "Enumerator to follow up and record the displacement date."
  )
  
  run_check(
    hh %>% filter(!is.na(fcs_max) & fcs_max > 7),
    "C06", "FCS value out of range", "Household",
    issue_fn  = function(r) sprintf("HH %s has at least one FCS value above 7 (max: %s).", r$hh_id_assessment, r$fcs_max),
    action_fn = function(r) "Review all FCS entries and correct any values above 7."
  )
  
  run_check(
    hh %>% filter(fcs_all_zero),
    "C07", "All FCS values are zero", "Household",
    issue_fn  = function(r) sprintf("HH %s has all FCS food group values recorded as 0.", r$hh_id_assessment),
    action_fn = function(r) "Enumerator to confirm responses with household."
  )
  
  run_check(
    hh %>% filter(!is.na(rcsi_max) & rcsi_max > 7),
    "C08", "rCSI value out of range", "Household",
    issue_fn  = function(r) sprintf("HH %s has at least one rCSI value above 7 (max: %s).", r$hh_id_assessment, r$rcsi_max),
    action_fn = function(r) "Review all rCSI entries and correct any values above 7."
  )
  
  run_check(
    hh %>% filter(is.na(hoh_count) | hoh_count == 0),
    "C09", "No Head of Household recorded", "Household",
    issue_fn  = function(r) sprintf("HH %s has no member recorded as Head of Household.", r$hh_id_assessment),
    action_fn = function(r) "Review member roster and mark the correct member as HoH."
  )
  
  # run_check(
  #   hh %>% filter(!is.na(duration_mins) &
  #                   (duration_mins < MIN_DURATION_MINS | duration_mins > MAX_DURATION_MINS)),
  #   "C20", "HH interview duration outside expected range", "Household",
  #   issue_fn  = function(r) sprintf("HH %s interview was %.1f minutes. Expected %d-%d minutes.",
  #                                   r$hh_id_assessment, r$duration_mins,
  #                                   MIN_DURATION_MINS, MAX_DURATION_MINS),
  #   action_fn = function(r) if (r$duration_mins < MIN_DURATION_MINS)
  #     "Interview was unusually short. Verify data quality with enumerator."
  #   else
  #     "Interview was unusually long. Check for data entry delays or form left open."
  # )
  
  members_working_counts <- members %>%
    group_by(hh_id_assessment) %>%
    summarise(members_working = sum(member_working == "Yes", na.rm = TRUE), .groups = "drop")
  
  run_check(
    hh %>%
      left_join(members_working_counts, by = "hh_id_assessment") %>%
      mutate(members_working = replace_na(members_working, 0)) %>%
      filter((hh_working_count == 0 & members_working > 0) |
               (hh_working_count > 0  & members_working == 0)),
    "C22", "HH working count vs member working status mismatch", "Household",
    issue_fn  = function(r) sprintf(
      "HH %s has %d working members at HH level but %d members marked working in roster.",
      r$hh_id_assessment, r$hh_working_count, r$members_working),
    action_fn = function(r) "Review member working status entries and reconcile with the HH-level count."
  )
  
  run_check(
    hh %>% filter(!is.na(rcsi_1) & !is.na(rcsi_2) & !is.na(rcsi_3) &
                    !is.na(rcsi_4) & !is.na(rcsi_5) &
                    rcsi_1 == 7 & rcsi_2 == 7 & rcsi_3 == 7 & rcsi_4 == 7 & rcsi_5 == 7),
    "C23", "All rCSI values are 7 (possible straight-lining)", "Household",
    issue_fn  = function(r) sprintf(
      "HH %s has all five rCSI values recorded as 7. This is extremely unlikely.",
      r$hh_id_assessment),
    action_fn = function(r) "Enumerator to re-verify rCSI responses with the household."
  )
  
  member_roster_counts <- members %>%
    group_by(hh_id_assessment) %>%
    summarise(roster_count = n(), .groups = "drop")
  
  run_check(
    hh %>%
      left_join(member_roster_counts, by = "hh_id_assessment") %>%
      mutate(roster_count = replace_na(roster_count, 0L)) %>%
      filter(!is.na(hh_member_count) & hh_member_count != roster_count),
    "C24", "Roster count does not match reported HH size", "Household",
    issue_fn  = function(r) sprintf(
      "HH %s reported %d members but the roster contains %d records.",
      r$hh_id_assessment, r$hh_member_count, r$roster_count),
    action_fn = function(r) sprintf(
      "%s. Review the member roster and reconcile with the HH-level count.",
      if (r$roster_count < r$hh_member_count) "Roster is missing members"
      else "Roster has more entries than reported")
  )
  
  # ---------------------------------------------------------------------------
  # STEP 6 — MEMBER LEVEL CHECKS
  # ---------------------------------------------------------------------------
  
  run_check(
    members %>% filter(!is.na(dob) & dob > Sys.Date()),
    "C10", "Date of birth in the future", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) has a date of birth of %s which is in the future.", r$full_name, r$hh_id_assessment, r$dob),
    action_fn = function(r) "Correct the date of birth for this member.",
    has_name  = TRUE
  )
  
  run_check(
    members %>% filter(member_hoh == "Yes" & !is.na(member_age) & member_age < 18),
    "C11", "Head of Household is a child", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) is recorded as HoH but is aged %s, under 18.", r$full_name, r$hh_id_assessment, r$member_age),
    action_fn = function(r) "Verify whether this is a genuine child-headed household or a data entry error.",
    has_name  = TRUE
  )
  
  run_check(
    members %>% filter(member_plw == "Yes" & gender == "Male"),
    "C12", "PLW flagged for male member", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) is recorded as male but flagged as pregnant or lactating.", r$full_name, r$hh_id_assessment),
    action_fn = function(r) "Correct either the gender or the PLW status for this member.",
    has_name  = TRUE
  )
  
  run_check(
    members %>% filter(member_plw == "Yes" & !is.na(member_age) & (member_age < 15 | member_age > 49)),
    "C13", "PLW flagged outside reproductive age", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) is aged %s but flagged as pregnant or lactating. Expected age 15-49.", r$full_name, r$hh_id_assessment, r$member_age),
    action_fn = function(r) "Verify age and PLW status with enumerator.",
    has_name  = TRUE
  )
  
  run_check(
    members %>% filter(member_working == "Yes" & !is.na(member_age) & member_age < 15),
    "C14", "Possible child labour", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) is aged %s and recorded as engaged in work.", r$full_name, r$hh_id_assessment, r$member_age),
    action_fn = function(r) "Verify with enumerator. Flag for protection follow-up if confirmed.",
    has_name  = TRUE
  )
  
  run_check(
    members %>%
      filter(member_disability_yn == "Yes") %>%
      filter(is.na(wg_seeing) & is.na(wg_hearing) & is.na(wg_walking) &
               is.na(wg_remembering) & is.na(wg_selfcare) & is.na(wg_communicating)),
    "C15", "Disability flagged but no WG questions answered", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) is recorded as having a disability but no Washington Group questions were answered.", r$full_name, r$hh_id_assessment),
    action_fn = function(r) "Complete the Washington Group questions for this member.",
    has_name  = TRUE
  )
  
  run_check(
    members %>%
      filter(member_disability_yn == "No") %>%
      filter(wg_seeing %in% WG_PROBLEM_VALUES | wg_hearing %in% WG_PROBLEM_VALUES |
               wg_walking %in% WG_PROBLEM_VALUES | wg_remembering %in% WG_PROBLEM_VALUES |
               wg_selfcare %in% WG_PROBLEM_VALUES | wg_communicating %in% WG_PROBLEM_VALUES),
    "C16", "WG scores indicate difficulty but disability marked No", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) has WG scores indicating significant difficulty but disability is recorded as No.", r$full_name, r$hh_id_assessment),
    action_fn = function(r) "Review disability status and WG responses for this member.",
    has_name  = TRUE
  )
  
  run_check(
    members %>% filter(member_nin_yn == "Yes" & (is.na(member_nin) | str_trim(member_nin) == "")),
    "C17", "NIN marked available but not entered", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) has NIN marked as available but no NIN number was entered.", r$full_name, r$hh_id_assessment),
    action_fn = function(r) "Enter the NIN number or correct the NIN availability response.",
    has_name  = TRUE
  )
  
  run_check(
    members %>% filter(!is.na(child_age_months) &
                         child_age_months >= 6 & child_age_months <= 23 &
                         is.na(mad_breastfed)),
    "C18", "MAD not completed for eligible child", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) is aged %d months and eligible for MAD but the MAD questions were not completed.", r$full_name, r$hh_id_assessment, r$child_age_months),
    action_fn = function(r) "Complete the MAD section for this child.",
    has_name  = TRUE
  )
  
  run_check(
    members %>% filter((is.na(child_age_months) | child_age_months < 6 | child_age_months > 23) &
                         !is.na(mad_breastfed)),
    "C19", "MAD completed for ineligible child", "Member",
    issue_fn  = function(r) sprintf("%s (HH %s) is aged %s months but the MAD section was completed.",
                                    r$full_name, r$hh_id_assessment,
                                    ifelse(is.na(r$child_age_months), "unknown", as.character(r$child_age_months))),
    action_fn = function(r) "Review whether MAD was answered for the correct child.",
    has_name  = TRUE
  )
  
  # run_check(
  #   members %>% filter(!is.na(duration_mins) &
  #                        (duration_mins < MIN_DURATION_MINS | duration_mins > MAX_DURATION_MINS)),
  #   "C21", "Member interview duration outside expected range", "Member",
  #   issue_fn  = function(r) sprintf("%s (HH %s) member interview was %.1f minutes. Expected %d-%d minutes.",
  #                                   r$full_name, r$hh_id_assessment, r$duration_mins,
  #                                   MIN_DURATION_MINS, MAX_DURATION_MINS),
  #   action_fn = function(r) if (r$duration_mins < MIN_DURATION_MINS)
  #     "Interview was unusually short. Verify data quality with enumerator."
  #   else
  #     "Interview was unusually long. Check for data entry delays or form left open.",
  #   has_name  = TRUE
  # )
  
  
  # ---------------------------------------------------------------------------
  # ---------------------------------------------------------------------------
  # STEP 7 — BUILD CLEANING REPORT
  # ---------------------------------------------------------------------------
  
  cleaning_report <- data.frame(stringsAsFactors = FALSE)
  
  if (length(clean_flags) == 0) {
    message("  No cleaning issues found.")
  } else {
    cleaning_report <- bind_rows(clean_flags) %>%
      arrange(check_id, hh_id)
    
    message(sprintf("  Cleaning report: %d flag(s) across %d check type(s).",
                    nrow(cleaning_report), n_distinct(cleaning_report$check_id)))
  }
  
  
  # ---------------------------------------------------------------------------
  # STEP 8 — SAVE LOCAL REVIEW REPORT
  # ---------------------------------------------------------------------------
  
  write_report(cleaning_report, "cleaning-flags.csv")
  return(list(
    flagged_hh_record_ids = unique(na.omit(cleaning_report$hh_record_id)),
    n_flags               = nrow(cleaning_report),
    report                = cleaning_report
  ))
  
}

result <- run_cleaning()
result$n_flags
invisible(result$report)