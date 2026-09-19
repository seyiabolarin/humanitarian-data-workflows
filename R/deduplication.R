# Public offline adaptation. Candidate flags require human review; no automatic exclusions.
source("R/local_io.R")
START_DATE <- NULL
N_DAYS <- NULL
PARTNER_FILTER <- NULL
LGA_FILTER <- NULL


library(dplyr)
library(stringr)
library(stringdist)
library(tidyr)
library(lubridate)

# =============================================================================
# deduplication.R
# DRC Nigeria — Deduplication Script
#
# Call run_dedup(collected_hhs) to execute.
#
# collected_hhs must be a data frame with at minimum:
#   record_id        — ActivityInfo ._id of the HH record
#   hh_id_assessment — the human-readable HH ID
#
# Returns:
#   $flagged_hh_record_ids — HH ._id values appearing in any flag (both sides)
#                            scoped to the collected_hhs batch
#   $n_flags               — total number of local candidate flags
#   $report                — full flag_report data frame
#
# Match types:
#   Exact Match  — identical dedup_key (name + gender + dob composite)
#   NIN Match    — shared National Identification Number
#   Fuzzy Match  — near-identical name with matching gender/age/LGA/HH size
#
# Project context:
#   All flags include project_name for both members and a same_project flag.
#   Cross-project candidate matches are flagged
#   but labelled "Cross-Project" so reviewers can triage appropriately.
#
# HoH Sub-ID:
#   Each member now carries a hoh_sub_id — a concat of the Head of Household's
#   FN3 + LN3 + age + gender + marital status. This anchors every member to a
#   household identity so verifiers can quickly see if the same child appears
#   under different HoHs, or if all children correctly share one household.
#
# Adding a new data source:
#   Configure each local input pair in DEDUP_SOURCES and validate its column aliases.
# =============================================================================


# =============================================================================
# FORM IDs & FIELD CONSTANTS
# =============================================================================

DEDUP_SOURCES <- list(list(label = "Demo", households_csv = "dedup-households.csv", members_csv = "dedup-members.csv"))

pull_source <- function(src) {
  
  # ── Pull HH lookup (record_id + hh_id_assessment + project_name) ─────────
  hh_raw <- read_input(src$households_csv)

  hh_lookup <- hh_raw %>%
    rename(
      hh_record_id     = record_id,
      hh_id_assessment = hh_id_assessment,
      project_name     = project_name
    )
  
  # ── Pull member records (now includes member_hoh + hoh_marital_status) ───
  mem_raw <- read_input(src$members_csv)

  # ── Join project name from HH lookup ─────────────────────────────────────
  members <- mem_raw %>%
    left_join(hh_lookup, by = "hh_id_assessment") %>%
    mutate(source_label = src$label)
  
  
  #  KEEP ONLY HEAD OF HOUSEHOLD (no age filter)
  members <- members %>%
    filter(
      toupper(str_trim(as.character(member_hoh))) == "YES")
  
  
  # ── Build hoh_sub_id from the HoH member row and broadcast to the household
  # The HoH row is the member where member_hoh == "Yes" (or equivalent choice).
  # hoh_marital_status is only filled on that row.
  # We derive one hoh_sub_id per household and join it to every member.
  hoh_lookup <- members %>%
    filter(toupper(str_trim(as.character(member_hoh))) == "YES") %>%
    mutate(
      hoh_sub_id = paste(
        toupper(str_trim(first_name)),
        toupper(str_trim(last_name)),
        as.character(member_age),
        toupper(str_trim(gender)),
        toupper(str_trim(hoh_marital_status)),
        sep = "_"
      )
    ) %>%
    select(hh_id_assessment, hoh_sub_id) %>%
    # Guard against households with multiple members incorrectly flagged as HoH
    # (data entry error). Keep the first occurrence and log a warning.
    group_by(hh_id_assessment) %>%
    mutate(hoh_count = n()) %>%
    ungroup() %>%
    { 
      multi <- filter(., hoh_count > 1) %>% pull(hh_id_assessment) %>% unique()
      #  STORE MULTI-HOH HOUSEHOLDS
      invisible(multi)
      if (length(multi) > 0)
        warning(sprintf(
          "  %d household(s) have multiple HoH members — keeping first: %s",
          length(multi), paste(multi, collapse = ", ")
        ))
      .
    } %>%
    group_by(hh_id_assessment) %>%
    slice(1) %>%
    ungroup() %>%
    select(hh_id_assessment, hoh_sub_id)
  
  members %>%
    left_join(hoh_lookup, by = "hh_id_assessment")
}


# =============================================================================
# =============================================================================

run_dedup <- function(collected_hhs = NULL) {
  
  message(sprintf("  Running deduplication across %d source(s)...", length(DEDUP_SOURCES)))
  
  # ---------------------------------------------------------------------------
  # STEP 1 — PULL AND COMBINE ALL SOURCES
  # Dedup always runs across ALL records (not just the current batch) so that
  # cross-batch duplicates are caught. collected_hhs is used later to scope
  # which HH record IDs are returned as flagged for this batch.
  # ---------------------------------------------------------------------------
  
  all_members <- bind_rows(lapply(DEDUP_SOURCES, pull_source))
  multi_hoh_hhs <- all_members %>% count(hh_id_assessment) %>% filter(n > 1) %>% pull(hh_id_assessment)
  
  # -------------------------
  # DATE FILTER
  # -------------------------
  if (!is.null(START_DATE) && !is.null(N_DAYS)) {
    start_dt <- ymd(START_DATE)
    end_dt   <- start_dt + days(N_DAYS) - seconds(1)
    
    all_members <- all_members %>%
      filter(
        !is.na(date_assessment),
        ymd(date_assessment) >= start_dt,
        ymd(date_assessment) <= end_dt
      )
  }
  
  # -------------------------
  # PARTNER FILTER
  # -------------------------
  if (!is.null(PARTNER_FILTER) && any(nzchar(PARTNER_FILTER))) {
    pattern <- paste(PARTNER_FILTER, collapse = "|")
    
    all_members <- all_members %>%
      filter(
        !is.na(partner),
        str_detect(tolower(partner), tolower(pattern))
      )
  }
  
  # -------------------------
  # LGA FILTER
  # -------------------------
  if (!is.null(LGA_FILTER) && any(nzchar(LGA_FILTER))) {
    pattern <- paste(LGA_FILTER, collapse = "|")
    
    all_members <- all_members %>%
      filter(
        !is.na(lga),
        str_detect(tolower(lga), tolower(pattern))
      )
  }
  
  message(sprintf("  Pulled %d member record(s) from %d source(s).",
                  nrow(all_members),
                  length(DEDUP_SOURCES)))
  
  # ---------------------------------------------------------------------------
  # STEP 2 — NORMALISE
  # ---------------------------------------------------------------------------
  
  if (anyNA(all_members$record_id) || anyDuplicated(all_members$record_id))
    stop("Member record IDs must be unique and nonmissing across the selected sources.")
  members <- all_members %>%
    mutate(
      age_num        = as.integer(member_age),
      size_num       = as.integer(hh_size),
      lga_norm       = toupper(str_trim(lga)),
      ward_norm      = toupper(str_trim(ward)),
      comm_vill_norm = toupper(str_trim(comm_vill)),
      fn3            = str_sub(str_pad(toupper(str_trim(first_name)), 3, "right", "_"), 1, 3),
      ln3            = str_sub(str_pad(toupper(str_trim(last_name)),  3, "right", "_"), 1, 3),
      fn5            = str_sub(str_pad(toupper(str_trim(first_name)), 5, "right", "_"), 1, 5),
      ln5            = str_sub(str_pad(toupper(str_trim(last_name)),  5, "right", "_"), 1, 5),
      project_norm   = toupper(str_trim(as.character(project_name))),
      hoh_sub_id     = toupper(str_trim(as.character(hoh_sub_id))),
      # Build key in R from raw fields — 3 letters keeps the exact match pool broad
      # so fewer records fall through to fuzzy matching
      # Format: FN3 + LN3 + GENDER + AGE + HH_STATUS + HH_SIZE + LGA
      key_norm = paste0(
        fn3,
        ln3,
        toupper(str_trim(gender)),
        as.character(age_num),
        toupper(str_trim(hh_status)),
        as.character(size_num),
        lga_norm
      ),
      # Augmented key: member key + ward + community/village for tighter exact matching
      key_norm_full  = paste0(key_norm, "|", ward_norm, "|", comm_vill_norm)
    )
  
  # ---------------------------------------------------------------------------
  # STEP 3 — EXACT MATCH DETECTION
  # ---------------------------------------------------------------------------
  
  key_counts <- members %>%
    filter(!is.na(key_norm_full) & key_norm_full != "||") %>%
    group_by(key_norm_full) %>%
    summarise(exact_count = n(), .groups = "drop") %>%
    filter(exact_count > 1)
  
  members <- members %>%
    left_join(key_counts, by = "key_norm_full") %>%
    mutate(exact_flag = coalesce(exact_count > 1, FALSE))
  
  message(sprintf("  Exact matches: %d member(s) flagged across %d key(s).",
                  sum(members$exact_flag),
                  n_distinct(members$key_norm[members$exact_flag])))
  
  # ---------------------------------------------------------------------------
  # STEP 4 — NIN MATCH DETECTION
  # ---------------------------------------------------------------------------
  
  members <- members %>% mutate(member_nin = toupper(gsub("[^A-Za-z0-9]", "", member_nin)))
  members$member_nin[members$member_nin %in% c("", "NA", "NONE", "UNKNOWN", "NOTAVAILABLE", "00000000000")] <- NA_character_
  nin_counts <- members %>%
    filter(!is.na(member_nin) & str_trim(member_nin) != "") %>%
    group_by(member_nin) %>%
    summarise(nin_count = n(), .groups = "drop") %>%
    filter(nin_count > 1)
  
  members <- members %>%
    mutate(nin_flag = !is.na(member_nin) &
             str_trim(member_nin) != "" &
             member_nin %in% nin_counts$member_nin)
  
  message(sprintf("  NIN matches: %d member(s) share a NIN.",
                  sum(members$nin_flag)))
  
  # ---------------------------------------------------------------------------
  # STEP 5 — FUZZY MATCH DETECTION
  # ---------------------------------------------------------------------------
  
  
  candidates <- members %>%
    filter(!is.na(age_num), !is.na(size_num), !is.na(fn5), !is.na(ln5), fn5 != "", ln5 != "") %>%
    select(
      record_id,
      hh_id_assessment,
      fn5, ln5, gender, age_num, size_num,
      lga_norm, ward_norm, comm_vill_norm,
      key_norm, project_norm, source_label, hoh_sub_id
    )
  
  fuzzy_pairs <- candidates %>%
    rename_with(~ paste0(.x, "_a")) %>%
    cross_join(candidates %>% rename_with(~ paste0(.x, "_b"))) %>%
    filter(
      record_id_a != record_id_b,
      record_id_a < record_id_b,
      hh_id_assessment_a != hh_id_assessment_b
    ) %>%
    mutate(
      gender_match    = gender_a == gender_b,
      lga_match       = lga_norm_a == lga_norm_b,
      ward_match      = ward_norm_a == ward_norm_b,
      comm_vill_match = comm_vill_norm_a == comm_vill_norm_b,
      age_diff        = abs(age_num_a - age_num_b),
      age_match       = age_diff <= 1,
      # HH size must be exact — unless age is a perfect match, where ±1 is allowed
      size_match      = (size_num_a == size_num_b) |
        (age_diff == 0 & abs(size_num_a - size_num_b) <= 1),
      name_dist       = stringdist(paste0(fn5_a, ln5_a),
                                   paste0(fn5_b, ln5_b), method = "lv"),
      name_match      = name_dist <= 1  # max 1 edit across combined fn5+ln5
    ) %>%
    filter(gender_match & lga_match & ward_match & comm_vill_match & age_match & size_match & name_match)
  
  message(sprintf("  Fuzzy matches: %d pair(s) found.", nrow(fuzzy_pairs)))
  
  # ---------------------------------------------------------------------------
  # STEP 6 — BUILD FLAG REPORT
  # ---------------------------------------------------------------------------
  
  flag_rows    <- list()
  flag_counter <- 0L
  
  val <- function(rec, col) {
    v <- rec[[col]]
    if (length(v) == 0 || is.null(v)) return(NA_character_)
    as.character(v[[1]])
  }
  
  make_member_url <- function(...) NA_character_

  project_scope_label <- function(proj_a, proj_b) {
    if (!is.na(proj_a) && !is.na(proj_b) && proj_a == proj_b)
      "Same project" else "Cross-project"
  }
  
  
  make_flag_row <- function(flag_id, flag_type, explanation,
                            rec_a, rec_b, severity_value) {
    data.frame(
      flag_id              = flag_id,
      flag_type            = flag_type,
      flag_explanation     = explanation,
      flag_status          = "Pending Review",
      severity             = severity_value,
      reviewed_by          = NA_character_,
      review_date          = NA_character_,
      notes                = NA_character_,
      
      record_a_id          = val(rec_a, "record_id"),
      record_a_hh_rid      = val(rec_a, "hh_record_id"),
      record_a_name        = paste(val(rec_a, "first_name"), val(rec_a, "last_name")),
      record_a_gender      = val(rec_a, "gender"),
      record_a_age         = val(rec_a, "member_age"),
      record_a_hh_id       = val(rec_a, "hh_id_assessment"),
      record_a_lga         = val(rec_a, "lga"),
      record_a_status      = val(rec_a, "hh_status"),
      record_a_hhsize      = val(rec_a, "hh_size"),
      record_a_date        = val(rec_a, "date_assessment"),
      record_a_staff       = val(rec_a, "staff_select"),
      record_a_partner     = val(rec_a, "partner"),
      record_a_source      = val(rec_a, "source_label"),
      record_a_project     = val(rec_a, "project_norm"),
      record_a_url         = make_member_url(val(rec_a, "record_id")),
      record_a_hoh_sub_id  = val(rec_a, "hoh_sub_id"),
      record_a_ward        = val(rec_a, "ward"),
      record_a_comm_vill   = val(rec_a, "comm_vill"),
      
      record_b_id          = val(rec_b, "record_id"),
      record_b_hh_rid      = val(rec_b, "hh_record_id"),
      record_b_name        = paste(val(rec_b, "first_name"), val(rec_b, "last_name")),
      record_b_gender      = val(rec_b, "gender"),
      record_b_age         = val(rec_b, "member_age"),
      record_b_hh_id       = val(rec_b, "hh_id_assessment"),
      record_b_lga         = val(rec_b, "lga"),
      record_b_status      = val(rec_b, "hh_status"),
      record_b_hhsize      = val(rec_b, "hh_size"),
      record_b_date        = val(rec_b, "date_assessment"),
      record_b_staff       = val(rec_b, "staff_select"),
      record_b_partner     = val(rec_b, "partner"),
      record_b_source      = val(rec_b, "source_label"),
      record_b_project     = val(rec_b, "project_norm"),
      record_b_url         = make_member_url(val(rec_b, "record_id")),
      record_b_hoh_sub_id  = val(rec_b, "hoh_sub_id"),
      record_b_ward        = val(rec_b, "ward"),
      record_b_comm_vill   = val(rec_b, "comm_vill"),
      match_key = paste(
        sort(c(val(rec_a, "record_id"), val(rec_b, "record_id"))),
        collapse = "_"
      ),
      
      stringsAsFactors = FALSE
    )
  }
  
  # ── MULTIPLE HoH FLAGS ─────────────────────────────────────────────
  
  if (exists("multi_hoh_hhs") && length(multi_hoh_hhs) > 0) {
    
    for (hh in multi_hoh_hhs) {
      
      recs <- members %>% filter(hh_id_assessment == hh)
      
      if (nrow(recs) < 2) next
      
      rec_a <- recs[1, ]
      rec_b <- recs[2, ]
      
      flag_counter <- flag_counter + 1L
      
      explanation <- sprintf(
        "[Data Quality Issue] Household %s has multiple members marked as Head of Household. This is invalid. Please verify which member is the correct HoH.",
        hh
      )
       type <- "Multiple HoH"
       severity <- "Data Quality"
      flag_rows[[flag_counter]] <- make_flag_row(
        paste0(
          "DEDUP-",
          format(Sys.time(), "%Y%m%d%H%M%S"),
          "-",
          flag_counter
        ),
        type,
        explanation,
        rec_a,
        rec_b,
        severity
      )
    }
  } 
  
  # ── EXACT MATCHES ──────────────────────────────────────────────────────────
  exact_groups <- members %>%
    filter(exact_flag) %>%
    group_by(key_norm_full) %>%
    summarise(record_ids = list(record_id), .groups = "drop")
  
  for (i in seq_len(nrow(exact_groups))) {
    ids   <- exact_groups$record_ids[[i]]
    key   <- exact_groups$key_norm_full[[i]]
    for (pair in combn(unique(ids), 2, simplify = FALSE)) {
    rec_a <- members %>% filter(record_id == pair[[1]])
    rec_b <- members %>% filter(record_id == pair[[2]])
    #  PREVENT SAME HOUSEHOLD MATCH
    if (val(rec_a, "hh_id_assessment") == val(rec_b, "hh_id_assessment")) next
    flag_counter <- flag_counter + 1L
    
    scope <- project_scope_label(val(rec_a, "project_norm"), val(rec_b, "project_norm"))
    type  <- sprintf("Exact Match (%s)", scope)
    
    # ── NEW: note HoH sub-IDs in the explanation to help verifiers ──────────
    hoh_note <- if (!is.na(val(rec_a, "hoh_sub_id")) && !is.na(val(rec_b, "hoh_sub_id"))) {
      if (val(rec_a, "hoh_sub_id") == val(rec_b, "hoh_sub_id"))
        sprintf("Both records share the same HoH sub-ID [%s] — likely same household.", val(rec_a, "hoh_sub_id"))
      else
        sprintf("HoH sub-IDs differ (%s vs %s) — member may appear under two different households.", val(rec_a, "hoh_sub_id"), val(rec_b, "hoh_sub_id"))
    } else { "" }
    
    explanation <- sprintf(
      "[%s] Candidate exact-key match found. %s %s (HH %s, %s, interviewed by %s on %s, %s) and %s %s (HH %s, %s, interviewed by %s on %s, %s) share an identical dedup key [%s]. Both are %s, age %s, %s, household size %s, in %s. %s Please confirm if this is the same person registered twice.",
      scope,
      val(rec_a,"first_name"), val(rec_a,"last_name"), val(rec_a,"hh_id_assessment"),
      val(rec_a,"source_label"), val(rec_a,"staff_select"), val(rec_a,"date_assessment"), val(rec_a,"partner"),
      val(rec_b,"first_name"), val(rec_b,"last_name"), val(rec_b,"hh_id_assessment"),
      val(rec_b,"source_label"), val(rec_b,"staff_select"), val(rec_b,"date_assessment"), val(rec_b,"partner"),
      key,
      val(rec_a,"gender"), val(rec_a,"member_age"), val(rec_a,"hh_status"),
      val(rec_a,"hh_size"), val(rec_a,"lga"),
      hoh_note
    )
    
    severity <- "High"
    flag_rows[[flag_counter]] <- make_flag_row(
      paste0(
        "DEDUP-",
        format(Sys.time(), "%Y%m%d%H%M%S"),
        "-",
        flag_counter
      ),
      type,
      explanation,
      rec_a,
      rec_b,
      severity
    )
    }
  }
  
  # ── NIN MATCHES ────────────────────────────────────────────────────────────
  nin_groups <- members %>%
    filter(nin_flag) %>%
    group_by(member_nin) %>%
    summarise(record_ids = list(record_id), .groups = "drop")
  
  for (i in seq_len(nrow(nin_groups))) {
    ids   <- nin_groups$record_ids[[i]]
    nin   <- nin_groups$member_nin[[i]]
    for (pair in combn(unique(ids), 2, simplify = FALSE)) {
    rec_a <- members %>% filter(record_id == pair[[1]])
    rec_b <- members %>% filter(record_id == pair[[2]])
    #  PREVENT SAME HOUSEHOLD MATCH
    if (val(rec_a, "hh_id_assessment") == val(rec_b, "hh_id_assessment")) next
    flag_counter <- flag_counter + 1L
    
    scope <- project_scope_label(val(rec_a, "project_norm"), val(rec_b, "project_norm"))
    type  <- sprintf("NIN Match (%s)", scope)
    
    # ── NEW: note HoH sub-IDs ────────────────────────────────────────────────
    hoh_note <- if (!is.na(val(rec_a, "hoh_sub_id")) && !is.na(val(rec_b, "hoh_sub_id"))) {
      if (val(rec_a, "hoh_sub_id") == val(rec_b, "hoh_sub_id"))
        sprintf("Both records share the same HoH sub-ID [%s].", val(rec_a, "hoh_sub_id"))
      else
        sprintf("HoH sub-IDs differ (%s vs %s) — member appears under two different households.", val(rec_a, "hoh_sub_id"), val(rec_b, "hoh_sub_id"))
    } else { "" }
    
    explanation <- sprintf(
      "[%s] NIN match found. %s %s (HH %s, %s, interviewed by %s on %s, %s) and %s %s (HH %s, %s, interviewed by %s on %s, %s) share the same National Identification Number [%s]. %s Review the source identity information; shared or mistyped identifiers do not establish identity.",
      scope,
      val(rec_a,"first_name"), val(rec_a,"last_name"), val(rec_a,"hh_id_assessment"),
      val(rec_a,"source_label"), val(rec_a,"staff_select"), val(rec_a,"date_assessment"), val(rec_a,"partner"),
      val(rec_b,"first_name"), val(rec_b,"last_name"), val(rec_b,"hh_id_assessment"),
      val(rec_b,"source_label"), val(rec_b,"staff_select"), val(rec_b,"date_assessment"), val(rec_b,"partner"),
      nin,
      hoh_note
    )
    severity <- "Critical"
    flag_rows[[flag_counter]] <- make_flag_row(
      paste0(
        "DEDUP-",
        format(Sys.time(), "%Y%m%d%H%M%S"),
        "-",
        flag_counter
      ),
      type,
      explanation,
      rec_a,
      rec_b,
      severity
    )
    }
  }
  
  # ── FUZZY MATCHES ──────────────────────────────────────────────────────────
  if (nrow(fuzzy_pairs) > 0) {
    
    detail_cols <- c("record_id", "first_name", "last_name", "member_age",
                     "hh_id_assessment", "lga", "ward", "comm_vill",
                     "hh_status", "hh_size", "date_assessment",
                     "staff_select", "partner", "hh_record_id",
                     "project_norm", "source_label", "hoh_sub_id")
    
    fuzzy_full <- fuzzy_pairs %>%
      left_join(members %>% select(all_of(detail_cols)),
                by = c("record_id_a" = "record_id")) %>%
      rename_with(~ paste0(.x, "_det_a"),
                  all_of(setdiff(detail_cols, "record_id"))) %>%
      left_join(members %>% select(all_of(detail_cols)),
                by = c("record_id_b" = "record_id")) %>%
      rename_with(~ paste0(.x, "_det_b"),
                  all_of(setdiff(detail_cols, "record_id")))
    
    for (i in seq_len(nrow(fuzzy_full))) {
      r <- fuzzy_full[i, ]
      flag_counter <- flag_counter + 1L
      
      scope <- project_scope_label(r$project_norm_det_a, r$project_norm_det_b)
      type  <- sprintf("Fuzzy Match (%s)", scope)
      
      diff_parts <- c()
      if (r$name_dist > 0)
        diff_parts <- c(diff_parts, sprintf(
          "name differs by %d character(s) (%s %s vs %s %s)",
          r$name_dist, r$first_name_det_a, r$last_name_det_a,
          r$first_name_det_b, r$last_name_det_b))
      if (abs(coalesce(r$age_num_a, -99L) - coalesce(r$age_num_b, -99L)) > 0)
        diff_parts <- c(diff_parts, sprintf(
          "age differs by %d year(s) (%s vs %s)",
          abs(r$age_num_a - r$age_num_b), r$age_num_a, r$age_num_b))
      if (abs(coalesce(r$size_num_a, -99L) - coalesce(r$size_num_b, -99L)) > 0)
        diff_parts <- c(diff_parts, sprintf(
          "household size differs by %d (%s vs %s)",
          abs(r$size_num_a - r$size_num_b), r$size_num_a, r$size_num_b))
      
      # ── NEW: HoH sub-ID comparison note ─────────────────────────────────
      hoh_note <- if (!is.na(r$hoh_sub_id_det_a) && !is.na(r$hoh_sub_id_det_b)) {
        if (r$hoh_sub_id_det_a == r$hoh_sub_id_det_b)
          sprintf("Both records share the same HoH sub-ID [%s].", r$hoh_sub_id_det_a)
        else
          sprintf("HoH sub-IDs differ (%s vs %s) — member may be in two different households.", r$hoh_sub_id_det_a, r$hoh_sub_id_det_b)
      } else { "" }
      
      explanation <- sprintf(
        "[%s] Near-match found. %s %s (HH %s, %s, interviewed by %s on %s, %s) and %s %s (HH %s, %s, interviewed by %s on %s, %s) are likely the same person. Differences: %s. All other dedup criteria match. %s",
        scope,
        r$first_name_det_a, r$last_name_det_a, r$hh_id_assessment_det_a,
        r$source_label_det_a, r$staff_select_det_a, r$date_assessment_det_a, r$partner_det_a,
        r$first_name_det_b, r$last_name_det_b, r$hh_id_assessment_det_b,
        r$source_label_det_b, r$staff_select_det_b, r$date_assessment_det_b, r$partner_det_b,
        paste(diff_parts, collapse = "; "),
        hoh_note
      )
      
      rec_a <- data.frame(
        record_id = r$record_id_a, hh_record_id = r$hh_record_id_det_a,
        first_name = r$first_name_det_a, last_name = r$last_name_det_a,
        gender = r$gender_a, member_age = r$member_age_det_a,
        hh_id_assessment = r$hh_id_assessment_det_a, lga = r$lga_det_a,
        ward = r$ward_det_a, comm_vill = r$comm_vill_det_a,
        hh_status = r$hh_status_det_a, hh_size = r$hh_size_det_a,
        date_assessment = r$date_assessment_det_a, staff_select = r$staff_select_det_a,
        partner = r$partner_det_a, source_label = r$source_label_det_a,
        project_norm = r$project_norm_det_a, hoh_sub_id = r$hoh_sub_id_det_a,
        stringsAsFactors = FALSE
      )
      rec_b <- data.frame(
        record_id = r$record_id_b, hh_record_id = r$hh_record_id_det_b,
        first_name = r$first_name_det_b, last_name = r$last_name_det_b,
        gender = r$gender_b, member_age = r$member_age_det_b,
        hh_id_assessment = r$hh_id_assessment_det_b, lga = r$lga_det_b,
        ward = r$ward_det_b, comm_vill = r$comm_vill_det_b,
        hh_status = r$hh_status_det_b, hh_size = r$hh_size_det_b,
        date_assessment = r$date_assessment_det_b, staff_select = r$staff_select_det_b,
        partner = r$partner_det_b, source_label = r$source_label_det_b,
        project_norm = r$project_norm_det_b, hoh_sub_id = r$hoh_sub_id_det_b,
        stringsAsFactors = FALSE
      )
      
      severity <- "Medium"
      flag_rows[[flag_counter]] <- make_flag_row(
        paste0(
          "DEDUP-",
          format(Sys.time(), "%Y%m%d%H%M%S"),
          "-",
          flag_counter
        ),
        type,
        explanation,
        rec_a,
        rec_b,
        severity
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # STEP 7 — COMPILE REPORT
  # ---------------------------------------------------------------------------
  
  if (length(flag_rows) == 0) {
    message("  No candidate matches found under the configured rules.")
    flag_report <- data.frame()
  } else {
    flag_report <- bind_rows(flag_rows) %>%
      arrange(flag_type, flag_id)
    
    message(sprintf(
      "  Flag report: %d total — %d exact, %d NIN, %d fuzzy.",
      nrow(flag_report),
      sum(str_starts(flag_report$flag_type, "Exact")),
      sum(str_starts(flag_report$flag_type, "NIN")),
      sum(str_starts(flag_report$flag_type, "Fuzzy"))
    ))
  }
  
  # ---------------------------------------------------------------------------
  # STEP 8 — RESOLVE PARENT RECORD & PUSH FLAGS
  # ---------------------------------------------------------------------------
  
  write_report(flag_report, "dedup-review-flags.csv")

  all_flagged_hh_rids <- unique(na.omit(c(
    flag_report$record_a_hh_rid,
    flag_report$record_b_hh_rid
  )))
  
  batch_flagged_hh_rids <- if (is.null(collected_hhs)) {
    all_flagged_hh_rids  # no batch scope — return all flagged records
  } else {
    intersect(all_flagged_hh_rids, collected_hhs$record_id)
  }
  
  list(
    flagged_hh_record_ids = batch_flagged_hh_rids,
    n_flags               = nrow(flag_report),
    report                = flag_report
  )
}

#  RUN DEDUP PROCESS
result <- run_dedup()

#  CHECK OUTPUT
result$n_flags
invisible(result$report)