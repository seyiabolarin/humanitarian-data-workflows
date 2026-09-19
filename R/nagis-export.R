# Offline adaptation of the household/representative export workflow.
# No API credentials, attachment URLs, identity scans or remote record writes.
# This creates a REVIEW export, not an assistance eligibility decision.
source("R/local_io.R")

prepare_export <- function(households, members, as_of) {
  as_of <- as.Date(as_of)
  if (length(as_of) != 1 || is.na(as_of)) stop("Supply an explicit as_of date.")
  stopifnot(all(c("hh_id_assessment", "project_name") %in% names(households)),
    all(c("record_id", "hh_id_assessment", "first_name", "last_name", "member_dob", "member_hoh", "member_proxy_hoh") %in% names(members)))
  if (anyNA(households$hh_id_assessment) || anyDuplicated(households$hh_id_assessment))
    stop("Household IDs must be unique and nonmissing.")
  yes <- function(x) !is.na(x) & tolower(trimws(x)) == "yes"
  # Only designated representatives enter this export; unrelated children are excluded.
  m <- members[yes(members$member_hoh) | yes(members$member_proxy_hoh), , drop = FALSE]
  dob <- as.Date(m$member_dob)
  m$age <- as.integer(format(as_of, "%Y")) - as.integer(format(dob, "%Y")) -
    as.integer(format(as_of, "%m%d") < format(dob, "%m%d"))
  m$role <- ifelse(yes(m$member_hoh), "HoH", "Proxy")
  key <- paste(m$hh_id_assessment, m$role, sep = "|")
  ambiguous <- duplicated(key) | duplicated(key, fromLast = TRUE)
  m$review_reason <- ifelse(!m$hh_id_assessment %in% households$hh_id_assessment, "Household not found",
    ifelse(is.na(m$age) | dob > as_of, "Missing or invalid date of birth",
      ifelse(m$age < 18, "Representative below example age threshold",
        ifelse(ambiguous, "Multiple representatives for the same role", "Ready for review"))))
  m$review_reason[yes(m$member_hoh) & yes(m$member_proxy_hoh)] <- "Both roles selected"
  merge(m, households, by = "hh_id_assessment", all.x = TRUE, sort = TRUE)
}

if (sys.nframe() == 0) {
  households <- read_input("nagis-households.csv")
  members <- read_input("nagis-members.csv")
  as_of <- Sys.getenv("WORKFLOW_AS_OF", "2026-01-01")
  write_report(prepare_export(households, members, as_of), "representatives-review.csv")
}
