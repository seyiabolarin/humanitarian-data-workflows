# Run scripts from the repository root. Only explicit local CSV files are read.
# Private inputs and all generated reports are excluded by .gitignore.
DATA_DIR <- Sys.getenv("WORKFLOW_DATA_DIR", "data/private")
OUTPUT_DIR_LOCAL <- "outputs"
read_input <- function(filename) {
  path <- file.path(DATA_DIR, filename)
  if (!file.exists(path)) stop(paste("Missing local input:", filename,
    "— see docs/input-columns.json and data/templates. No online request was made."))
  # Read as text so IDs and leading zeroes survive. Scripts cast numeric fields.
  data <- read.csv(path, colClasses = "character", check.names = FALSE,
           stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8")
  # Cast only documented measurements; never auto-convert IDs, names or NIN.
  numeric_fields <- c(paste0("fcs", 1:10), paste0("rcsi_", 1:5),
    "member_age", "child_age_months", "hh_member_count", "hh_working_count",
    "hh_size", "mad_meal_freq", "mad_milk_feeds")
  for (column in intersect(names(data), numeric_fields)) {
    converted <- suppressWarnings(as.numeric(data[[column]]))
    if (any(!is.na(data[[column]]) & is.na(converted)))
      stop(paste("Invalid numeric input in column:", column))
    data[[column]] <- converted
  }
  data
}
write_report <- function(data, filename) {
  dir.create(OUTPUT_DIR_LOCAL, recursive = TRUE, showWarnings = FALSE)
  write.csv(data, file.path(OUTPUT_DIR_LOCAL, filename), row.names = FALSE, na = "")
}
