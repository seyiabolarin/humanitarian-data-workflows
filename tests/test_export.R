source("R/nagis-export.R")
h <- data.frame(hh_id_assessment = c("DEMO-H1", "DEMO-H2"), project_name = "Fictional project")
m <- data.frame(record_id = paste0("DEMO-", 1:5), hh_id_assessment = c("DEMO-H1", "DEMO-H1", "DEMO-H1", "DEMO-H2", "DEMO-H2"),
 first_name = "Sample", last_name = "Person", member_dob = c("1990-01-01", "1991-01-01", "2010-01-01", NA, "2027-01-01"),
 member_hoh = c("Yes", "Yes", "No", "Yes", "No"), member_proxy_hoh = c("No", "No", "Yes", "No", "Yes"))
r <- prepare_export(h, m, "2026-01-01")
stopifnot(nrow(r) == 5,
 sum(r$review_reason == "Multiple representatives for the same role") == 2,
 sum(r$review_reason == "Missing or invalid date of birth") == 2,
 sum(r$review_reason == "Representative below example age threshold") == 1)
cat("PASS representative review: duplicate roles, missing/future DOB and age threshold\n")
