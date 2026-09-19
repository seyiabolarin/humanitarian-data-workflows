# Source review and public adaptation

The source files were reviewed as text; their live queries, downloads and remote writes were not executed. Original files remain unchanged outside this repository. This release contains public adaptations, not operational drop-in replacements.

## Per-file record

| Supplied source | Public location | Review and changes |
| --- | --- | --- |
| More updated NAGIS Script_2026_05_18.R | R/nagis-export.R | Replaced remote household/member queries and attachment retrieval with local CSV joins. Removed private schema IDs, identity-document and photograph URLs. Added explicit as-of date, missing/future DOB handling and multiple-representative review. No automatic eligibility decision. |
| VAF Scoring Analysis.R | R/vaf-scoring-analysis.R | Removed private field mappings and service setup. Retained scoring and chart logic as an offline reference. Corrected demographic-share aggregation and missing rCSI category handling. Outputs stay local. |
| Finalized Deduplication_Script.R | R/deduplication.R | Removed live queries, record links and remote tracker writes. Local inputs retain candidate matching and human-review reports. Aligned exact grouping keys, corrected the date-window boundary removed missing-age sentinel comparisons, normalized identifier placeholders, and expanded exact/NIN groups to every record pair. Fuzzy candidates now include members with an exact match elsewhere. |
| Most Updated Cleaning file_2026.05.25.R | R/data-cleaning.R | Removed private queries, form IDs and remote flag writes. Retained household/member consistency checks; corrected household partner lookup, the date-window boundary and all-missing food-response handling. |
| ActivityInfo Distribution List.ipynb | notebooks/01-distribution-register.ipynb | Removed personal paths, project filenames, logos and saved logs. Refactored export layout into a shared local renderer with widths fitted to the page and escaped text. |
| ActivityInfo.ipynb | notebooks/02-editable-register.ipynb | Removed live picture URLs and external office conversion. Demonstrates editable DOCX plus independently generated PDF from the same fictional rows. |
| Beneficiary Generation.ipynb | notebooks/03-beneficiary-register.ipynb | Removed community-specific priority lists, paths and branding. Replaced faulty sex/residency sorting with explicit district/community/name ordering. Corrected overflowing PDF widths. |
| Beneficiary Generation-Copy3.ipynb | notebooks/04-cash-register-variant.ipynb | Consolidated duplicated rendering code. Card-number and amount columns now use the same column definition as their headers and widths. |
| Picture Compression.ipynb | notebooks/05-picture-compression.ipynb | Removed saved filename logs and personal paths. Preserves folder structure and source files, avoids extension collisions, applies EXIF orientation and strips metadata from derived copies. |
| Picture Download from ActivityInfo.ipynb | notebooks/06-local-attachments.ipynb | Removed an embedded credential, URLs, logs and authenticated download behaviour. Public example prepares local placeholder attachments only; no token is sent to any destination. |
| Validation List for ActivityInfo.ipynb | notebooks/07-validation-register.ipynb | Removed operational inputs, paths and branding. Shared renderer creates aligned verification columns without overwriting source photographs. |

## Security follow-up for the owner

One original notebook contained an embedded API credential. Its value is not reproduced here and its validity was not tested. If still active, revoke or rotate it in ActivityInfo. Never publish the originals or their notebook outputs, including in Git history. Removing a token from a new copy does not invalidate the old token.

## Scope and remaining limitations

- There are 22 active cleaning checks: C01–C19 plus C22–C24. Duration checks C20 and C21 remain commented out, as in the source.
- Deduplication operates on heads of household in the selected input scope. It is candidate detection, not proof of duplicate identity. The abbreviated name keys, matching thresholds, multiple-HoH handling and pairwise fuzzy search require review before production use. Date filters narrow the matching universe; leave them unset to compare all supplied history. The fuzzy search has quadratic cost and is not suitable for unbounded datasets.
- The original VAF weights, labels and thresholds remain programme-specific. Food-consumption cut-offs, rCSI interpretation/weight ordering, missing-data propagation and pregnancy/breastfeeding choice labels require subject-matter validation against the approved tools. The public code does not certify these rules or determine who receives assistance.
- Public document examples deliberately omit real phones, national IDs, photographs, donor logos and project-specific prioritisation. They demonstrate data preparation and layout, not a production distribution template.
- Live ActivityInfo integration, record writing and authenticated attachment downloading are outside this public release. No live system was contacted during testing.
- Python notebooks were refactored into reusable helper functions and small demonstrations. Refer to their mapped originals above for provenance; they should not be described as unchanged operational source code.

## Validation

Run `python tests/run_notebooks.py` and `python -m unittest discover -s tests -v` from the repository root. The release notes record the checks actually completed. Notebooks ship without saved cell outputs or execution metadata to keep future publication hygiene consistent.
