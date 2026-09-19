# Release validation

Checked on 19 September 2026.

- Seven Python notebooks: every code cell executed in order against bundled fictional data, with a fresh namespace for each notebook. No live API requests.
- Five Python tests passed: duplicate input IDs; image preservation and metadata removal; PDF pagination and card/amount alignment; local attachment naming; picture path containment.
- Five R files parsed successfully. Synthetic tests passed for the dependency-free representative export, including duplicate roles, missing/future dates of birth and the example age threshold.
- Full execution of `data-cleaning.R`, `deduplication.R` and `vaf-scoring-analysis.R` was not completed: required R packages and approved mapped CSV inputs were unavailable. Follow the README setup commands to validate them locally. Syntax checks do not validate scoring methodology or operational correctness.
- Notebooks were inspected as structured JSON and their Python cells were executed with `tests/run_notebooks.py`. The release was not run through a Jupyter kernel or nbformat validator because those packages were unavailable. The notebooks contain standard Python cells only, no magics, and ship without saved outputs.
- A generated register PDF was visually inspected. Automated PDF checks also verify pagination and complete row/column content. The editable DOCX was generated successfully but was not separately rendered for a visual review.
- Publication scan checked the deliverable against 168 private source identifiers and the one embedded credential found in the originals. None remained. No original notebook outputs, local user paths, source photographs or operational input files were packaged.

Automated checks are included under `tests/`. The GitHub workflow runs Python examples and tests plus R syntax and representative-export tests; it does not validate the programme scoring rules.
