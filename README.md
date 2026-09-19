# Humanitarian data workflows

**Seyi Abolarin · Information Management · MEAL · Data Analytics · GIS**

Public, privacy-reviewed adaptations of Python and R workflows I developed during my work with the Danish Refugee Council: structured data checks, candidate duplicate review, vulnerability analysis and beneficiary register preparation.

These examples show the engineering approach. They contain no programme datasets, real beneficiary images, operational form IDs, API credentials or private service links. This is an individual portfolio collection, not an official DRC product.

## Explore the work

| Capability | Start here |
| --- | --- |
| Distribution and verification registers | [Notebook 01](notebooks/01-distribution-register.ipynb), [Notebook 07](notebooks/07-validation-register.ipynb) |
| Editable documents and cash register variants | [Notebook 02](notebooks/02-editable-register.ipynb), [Notebook 03](notebooks/03-beneficiary-register.ipynb), [Notebook 04](notebooks/04-cash-register-variant.ipynb) |
| Picture preparation | [Notebook 05](notebooks/05-picture-compression.ipynb), [Notebook 06](notebooks/06-local-attachments.ipynb) |
| Household and member consistency checks | [R/data-cleaning.R](R/data-cleaning.R) |
| Candidate duplicate review | [R/deduplication.R](R/deduplication.R) |
| Vulnerability scoring and disaggregation | [R/vaf-scoring-analysis.R](R/vaf-scoring-analysis.R) |
| Household representative export | [R/nagis-export.R](R/nagis-export.R) |

Read the [per-file review and limitations](docs/REVIEW.md) before reusing the code. All 11 supplied files are accounted for, including the earlier cash-register variant.

## Run the Python examples

Python 3.11 or newer is recommended.

```powershell
python -m venv .venv
.venv\Scripts\python -m pip install -r requirements.txt
.venv\Scripts\python tests/run_notebooks.py
.venv\Scripts\python -m unittest discover -s tests -v
```

The runner executes each notebook's Python cells in order in a fresh namespace. It uses three fictional records and geometric image placeholders. Files appear in `outputs/`, which Git ignores. To use the notebook interface, install JupyterLab separately and open `notebooks/`.

## Read and adapt the R workflows

Run from the repository root. Install dependencies explicitly:

```r
install.packages(c("dplyr", "stringr", "tidyr", "lubridate", "stringdist", "ggplot2", "scales"))
```

Input headers are documented in [docs/input-columns.json](docs/input-columns.json), with empty CSV templates under `data/templates/`. Place approved exports with these aliases in `data/private/`. The default scripts do **not** connect to ActivityInfo. The original field mapping and automated remote flag writes have been removed.

```powershell
Rscript R/data-cleaning.R
Rscript R/deduplication.R
Rscript R/vaf-scoring-analysis.R
$env:WORKFLOW_AS_OF = "2026-01-01"
Rscript R/nagis-export.R
```

The full R cleaning, deduplication and scoring workflows require locally prepared inputs and their listed packages. They have been syntax checked, but not executed end-to-end in this release. The dependency-free representative export has synthetic tests. Scoring thresholds are retained as programme-specific examples, not validated universal criteria or automatic eligibility rules. Duplicate flags require human review.

## Add another project

Add a clearly named notebook or R script, describe the input/output contract, use fictional fixtures, update the review table and run the checks. Clear notebook outputs before committing any notebook that has touched real records. Inspect staged changes for credentials, personal information, private IDs and attachments; `.gitignore` is only a first layer.

## Rights and attribution

The code is presented as a professional portfolio showcase. No open-source licence is granted in this release. Employer and third-party rights are not transferred by this repository. No organisational logos or beneficiary photographs are included.

[Portfolio](https://seyiabolarin.github.io/projects/) · [GitHub profile](https://github.com/seyiabolarin)
