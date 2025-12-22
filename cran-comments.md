## Resubmission

This is a resubmission following CRAN feedback.

## Test environments
- macOS (local), R 4.4.x
- Windows Server 2022, R-devel (CRAN checks)
- Debian GNU/Linux, R-devel (CRAN checks)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
- "Possibly misspelled words in DESCRIPTION": These are proper nouns or domain-specific terms (Dedoose, DedooseR, codebooks).
- "unable to verify current time": This appears to be environment-related; no files in the package have future timestamps.

## Changes since last submission
- Updated DESCRIPTION to use a canonical URL with trailing slash.
- Revised Title and Description per CRAN guidelines.
- Fixed NEWS.md heading consistency.
- Bumped version to 2.0.0.1.
