# Chicago Crime Hotspot

This repository contains analysis code and datasets for identifying crime hotspots in Chicago. The project contains R scripts, example outputs and CSV datasets used for exploratory data analysis and visualization.

## Contents

- `Crime_Hotspot_Chicago.R` — Primary R script for the hotspot analysis and visualizations.
- `Code.R` — Supporting R code used during exploration.
- `Crimes_2020.csv`, `Crimes_2025.csv` — Example datasets used for analysis (sensitive/personal data should be handled according to policy).
- `Rplot_*.png` — Generated plot images from the analysis.

## Purpose

The goal of this project is to detect and visualize geographic concentrations of crime in Chicago, demonstrate approaches for hotspot detection, and provide reproducible R code for analysis and plotting.

## Quick start

1. Install R and the dependencies required by the scripts. Typical packages used are `tidyverse`, `sf`, `ggplot2`, and `spatstat`. You can install packages in R with:

```r
install.packages(c("tidyverse", "sf", "ggplot2", "spatstat"))
```

2. Open `Crime_Hotspot_Chicago.R` in RStudio or run it from an R terminal:

```r
source("Crime_Hotspot_Chicago.R")
```

3. The scripts read the CSV files in the repository. If you replace or update the CSVs, ensure column names expected by the scripts are preserved.

## Notes and best practices

- Large binary or intermediate files (for example `.RData`, `.Rhistory` or plot images) are included here for convenience. Consider removing them from the repository and adding them to `.gitignore` if they are not needed in version control.
- Do not commit sensitive personal data to public repositories. If the crime CSVs contain personally-identifying information, remove or anonymize those fields before publishing.

## Contact

If you have questions about the code or datasets, open an issue in the repository or comment.

---