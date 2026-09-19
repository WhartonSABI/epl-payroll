# EPL Payroll and Points

This repository supports an article for Wharton Sports Analytics and Business Initiative on the relationship between Premier League wage spending and league points from 2013-14 through 2024-25.

The main result is that payroll and points are strongly related in pooled league data, but the average within-club relationship weakens once club identity is controlled for. The clearest split is in the interaction model: payroll is strongly associated with points for non-Big-Six clubs, while the Big Six slope is statistically unclear.

## Repository Structure

- `data/raw/`: source wage and points CSVs.
- `data/processed/`: merged club-season dataset used by the models.
- `scripts/`: reproducible R pipeline.
- `outputs/`: model estimates and fit statistics.
- `article/`: LaTeX article, publication figures, and compiled PDF.
- `plots/`: exploratory plot output.

## Requirements

The analysis uses R with these packages:

- `dplyr`
- `ggplot2`
- `scales`

To build the PDF, install a LaTeX distribution with `latexmk`.

## Reproduce The Pipeline

Run these commands from the repository root:

```sh
Rscript scripts/01_data-prep.R
Rscript scripts/02_eda.R
Rscript scripts/03_analysis.R
Rscript scripts/04_article-plots.R
```

Then compile the article:

```sh
cd article
latexmk -pdf -interaction=nonstopmode main.tex
```

## Key Outputs

- `outputs/model_estimates.csv.gz`: coefficient estimates used in the article.
- `outputs/model_fit.csv.gz`: adjusted R-squared, residual standard error, and sample size by model.
- `article/01_overall_relationship.png`: pooled payroll-points relationship.
- `article/02_team_effects.png`: within-club demeaned relationship.
- `article/03_big_six_interaction.png`: Big Six vs non-Big-Six interaction slopes.
- `article/main.pdf`: compiled article draft.
