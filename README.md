# Performance Analysis

R-based analysis of performance data across multiple groups and experimental conditions.

## Overview

This repository contains an R analysis script demonstrating a reproducible workflow for processing, analyzing, and visualizing performance data.

The analysis includes data preparation, descriptive statistics, statistical testing, mixed-effects modeling, effect-size estimation, and data visualization.

## Analysis

The analysis script includes:

- Data cleaning and preparation
- Descriptive statistics
- Group-level comparisons using ANOVA
- Shapiro-Wilk normality tests
- Mixed-effects modeling
- Estimated marginal means and pairwise comparisons
- Effect-size calculations
- Data visualization

## Data

The original dataset is not included in this repository.

The analysis script is structured to work with a dataset containing participant-level observations, group information, condition information, ratings, scores, and performance measures.

Generic variable and category names are used in the public version of the analysis script.

## Software and Packages

The analysis was conducted in R.

Required R packages include:

- `tidyverse`
- `lme4`
- `lmerTest`
- `emmeans`
- `effectsize`

## Repository Structure

```text
performance-analysis/
│
├── performance_analysis.R
├── README.md
└── LICENSE

```markdown
## AI Assistance

AI-assisted tools were used during the development and refinement of this analysis script, including for coding support, debugging, and documentation. The analysis decisions, interpretation of results, and final review of the code were conducted by the author.
