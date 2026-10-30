# ahs-extension-productivity-rwanda
Shiny dashboard exploring agricultural extension access and crop productivity in Rwanda using NISR AHS 2024 data.
# Agricultural Extension Services and Crop Productivity in Rwanda

**NISR Big Data Hackathon 2026**

## 1. Project Overview

This project investigates whether access to agricultural extension services is associated with crop productivity among farming households in Rwanda.

Using data from the National Institute of Statistics of Rwanda (NISR) Agricultural Household Survey (AHS) 2024, Season A, the project applies statistical analysis and data visualization to explore the relationship between agricultural extension services and crop productivity.

## 2. Research Question

Is access to agricultural extension services associated with crop productivity among farming households in Rwanda?

## 3. Project Objectives

- Explore crop productivity among farming households.
- Examine differences in productivity between households with and without access to agricultural extension services.
- Compare productivity patterns across crops and provinces.
- Apply statistical methods to investigate the relationship between extension access and crop productivity.
- Develop an interactive dashboard to communicate findings.

## 4. Dataset

**Source:** National Institute of Statistics of Rwanda (NISR)

**Survey:** Agricultural Household Survey (AHS) 2024

**Period:** Season A

The analysis uses selected variables, including:

- Household identification
- Crop name
- Household median crop productivity (kg/ha)
- Agricultural extension access
- District and province
- Fertilizer-use proportions
- Irrigation proportion
- Survey weight and cluster information

The original microdata is not included in this repository unless redistribution is explicitly authorized by NISR.

## 5. Tools and Technologies

- R and RStudio
- dplyr and tidyr
- ggplot2
- Shiny
- Statistical modelling
- GitHub

## 6. Methodology

The project follows these stages:

1. Data import and understanding.
2. Data quality assessment.
3. Data cleaning and preparation.
4. Exploratory data analysis.
5. Statistical comparisons between extension-access groups.
6. Regression analysis.
7. Sensitivity analysis for extreme observations.
8. Interactive dashboard development.

## 7. Interactive Dashboard

The Shiny dashboard is designed to allow users to:

- Filter data by crop.
- Compare households with and without extension access.
- Explore productivity across provinces.
- View summary indicators and visualizations.
- Examine the underlying data.

**Dashboard link:** To be added after deployment.

## 8. Repository Structure

```text
ahs-extension-productivity-rwanda/
│
├── README.md
├── app.R
├── clean_data.R
├── complete_analysis_dashboard.R
├── seasonA_province_cleaned.csv
└── plots/
```

*Only files that actually exist and are permitted for sharing should be included in the repository.*

## 9. Expected Contribution

The project aims to provide evidence that can support understanding of agricultural extension services and crop productivity in Rwanda.

The findings may help identify differences in productivity across farming households and inform discussions about agricultural support services.

## 10. Limitations

- The analysis focuses on selected crops and Season A data.
- Observed associations do not necessarily imply causation.
- Extreme productivity observations require careful interpretation.
- Survey weights and clustering must be considered when making population-level conclusions.
- Results depend on the available variables and survey documentation.

## 11. Acknowledgement

I sincerely acknowledge the National Institute of Statistics of Rwanda (NISR) for providing the data used in this project and for organizing the Big Data Hackathon 2026.

I also appreciate the learning opportunities and support that have strengthened my skills in statistical analysis, data visualization, and data science.

---

**Project:** NISR Big Data Hackathon 2026  
**Research area:** Agricultural Statistics and Data Analytics  
**Programming language:** R
