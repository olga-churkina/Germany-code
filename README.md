## Replication code for "A quasi-experiment in international student mobility: Germany’s fee re-introductions" in European Journal of Higher Education

###  Package installations

The following R packages are used in the analysis:

- readxl (Excel files)
- plm (panel data)
- lmtest (Breusch-Pagan and Breusch-Godfrey tests)
- dplyr (data manipulations)
- ggplot2 (visualizations)
- Synth (synthetic control analysis)
- SCtools (extensions for synthetic controls analysis)
- stringr (string manipulations)
- stats (statistical functions)
- cvTools (cross-validation tools for regression models)
- stargazer (exporting to LaTeX)
- furrr, purr & magrittr (speed the calculations)

### Data Analysis

The R code `Gernamy code.R` consists of two parts: extended difference-in-differences and synthetic control analyses. First, we generated two-way fixed effects difference-in-differences estimates from three regressions: one full-set regression and two regressions using 'stable' and 'small' state subgroups (see the paper for more details). Second, we created seven synthetic controls, one for each fee re-introducing state, and used them to generate fictitious outcomes (i.e., the incoming student cohorts had the state followed the same trend of its synthetic control which ‘never reintroduced’ fees). The second part of the analysis includes two visualizations: comparison of observed and synthetic trends and "spaghetti charts" for each state. The analysis is followed by robustness checks.
