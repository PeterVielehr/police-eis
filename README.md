# Police Early Intervention System (EIS)

This project is a data-driven Early Intervention System (EIS) for police departments. It uses departmental data to predict which officers are likely to have an adverse interaction with the public. An adverse incident can be defined on a department by department basis, but typically includes unjustified uses of force, officer injuries, preventable accidents and sustained complaints. This is done such that additional training, counseling and other resources can be provided to the officer _before_ any adverse interactions occur.

## R Tidyverse Pipeline

An experimental implementation in R is available in the `R/pipeline.R` script.
It implements feature engineering blocks with a tidyverse workflow and can
train either a logistic regression or an XGBoost model using **tidymodels**.
The result includes model metrics, a confusion matrix, and
per-officer SHAP values derived with `fastshap`.

```r
source("R/pipeline.R")
result <- run_pipeline("path/to/incidents.csv", model = "xgboost")
print(result$metrics)
result$confusion
head(result$shap)
```

The CSV is expected to contain `officer_id`, `event_datetime`, `event_type`, and
any columns needed for the individual feature blocks (for example
`suspension_type`, `incident_type`, `shift_type`, `driver_race`, `bodycam_on`,
`manual_shutoff`, `missing_footage`, etc.) along with an `outcome` flag.

Body-worn camera features rely on records where `event_type` is `bodycam` and
use the `bodycam_on`, `manual_shutoff`, and `missing_footage` fields.

Peer context features additionally require an `incident_id` to link officers
who respond to the same incident. The data should also include `complaint` and
`use_of_force` events with dates so that colleagues with recent issues can be
identified. Network centrality metrics are calculated with the optional
`igraph` package.

## Issues

Please use [Github's issue tracker](https://github.com/dssg/police-eis/issues/new) to report issues and suggestions.

## Contributors

* 2016: Tom Davidson, Henry Hinnefeld, Sumedh Joshi, Jonathan Keane, Joshua Mausolf, Lin Taylor, Ned Yoxall, Joe Walsh (Technical Mentor), Jennifer Helsby (Technical Mentor), Allison Weil (Project Manager)
* 2015: Jennifer Helsby, Samuel Carton, Kenneth Joseph, Ayesha Mahmud, Youngsoo Park, Joe Walsh (Technical Mentor), Lauren Haynes (Project Manager).
