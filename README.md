# Police Early Intervention System (EIS)

![Build Status](https://travis-ci.org/dssg/police-eis.svg)
[![Documentation Status](https://readthedocs.org/projects/police-eis/badge/?version=latest)](http://police-eis.readthedocs.org/en/latest/?badge=latest)

This is a data-driven Early Intervention System (EIS) for police departments. The system uses a police department's data to predict which officers are likely to have an adverse interaction with the public. An adverse incident can be defined on a department by department basis, but typically includes unjustified uses of force, officer injuries, preventable accidents and sustained complaints. This is done such that additional training, counseling and other resources can be provided to the officer _before_ any adverse interactions occur.

## R Tidyverse Pipeline

An experimental implementation in R is available in the `R/pipeline.R` script.
It translates the Python feature engineering blocks into a tidyverse workflow
and can train either a logistic regression or an XGBoost model using
**tidymodels**.  The result includes model metrics, a confusion matrix,
per-officer SHAP values derived with `fastshap`, and a global feature
importance table with an accompanying `plot_shap_importance()` helper.


```r
source("R/pipeline.R")
result <- run_pipeline("path/to/incidents.csv", model = "xgboost")
print(result$metrics)
result$confusion
head(result$shap)
result$shap_summary
plot_shap_importance(result$shap)
```


Feature engineering helpers are organized in `R/features.R`.  In addition to
the incident, shift, arrest, and traffic stop summaries, the feature set now
includes time-of-day and calendar attributes from dispatch records (minute,
hour, day of week, quarter, month, and year), detailed complaint outcome counts,
use-of-force metrics such as unjustified force, suspect injuries,
temporal change indicators, and disproportionate-force ratios, and peer
context counts of incidents involving colleagues with recent complaints or
use-of-force events.

The CSV is expected to contain `officer_id`, `event_datetime`, `event_type`, and
any columns needed for the individual feature blocks (for example
`incident_id`, `suspension_type`, `incident_type`, `shift_type`, `driver_race`,
`use_of_force_type`, `in_response_to_resisting_arrest`, or `source`) along
with an `outcome` flag.

### Daily predictions and risk tiers

The helper functions in `R/predictions.R` allow the trained model to score new
data each day, store the results in a SQLite database, and categorise officers
into low, moderate, or high risk based on their prediction history.  The risk
score combines the average predicted probability with the recent trend in
scores so that consistently high and rapidly increasing risks are both
highlighted.  Rather than using a fixed flag rate, thresholds for the risk
categories can be learned from validation data to maximise accuracy while
reducing false negatives.

```r
source("R/pipeline.R")
source("R/predictions.R")

# Train a model once
model <- run_pipeline("path/to/incidents.csv", model = "xgboost")$model

# Score new data for today and store the probabilities
today <- load_data("path/to/today_incidents.csv")
predict_and_store(model, today, db_path = "data/eis_predictions.sqlite")

# Derive risk tiers from the accumulated prediction history using
# outcomes from a validation set to learn dynamic thresholds
labels <- tibble(officer_id = c(1, 2), outcome = c(1, 0))
risk <- categorize_risk("data/eis_predictions.sqlite", labels = labels)
head(risk)
```

These helpers require the `DBI` and `RSQLite` packages for database access.



