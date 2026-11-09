# Statistics coursework

Code from three statistics courses. Each folder is one course project with its own data and scripts.

| Folder | Course |
| --- | --- |
| `regression-car-prices/` | MSG501 Statistical learning with regression models, Autumn 25 |
| `bayesian-hmc-election/` | MSA102 Computational methods for Bayesian statistics, Autumn 25 |
| `biomedicine/` | MSA680 Data science for biomedicine, Spring 26 |

## MSG501: Regression analysis of car prices

Final project in the course Statistical learning with regression models (MSG501, Autumn 25).

- `carAnalysis.R`: predicts car prices from engine size, horsepower, wheelbase, fuel type, car body and drive wheel.
- `poissonControllers.R`: a separate exercise from the same project (Poisson regression of errors made by air traffic controllers). It needs `controllers.dat`, which is not included, so the script skips itself when the file is missing.

Methods: exploratory plots, correlation heatmap, VIF tests, linear regression, outlier and influence checks (Cook's distance, leverage, change in slopes), backward selection with AIC, log transformation, forward comparison with AIC, BIC and adjusted R squared, all possible subsets regression, F tests, Poisson regression with an offset, dispersion test, standardized Pearson residuals with a Bonferroni limit.

## MSA102: Hamiltonian Monte Carlo for election night

Project in Computational methods for Bayesian statistics (MSA102, Autumn 25).

- `HamiltonianMonteCarlo.R`: predicts the outcome of an election as results come in during the night. It uses the result of the previous election, exit polls and the final results received by time t.

Methods: a hierarchical model on the log odds scale, analytic gradient checked against `numDeriv`, a leapfrog integrator, Hamiltonian Monte Carlo sampling, and simulation of the number of seats to get the probability that party A wins a majority over time.

## MSA680: Data science for biomedicine

Lab projects in Data science for biomedicine (MSA680, Spring 26).

1. `01-ancova/`: ANCOVA models and least squares means (R and SAS).
2. `02-linear-mixed-models/`: linear mixed models, missing data, mean response profiles, covariance matrices and serial correlation (SAS only).
3. `03-survival-analysis/`: Kaplan-Meier estimates, cumulative hazards, log rank test, partial likelihood and Cox proportional hazards models.
4. `04-simulation/`: simulation of a clinical trial, power calculations, and Type I and Type II error estimates.

### SAS files

The `.sas` files were written for SAS OnDemand for Academics (SAS Studio). To run them, upload the `.sas` file and its data to SAS Studio and set `%let datadir = ...;` at the top of the file to the folder that holds the data. They have not been run as part of this repository.

## How to run

Requirements: R 4.x and these CRAN packages: car, corrplot, olsrr, AER, numDeriv, tidyverse, scales, emmeans, GGally, survival, survminer, pwr, foreach.

```r
install.packages(c("car", "corrplot", "olsrr", "AER", "numDeriv", "tidyverse", "scales",
                   "emmeans", "GGally", "survival", "survminer", "pwr", "foreach"))
```

Each script expects its own folder as the working directory. Plots are saved as png files in that folder.

```sh
(cd regression-car-prices && Rscript carAnalysis.R)
(cd regression-car-prices && Rscript poissonControllers.R)   # needs controllers.dat
(cd bayesian-hmc-election && Rscript HamiltonianMonteCarlo.R)
(cd biomedicine/01-ancova && Rscript ancovaR.R)
(cd biomedicine/03-survival-analysis && Rscript survAnalysis.R)
(cd biomedicine/04-simulation && Rscript simulation.R)
```

Run times on a laptop with R 4.5.2: about 3 seconds for `carAnalysis.R` and `ancovaR.R`, 7 seconds for `HamiltonianMonteCarlo.R`, 11 seconds for `survAnalysis.R` and 45 seconds for `simulation.R`. Scripts with random steps use a fixed seed, so the numbers are the same on each run.

## Results

These numbers come from running the scripts in this repository.

- Car prices: the model chosen by backward selection (engine size, horsepower, wheelbase, fuel type, car body) has adjusted R squared 0.862 on the training data without outliers. The simple model with engine size and drive wheel has adjusted R squared 0.794 on the full data.
- Election HMC: the analytic gradient of the potential energy matches the numerical gradient from `numDeriv` at the test point.
- WHAS500: 215 deaths among 500 patients, Kaplan-Meier median survival 1627 days (95% log-log interval 1506 to 2353).
- Simulation: the simulated power of the z test with effect 0.5 is 0.31 for n = 8 and 0.69 for n = 24, close to the analytic values 0.293 and 0.688.

## Data sources

- `regression-car-prices/CarPrice.csv`: the Kaggle "Car Price Prediction" dataset. It is based on the [UCI Automobile dataset](https://archive.ics.uci.edu/dataset/10/automobile) (Schlimmer, 1987), which is licensed CC BY 4.0. The Kaggle copy may have different terms.
- `regression-car-prices/controllers.dat` (not included): provided by the course.
- `bayesian-hmc-election/election25.txt`: provided by the course.
- `biomedicine/01-ancova/diet.txt`: provided by the course.
- `biomedicine/02-linear-mixed-models/growth*.sas7bdat`: provided by the course. This looks like the dental growth data of Potthoff and Roy (1964), with two versions that have missing values.
- `biomedicine/03-survival-analysis/whas500.csv`: Worcester Heart Attack Study, from Hosmer, D.W., Lemeshow, S. and May, S. (2008), Applied Survival Analysis, 2nd edition, Wiley.
