# fairq-features

This repository contains R code that creates all features and coordinates for modelling, that are based on Berlin's city structure and writes them into the features schema of the database.


## How to use this package

- Create an `.Renviron` file in the project folder and fill it with credentials, see `.Renviron_template` for the structure.
- Build the R package.
- Create database as described in https://github.com/INWT/fairq-data/tree/public/inst/db (schema `fairq_raw` and `fairq_features`).
- Before running the ETL, consider setting a memory limit via `inst/RScripts/set_memory_limit.R`, because geodata feature engineering is computationally expensive. 
- Run `inst/RScripts/main.R` to start the ETL for all features/coordinates. 
- The folder `inst/reports` contains reports on feature validation and plausibility of various features.

## Input and output

### Input

- Database, schema `fairq_raw`. For a few features, additional input from previously created features stored in the schema `fairq_features` is needed, too. Hence the order of feature creation matters.

### Output

- Database, schema `fairq_features`.
