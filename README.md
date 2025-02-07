# Insight into Property Prices in Bournemouth

## Overview
This repository contains an analysis of property prices in Bournemouth, England, focusing on key factors affecting house valuations. The study explores variations in cost based on size, energy efficiency, and regional affordability.

## Contents
- **Poster**: A detailed statistical analysis using a Generalized Linear Model (GLM) with a Gamma distribution to model property prices.
- **Code and Visualizations**: Graphical insights into the distribution of prices, model diagnostics, and affordability across different regions.
- **Shiny App**: An interactive web application allowing users to explore property price trends and model predictions.

## Data & Methodology
- Data sourced from University College London and the UK Land Registry.
- Key predictors include:
  - Total floor area
  - Number of habitable rooms
  - Property type (Maisonette, Flat, House, Bungalow)
  - Energy efficiency ratings
  - Environmental impact ratings
- The analysis was conducted in R, utilizing GLMs to model price variation.

## How to Use the Shiny App
1. Clone this repository.
2. Open the `shiny_app` directory.
3. Run `app.R` in RStudio or another R environment.
4. Interact with the data through various filters and visualizations.

## Results Summary
- A 1% increase in total floor area correlates with a proportional price increase.
- Energy-efficient houses tend to be higher in price.
- Notable price variations exist between the eastern and western constituencies of Bournemouth.



## Dependencies
- R (v4.0+)
- ggplot2
- shiny
- dplyr
- tidymodels


