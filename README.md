# Australian Wine Sales Forecasting Dashboard

**Course:** ADS-506 – Applied Time Series Analysis  
**University:** University of San Diego  
**Student:** Bereket Tarekegn  
**Assignment:** Week 5 – Data Storytelling with Shiny

## Project Overview

This project presents an interactive R Shiny dashboard for analyzing and forecasting monthly Australian wine sales from January 1980 through December 1994.

The dashboard allows users to explore historical sales patterns, compare forecasting models, and evaluate prediction accuracy across six wine varieties.

## Forecasting Models

Three forecasting approaches are implemented:

- **TSLM:** Linear regression with trend and monthly seasonality.
- **ETS:** Exponential smoothing with automatically selected model specifications.
- **ARIMA:** Automatically selected autoregressive integrated moving average models.

## Dashboard Features

- Interactive selection of wine varieties and historical date ranges.
- Adjustable validation periods and forecast horizons.
- Historical sales visualizations.
- Forecast comparisons with prediction intervals.
- Training and validation accuracy metrics (RMSE, MAE, and MAPE).
- Model specifications for TSLM, ETS, and ARIMA.

## Key Findings

Forecasting performance varies across wine varieties.

Using a 12-month validation period:

- **Red wine:** ARIMA achieved the lowest RMSE (257.86).
- **Sparkling wine:** ETS achieved the lowest RMSE (444.48).
- **Rose wine:** ETS achieved the lowest RMSE (9.17).

These results suggest that different wine varieties benefit from different forecasting approaches.

## Live Shiny Application

https://bereket-tarekegn-ads506.shinyapps.io/australian-wine-forecast/

## Project Files

- `app.R` — R Shiny application and forecasting code.
- `AustralianWines.csv` — Monthly Australian wine sales dataset.

## How to Run

1. Download the project files.
2. Open `app.R` in RStudio.
3. Install the required R packages: `shiny`, `fpp3`, `readr`, `dplyr`, `tidyr`, `ggplot2`, `DT`, and `lubridate`.
4. Click **Run App**.

