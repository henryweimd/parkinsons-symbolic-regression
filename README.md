# Symbolic Regression vs Traditional Models (Parkinson's Dataset)

This repository contains an interactive R Shiny application that compares the performance and approach of **Symbolic Regression** (via Grammatical Evolution) against standard **Linear Regression** on a medical dataset.

## The Dataset
The app uses a subset of the **Parkinson's Telemonitoring Dataset** from the UCI Machine Learning Repository. It attempts to predict the clinical `motor_UPDRS` severity score using patient age and various vocal cord measurements (Jitter and Shimmer). 

*Note: A static copy of the dataset is included directly in this repository (`parkinsons_updrs.data`) to ensure the app remains functional even if the source URL goes offline.*

## What is Symbolic Regression?
Instead of assuming a pre-defined mathematical structure (like $y = mx + b$), Symbolic Regression uses evolutionary algorithms to "invent" mathematical formulas from scratch. The algorithm starts with random math operators (addition, sine, logs) and variables, evaluates how well they fit the data, and breeds the best equations together over multiple generations to find an optimal formula.

## Running the App
This app relies on the `gramEvol` and `shiny` R packages. It has been optimized to be lightweight and fast, meaning you can run it purely in the browser using WebAssembly.

To run it instantly in your browser without installing R:
1. Copy the contents of `app.R`
2. Paste it into the [Shinylive Editor](https://shinylive.io/r/editor/)
3. Click "Run"

Alternatively, you can connect this repository to **Posit Connect Cloud** for automatic Git-backed deployments.
